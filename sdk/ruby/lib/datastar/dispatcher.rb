# frozen_string_literal: true

module Datastar
  class Dispatcher
    BLANK_BODY = [].freeze
    SSE_CONTENT_TYPE = 'text/event-stream'
    HTTP_ACCEPT = 'HTTP_ACCEPT'

    attr_reader :request, :response

    def initialize(
      request:, 
      response: nil, 
      view_context: nil, 
      spawner: Datastar.config.spawner,
      error_callback: Datastar.config.error_callback
    )
      @on_connect = []
      @on_client_disconnect = []
      @on_server_disconnect = []
      @on_error = [error_callback]
      @streamers = []
      @queue = nil
      @spawner = spawner
      @view_context = view_context
      @request = request
      @response = Rack::Response.new(BLANK_BODY, 200, response&.headers || {})
      @response.content_type = SSE_CONTENT_TYPE
      @response.headers['Cache-Control'] = 'no-cache'
      @response.headers['Connection'] = 'keep-alive'
      @response.delete_header 'Content-Length'
    end

    def sse?
      @request.get_header(HTTP_ACCEPT) == SSE_CONTENT_TYPE
    end

    def on_connect(callable = nil, &block)
      @on_connect << (callable || block)
      self
    end

    def on_client_disconnect(callable = nil, &block)
      @on_client_disconnect << (callable || block)
      self
    end

    def on_server_disconnect(callable = nil, &block)
      @on_server_disconnect << (callable || block)
      self
    end

    def on_error(callable = nil, &block)
      @on_error << (callable || block)
      self
    end

    def signals
      @signals ||= parse_signals(request).freeze
    end

    def merge_fragments(fragments, options = BLANK_OPTIONS)
      stream do |stream|
        stream.merge_fragments(fragments, options)
      end
    end

    def remove_fragments(selector, options = BLANK_OPTIONS)
      stream do |stream|
        stream.remove_fragments(selector, options)
      end
    end

    def merge_signals(signals, options = BLANK_OPTIONS)
      stream do |stream|
        stream.merge_signals(signals, options)
      end
    end

    def remove_signals(paths, options = BLANK_OPTIONS)
      stream do |stream|
        stream.remove_signals(paths, options)
      end
    end

    def execute_script(script, options = BLANK_OPTIONS)
      stream do |stream|
        stream.execute_script(script, options)
      end
    end

    def stream(streamer = nil, &block)
      raise SSEUnsupportedError.new(request.get_header(HTTP_ACCEPT)) unless sse?

      streamer ||= block
      @streamers << streamer

      body = if @streamers.size == 1
               proc do |out|
                 conn_generator = ServerSentEventGenerator.new(out, signals:, view_context: @view_context)
                 @on_connect.each { |callable| callable.call(conn_generator) }

                 streamer.call(conn_generator)

                 @on_server_disconnect.each { |callable| callable.call(conn_generator) }
               rescue IOError, Errno::EPIPE, Errno::ECONNRESET => e
                 @on_client_disconnect.each { |callable| callable.call(out) }
               rescue Exception => e
                 @on_error.each { |callable| callable.call(e) }
               ensure
                 # Ensure the stream is closed
                 out.close
               end
             else
               @queue ||= Queue.new

               proc do |out|
                 signs = signals
                 conn_generator = ServerSentEventGenerator.new(out, signals: signs, view_context: @view_context)
                 @on_connect.each { |callable| callable.call(conn_generator) }

                 threads = @streamers.map do |streamer|
                  @spawner.spawn do
                     # TODO: Review thread-safe view context
                     generator = ServerSentEventGenerator.new(@queue, signals: signs, view_context: @view_context)
                     streamer.call(generator)
                     @queue << :done
                   rescue StandardError => ex
                    @queue << ex
                   end
                 end

                 done_count = 0

                 while (data = @queue.pop)
                   if data == :done
                     done_count += 1
                     if done_count == threads.size
                       @queue << nil
                     end
                   elsif data.is_a?(Exception)
                    raise data
                   else
                     out << data
                   end
                 end

                 @on_server_disconnect.each { |callable| callable.call(conn_generator) }
               rescue IOError, Errno::EPIPE, Errno::ECONNRESET => e
                 @on_client_disconnect.each { |callable| callable.call(out) }
               rescue Exception => e
                 @on_error.each { |callable| callable.call(e) }
               ensure
                 threads&.each(&:kill)
                 out.close
               end
             end

      response.body = body
    end

    private

    def parse_signals(request)
      if request.post? || request.put? || request.patch?
        payload = request.env['action_dispatch.request.request_parameters']
        if payload
          return payload['event'] || {}
        elsif request.media_type == 'application/json'
          request.body.rewind
          return JSON.parse(request.body.read)
        elsif request.media_type == 'multipart/form-data'
          return request.params
        end
      else
        query = request.params['datastar']
        return query ? JSON.parse(query) : request.params
      end

      {}
    end
  end
end
