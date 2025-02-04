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
      executor: Datastar.config.executor,
      error_callback: Datastar.config.error_callback
    )
      @on_connect = []
      @on_client_disconnect = []
      @on_server_disconnect = []
      @on_error = [error_callback]
      @streamers = []
      @queue = nil
      @executor = executor
      @view_context = view_context
      @request = request
      @response = Rack::Response.new(BLANK_BODY, 200, response&.headers || {})
      @response.content_type = SSE_CONTENT_TYPE
      @response.headers['Cache-Control'] = 'no-cache'
      @response.headers['Connection'] = 'keep-alive'
      @response.delete_header 'Content-Length'
      @executor.prepare(@response)
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
      stream do |sse|
        sse.merge_fragments(fragments, options)
      end
    end

    def remove_fragments(selector, options = BLANK_OPTIONS)
      stream do |sse|
        sse.remove_fragments(selector, options)
      end
    end

    def merge_signals(signals, options = BLANK_OPTIONS)
      stream do |sse|
        sse.merge_signals(signals, options)
      end
    end

    def remove_signals(paths, options = BLANK_OPTIONS)
      stream do |sse|
        sse.remove_signals(paths, options)
      end
    end

    def execute_script(script, options = BLANK_OPTIONS)
      stream do |sse|
        sse.execute_script(script, options)
      end
    end

    def stream(streamer = nil, &block)
      raise SSEUnsupportedError.new(request.get_header(HTTP_ACCEPT)) unless sse?

      streamer ||= block
      @streamers << streamer

      body = if @streamers.size == 1
        stream_one(streamer) 
      else
        stream_many(streamer) 
      end

      @response.body = body
      self
    end

    private

    # Produce a response body for a single stream
    # In this case, the SSE generator can write directly to the socket
    #
    # @param streamer [#call(ServerSentEventGenerator)]
    # @return [Proc]
    # @api private
    def stream_one(streamer)
      proc do |socket|
        generator = ServerSentEventGenerator.new(socket, signals:, view_context: @view_context)
        @on_connect.each { |callable| callable.call(generator) }
        handling_errors(generator, socket) do
          streamer.call(generator)
        end
      ensure
        socket.close
      end
    end

    # Produce a response body for multiple streams
    # Each "streamer" is spawned in a separate thread
    # and they write to a shared queue
    # Then we wait on the queue and write to the socket
    # In this way we linearize socket writes
    # Exceptions raised in streamer threads are pushed to the queue
    # so that the main thread can re-raise them and handle them linearly.
    #
    # @param streamer [#call(ServerSentEventGenerator)]
    # @return [Proc]
    # @api private
    def stream_many(streamer)
      @queue ||= Queue.new

      proc do |socket|
        signs = signals
        conn_generator = ServerSentEventGenerator.new(socket, signals: signs, view_context: @view_context)
        @on_connect.each { |callable| callable.call(conn_generator) }

        threads = @streamers.map do |streamer|
          @executor.spawn do
            # TODO: Review thread-safe view context
            generator = ServerSentEventGenerator.new(@queue, signals: signs, view_context: @view_context)
            streamer.call(generator)
            @queue << :done
          rescue StandardError => e
            @queue << e
          end
        end

        handling_errors(conn_generator, socket) do
          done_count = 0

          while (data = @queue.pop)
            if data == :done
              done_count += 1
              @queue << nil if done_count == threads.size
            elsif data.is_a?(Exception)
              raise data
            else
              socket << data
            end
          end
        end
      ensure
        @executor.stop(threads) if threads
        socket.close
      end
    end

    def handling_errors(generator, socket, &)
      yield

      @on_server_disconnect.each { |callable| callable.call(generator) }
    rescue IOError, Errno::EPIPE, Errno::ECONNRESET => e
      @on_client_disconnect.each { |callable| callable.call(socket) }
    rescue Exception => e
      @on_error.each { |callable| callable.call(e) }
    end

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
