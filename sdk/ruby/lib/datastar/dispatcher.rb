# frozen_string_literal: true

module Datastar
  class Dispatcher
    BLANK_BODY = [].freeze
    SSE_CONTENT_TYPE = 'text/event-stream'
    HTTP_ACCEPT = 'HTTP_ACCEPT'

    attr_reader :request, :response

    def initialize(request:, response:, view_context: nil)
      @on_connect = []
      @on_disconnect = []
      @on_error = []
      @view_context = view_context
      @request = request
      @response = Rack::Response.new(BLANK_BODY, 200, response.headers)
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

    def on_disconnect(callable = nil, &block)
      @on_disconnect << (callable || block)
      self
    end

    def on_error(callable = nil, &block)
      @on_error << (callable || block)
      self
    end

    def signals
      @signals ||= parse_signals(request)
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

      body = proc do |out|
        @on_connect.each { |callable| callable.call(out) }

        streamer.call(ServerSentEventGenerator.new(out, signals:, view_context: @view_context))
        # TODO: some exceptions such as Puma::ConnectionError
        # mean the client disconnected, which is not really an error
        # and perhaps should tigger an on_close callback
        # Other exceptions should be re-raised
      rescue IOError, Errno::EPIPE, Errno::ECONNRESET => ex
        @on_disconnect.each { |callable| callable.call(out) }
      rescue Exception => ex
        @on_error.each { |callable| callable.call(ex) }
      ensure
        # Ensure the stream is closed
        out.close
      end

      response.body = body
    end
  end
end
