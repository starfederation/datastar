# frozen_string_literal: true

module Datastar
  class Dispatcher
    BLANK_BODY = [].freeze
    SSE_CONTENT_TYPE = 'text/event-stream'
    HTTP_ACCEPT = 'HTTP_ACCEPT'

    attr_reader :request, :response

    def initialize(request:, response:, view_context: nil)
      @view_context = view_context
      @request = request
      @response = Rack::Response.new(BLANK_BODY, 200, response.headers)
      @response.content_type = SSE_CONTENT_TYPE
      @response.headers['Cache-Control'] = 'no-cache'
      @response.delete_header 'Content-Length'
    end

    def sse?
      @request.get_header(HTTP_ACCEPT) == SSE_CONTENT_TYPE
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
        streamer.call(ServerSentEventGenerator.new(out, view_context: @view_context))
        # TODO: some exceptions such as Puma::ConnectionError
        # mean the client disconnected, which is not really an error
        # and perhaps should tigger an on_close callback
        # Other excetions should be re-raised
      ensure
        # Ensure the stream is closed
        # TODO: trigger on_close callback here
        out.close
      end

      response.body = body
    end
  end
end
