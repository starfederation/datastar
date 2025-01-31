# frozen_string_literal: true

module Datastar
  class ServerSentEventGenerator
    SSE_CONTENT_TYPE = 'text/event-stream'
    HTTP_ACCEPT = 'HTTP_ACCEPT'

    attr_reader :request, :response

    def initialize(request:, response:)
      @request = request
      @response = response
      @response.content_type = SSE_CONTENT_TYPE
    end

    def sse?
      @request.get_header(HTTP_ACCEPT) == SSE_CONTENT_TYPE
    end
  end
end
