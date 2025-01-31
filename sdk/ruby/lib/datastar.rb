# frozen_string_literal: true

require_relative 'datastar/version'

module Datastar
  BLANK_OPTIONS = {}.freeze

  Error = Class.new(StandardError)

  class SSEUnsupportedError < Error
    def initialize(accept)
      super("Server Sent Events are not supported for Accept: #{accept}")
    end
  end

  def self.new(request:, response:, view_context: nil)
    Dispatcher.new(request:, response:, view_context:)
  end
end

require_relative 'datastar/dispatcher'
require_relative 'datastar/server_sent_event_generator'
