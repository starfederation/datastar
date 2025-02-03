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

  def self.config
    @config ||= Configuration.new
  end

  def self.configure(&)
    yield config if block_given?
    config.freeze
    config
  end

  def self.new(...)
    Dispatcher.new(...)
  end
end

require_relative 'datastar/configuration'
require_relative 'datastar/dispatcher'
require_relative 'datastar/server_sent_event_generator'
require_relative 'datastar/rails/railtie' if defined?(Rails::Railtie)
