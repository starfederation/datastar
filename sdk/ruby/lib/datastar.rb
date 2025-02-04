# frozen_string_literal: true

require_relative 'datastar/version'
require_relative 'datastar/consts'

module Datastar
  BLANK_OPTIONS = {}.freeze

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
require_relative 'datastar/railtie' if defined?(Rails::Railtie)
