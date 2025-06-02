# frozen_string_literal: true

require 'thread'
require 'logger'

module Datastar
  # The default executor based on Ruby threads
  class ThreadExecutor
    def new_queue = Queue.new

    def prepare(response); end

    def spawn(&block)
      Thread.new(&block)
    end

    def stop(threads)
      threads.each(&:kill)
    end
  end

  # Datastar configuration
  # @example
  #
  #  Datastar.configure do |config|
  #    config.on_error do |error|
  #      Sentry.notify(error)
  #    end
  #  end
  #
  # You'd normally do this on app initialization 
  # For example in a Rails initializer
  class Configuration
    RACK_FINALIZE = ->(_view_context, response) { response.finish }
    DEFAULT_HEARTBEAT = 3

    attr_accessor :executor, :error_callback, :finalize, :heartbeat, :logger

    def initialize
      @executor = ThreadExecutor.new
      @finalize = RACK_FINALIZE
      @heartbeat = DEFAULT_HEARTBEAT
      @logger = Logger.new(STDOUT)
      @error_callback = proc do |e|
        @logger.error("#{e.class} (#{e.message}):\n#{e.backtrace.join("\n")}")
      end
    end

    def on_error(callable = nil, &block)
      @error_callback = callable || block
      self
    end
  end
end
