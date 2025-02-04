# frozen_string_literal: true

require 'thread'

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
    NOOP_CALLBACK = ->(_error) {}
    RACK_FINALIZE = ->(_view_context, response) { response.finish }

    attr_accessor :executor, :error_callback, :finalize

    def initialize
      @executor = ThreadExecutor.new
      @error_callback = NOOP_CALLBACK
      @finalize = RACK_FINALIZE
    end

    def on_error(callable = nil, &block)
      @error_callback = callable || block
      self
    end
  end
end
