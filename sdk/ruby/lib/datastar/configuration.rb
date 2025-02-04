# frozen_string_literal: true

require 'thread'

module Datastar
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

  class Configuration
    NOOP_CALLBACK = ->(_error) {}

    attr_accessor :executor, :error_callback

    def initialize
      @executor = ThreadExecutor.new
      @error_callback = NOOP_CALLBACK
    end

    def on_error(callable = nil, &block)
      @error_callback = callable || block
      self
    end
  end
end
