# frozen_string_literal: true

module Datastar
  class ThreadSpawner
    def spawn(&block)
      Thread.new do
        Thread.current.abort_on_exception = true
        block.call
      end
    end
  end

  class Configuration
    NOOP_CALLBACK = ->(_error) {}

    attr_accessor :spawner, :error_callback

    def initialize
      @spawner = ThreadSpawner.new
      @error_callback = NOOP_CALLBACK
    end

    def on_error(callable = nil, &block)
      @error_callback = callable || block
      self
    end
  end
end
