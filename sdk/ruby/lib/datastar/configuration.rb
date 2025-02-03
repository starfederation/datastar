# frozen_string_literal: true

module Datastar
  class ThreadSpawner
    def spawn(&block)
      Thread.new(&block)
    end
  end

  class Configuration
    attr_accessor :spawner

    def initialize
      @spawner = ThreadSpawner.new
    end
  end
end
