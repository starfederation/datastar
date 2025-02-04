# frozen_string_literal: true

require 'async'
require 'async/queue'

module Datastar
  class AsyncExecutor
    def initialize
      # Async::Task instances
      # that raise exceptions log
      # the error with :warn level,
      # even if the exception is handled upstream
      # See https://github.com/socketry/async/blob/9851cb945ae49a85375d120219000fe7db457307/lib/async/task.rb#L204
      # Not great to silence these logs for ALL tasks
      # in a Rails app (I ony want to silence them for Datastar tasks)
      Console.logger.disable(Async::Task)
    end

    def new_queue = Async::Queue.new

    def prepare(response); end

    def spawn(&block)
      Async(&block)
    end

    def stop(threads)
      threads.each(&:stop)
    end
  end
end
