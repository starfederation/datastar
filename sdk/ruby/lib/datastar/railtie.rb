# frozen_string_literal: true

module Datastar
  class Railtie < ::Rails::Railtie
    # See https://guides.rubyonrails.org/threading_and_code_execution.html#wrapping-application-code
    class RailsThreadExecutor
      def prepare(response); end

      def spawn(&block)
        Thread.new do
          Rails.application.executor.wrap(&block)
        end
      end

      def stop(threads)
        threads.each(&:kill)
      end
    end

    class RailsAsyncExecutor
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

      def prepare(response)
        response.delete_header 'Connection'
      end

      def spawn(&block)
        Async do
          Rails.application.executor.wrap(&block)
        end
      end

      def stop(tasks)
        tasks.each(&:stop)
      end
    end

    initializer 'datastar' do |_app|
      Datastar.config.executor = if config.active_support.isolation_level == :fiber
                                   RailsAsyncExecutor.new
                                 else
                                   RailsThreadExecutor.new
                                 end
    end
  end
end
