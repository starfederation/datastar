# frozen_string_literal: true

module Datastar
  class Railtie < ::Rails::Railtie
    # See https://guides.rubyonrails.org/threading_and_code_execution.html#wrapping-application-code
    class RailsThreadSpawner
      def spawn(&block)
        Thread.new do
          Thread.current.abort_on_exception = true
          Rails.application.executor.wrap(&block)
        end
      end
    end

    initializer 'datastar' do |_app|
      # TODO: detect configure isolation_level
      # and use a FiberSpawner if it is :fiber
      Datastar.config.spawner = RailsThreadSpawner.new
    end
  end
end
