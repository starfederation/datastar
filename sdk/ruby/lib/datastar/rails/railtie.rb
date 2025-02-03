# frozen_string_literal: true

module Datastar
  module Rails
    class Railtie < ::Rails::Railtie
      class RailsThreadSpawner
        def spawn(&block)
          Thread.new do
            ::Rails.application.executor.wrap(&block)
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
end
