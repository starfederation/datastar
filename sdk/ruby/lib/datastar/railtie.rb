# frozen_string_literal: true

module Datastar
  class Railtie < ::Rails::Railtie
    FINALIZE = proc do |view_context, response|
      view_context.response = response
    end

    initializer 'datastar' do |_app|
      Datastar.config.finalize = FINALIZE

      Datastar.config.executor = if config.active_support.isolation_level == :fiber
                                   require 'datastar/rails_async_executor'
                                   RailsAsyncExecutor.new
                                 else
                                   require 'datastar/rails_thread_executor'
                                   RailsThreadExecutor.new
                                 end
    end
  end
end
