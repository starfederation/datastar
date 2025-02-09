# frozen_string_literal: true

module Datastar
  class Railtie < ::Rails::Railtie
    FINALIZE = proc do |view_context, response|
      case view_context
      when ActionView::Base
        view_context.controller.response = response
      else
        raise ArgumentError, 'view_context must be an ActionView::Base'
      end
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
