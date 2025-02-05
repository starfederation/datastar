# frozen_string_literal: true

module Datastar
  # See https://guides.rubyonrails.org/threading_and_code_execution.html#wrapping-application-code
  class RailsThreadExecutor < Datastar::ThreadExecutor
    def spawn(&block)
      Thread.new do
        Rails.application.executor.wrap(&block)
      end
    end
  end
end
