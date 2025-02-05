# frozen_string_literal: true

require 'datastar/async_executor'

module Datastar
  class RailsAsyncExecutor < Datastar::AsyncExecutor
    def prepare(response)
      response.delete_header 'Connection'
    end

    def spawn(&block)
      Async do
        Rails.application.executor.wrap(&block)
      end
    end
  end
end
