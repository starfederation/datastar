# frozen_string_literal: true

require_relative 'datastar/version'

module Datastar
  def self.new(request:, response:)
    Dispatcher.new(request:, response:)
  end
end

require_relative 'datastar/dispatcher'
require_relative 'datastar/server_sent_event_generator'
