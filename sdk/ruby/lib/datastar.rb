# frozen_string_literal: true

require_relative 'datastar/version'

module Datastar
  class Error < StandardError; end
  # Your code goes here...
end

require_relative 'datastar/server_sent_event_generator'
