# frozen_string_literal: true

require 'json'

module Datastar
  class ServerSentEventGenerator
    MSG_END = "\n\n"

    SSE_OPTION_MAPPING = {
      'eventId' => 'id',
      'retryDuration' => 'retry',
      'id' => 'id',
      'retry' => 'retry',
    }.freeze

    OPTION_DEFAULTS = {
      'retry' => Consts::DEFAULT_SSE_RETRY_DURATION,
      Consts::AUTO_REMOVE_DATALINE_LITERAL => Consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE,
      Consts::PATCH_MODE_DATALINE_LITERAL => Consts::DEFAULT_ELEMENT_PATCH_MODE,
      Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL => Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS,
      Consts::ONLY_IF_MISSING_DATALINE_LITERAL => Consts::DEFAULT_MERGE_SIGNALS_ONLY_IF_MISSING,
    }.freeze

    # ATTRIBUTE_DEFAULTS = {
    #   'type' => 'module'
    # }.freeze
    ATTRIBUTE_DEFAULTS = Consts::DEFAULT_EXECUTE_SCRIPT_ATTRIBUTES
      .split("\n")
      .map { |attr| attr.split(' ') }
      .to_h
      .freeze

    attr_reader :signals

    def initialize(stream, signals:, view_context: nil)
      @stream = stream
      @signals = signals
      @view_context = view_context
    end

    # Sometimes we'll want to run periodic checks to ensure the connection is still alive
    # ie. the browser hasn't disconnected
    # For example when idle listening on an event bus.
    def check_connection!
      @stream << MSG_END
    end

    def patch_elements(elements, options = BLANK_OPTIONS)
      # Support Phlex components
      # And Rails' #render_in interface
      elements = if elements.respond_to?(:render_in)
        elements.render_in(view_context)
      elsif elements.respond_to?(:call)
        elements.call(view_context:)
      else
        elements.to_s
      end

      element_lines = elements.to_s.split("\n")

      buffer = +"event: datastar-patch-elements\n"
      build_options(options, buffer)
      element_lines.each { |line| buffer << "data: #{Consts::ELEMENTS_DATALINE_LITERAL} #{line}\n" }

      write(buffer)
    end

    def remove_fragments(selector, options = BLANK_OPTIONS)
      buffer = +"event: datastar-remove-fragments\n"
      build_options(options, buffer)
      buffer << "data: selector #{selector}\n"
      write(buffer)
    end

    def merge_signals(signals, options = BLANK_OPTIONS)
      signals = JSON.dump(signals) unless signals.is_a?(String)

      buffer = +"event: datastar-merge-signals\n"
      build_options(options, buffer)
      buffer << "data: signals #{signals}\n"
      write(buffer)
    end

    def remove_signals(paths, options = BLANK_OPTIONS)
      paths = [paths].flatten

      buffer = +"event: datastar-remove-signals\n"
      build_options(options, buffer)
      paths.each { |path| buffer << "data: paths #{path}\n" }
      write(buffer)
    end

    def execute_script(script, options = BLANK_OPTIONS)
      buffer = +"event: datastar-execute-script\n"
      build_options(options, buffer)
      scripts = script.to_s.split("\n")
      scripts.each do |sc|
        buffer << "data: script #{sc}\n"
      end
      write(buffer)
    end

    def redirect(url)
      execute_script %(setTimeout(() => { window.location = '#{url}' }))
    end

    def write(buffer)
      buffer << MSG_END
      @stream << buffer
    end

    private

    attr_reader :view_context, :stream

    def build_options(options, buffer)
      options.each do |k, v|
        k = camelize(k)
        if (sse_key = SSE_OPTION_MAPPING[k])
          default_value = OPTION_DEFAULTS[sse_key]
          buffer << "#{sse_key}: #{v}\n" unless v == default_value
        elsif v.is_a?(Hash)
          v.each do |kk, vv| 
            default_value = ATTRIBUTE_DEFAULTS[kk.to_s]
            buffer << "data: #{k} #{kk} #{vv}\n" unless vv == default_value
          end
        else
          default_value = OPTION_DEFAULTS[k]
          buffer << "data: #{k} #{v}\n" unless v == default_value
        end
      end
    end

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end
  end
end
