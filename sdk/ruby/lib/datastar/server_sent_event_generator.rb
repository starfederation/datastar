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
      'retry' => 1000,
      'autoRemove' => true,
      'mergeMode' => 'morph',
      'settleDuration' => 300,
      'useViewTransition' => false,
      'onlyIfMissing' => false,
    }.freeze

    ATTRIBUTE_DEFAULTS = {
      'type' => 'module'
    }.freeze

    attr_reader :signals

    def initialize(stream, signals:, view_context: nil)
      @stream = stream
      @signals = signals
      @view_context = view_context
    end

    def merge_fragments(fragments, options = BLANK_OPTIONS)
      # Support Phlex components
      fragments = fragments.call(view_context:) if fragments.respond_to?(:call)
      fragment_lines = fragments.to_s.split("\n")

      buffer = +"event: datastar-merge-fragments\n"
      build_options(options, buffer)
      fragment_lines.each { |line| buffer << "data: fragments #{line}\n" }

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
