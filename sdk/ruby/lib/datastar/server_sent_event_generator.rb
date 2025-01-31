# frozen_string_literal: true

require 'json'

module Datastar
  class ServerSentEventGenerator
    SSE_OPTION_MAPPING = {
      'eventId' => 'id',
      'retryDuration' => 'retry',
      'id' => 'id',
      'retry' => 'retry',
    }.freeze

    def initialize(stream, view_context: nil)
      @stream = stream
      @view_context = view_context
    end

    def merge_fragments(fragments, options = BLANK_OPTIONS)
      # Support Phlex components
      fragments = fragments.call(view_context:) if fragments.respond_to?(:call)
      fragment_lines = fragments.to_s.split("\n")

      buffer = +"event: datastar-merge-fragments\n"
      build_options(options, buffer)
      fragment_lines.each { |line| buffer << "data: fragments #{line}\n" }

      buffer << "\n"
      @stream << buffer
    end

    def remove_fragments(selector, options = BLANK_OPTIONS)
      buffer = +"event: datastar-remove-fragments\n"
      build_options(options, buffer)
      buffer << "data: selector #{selector}\n\n"
      @stream << buffer
    end

    def merge_signals(signals, options = BLANK_OPTIONS)
      signals = JSON.dump(signals) unless signals.is_a?(String)

      buffer = +"event: datastar-merge-signals\n"
      build_options(options, buffer)
      buffer << "data: signals #{signals}\n\n"
      @stream << buffer
    end

    def remove_signals(paths, options = BLANK_OPTIONS)
      paths = [paths].flatten

      buffer = +"event: datastar-remove-signals\n"
      build_options(options, buffer)
      paths.each { |path| buffer << "data: paths #{path}\n" }
      buffer << "\n"
      @stream << buffer
    end

    def execute_script(script, options = BLANK_OPTIONS)
      buffer = +"event: datastar-execute-script\n"
      build_options(options, buffer)
      buffer << "data: script #{script}\n\n"
      @stream << buffer
    end

    private

    attr_reader :view_context, :stream

    def build_options(options, buffer)
      options.each do |k, v|
        k = camelize(k)
        if (sse_key = SSE_OPTION_MAPPING[k])
          buffer << "#{sse_key}: #{v}\n"
        else
          buffer << "data: #{k} #{v}\n"
        end
      end
    end

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end
  end
end
