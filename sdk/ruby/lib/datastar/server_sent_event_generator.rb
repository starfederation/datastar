# frozen_string_literal: true

require 'json'

module Datastar
  class ServerSentEventGenerator
    def initialize(stream, view_context: nil)
      @stream = stream
      @view_context = view_context
    end

    def merge_fragments(fragments, options = BLANK_OPTIONS)
      # Support Phlex components
      fragments = fragments.call(view_context:) if fragments.respond_to?(:call)

      data = build_options(options)
      data_lines = fragments.to_s.split("\n")
      data.concat(data_lines.map { |d| "fragments #{d}" })

      data_str = data.map { |d| "data: #{d}" }.join("\n")
      @stream << %(event: datastar-merge-fragments\n#{data_str}\n\n)
    end

    def merge_signals(signals, options = BLANK_OPTIONS)
      signals = JSON.dump(signals) unless signals.is_a?(String)

      data = build_options(options)
      data.concat([%(signals #{signals})])

      data_str = data.map { |d| "data: #{d}" }.join("\n")
      @stream << %(event: datastar-merge-signals\n#{data_str}\n\n)
    end

    private

    attr_reader :view_context, :stream

    def build_options(options)
      options.each.with_object([]) do |(k, v), acc|
        acc << "#{camelize(k)} #{v}"
      end
    end

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end
  end
end
