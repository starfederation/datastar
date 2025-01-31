# frozen_string_literal: true

module Datastar
  class ServerSentEventGenerator
    BLANK_OPTIONS = {}.freeze

    def initialize(stream, view_context: nil)
      @stream = stream
      @view_context = view_context
    end

    def fragment(html, modifiers = {})
    end

    def merge_fragments(fragments, options = BLANK_OPTIONS)
      # Support Phlex components
      fragments = fragments.call(view_context:) if fragments.respond_to?(:call)

      data = options.each.with_object([]) do |(k, v), acc|
        acc << "#{camelize(k)} #{v}"
      end
      data_lines = fragments.to_s.split("\n")
      data.concat(data_lines.map { |d| "fragments #{d}" })

      data_str = data.map { |d| "data: #{d}" }.join("\n")
      @stream << %(event: datastar-merge-fragments\n#{data_str}\n\n)
    rescue StandardError => e
      Rails.logger.error "Error merging fragments: #{e.message}"
      raise
    end

    private

    attr_reader :view_context, :stream

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end
  end
end
