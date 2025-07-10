# frozen_string_literal: true

require 'json'

module Datastar
  class ServerSentEventGenerator
    MSG_END = "\n"

    SSE_OPTION_MAPPING = {
      'eventId' => 'id',
      'retryDuration' => 'retry',
      'id' => 'id',
      'retry' => 'retry',
    }.freeze

    OPTION_DEFAULTS = {
      'retry' => Consts::DEFAULT_SSE_RETRY_DURATION,
      Consts::MODE_DATALINE_LITERAL => Consts::DEFAULT_ELEMENT_PATCH_MODE,
      Consts::USE_VIEW_TRANSITION_DATALINE_LITERAL => Consts::DEFAULT_ELEMENTS_USE_VIEW_TRANSITIONS,
      Consts::ONLY_IF_MISSING_DATALINE_LITERAL => Consts::DEFAULT_PATCH_SIGNALS_ONLY_IF_MISSING,
    }.freeze

    SIGNAL_SEPARATOR = '.'

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
      elements = Array(elements).compact
      rendered_elements = elements.map do |element|
        render_element(element)
      end

      element_lines = rendered_elements.flat_map do |el|
        el.to_s.split("\n")
      end

      buffer = +"event: datastar-patch-elements\n"
      build_options(options, buffer)
      element_lines.each { |line| buffer << "data: #{Consts::ELEMENTS_DATALINE_LITERAL} #{line}\n" }

      write(buffer)
    end

    def remove_elements(selector, options = BLANK_OPTIONS)
      patch_elements(
        nil, 
        options.merge(
          Consts::MODE_DATALINE_LITERAL => Consts::ElementPatchMode::REMOVE,
          selector:
        )
      )
    end

    def patch_signals(signals, options = BLANK_OPTIONS)
      buffer = +"event: datastar-patch-signals\n"
      build_options(options, buffer)
      case signals
      when Hash
        signals = JSON.dump(signals)
        buffer << "data: signals #{signals}\n"
      when String
        multi_data_lines(signals, buffer, Consts::SIGNALS_DATALINE_LITERAL)
      end
      write(buffer)
    end

    def remove_signals(paths, options = BLANK_OPTIONS)
      paths = [paths].flatten
      signals = paths.each.with_object({}) do |path, acc|
        parts = path.split(SIGNAL_SEPARATOR)
        set_nested_value(acc, parts, nil)
      end

      patch_signals(signals, options)
    end

    def execute_script(script, options = BLANK_OPTIONS)
      options = camelize_keys(options)
      auto_remove = options.key?('autoRemove') ? options.delete('autoRemove') : true
      attributes = options.delete('attributes') || BLANK_OPTIONS
      script_tag = +"<script"
      attributes.each do |k, v|
        script_tag << %( #{camelize(k)}="#{v}")
      end
      script_tag << %( data-effect="el.remove()") if auto_remove
      script_tag << ">#{script}</script>"

      options[Consts::SELECTOR_DATALINE_LITERAL] = 'body'
      options[Consts::MODE_DATALINE_LITERAL] = Consts::ElementPatchMode::APPEND

      patch_elements(script_tag, options)
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

    # Support Phlex components
    # And Rails' #render_in interface
    def render_element(element)
      if element.respond_to?(:render_in)
        element.render_in(view_context)
      elsif element.respond_to?(:call)
        element.call(view_context:)
      else
        element
      end
    end

    def build_options(options, buffer)
      options.each do |k, v|
        k = camelize(k)
        if (sse_key = SSE_OPTION_MAPPING[k])
          default_value = OPTION_DEFAULTS[sse_key]
          buffer << "#{sse_key}: #{v}\n" unless v == default_value
        elsif v.is_a?(Hash)
          v.each do |kk, vv| 
            buffer << "data: #{k} #{kk} #{vv}\n"
          end
        elsif v.is_a?(Array)
          if k == Consts::SELECTOR_DATALINE_LITERAL
            buffer << "data: #{k} #{v.join(', ')}\n"
          else
            buffer << "data: #{k} #{v.join(' ')}\n"
          end
        else
          default_value = OPTION_DEFAULTS[k]
          buffer << "data: #{k} #{v}\n" unless v == default_value
        end
      end
    end

    def camelize_keys(options)
      options.each.with_object({}) do |(key, value), acc|
        value = camelize_keys(value) if value.is_a?(Hash)
        acc[camelize(key)] = value
      end
    end

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end

    # Take a string, split it by newlines,
    # and write each line as a separate data line
    def multi_data_lines(data, buffer, key)
      lines = data.to_s.split("\n")
      lines.each do |line|
        buffer << "data: #{key} #{line}\n"
      end
    end

    def set_nested_value(hash, path, value)
      # Navigate to the parent hash using all but the last segment
      parent = path[0...-1].reduce(hash) do |current_hash, key|
        current_hash[key] ||= {}
      end

      # Set the final key to the value
      parent[path.last] = value
    end
  end
end
