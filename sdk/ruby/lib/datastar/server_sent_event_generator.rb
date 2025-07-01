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
          Consts::PATCH_MODE_DATALINE_LITERAL => Consts::ElementPatchMode::REMOVE,
          selector:
        )
      )
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
      options = options.dup
      auto_remove = options.key?(:auto_remove) ? options.delete(:auto_remove) : Consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE
      attributes = options.delete(:attributes) || BLANK_OPTIONS
      script_tag = +"<script"
      attributes.each do |k, v|
        script_tag << %( #{camelize(k)}="#{v}")
      end
      script_tag << %( onload="this.remove()") if auto_remove
      script_tag << ">#{script}</script>"

      options[Consts::SELECTOR_DATALINE_LITERAL] = 'body'
      options[Consts::PATCH_MODE_DATALINE_LITERAL] = Consts::ElementPatchMode::APPEND

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
            default_value = ATTRIBUTE_DEFAULTS[kk.to_s]
            buffer << "data: #{k} #{kk} #{vv}\n" unless vv == default_value
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

    def camelize(str)
      str.to_s.split('_').map.with_index { |word, i| i == 0 ? word : word.capitalize }.join
    end
  end
end
