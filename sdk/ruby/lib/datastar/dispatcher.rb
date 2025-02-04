# frozen_string_literal: true

module Datastar
  # The Dispatcher encapsulates the logic of handling a request
  # and sending updating a response with datastar messages.
  # You'll normally instantiate a Dispatcher in your controller action of Rack handler
  # via Datastar.new.
  # @example
  #
  #  datastar = Datastar.new(request:, response:, view_context: self)
  #
  #  # One-off fragment response
  #  datastar.merge_fragments(template)
  #
  #  # Streaming response with multiple messages
  #  datastar.stream do |sse|
  #    sse.merge_fragments(template)
  #    10.times do |i|
  #      sleep 0.1
  #      sse.merge_signals(count: i)
  #    end
  #  end
  #
  class Dispatcher
    BLANK_BODY = [].freeze
    SSE_CONTENT_TYPE = 'text/event-stream'
    HTTP_ACCEPT = 'HTTP_ACCEPT'

    attr_reader :request, :response

    # @option request [Rack::Request] the request object
    # @option response [Rack::Response, nil] the response object
    # @option view_context [Object, nil] the view context object, to use when rendering templates. Ie. a controller, or Sinatra app.
    # @option executor [Object] the executor object to use for managing threads and queues
    # @option error_callback [Proc] the callback to call when an error occurs
    # @option finalize [Proc] the callback to call when the response is finalized
    def initialize(
      request:,
      response: nil,
      view_context: nil,
      executor: Datastar.config.executor,
      error_callback: Datastar.config.error_callback,
      finalize: Datastar.config.finalize
    )
      @on_connect = []
      @on_client_disconnect = []
      @on_server_disconnect = []
      @on_error = [error_callback]
      @finalize = finalize
      @streamers = []
      @queue = nil
      @executor = executor
      @view_context = view_context
      @request = request
      @response = Rack::Response.new(BLANK_BODY, 200, response&.headers || {})
      @response.content_type = SSE_CONTENT_TYPE
      @response.headers['Cache-Control'] = 'no-cache'
      @response.headers['Connection'] = 'keep-alive'
      @response.delete_header 'Content-Length'
      @executor.prepare(@response)
    end

    # Check if the request accepts SSE responses
    # @return [Boolean]
    def sse?
      @request.get_header(HTTP_ACCEPT) == SSE_CONTENT_TYPE
    end

    # Register an on-connect callback
    # Triggered when the request is handled
    # @param callable [Proc, nil] the callback to call
    # @yieldparam sse [ServerSentEventGenerator] the generator object
    # @return [self]
    def on_connect(callable = nil, &block)
      @on_connect << (callable || block)
      self
    end

    # Register a callback for client disconnection
    # Ex. when the browser is closed mid-stream
    # @param callable [Proc, nil] the callback to call
    # @return [self]
    def on_client_disconnect(callable = nil, &block)
      @on_client_disconnect << (callable || block)
      self
    end

    # Register a callback for server disconnection
    # Ex. when the server finishes serving the request
    # @param callable [Proc, nil] the callback to call
    # @return [self]
    def on_server_disconnect(callable = nil, &block)
      @on_server_disconnect << (callable || block)
      self
    end

    # Register a callback server-side exceptions
    # Ex. when one of the server threads raises an exception
    # @param callable [Proc, nil] the callback to call
    # @return [self]
    def on_error(callable = nil, &block)
      @on_error << (callable || block)
      self
    end

    # Parse and returns Datastar signals sent by the client.
    # See https://data-star.dev/guide/getting_started#data-signals
    # @return [Hash]
    def signals
      @signals ||= parse_signals(request).freeze
    end

    # Send one-off fragments to the UI
    # See https://data-star.dev/reference/sse_events#datastar-merge-fragments
    # @example
    #
    #  datastar.merge_fragments(%(<div id="foo">\n<span>hello</span>\n</div>\n))
    #  # or a Phlex view object
    #  datastar.merge_fragments(UserComponet.new)
    #
    # @param fragments [String, #call(view_context: Object) => Object] the HTML fragment or object
    # @param options [Hash] the options to send with the message
    def merge_fragments(fragments, options = BLANK_OPTIONS)
      stream do |sse|
        sse.merge_fragments(fragments, options)
      end
    end

    # One-off remove fragments from the UI
    # See https://data-star.dev/reference/sse_events#datastar-remove-fragments
    # @example
    #
    #  datastar.remove_fragments('#users')
    #
    # @param selector [String] a CSS selector for the fragment to remove
    # @param options [Hash] the options to send with the message
    def remove_fragments(selector, options = BLANK_OPTIONS)
      stream do |sse|
        sse.remove_fragments(selector, options)
      end
    end

    # One-off merge signals in the UI
    # See https://data-star.dev/reference/sse_events#datastar-merge-signals
    # @example
    #
    #  datastar.merge_signals(count: 1, toggle: true)
    #
    # @param signals [Hash] signals to merge
    # @param options [Hash] the options to send with the message
    def merge_signals(signals, options = BLANK_OPTIONS)
      stream do |sse|
        sse.merge_signals(signals, options)
      end
    end

    # One-off remove signals from the UI
    # See https://data-star.dev/reference/sse_events#datastar-remove-signals
    # @example
    #
    #  datastar.remove_signals(['user.name', 'user.email'])
    #
    # @param paths [Array<String>] object paths to the signals to remove
    # @param options [Hash] the options to send with the message
    def remove_signals(paths, options = BLANK_OPTIONS)
      stream do |sse|
        sse.remove_signals(paths, options)
      end
    end

    # One-off execute script in the UI
    # See https://data-star.dev/reference/sse_events#datastar-execute-script
    # @example
    #
    #  datastar.execute_scriprt(%(alert('Hello World!'))
    #
    # @param script [String] the script to execute
    # @param options [Hash] the options to send with the message
    def execute_script(script, options = BLANK_OPTIONS)
      stream do |sse|
        sse.execute_script(script, options)
      end
    end

    # Start a streaming response
    # A generator object is passed to the block
    # The generator supports all the Datastar methods listed above (it's the same type)
    # But you can call them multiple times to send multiple messages down an open SSE connection.
    # @example
    #
    #  datastar.stream do |sse|
    #    total = 300
    #    sse.merge_fragments(%(<progress data-signal-progress="0" id="progress" max="#{total}" data-attr-value="$progress">0</progress>))
    #    total.times do |i|
    #      sse.merge_signals(progress: i)
    #    end
    #  end
    #
    # This methods also captures exceptions raised in the block and triggers
    # any error callbacks. Client disconnection errors trigger the @on_client_disconnect callbacks.
    # Finally, when the block is done streaming, the @on_server_disconnect callbacks are triggered.
    #
    # When multiple streams are scheduled this way, 
    # this SDK will spawn each block in separate threads (or fibers, depending on executor)
    # and linearize their writes to the connection socket
    # @example
    #
    #  datastar.stream do |sse|
    #    # update things here
    #  end
    #
    #  datastar.stream do |sse|
    #    # more concurrent updates here
    #  end
    #
    # As a last step, the finalize callback is called with the view context and the response
    # This is so that different frameworks can setup their responses correctly.
    # By default, the built-in Rack finalzer just returns the resposne Array which can be used by any Rack handler.
    # On Rails, the Rails controller response is set to this objects streaming response.
    #
    # @param streamer [#call(ServerSentEventGenerator), nil] a callable to call with the generator
    # @yieldparam sse [ServerSentEventGenerator] the generator object
    # @return [Object] depends on the finalize callback
    def stream(streamer = nil, &block)
      streamer ||= block
      @streamers << streamer

      body = if @streamers.size == 1
        stream_one(streamer) 
      else
        stream_many(streamer) 
      end

      @response.body = body
      @finalize.call(@view_context, @response)
    end

    private

    # Produce a response body for a single stream
    # In this case, the SSE generator can write directly to the socket
    #
    # @param streamer [#call(ServerSentEventGenerator)]
    # @return [Proc]
    # @api private
    def stream_one(streamer)
      proc do |socket|
        generator = ServerSentEventGenerator.new(socket, signals:, view_context: @view_context)
        @on_connect.each { |callable| callable.call(generator) }
        handling_errors(generator, socket) do
          streamer.call(generator)
        end
      ensure
        socket.close
      end
    end

    # Produce a response body for multiple streams
    # Each "streamer" is spawned in a separate thread
    # and they write to a shared queue
    # Then we wait on the queue and write to the socket
    # In this way we linearize socket writes
    # Exceptions raised in streamer threads are pushed to the queue
    # so that the main thread can re-raise them and handle them linearly.
    #
    # @param streamer [#call(ServerSentEventGenerator)]
    # @return [Proc]
    # @api private
    def stream_many(streamer)
      @queue ||= @executor.new_queue

      proc do |socket|
        signs = signals
        conn_generator = ServerSentEventGenerator.new(socket, signals: signs, view_context: @view_context)
        @on_connect.each { |callable| callable.call(conn_generator) }

        threads = @streamers.map do |streamer|
          @executor.spawn do
            # TODO: Review thread-safe view context
            generator = ServerSentEventGenerator.new(@queue, signals: signs, view_context: @view_context)
            streamer.call(generator)
            @queue << :done
          rescue StandardError => e
            @queue << e
          end
        end

        handling_errors(conn_generator, socket) do
          done_count = 0

          while (data = @queue.pop)
            if data == :done
              done_count += 1
              @queue << nil if done_count == threads.size
            elsif data.is_a?(Exception)
              raise data
            else
              socket << data
            end
          end
        end
      ensure
        @executor.stop(threads) if threads
        socket.close
      end
    end

    # Run a streaming block while handling errors
    # @param generator [ServerSentEventGenerator]
    # @param socket [IO]
    # @yield
    # @api private
    def handling_errors(generator, socket, &)
      yield

      @on_server_disconnect.each { |callable| callable.call(generator) }
    rescue IOError, Errno::EPIPE, Errno::ECONNRESET => e
      @on_client_disconnect.each { |callable| callable.call(socket) }
    rescue Exception => e
      @on_error.each { |callable| callable.call(e) }
    end

    # Parse signals from the request
    # Support Rails requests with already parsed request bodies
    #
    # @param request [Rack::Request]
    # @return [Hash]
    # @api private
    def parse_signals(request)
      if request.post? || request.put? || request.patch?
        payload = request.env['action_dispatch.request.request_parameters']
        if payload
          return payload['event'] || {}
        elsif request.media_type == 'application/json'
          request.body.rewind
          return JSON.parse(request.body.read)
        elsif request.media_type == 'multipart/form-data'
          return request.params
        end
      else
        query = request.params['datastar']
        return query ? JSON.parse(query) : request.params
      end

      {}
    end
  end
end
