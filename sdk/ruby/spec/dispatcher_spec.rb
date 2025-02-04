# frozen_string_literal: true

class TestSocket
  attr_reader :lines, :open
  def initialize
    @lines = []
    @open = true
  end

  def <<(line)
    @lines << line
  end

  def close = @open = false
end

RSpec.describe Datastar::Dispatcher do
  subject(:dispatcher) { Datastar.new(request:, response:, view_context:) }

  let(:request) { build_request('/events') }
  let(:response) { Rack::Response.new(nil, 200) }
  let(:view_context) { double('View context') }

  describe '#initialize' do
    it 'sets Content-Type to text/event-stream' do
      expect(dispatcher.response['Content-Type']).to eq('text/event-stream')
    end

    it 'sets Cache-Control to no-cache' do
      expect(dispatcher.response['Cache-Control']).to eq('no-cache')
    end

    it 'sets Connection to keep-alive' do
      expect(dispatcher.response['Connection']).to eq('keep-alive')
    end
  end

  describe '#merge_fragments' do
    it 'produces a streameable response body with D* fragments' do
      dispatcher.merge_fragments %(<div id="foo">\n<span>hello</span>\n</div>\n)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq(["event: datastar-merge-fragments\ndata: fragments <div id=\"foo\">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n\n"])
    end

    it 'takes D* options' do
      dispatcher.merge_fragments(
        %(<div id="foo">\n<span>hello</span>\n</div>\n),
        id: 72,
        retry_duration: 2000,
        settle_duration: 1000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-fragments\nid: 72\nretry: 2000\ndata: settleDuration 1000\ndata: fragments <div id="foo">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n\n)])
    end

    it 'omits retry if using default value' do
      dispatcher.merge_fragments(
        %(<div id="foo">\n<span>hello</span>\n</div>\n),
        id: 72,
        retry_duration: 1000,
        settle_duration: 1000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-fragments\nid: 72\ndata: settleDuration 1000\ndata: fragments <div id="foo">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n\n)])
    end

    it 'works with #call(view_context:) interfaces' do
      template_class = Class.new do
        def self.call(view_context:) = %(<div id="foo">\n<span>#{view_context}</span>\n</div>\n)
      end

      dispatcher.merge_fragments(
        template_class,
        id: 72,
        retry_duration: 2000,
        settle_duration: 1000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.lines).to eq([%(event: datastar-merge-fragments\nid: 72\nretry: 2000\ndata: settleDuration 1000\ndata: fragments <div id="foo">\ndata: fragments <span>#{view_context}</span>\ndata: fragments </div>\n\n\n)])
    end
  end

  describe '#remove_fragments' do
    it 'produces D* remove fragments' do
      dispatcher.remove_fragments('#list-item-1')
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-fragments\ndata: selector #list-item-1\n\n\n)])
    end

    it 'takes D* options' do
      dispatcher.remove_fragments('#list-item-1', id: 72, settle_duration: 1000)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-fragments\nid: 72\ndata: settleDuration 1000\ndata: selector #list-item-1\n\n\n)])
    end
  end

  describe '#merge_signals' do
    it 'produces a streameable response body with D* signals' do
      dispatcher.merge_signals %({ "foo": "bar" })
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\ndata: signals { "foo": "bar" }\n\n\n)])
    end

    it 'takes a Hash of signals' do
      dispatcher.merge_signals(foo: 'bar')
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\ndata: signals {"foo":"bar"}\n\n\n)])
    end

    it 'takes D* options' do
      dispatcher.merge_signals({foo: 'bar'}, event_id: 72, retry_duration: 2000, only_if_missing: true)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\nid: 72\nretry: 2000\ndata: onlyIfMissing true\ndata: signals {"foo":"bar"}\n\n\n)])
    end
  end

  describe '#remove_signals' do
    it 'produces a streameable response body with D* remove-signals' do
      dispatcher.remove_signals ['user.name', 'user.email']
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-signals\ndata: paths user.name\ndata: paths user.email\n\n\n)])
    end

    it 'takes D* options' do
      dispatcher.remove_signals 'user.name', event_id: 72, retry_duration: 2000
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-signals\nid: 72\nretry: 2000\ndata: paths user.name\n\n\n)])
    end
  end

  describe '#execute_script' do
    it 'produces a streameable response body with D* execute-script' do
      dispatcher.execute_script %(alert('hello'))
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: script alert('hello')\n\n\n)])
    end

    it 'splits multi-line script into multiple data lines' do
      dispatcher.execute_script %(alert('hello');\nalert('world'))
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: script alert('hello');\ndata: script alert('world')\n\n\n)])
    end

    it 'takes D* options' do
      dispatcher.execute_script %(alert('hello')), event_id: 72, auto_remove: false
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\nid: 72\ndata: autoRemove false\ndata: script alert('hello')\n\n\n)])
    end

    it 'omits autoRemove true' do
      dispatcher.execute_script %(alert('hello')), event_id: 72, auto_remove: true
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\nid: 72\ndata: script alert('hello')\n\n\n)])
    end

    it 'takes attributes Hash' do
      dispatcher.execute_script %(alert('hello')), attributes: { type: 'text/javascript', title: 'alert' }
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: attributes type text/javascript\ndata: attributes title alert\ndata: script alert('hello')\n\n\n)])
    end

    it 'takes attributes Hash' do
      dispatcher.execute_script %(alert('hello')), attributes: { type: 'module', title: 'alert' }
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: attributes title alert\ndata: script alert('hello')\n\n\n)])
    end
  end

  describe '#signals' do
    context 'with POST request' do
      specify 'Rails parsed parameters' do
        request = build_request(
          '/events', 
          method: 'POST', 
          headers: { 
            'action_dispatch.request.request_parameters' => { 'event' => { 'foo' => 'bar' } }
          }
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq({ 'foo' => 'bar' })
      end

      specify 'no signals in Rails parameters' do
        request = build_request(
          '/events', 
          method: 'POST', 
          headers: { 
            'action_dispatch.request.request_parameters' => {}
          }
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq({})
      end

      specify 'JSON request with signals in body' do
        request = build_request(
          '/events', 
          method: 'POST', 
          content_type: 'application/json',
          body: %({ "foo": "bar" })
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq({ 'foo' => 'bar' })
      end

      specify 'multipart form request' do
        request = build_request(
          '/events', 
          method: 'POST', 
          content_type: 'multipart/form-data',
          body: 'user[name]=joe&user[email]=joe@email.com'
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq('user' => { 'name' => 'joe', 'email' => 'joe@email.com' })
      end
    end

    context 'with GET request' do
      specify 'with signals in ?datastar=[JSON signals]' do
        query = %({"foo":"bar"})
        request = build_request(
          %(/events?datastar=#{URI.encode_uri_component(query)}), 
          method: 'GET', 
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq('foo' => 'bar')
      end

      specify 'with no signals' do
        request = build_request(
          %(/events), 
          method: 'GET', 
        )

        dispatcher = Datastar.new(request:, response:)
        expect(dispatcher.signals).to eq({})
      end
    end
  end

  describe '#stream' do
    it 'writes multiple events to socket' do
      dispatcher.stream do |sse|
        sse.merge_fragments %(<div id="foo">\n<span>hello</span>\n</div>\n)
        sse.merge_signals(foo: 'bar')
      end

      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines.size).to eq(2)
      expect(socket.lines[0]).to eq("event: datastar-merge-fragments\ndata: fragments <div id=\"foo\">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n\n")
      expect(socket.lines[1]).to eq("event: datastar-merge-signals\ndata: signals {\"foo\":\"bar\"}\n\n\n")
    end

    it 'spawns multiple streams in threads, triggering callbacks only once' do
      disconnects = []

      dispatcher = Datastar
                    .new(request:, response:)
                    .on_server_disconnect { |_| disconnects << true }
        .on_error { |err| puts err.backtrace.join("\n") }

      dispatcher.stream do |sse|
        sleep 0.01
        sse.merge_fragments %(<div id="foo">\n<span>hello</span>\n</div>\n)
      end

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end

      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines.size).to eq(2)
      expect(socket.lines[0]).to eq("event: datastar-merge-signals\ndata: signals {\"foo\":\"bar\"}\n\n\n")
      expect(socket.lines[1]).to eq("event: datastar-merge-fragments\ndata: fragments <div id=\"foo\">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n\n")
      expect(disconnects).to eq([true])
    end

    it 'catches exceptions raised from threads' do
      Thread.report_on_exception = false
      errs = []

      dispatcher = Datastar
                    .new(request:, response:)
                    .on_error { |err| errs << err }

      dispatcher.stream do |sse|
        sleep 0.01
        raise ArgumentError, 'Invalid argument'
      end

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end

      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(errs.first).to be_a(ArgumentError)
      Thread.report_on_exception = true
    end

    specify '#signals' do
      request = build_request(
        %(/events), 
        method: 'POST', 
        content_type: 'multipart/form-data',
        body: 'user[name]=joe&user[email]=joe@email.com'
      )

      dispatcher = Datastar.new(request:, response:)
      signals = nil

      dispatcher.stream do |sse|
        signals = sse.signals
      end
      socket = TestSocket.new
      dispatcher.response.body.call(socket)

      expect(signals['user']['name']).to eq('joe')
    end

    specify '#on_connect' do
      connected = false
      dispatcher.on_connect { |conn| connected = true }
      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      socket = TestSocket.new
      # allow(socket).to receive(:<<).and_raise(Errno::EPIPE, 'Socket closed')
      #
      dispatcher.response.body.call(socket)
      expect(connected).to be(true)
    end

    specify '#on_client_disconnect' do
      events = []
      dispatcher
        .on_connect { |conn| events << true }
        .on_client_disconnect { |conn| events << false }

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      socket = TestSocket.new
      allow(socket).to receive(:<<).and_raise(Errno::EPIPE, 'Socket closed')
      
      dispatcher.response.body.call(socket)
      expect(events).to eq([true, false])
    end

    specify '#on_server_disconnect' do
      events = []
      dispatcher
        .on_connect { |conn| events << true }
        .on_server_disconnect { |conn| events << false }

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      socket = TestSocket.new
      
      dispatcher.response.body.call(socket)
      expect(events).to eq([true, false])
    end

    specify '#on_error' do
      errors = []
      dispatcher.on_error { |ex| errors << ex }

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      socket = TestSocket.new
      allow(socket).to receive(:<<).and_raise(ArgumentError, 'Invalid argument')
      
      dispatcher.response.body.call(socket)
      expect(errors.first).to be_a(ArgumentError)
    end

    specify 'with global on_error' do
      errs = []
      Datastar.config.on_error { |ex| errs << ex }
      socket = TestSocket.new
      allow(socket).to receive(:<<).and_raise(ArgumentError, 'Invalid argument')
      
      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      dispatcher.response.body.call(socket)
      expect(errs.first).to be_a(ArgumentError)
    end
  end

  private

  def build_request(path, method: 'GET', body: nil, content_type: 'application/json', accept: 'text/event-stream', headers: {})
    headers = { 
      'HTTP_ACCEPT' => accept, 
      'CONTENT_TYPE' => content_type,
      'REQUEST_METHOD' => method,
      Rack::RACK_INPUT => body ? StringIO.new(body) : nil
    }.merge(headers)

    Rack::Request.new(Rack::MockRequest.env_for(path, headers))
  end
end
