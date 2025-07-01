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
  include DispatcherExamples

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

    it 'sets X-Accel-Buffering: no for NGinx and other proxies' do
      expect(dispatcher.response['X-Accel-Buffering']).to eq('no')
    end

    it 'does not set Connection header if not HTTP/1.1' do
      request.env['SERVER_PROTOCOL'] = 'HTTP/2.0'
      expect(dispatcher.response['Connection']).to be_nil
    end
  end

  specify '.from_rack_env' do
    dispatcher = Datastar.from_rack_env(request.env)

    expect(dispatcher.response['Content-Type']).to eq('text/event-stream')
    expect(dispatcher.response['Cache-Control']).to eq('no-cache')
    expect(dispatcher.response['Connection']).to eq('keep-alive')
  end

  specify '#sse?' do
    expect(dispatcher.sse?).to be(true)
    request = build_request('/events', headers: { 'HTTP_ACCEPT' => 'application/json' })

    dispatcher = Datastar.new(request:, response:, view_context:)
    expect(dispatcher.sse?).to be(false)
  end

  describe '#patch_elements' do
    it 'produces a streameable response body with D* fragments' do
      dispatcher.patch_elements %(<div id="foo">\n<span>hello</span>\n</div>\n)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq(["event: datastar-patch-elements\ndata: elements <div id=\"foo\">\ndata: elements <span>hello</span>\ndata: elements </div>\n\n\n"])
    end

    it 'takes D* options' do
      dispatcher.patch_elements(
        %(<div id="foo">\n<span>hello</span>\n</div>\n),
        id: 72,
        retry_duration: 2000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\nid: 72\nretry: 2000\ndata: elements <div id="foo">\ndata: elements <span>hello</span>\ndata: elements </div>\n\n\n)])
    end

    it 'omits retry if using default value' do
      dispatcher.patch_elements(
        %(<div id="foo">\n<span>hello</span>\n</div>\n),
        id: 72,
        retry_duration: 1000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\nid: 72\ndata: elements <div id="foo">\ndata: elements <span>hello</span>\ndata: elements </div>\n\n\n)])
    end

    it 'works with #call(view_context:) interfaces' do
      template_class = Class.new do
        def self.call(view_context:) = %(<div id="foo">\n<span>#{view_context}</span>\n</div>\n)
      end

      dispatcher.patch_elements(
        template_class,
        id: 72,
        retry_duration: 2000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\nid: 72\nretry: 2000\ndata: elements <div id="foo">\ndata: elements <span>#{view_context}</span>\ndata: elements </div>\n\n\n)])
    end

    it 'works with #render_in(view_context, &) interfaces' do
      template_class = Class.new do
        def self.render_in(view_context) = %(<div id="foo">\n<span>#{view_context}</span>\n</div>\n)
      end

      dispatcher.patch_elements(
        template_class,
        id: 72,
        retry_duration: 2000
      )
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\nid: 72\nretry: 2000\ndata: elements <div id="foo">\ndata: elements <span>#{view_context}</span>\ndata: elements </div>\n\n\n)])
    end
  end

  describe '#remove_elements' do
    it 'produces D* patch elements with "remove" mode' do
      dispatcher.remove_elements('#list-item-1')
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\ndata: mode remove\ndata: selector #list-item-1\n\n\n)])
    end

    it 'takes D* options' do
      dispatcher.remove_elements('#list-item-1', id: 72)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\nid: 72\ndata: mode remove\ndata: selector #list-item-1\n\n\n)])
    end

    it 'takes an array of selectors' do
      dispatcher.remove_elements(%w[#item1 #item2])
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-patch-elements\ndata: mode remove\ndata: selector #item1, #item2\n\n\n)])
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
      dispatcher.execute_script %(alert('hello')), event_id: 72, auto_remove: !Datastar::Consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\nid: 72\ndata: autoRemove false\ndata: script alert('hello')\n\n\n)])
    end

    it 'omits autoRemove true' do
      dispatcher.execute_script %(alert('hello')), event_id: 72, auto_remove: Datastar::Consts::DEFAULT_EXECUTE_SCRIPT_AUTO_REMOVE
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

  describe '#redirect' do
    it 'sends an execute_script event with a window.location change' do
      dispatcher.redirect '/guide'
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: script setTimeout(() => { window.location = '/guide' })\n\n\n)])
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
      socket = TestSocket.new
      dispatcher.on_error do |ex|
        raise ex
      end
      dispatcher.stream do |sse|
        sse.patch_elements %(<div id="foo">\n<span>hello</span>\n</div>\n)
        sse.merge_signals(foo: 'bar')
      end

      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines.size).to eq(2)
      expect(socket.lines[0]).to eq("event: datastar-patch-elements\ndata: elements <div id=\"foo\">\ndata: elements <span>hello</span>\ndata: elements </div>\n\n\n")
      expect(socket.lines[1]).to eq("event: datastar-merge-signals\ndata: signals {\"foo\":\"bar\"}\n\n\n")
    end

    it 'returns a Rack array response' do
      status, headers, _body = dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      expect(status).to eq(200)
      expect(headers['content-type']).to eq('text/event-stream')
      expect(headers['cache-control']).to eq('no-cache')
      expect(headers['connection']).to eq('keep-alive')
    end

    context 'with multiple streams' do
      let(:executor) { Datastar.config.executor }

      describe 'default thread-based executor' do
        it_behaves_like 'a dispatcher handling concurrent streams'
      end

      describe 'Async-based executor' do
        around do |example|
          Sync do
            example.run
          end
        end

        let(:executor) { Datastar::AsyncExecutor.new }
        it_behaves_like 'a dispatcher handling concurrent streams'
      end
    end

    specify ':heartbeat enabled' do
      dispatcher = Datastar.new(request:, response:, heartbeat: 0.001)
      connected = true
      block_called = false
      dispatcher.on_client_disconnect { |conn| connected = false }

      socket = TestSocket.new
      allow(socket).to receive(:<<).with("\n\n").and_raise(Errno::EPIPE, 'Socket closed')

      dispatcher.stream do |sse|
        sleep 10
        block_called = true
      end

      dispatcher.response.body.call(socket)
      expect(connected).to be(false)
      expect(block_called).to be(false)
    end

    specify ':heartbeat disabled' do
      dispatcher = Datastar.new(request:, response:, heartbeat: false)
      connected = true
      block_called = false
      dispatcher.on_client_disconnect { |conn| connected = false }

      socket = TestSocket.new
      allow(socket).to receive(:<<).with("\n\n").and_raise(Errno::EPIPE, 'Socket closed')

      dispatcher.stream do |sse|
        sleep 0.001
        block_called = true
      end

      dispatcher.response.body.call(socket)
      expect(connected).to be(true)
      expect(block_called).to be(true)
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

    specify '#check_connection triggers #on_client_disconnect' do
      events = []
      dispatcher
        .on_connect { |conn| events << true }
        .on_client_disconnect { |conn| events << false }

      dispatcher.stream do |sse|
        sse.check_connection!
      end
      socket = TestSocket.new
      allow(socket).to receive(:<<).with("\n\n").and_raise(Errno::EPIPE, 'Socket closed')
      
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
      allow(Datastar.config.logger).to receive(:error)
      errors = []
      dispatcher.on_error { |ex| errors << ex }

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end
      socket = TestSocket.new
      allow(socket).to receive(:<<).and_raise(ArgumentError, 'Invalid argument')
      
      dispatcher.response.body.call(socket)
      expect(errors.first).to be_a(ArgumentError)
      expect(Datastar.config.logger).to have_received(:error).with(/ArgumentError \(Invalid argument\):/)
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
