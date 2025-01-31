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
  end

  describe '#merge_fragments' do
    it 'produces a streameable response body with D* fragments' do
      dispatcher.merge_fragments %(<div id="foo">\n<span>hello</span>\n</div>\n)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq(["event: datastar-merge-fragments\ndata: fragments <div id=\"foo\">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n"])
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
      expect(socket.lines).to eq([%(event: datastar-merge-fragments\nid: 72\nretry: 2000\ndata: settleDuration 1000\ndata: fragments <div id="foo">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n)])
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
      expect(socket.lines).to eq([%(event: datastar-merge-fragments\nid: 72\nretry: 2000\ndata: settleDuration 1000\ndata: fragments <div id="foo">\ndata: fragments <span>#{view_context}</span>\ndata: fragments </div>\n\n)])
    end
  end

  describe '#remove_fragments' do
    it 'produces D* remove fragments' do
      dispatcher.remove_fragments('#list-item-1')
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-fragments\ndata: selector #list-item-1\n\n)])
    end

    it 'takes D* options' do
      dispatcher.remove_fragments('#list-item-1', id: 72, settle_duration: 1000)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-fragments\nid: 72\ndata: settleDuration 1000\ndata: selector #list-item-1\n\n)])
    end
  end

  describe '#merge_signals' do
    it 'produces a streameable response body with D* signals' do
      dispatcher.merge_signals %({ "foo": "bar" })
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\ndata: signals { "foo": "bar" }\n\n)])
    end

    it 'takes a Hash of signals' do
      dispatcher.merge_signals(foo: 'bar')
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\ndata: signals {"foo":"bar"}\n\n)])
    end

    it 'takes D* options' do
      dispatcher.merge_signals({foo: 'bar'}, event_id: 72, retry_duration: 2000, only_if_missing: true)
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-merge-signals\nid: 72\nretry: 2000\ndata: onlyIfMissing true\ndata: signals {"foo":"bar"}\n\n)])
    end
  end

  describe '#remove_signals' do
    it 'produces a streameable response body with D* remove-signals' do
      dispatcher.remove_signals ['user.name', 'user.email']
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-signals\ndata: paths user.name\ndata: paths user.email\n\n)])
    end

    it 'takes D* options' do
      dispatcher.remove_signals 'user.name', event_id: 72, retry_duration: 2000
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-remove-signals\nid: 72\nretry: 2000\ndata: paths user.name\n\n)])
    end
  end

  describe '#execute_script' do
    it 'produces a streameable response body with D* execute-script' do
      dispatcher.execute_script %(alert('hello'))
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: script alert('hello')\n\n)])
    end

    it 'takes D* options' do
      dispatcher.execute_script %(alert('hello')), event_id: 72, auto_remove: true
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\nid: 72\ndata: autoRemove true\ndata: script alert('hello')\n\n)])
    end

    it 'takes attributes Hash' do
      dispatcher.execute_script %(alert('hello')), attributes: { type: 'text/javascript', title: 'alert' }
      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines).to eq([%(event: datastar-execute-script\ndata: attributes type text/javascript\ndata: attributes title alert\ndata: script alert('hello')\n\n)])
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
      expect(socket.lines[0]).to eq("event: datastar-merge-fragments\ndata: fragments <div id=\"foo\">\ndata: fragments <span>hello</span>\ndata: fragments </div>\n\n")
      expect(socket.lines[1]).to eq("event: datastar-merge-signals\ndata: signals {\"foo\":\"bar\"}\n\n")
    end
  end

  private

  def build_request(path, body: nil, content_type: 'application/json', accept: 'text/event-stream')
    Rack::Request.new(Rack::MockRequest.env_for(
                        path,
                        'CONTENT_TYPE' => content_type,
                        'HTTP_ACCEPT' => accept,
                        Rack::RACK_INPUT => body ? StringIO.new(body) : nil
                      ))
  end
end
