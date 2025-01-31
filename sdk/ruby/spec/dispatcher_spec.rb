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
