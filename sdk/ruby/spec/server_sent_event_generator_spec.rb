# frozen_string_literal: true

RSpec.describe Datastar::ServerSentEventGenerator do
  subject(:generator) { described_class.new(request:, response:) }

  let(:request) { build_request('/events') }
  let(:response) { Rack::Response.new(nil, 200) }

  describe '#initialize' do
    it 'sets Content-Type to text/event-stream' do
      expect(generator.response['Content-Type']).to eq('text/event-stream')
    end
  end

  describe '#sse?' do
    it 'returns true if the request Accept header is text/event-stream' do
      expect(generator.sse?).to be(true)
    end

    it 'return false if the request Accept header is not text/event-stream' do
      request = build_request('/events', accept: 'application/json')
      generator = described_class.new(request:, response:)
      expect(generator.sse?).to be(false)
    end
  end

  describe '#merge_fragments' do
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
