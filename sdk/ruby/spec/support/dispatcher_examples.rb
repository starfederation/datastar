module DispatcherExamples
  RSpec.shared_examples 'a dispatcher handling concurrent streams' do
    it 'spawns multiple streams in threads, triggering callbacks only once' do
      disconnects = []

      dispatcher = Datastar
                    .new(request:, response:, executor:)
                    .on_server_disconnect { |_| disconnects << true }
        .on_error { |err| puts err.backtrace.join("\n") }

      dispatcher.stream do |sse|
        sleep 0.01
        sse.patch_elements %(<div id="foo">\n<span>hello</span>\n</div>\n)
      end

      dispatcher.stream do |sse|
        sse.merge_signals(foo: 'bar')
      end

      socket = TestSocket.new
      dispatcher.response.body.call(socket)
      expect(socket.open).to be(false)
      expect(socket.lines.size).to eq(2)
      expect(socket.lines[0]).to eq("event: datastar-merge-signals\ndata: signals {\"foo\":\"bar\"}\n\n\n")
      expect(socket.lines[1]).to eq("event: datastar-patch-elements\ndata: elements <div id=\"foo\">\ndata: elements <span>hello</span>\ndata: elements </div>\n\n\n")
      expect(disconnects).to eq([true])
    end

    it 'catches exceptions raised from threads' do
      Thread.report_on_exception = false
      errs = []

      dispatcher = Datastar
                    .new(request:, response:, executor:)
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
  end
end

