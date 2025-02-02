require 'bundler'
Bundler.setup(:test)

require 'datastar'

run do |env|
  request = Rack::Request.new(env)
  response = Rack::Response.new([], 200)
  datastar = Datastar
    .new(request:, response:, view_context: self)
    .on_connect do |socket|
      p ['connect', socket]
    end.on_disconnect do |socket|
      p ['disconnect', socket]
    end.on_error do |error|
      p ['exception', error]
      puts error.backtrace.join("\n")
    end

  if datastar.sse?
    datastar.stream do |sse|
      sse.signals['events'].each do |event|
        type = event.delete('type')
        case type
        when 'mergeSignals'
          arg = event.delete('signals')
          sse.merge_signals(arg, event)
        when 'removeSignals'
          arg = event.delete('paths')
          sse.remove_signals(arg, event)
        when 'executeScript'
          arg = event.delete('script')
          sse.execute_script(arg, event)
        when 'mergeFragments'
          arg = event.delete('fragments')
          sse.merge_fragments(arg, event)
        when 'removeFragments'
          arg = event.delete('selector')
          sse.remove_fragments(arg, event)
        end
      end
    end

    datastar.response.finish
	else
		[200, {'content-type' => 'text/plain'}, ['hello']]
	end
end
