require 'bundler'
Bundler.setup(:test)

require 'datastar'

# This is a test Rack endpoint to run
# Datastar's SDK test suite agains.
# To run:
#
#   # install dependencies
#   bundle install
#   # run this endpoint with Puma server
#   bundle exec puma examples/test.ru
#
# Then you can run SDK's test bash script:
# See https://github.com/starfederation/datastar/blob/develop/sdk/test/README.md
#
#   ./test-all.sh http://localhost:9292
#
run do |env|
  datastar = Datastar
             .from_rack_env(env)
             .on_connect do |socket|
    p ['connect', socket]
  end.on_server_disconnect do |socket|
    p ['server disconnect', socket]
  end.on_client_disconnect do |socket|
    p ['client disconnect', socket]
  end.on_error do |error|
    p ['exception', error]
    puts error.backtrace.join("\n")
  end

  datastar.stream do |sse|
    sse.signals['events'].each do |event|
      type = event.delete('type')
      case type
      when 'patchSignals'
        arg = event.delete('signals') || event.delete('signals-raw')
        sse.patch_signals(arg, event)
      when 'executeScript'
        arg = event.delete('script')
        sse.execute_script(arg, event)
      when 'patchElements'
        arg = event.delete('elements')
        sse.patch_elements(arg, event)
      else
        raise "Unknown event type: #{type}"
      end
    end
  end
end
