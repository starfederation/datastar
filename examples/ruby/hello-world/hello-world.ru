require 'bundler/setup'

require 'datastar'

# This is a test Rack endpoint
# with a hello world example using Datastar.
# To run:
#
#   # install dependencies
#   bundle install
#   # run this endpoint with Puma server
#   bundle exec puma ./hello-world.ru
#
#   Then open http://localhost:9292
#
HTML = File.read(File.expand_path('hello-world.html', __dir__))

run do |env|
  datastar = Datastar.from_rack_env(env)

  if datastar.sse?
    delay = (datastar.signals['delay'] || 0).to_i
    delay /= 1000.0 if delay.positive?
    message = 'Hello, world!'

    datastar.stream do |sse|
      message.size.times do |i|
        sse.merge_fragments(%(<div id="message">#{message[0..i]}</div>))
        sleep delay
      end
    end
  else
    [200, { 'content-type' => 'text/html' }, [HTML]]
  end
end

trap('INT') { exit }
