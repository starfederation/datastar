require 'bundler/setup'

require 'datastar'

# This is a test Rack endpoint
# to demo streaming Datastar updates from multiple threads.
# To run:
#
#   # install dependencies
#   bundle install
#   # run this endpoint with Puma server
#   bundle exec puma threads.ru
#
#   visit http://localhost:9292
#
INDEX = <<~HTML
  <!DOCTYPE html>
  <html>
    <head>
      <meta charset="UTF-8">
      <title>Datastar counter</title>
      <style>
        body { padding: 10em; }
        .counter {#{' '}
          font-size: 2em;#{' '}
          span { font-weight: bold; }
        }
      </style>
      <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.2/bundles/datastar.js"></script>
    </head>
    <body>
      <button#{' '}
        data-on-click="@get('/')"#{' '}
        data-indicator-heartbeat#{' '}
        >Start</button>
      <p class="counter">Slow thread: <span id="slow">waiting</span></p>
      <p class="counter">Fast thread: <span id="fast">waiting</span></p>
      <p id="connection">Disconnected...</p>
    </body>
  <html>
HTML

trap('INT') { exit }

run do |env|
  # Initialize Datastar with callbacks
  datastar = Datastar
             .from_rack_env(env)
             .on_connect do |sse|
    sse.patch_elements(%(<p id="connection">Connected...</p>))
    p ['connect', sse]
  end.on_server_disconnect do |sse|
    sse.patch_elements(%(<p id="connection">Done...</p>))
    p ['server disconnect', sse]
  end.on_client_disconnect do |socket|
    p ['client disconnect', socket]
  end.on_error do |error|
    p ['exception', error]
    puts error.backtrace.join("\n")
  end

  if datastar.sse?
    # This will run in its own thread / fiber
    datastar.stream do |sse|
      11.times do |i|
        sleep 1
        # Raising an error to demonstrate error handling
        # raise ArgumentError, 'This is an error' if i > 5

        sse.patch_elements(%(<span id="slow">#{i}</span>))
      end
    end

    # Another thread / fiber
    datastar.stream do |sse|
      1000.times do |i|
        sleep 0.01
        sse.patch_elements(%(<span id="fast">#{i}</span>))
      end
    end
  else
    [200, { 'content-type' => 'text/html' }, [INDEX]]
  end
end
