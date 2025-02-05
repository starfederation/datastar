require 'bundler'
Bundler.setup(:test)

require 'datastar'

# This is a test Rack endpoint
# With a dynamic progress bar example using Datastar.
# To run:
#
#   # install dependencies
#   bundle install
#   # run this endpoint with Puma server
#   puma examples/progress.ru
#
#   Then open http://localhost:9292
#
INDEX = <<~HTML
  <!DOCTYPE html>
  <html>
    <head>
      <meta charset="UTF-8">
      <title>Datastar progress bar</title>
      <style>
      body {#{' '}
        padding: 10em;
        font-family: Arial, sans-serif;
        font-size: 1.5em;
      }
      </style>
      <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@v1.0.0-beta.2/bundles/datastar.js"></script>
    </head>
    <body data-signals="{running: false}">
      <button data-attr-disabled="$running" data-on-click="@get('/')">Start</button>
      <p>
        <span id="progress">waiting</span>
      </p>
    </body>
  <html>
HTML

run do |env|
  datastar = Datastar.from_rack_env(env)
                     .on_connect do |socket|
    p ['connect', socket]
  end.on_client_disconnect do |socket|
    p ['client disconnect', socket]
  end.on_error do |error|
    p ['exception', error]
    puts error.backtrace.join("\n")
  end

  if datastar.sse?

    datastar.stream do |sse|
      total = 200

      # Replace progress element with progress bar
      # bind a `progress` signal to its value
      sse.merge_fragments <<~HTML
        <progress#{' '}
          data-signal-progress="0"#{' '}
          id="progress"#{' '}
          max="#{total}"#{' '}
          data-attr-value="$progress"
        >0</progress>
      HTML

      sse.merge_signals(running: true)

      # Move progress bar forward
      total.times do |i|
        sleep 0.01
        sse.merge_signals(progress: i)
      end

      # We're done!
      sse.merge_fragments(%(<strong id="progress">done!</strong>))
      sse.merge_signals(running: false, progress: 0)
    end

  else
    [200, { 'content-type' => 'text/html' }, [INDEX]]
  end
end
