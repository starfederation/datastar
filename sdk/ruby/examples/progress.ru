require 'bundler'
Bundler.setup(:test)

require 'datastar'

INDEX = <<-HTML
<!DOCTYPE html>
<html>
  <head>
    <meta charset="UTF-8">
    <title>Datastar counter</title>
    <style>
    body { 
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
  request = Rack::Request.new(env)
  response = Rack::Response.new([], 200)
  datastar = Datastar
    .new(request:, response:, view_context: self)
    .on_connect do |socket|
      p ['connect', socket]
    end.on_client_disconnect do |socket|
      p ['client disconnect', socket]
    end.on_error do |error|
      p ['exception', error]
      puts error.backtrace.join("\n")
    end

  total = 200

  if datastar.sse?

    datastar.stream do |sse|
      # Replace progress element with progress bar
      # bind a `progress` signal to its value
      sse.merge_fragments(%(<progress data-signal-progress="0" id="progress" max="#{total}" data-attr-value="$progress">0</progress>))
      sse.merge_signals(running: true)
      total.times do |i|
        sleep 0.01
        sse.merge_signals(progress: i)
      end
      sse.merge_signals(running: false)
      sse.merge_fragments(%(<strong id="progress">done!</strong>))
    end

	else
		[200, {'content-type' => 'text/html'}, [INDEX]]
	end
end
