# Datastar Ruby SDK

Implement the [Datastart SSE procotocol](https://data-star.dev/reference/sse_events) in Ruby. It can be used in any Rack handler, and Rails controllers.

## Installation

Install the gem and add to the application's Gemfile by executing:

```bash
bundle add datastar
```

Or point your `Gemfile` to the source

```bash
gem 'datastar', git: 'https://github.com/starfederation/datastar', glob: 'sdk/ruby/*.gemspec'
```

## Usage

### Initialize the Datastar dispatcher

In your Rack handler or Rails controller:

```ruby
# Rails controllers, as well as Sinatra and others, 
# already have request and response objects.
# `view_context` is optional and is used to render Rails templates.
# Or view components that need access to helpers, routes, or any other context.

datastar = Datastar.new(request:, response:, view_context:)

# In a Rack handler, you can instantiate from the Rack env
datastar = Datastar.from_rack_env(env)
```

### Sending updates to the browser

There are two ways to use this gem in HTTP handlers:

* One-off responses, where you want to send a single update down to the browser.
* Streaming responses, where you want to send multiple updates down to the browser.

#### One-off update:

```ruby
datastar.merge_fragments(%(<h1 id="title">Hello, World!</h1>))
```
In this mode, the response is closed after the fragment is sent.

#### Streaming updates

```ruby
datastar.stream do |sse|
  sse.merge_fragments(%(<h1 id="title">Hello, World!</h1>))
  # Streaming multiple updates
  100.times do |i|
    sleep 1
    sse.merge_fragments(%(<h1 id="title">Hello, World #{i}!</h1>))
  end
end
```
In this mode, the response is kept open until `stream` blocks have finished.

#### Concurrent streaming blocks

Multiple `stream` blocks will be launched in threads/fibers, and will run concurrently.
Their updates are linearized and sent to the browser as they are produced.

```ruby
# Stream to the browser from two concurrent threads
datastar.stream do |sse|
  100.times do |i|
    sleep 1
    sse.merge_fragments(%(<h1 id="slow">#{i}!</h1>))
  end
end

datastar.stream do |sse|
  1000.times do |i|
    sleep 0.1
    sse.merge_fragments(%(<h1 id="fast">#{i}!</h1>))
  end
end
```

See the [examples](https://github.com/starfederation/datastar/tree/main/examples/ruby) directory.

### Datastar methods

All these methods are available in both the one-off and the streaming modes.

#### `merge_fragments`
See https://data-star.dev/reference/sse_events#datastar-merge-fragments

```ruby
sse.merge_fragments(%(<div id="foo">\n<span>hello</span>\n</div>))

# or a Phlex view object
sse.merge_fragments(UserComponet.new)

# Or pass options
sse.merge_fragments(
  %(<div id="foo">\n<span>hello</span>\n</div>),
  merge_mode: 'append'
)
```

#### `remove_fragments`
 See https://data-star.dev/reference/sse_events#datastar-remove-fragments

```ruby
sse.remove_fragments('#users')
```

#### `merge_signals`
 See https://data-star.dev/reference/sse_events#datastar-merge-signals

```ruby
sse.merge_signals(count: 4, user: { name: 'John' })
```

#### `remove_signals`
 See https://data-star.dev/reference/sse_events#datastar-remove-signals

```ruby
sse.remove_signals(['user.name', 'user.email'])
```

#### `execute_script`
See https://data-star.dev/reference/sse_events#datastar-execute-script

```ruby
sse.execute_scriprt(%(alert('Hello World!'))
```

#### `signals`
See https://data-star.dev/guide/getting_started#data-signals

Returns signals sent by the browser.

```ruby
sse.signals # => { user: { name: 'John' } }
```

#### `redirect`
This is just a helper to send a script to update the browser's location.

```ruby
sse.redirect('/new_location')
```

### Lifecycle callbacks

#### `on_connect`
Register server-side code to run when the connection is first handled.

```ruby
datastar.on_connect do
  puts 'A user has connected'
end
```

#### `on_client_disconnect`
Register server-side code to run when the connection is closed by the client

```ruby
datastar.on_client_connect do
  puts 'A user has disconnected connected'
end
```

This callback's behaviour depends on the configured [heartbeat](#heartbeat)

#### `on_server_disconnect`

Register server-side code to run when the connection is closed by the server.
Ie when the served is done streaming without errors.

```ruby
datastar.on_server_connect do
  puts 'Server is done streaming'
end
```

#### `on_error`
Ruby code to handle any exceptions raised by streaming blocks.

```ruby
datastar.on_error do |exception|
  Sentry.notify(exception)
end
```
Note that this callback can be [configured globally](#global-configuration), too.

### heartbeat

By default, streaming responses (using the `#stream` block) launch a background thread/fiber to periodically check the connection.

This is because the browser could have disconnected during a long-lived, idle connection (for example waiting on an event bus).

The default heartbeat is 3 seconds, and it will close the connection and trigger [on_client_disconnect](#on_client_disconnect) callbacks if the client has disconnected.

In cases where a streaming block doesn't need a heartbeat and you want to save precious threads (for example a regular ticker update, ie non-idle), you can disable the heartbeat:

```ruby
datastar = Datastar.new(request:, response:, view_context:, heartbeat: false)

datastar.stream do |sse|
  100.times do |i|
    sleep 1
    sse.merge_signals count: i
  end
end
```

You can also set it to a different number (in seconds)

```ruby
heartbeat: 0.5
```

#### Manual connection check

If you want to check connection status on your own, you can disable the heartbeat and use `sse.check_connection!`, which will close the connection and trigger callbacks if the client is disconnected.

```ruby
datastar = Datastar.new(request:, response:, view_context:, heartbeat: false)

datastar.stream do |sse|
  # The event bus implementaton will check connection status when idle
  # by calling #check_connection! on it
  EventBus.subscribe('channel', sse) do |event|
    sse.merge_signals eventName: event.name
  end
end
```

### Global configuration

```ruby
Datastar.configure do |config|
  # Global on_error callback
  # Can be overriden on specific instances
  config.on_error do |exception|
    Sentry.notify(exception)
  end
  
  # Global heartbeat interval (or false, to disable)
  # Can be overriden on specific instances
  config.heartbeat = 0.3
end
```

### Rendering Rails templates

In Rails, make sure to initialize Datastar with the `view_context` in a controller.
This is so that rendered templates, components or views have access to helpers, routes, etc.

```ruby
datastar = Datastar.new(request:, response:, view_context:)

datastar.stream do |sse|
  10.times do |i|
    sleep 1
    tpl = render_to_string('events/user', layout: false, locals: { name: "David #{i}" })
    sse.merge_fragments tpl
  end
end
```

### Rendering Phlex components

`#merge_fragments` supports [Phlex](https://www.phlex.fun) component instances.

```ruby
sse.merge_fragments(UserComponent.new(user: User.first))
```

### Rendering ViewComponent instances

`#merge_fragments` also works with [ViewComponent](https://viewcomponent.org) instances.

```ruby
sse.merge_fragments(UserViewComponent.new(user: User.first))
```

### Rendering `#render_in(view_context)` interfaces

Any object that supports the `#render_in(view_context) => String` API can be used as a fragment.

```ruby
class MyComponent
  def initialize(name)
    @name = name
  end

  def render_in(view_context)
    "<div>Hello #{@name}</div>""
  end
end
```

```ruby
sse.merge_fragments MyComponent.new('Joe')
```



### Tests

```ruby
bundle exec rspec
```

#### Running Datastar's SDK test suite

Install dependencies.
```bash
bundle install
```

From this library's root, run the bundled-in test Rack app:

```bash
bundle puma examples/test.ru
```

Now run the test bash scripts in the `sdk/test` directory in this repo.

```bash
./test-all.sh http://localhost:9292
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and the created tag, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/starfederation/datastar.
