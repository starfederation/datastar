# Datastar Ruby SDK

Implement the Datastart SSE procotocol in Ruby. It can be used in any Rack handler, and Rails controllers.

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
# already have request and response objects
# In a "raw" Rack handler you will need to create them
#  request = Rack::Request.new(env)
#  response = Rack::Response.new([], 200)

datastar = Datastar.new(request:, response:, view_context: self)
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
Then their updates are linearized and sent to the browser.

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

Now run the test bash scripts in the `test` directory in this repo.

```bash
./test-all.sh http://localhost:9292
```

## Development

After checking out the repo, run `bin/setup` to install dependencies. Then, run `rake spec` to run the tests. You can also run `bin/console` for an interactive prompt that will allow you to experiment.

To install this gem onto your local machine, run `bundle exec rake install`. To release a new version, update the version number in `version.rb`, and then run `bundle exec rake release`, which will create a git tag for the version, push git commits and the created tag, and push the `.gem` file to [rubygems.org](https://rubygems.org).

## Contributing

Bug reports and pull requests are welcome on GitHub at https://github.com/starfederation/datastar.
