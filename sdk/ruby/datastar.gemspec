# frozen_string_literal: true

require_relative 'lib/datastar/version'

Gem::Specification.new do |spec|
  spec.name = 'datastar'
  spec.version = Datastar::VERSION
  spec.authors = ['Ismael Celis']
  spec.email = ['ismaelct@gmail.com']

  spec.summary = 'Ruby SDK for Datastar. Rack-compatible.'
  spec.homepage = 'https://github.com/starfederation/datastar#readme'
  spec.required_ruby_version = '>= 3.0.0'

  spec.metadata['homepage_uri'] = spec.homepage
  spec.metadata['source_code_uri'] = 'https://github.com/starfederation/datastar'

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  gemspec = File.basename(__FILE__)
  spec.files = IO.popen(%w[git ls-files -z], chdir: __dir__, err: IO::NULL) do |ls|
    ls.readlines("\x0", chomp: true).reject do |f|
      (f == gemspec) ||
        f.start_with?(*%w[bin/ test/ spec/ features/ .git appveyor Gemfile])
    end
  end
  spec.bindir = 'exe'
  spec.executables = spec.files.grep(%r{\Aexe/}) { |f| File.basename(f) }
  spec.require_paths = ['lib']

  # Uncomment to register a new dependency of your gem
  spec.add_dependency 'rack', '>= 3.1.14'
  spec.add_dependency 'json'
  spec.add_dependency 'logger'

  # For more information and examples about making a new gem, check out our
  # guide at: https://bundler.io/guides/creating_gem.html
end
