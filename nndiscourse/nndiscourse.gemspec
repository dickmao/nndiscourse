# frozen_string_literal: true

require File.expand_path('lib/nndiscourse/version', __dir__)

Gem::Specification.new do |spec|
  spec.name          = 'nndiscourse'
  spec.version       = Nndiscourse::VERSION
  spec.authors       = ['dickmao']
  spec.email         = []

  spec.summary       = 'API calls from nndiscourse.el to Discourse'
  spec.homepage      = 'https://github.com/dickmao/nndiscourse'
  spec.license       = 'GPLv3'

  spec.files         = Dir['lib/**/*.rb']
  spec.test_files    = spec.files.grep(%r{^spec/})
  spec.executables   = spec.files.grep(%r{^bin/}).map { |f| File.basename(f) }
  spec.require_path  = 'lib'

  spec.add_runtime_dependency 'jimson', '~> 0.11.0'
  spec.add_runtime_dependency 'thor', '~> 0.20.3'

  spec.add_development_dependency 'bundler', '~> 2.0'
  spec.add_development_dependency 'rake', '~> 11.1'
  spec.add_development_dependency 'rspec', '~> 3.4'
  spec.add_development_dependency 'rubocop', '~> 0.69'
end
