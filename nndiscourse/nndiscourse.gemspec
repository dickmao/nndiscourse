lib = File.expand_path("lib", __dir__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require "nndiscourse/version"

Gem::Specification.new do |spec|
  spec.name          = "nndiscourse"
  spec.version       = Nndiscourse::VERSION
  spec.authors       = ["dickmao"]
  spec.email         = ["none"]

  spec.summary       = %q{API calls from nndiscourse.el to Discourse}
  spec.homepage      = "https://github.com/dickmao/nndiscourse"
  spec.license       = "MIT"

  spec.metadata["allowed_push_host"] = "https://github.com"

  spec.metadata["homepage_uri"] = spec.homepage

  # Specify which files should be added to the gem when it is released.
  # The `git ls-files -z` loads the files in the RubyGem that have been added into git.
  # spec.files         = Dir.chdir(File.expand_path('..', __FILE__)) do
  #   `git ls-files -z`.split("\x0").reject { |f| f.match(%r{^(test|spec|features)/}) }
  # end

  spec.files         = Dir['lib/**/*.rb']
  spec.bindir        = "exe"
  spec.executables   = spec.files.grep(%r{^exe/}) { |f| File.basename(f) }
  spec.require_paths = ["lib"]

  spec.add_runtime_dependency "discourse_api"
  spec.add_runtime_dependency "jimson", "~> 0.11.0"
  spec.add_runtime_dependency "thor", "~> 0.20.3"

  spec.add_development_dependency "bundler", "~> 2.0"
  spec.add_development_dependency "rake", "~> 10.0"
  spec.add_development_dependency "rspec", "~> 3.2"
end
