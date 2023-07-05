# frozen_string_literal: true

require 'thor'
require 'nndiscourse'

# CLI documentation string
class CLI < Thor
  desc 'serve URL', 'Run the jimson server'
  method_option :port, aliases: '-p', desc: 'Port to listen on'
  def serve(url)
    Nndiscourse::Process.new(url, options[:port])
  end
end
