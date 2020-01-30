# frozen_string_literal: true

require 'nndiscourse/version'
require 'jimson'
require 'discourse_api'

module Nndiscourse
  # This proxies DiscourseApi::Client
  class Handler < DiscourseApi::Client
    extend Jimson::Handler

    def initialize(url)
      super(url)
    end

    def send(method_name, *params)
      params.map! do |param|
        if param.is_a?(Hash)
          param.each_with_object({}) { |(k, v), h| h[k.to_sym] = v }
        else
          param
        end
      end
      super(method_name, *params)
    end
  end

  # Process contains a Jimson Server instance
  class Process
    def initialize(url, port = 8999)
      @server = Jimson::Server.new(Handler.new(url), port: port, show_errors: true)
      @server.start
    end
  end
end
