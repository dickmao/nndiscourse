# frozen_string_literal: true

require 'nndiscourse/version'
require 'jimson'
require 'discourse_api'
require 'thor'

module Nndiscourse
  # This proxies DiscourseApi::Client
  class Handler < DiscourseApi::Client
    extend Jimson::Handler
    extend DiscourseApi::API::Categories
    extend DiscourseApi::API::Search
    extend DiscourseApi::API::SSO
    extend DiscourseApi::API::Tags
    extend DiscourseApi::API::Topics
    extend DiscourseApi::API::Polls
    extend DiscourseApi::API::Posts
    extend DiscourseApi::API::Users
    extend DiscourseApi::API::Groups
    extend DiscourseApi::API::Invite
    extend DiscourseApi::API::PrivateMessages
    extend DiscourseApi::API::Notifications
    extend DiscourseApi::API::Badges
    extend DiscourseApi::API::Email
    extend DiscourseApi::API::ApiKey
    extend DiscourseApi::API::Backups
    extend DiscourseApi::API::Dashboard
    extend DiscourseApi::API::Uploads
    extend DiscourseApi::API::UserActions
    extend DiscourseApi::API::SiteSettings

    attr_reader :opts

    def initialize(url, api_key, api_username, opts)
      super(url, api_key, api_username, 'User-Api-Key', 'User-Api-Client-Id')
      @opts = opts
    end
  end

  # Process contains a Jimson Server instance
  class Process
    attr_reader :opts

    def initialize(url, api_key, api_username, opts = {})
      @opts = opts
      @server = Jimson::Server.new(Handler.new(url, api_key, api_username, opts))
      @server.start
    end
  end

  # CLI documentation string
  class CLI < Thor
  end
end
