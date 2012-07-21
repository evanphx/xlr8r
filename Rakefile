# -*- ruby -*-

require 'rubygems'
require 'hoe'

Hoe.plugin :compiler
Hoe.plugin :git

ruby = ENV['RUBY'] || Gem.ruby

Hoe.spec 'xlr8r' do
  developer('Evan Phoenix', 'evan@phx.io')
end

file "rubyspec/core" do
  sh "git clone https://github.com/rubyspec/rubyspec.git"
end

file "mspec" do
  sh "git clone https://github.com/rubyspec/mspec.git"
end

file "rubyspec/tags/language" => ["mspec", "rubyspec/core"] do
  sh "#{ruby} mspec/bin/mspec tag -t #{ruby} rubyspec/core rubyspec/language || true"
end

task :build_ext do
  Dir.chdir "ext/xlr8r" do
    unless File.exist? "Makefile"
      sh "#{ruby} extconf.rb"
    end

    sh "make"
  end
end

desc "Run rubyspec against xlr8r"
task :rubyspec => [:build_ext, "mspec", "rubyspec/tags/language"] do
  sh "#{ruby} mspec/bin/mspec ci -t #{ruby} -T -r`pwd`/ext/xlr8r/xlr8r_ext.so rubyspec/core rubyspec/language"
end

desc "Run a specific spec"
task :run => [:build_ext] do
  spec = ENV['SPEC']
  sh "#{ruby} mspec/bin/mspec run -t #{ruby} -T -r`pwd`/ext/xlr8r/xlr8r_ext.so #{spec}"
end

# vim: syntax=ruby
