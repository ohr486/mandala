require "mandala"
require "thor"

class MandalaUtils < Thor
  desc "ping", "ping pong"
  def ping
    puts "pong"
  end
end
