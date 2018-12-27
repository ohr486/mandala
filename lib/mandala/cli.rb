require "mandala"
require "thor"

class MandalaUtils < Thor
  desc "ping", "ping pong"
  def ping
    puts "pong"
  end

  desc "version", "show current version"
  def version
    puts Mandala::VERSION
  end

  desc "graph", "show service graph"
  def graph
    # implement here
  end

  desc "monitor", "show service log"
  def monitor
    # implement here
  end
end
