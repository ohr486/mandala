require "mandala"
require "thor"

class Mandala::CLI
  def self.start(args=ARGV)
    new(args).start
  end

  def initialize(args=ARGV, **config)
    @args = args.dup
    @config = config
  end

  def start
    dump_cli_debug_info

    if false
      # TODO: impl here
    elsif is_cmd?
      # exec command
      exec_cmd
    elsif is_version?
      # show version
      dump_version
    else
      # show help
      dump_help
    end
  end

  # --- version ---
  def is_version?
    @args == ["-v"]
  end

  def dump_version
    sh = Thor::Shell::Basic.new
    sh.say "mandala version: #{Mandala::VERSION}"
  end

  # --- help ---
  def dump_help
    sh = Thor::Shell::Basic.new
    sh.say "=== TODO: impl here ===\n"
    sh.say "show helps here!"
  end

  # --- command ---
  def is_cmd?
    @args.length > 1 && @args[0] == "cmd"
  end

  def exec_cmd
    Mandala::Util.sh @args[1]
  end

  # --- debug info ---
  def dump_cli_debug_info
    sh = Thor::Shell::Basic.new
    sh.say "=== cli debug info ===\n"
    sh.say "args: #{@args}\n"
    sh.say "config: #{@config}\n"
    sh.say "======================"
  end
end
