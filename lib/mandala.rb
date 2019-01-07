require "mandala/version"
require "mandala/cli"

module Mandala
  class Error < StandardError; end
  # Your code goes here...

  autoload :Util, "mandala/util"
  autoload :Func, "mandala/func"
end
