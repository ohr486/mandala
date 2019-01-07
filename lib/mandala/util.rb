class Mandala::Util
  def self.sh(cmd)
    puts "#=> #{cmd}"
    system(cmd)
  end
end
