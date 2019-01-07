module Mandala::Func < Mandala::Aws::lambda
  def self.process(event, context, handler)
    new.send(handler, event, context)
  end
end
