class Mandala::Aws::Lambda
  attr_reader :event, :context, :handler
  def initialize(event, context, handler)
    @event = event
    @context = context
    @handler = handler
  end
end
