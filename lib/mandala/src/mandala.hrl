-define(key(M, K), maps:get(K, M)).
-define(line(Opts), mandala_utils:get_line(Opts)).

-record(mandala_tokenizer, {
  file=(<<"nofile">>),
  terminators=[],
  unescape=true,
  check_terminators=true,
  existing_atoms_only=false,
  static_atoms_encoder=nil,
  preserve_comments=nil,
  identifier_tokenizer=mandala_tokenizer,
  indentation=0,
  mismatch_hints=[],
  warn_on_unnecessary_quotes=true,
  warnings=[]
}).