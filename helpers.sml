structure Helpers =
  struct
    fun string_to_tokens(inputString: string): MumlTokens.token list =
      let
        val initial_strm = MumlLexer.streamifyInstream (TextIO.openString inputString)
        val lexer = MumlLexer.lex (AntlrStreamPos.mkSourcemap())
        fun dowork(strm) =
          let
            val lex_result = lexer strm
            val next_token = #1 lex_result
          in
            if next_token = MumlTokens.EOF
            then []
            else next_token :: dowork(#3 lex_result)
          end
      in
        dowork(initial_strm)
      end
  end
