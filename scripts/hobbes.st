/**
 * Name: hobbes
 * Description: Hobbes programming language.
 * Author: Brian Egge
 *
 * Hobbes is a language and an embedded JIT compiler developed at
 * Morgan Stanley.  It uses Haskell-like syntax with C-style comments.
 *
 * To install, copy this file to the enscript highlighting directory:
 *   /usr/share/enscript/hl/ (or wherever your enscript states are)
 * and add the following line to the namerules in enscript.st:
 *   /\.hob$/   hobbes;
 *
 * Usage:
 *   enscript -E hobbes -o output.ps input.hob
 */

state hobbes extends HighlightEntry
{
  /* Block comments. */
  /\/\*/ {
    comment_face (true);
    language_print ($0);
    call (c_comment);
    comment_face (false);
  }

  /* Single-line comments. */
  /\/\// {
    comment_face (true);
    language_print ($0);
    call (eat_one_line);
    comment_face (false);
  }

  /* Pragma annotations: {-# UNSAFE name #-} and {-# SAFE name #-} */
  /\{-#/ {
    builtin_face (true);
    language_print ($0);
    call (hobbes_pragma);
    builtin_face (false);
  }

  /* String constants. */
  /\"/ {
    string_face (true);
    language_print ($0);
    call (c_string);
    string_face (false);
  }

  /* Character constants. */
  /'(\\\\.|[^'\\\\])'/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Hex byte: 0XFF */
  /\b0X[0-9a-fA-F]{2}\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Hex byte array: 0xdeadbeef */
  /\b0x([0-9a-fA-F]{2})+\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* DateTime literals: 2024-01-15T10:30:00.123456 */
  /\b[0-9]{4}-[0-9]{2}-[0-9]{2}(T[0-9]{2}:[0-9]{2}(:[0-9]{2}(\.[0-9]+)?)?)?\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Time literals: 10:30:00.123 */
  /\b[0-9]{2}:[0-9]{2}(:[0-9]{2}(\.[0-9]+)?)?\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Float literals with suffix: 1.0f */
  /\b[0-9]+\.[0-9]+[fF]\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Percentage literals: 50.0% */
  /\b[0-9]+\.[0-9]+%/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Double literals: 1.0 */
  /\b[0-9]+\.[0-9]+\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Duration literals: 100ms, 50us, 10s, 5m, 2h, 1d, 5min, 2hour, 1day */
  /\b[0-9]+(ms|us|min|hour|day|[smhd])\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Int128 literals: 0H */
  /\b[0-9]+H\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Long literals: 0L */
  /\b[0-9]+[Ll]\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Short literals: 0S */
  /\b[0-9]+S\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Integer literals. */
  /\b[0-9]+\b/ {
    string_face (true);
    language_print ($0);
    string_face (false);
  }

  /* Type annotations: foo :: type */
  /::/ {
    type_face (true);
    language_print ($0);
    call (eat_one_line);
    type_face (false);
  }

  /* Keywords.
     (build-re '(and case class data default do else exists fn if import
     in instance let match matches module not of option or pack parse
     return then type unpack where with))
  */
  /\b(and|case|class|d(ata|efault|o)|e(lse|xists)|fn|i(f|mport|n(|stance))\
|let|match(|es)|module|not|o(f|ption|r)|pack|parse|return|t(hen|ype)\
|unpack|w(here|ith))\b/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /* Boolean literals. */
  /\b(true|false)\b/ {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* Built-in types.
     (build-re '(bool byte char short int long int128 float double
     time datetime timespan))
  */
  /\b(bool|byte|char|d(atetime|ouble)|float|int(|128)|long|short\
|time(|span))\b/ {
    type_face (true);
    language_print ($0);
    type_face (false);
  }

  /* Lambda: \x.expr */
  /\\\\/ {
    keyword_face (true);
    language_print ($0);
    keyword_face (false);
  }

  /* Constraint and function type arrows. */
  /(=>|->)/ {
    type_face (true);
    language_print ($0);
    type_face (false);
  }

  /* Mutable assignment operator. */
  /<-/ {
    reference_face (true);
    language_print ($0);
    reference_face (false);
  }

  /* Function definitions at the start of a line: funcName(...) */
  /^([a-zA-Z_][a-zA-Z_0-9]*)([ \t]*\()/ {
    /* Save captures before regmatch overwrites them. */
    fname = $1;
    ftail = $2;

    /* Skip keywords that happen to precede parentheses. */
    if (regmatch (fname, /^(and|case|class|d(ata|efault|o)|e(lse|xists)|fn|i(f|mport|n(|stance))|let|match(|es)|module|not|o(f|ption|r)|pack|parse|return|t(hen|ype)|unpack|w(here|ith))$/))
      {
        keyword_face (true);
        language_print (fname);
        keyword_face (false);
      }
    else
      {
        function_name_face (true);
        language_print (fname);
        function_name_face (false);
      }

    language_print (ftail);
  }
}

/* Pragma body: read until #-} */
state hobbes_pragma extends Highlight
{
  /#-\}/ {
    language_print ($0);
    return;
  }
}


/*
Local variables:
mode: c
End:
*/
