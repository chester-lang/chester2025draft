package readline

import readline.all._

import scala.scalanative.libc.stdlib.free
import scala.scalanative.unsafe._

@link("readline")
object facade {

  def readline(prompt: String): String = Zone {
    val cPrompt = toCString(prompt)
    val line = all.readline(cPrompt)
    val res = fromCString(line)
    free(line)
    res
  }

  def read_history(filename: String): Int = Zone {
    all.read_history(toCString(filename))
  }

  def write_history(filename: String): Int = Zone {
    all.write_history(toCString(filename))
  }

  def append_history(nelements: Int, filename: String): Int = Zone {
    all.append_history(nelements, toCString(filename))
  }

  def add_history(line: String): Unit = Zone {
    all.add_history(toCString(line))
  }

  def history_get(offset: Int): String = {
    val entry = all.history_get(offset)
    if (entry == null) null
    else fromCString((!entry).line)
  }

  @extern
  object externs {
    var history_base: CInt = extern
    var history_length: CInt = extern
    var history_max_entries: CInt = extern
    var history_offset: CInt = extern

    var history_lines_read_from_file: CInt = extern
    var history_lines_written_to_file: CInt = extern

    var history_expansion_char: CChar = extern
    var history_subst_char: CChar = extern
    var history_word_delimiters: CString = extern
    var history_comment_char: CChar = extern
    var history_no_expand_chars: CString = extern
    var history_search_delimiter_chars: CString = extern

    var history_quotes_inhibit_expansion: CInt = extern
    var history_quoting_state: CInt = extern

    var history_write_timestamps: CInt = extern
  }

  // Update the existing history_base and history_length functions
  def history_base: Int = externs.history_base
  def history_length: Int = externs.history_length

  // Add new functions for the other variables
  def history_max_entries: Int = externs.history_max_entries
  def history_offset: Int = externs.history_offset

  def history_lines_read_from_file: Int = externs.history_lines_read_from_file
  def history_lines_written_to_file: Int = externs.history_lines_written_to_file

  def history_expansion_char: Char = externs.history_expansion_char.toChar
  def history_subst_char: Char = externs.history_subst_char.toChar
  def history_word_delimiters: String = fromCString(
    externs.history_word_delimiters
  )
  def history_comment_char: Char = externs.history_comment_char.toChar
  def history_no_expand_chars: String = fromCString(
    externs.history_no_expand_chars
  )
  def history_search_delimiter_chars: String = fromCString(
    externs.history_search_delimiter_chars
  )

  def history_quotes_inhibit_expansion: Int =
    externs.history_quotes_inhibit_expansion
  def history_quoting_state: Int = externs.history_quoting_state

  def history_write_timestamps: Int = externs.history_write_timestamps

  def rl_bind_key(key: Int, function: rl_command_func_t): Int =
    all.rl_bind_key(key, function.asInstanceOf[Ptr[rl_command_func_t]])

  def rl_add_defun(name: String, function: rl_command_func_t, key: Int): Int =
    Zone {
      all.rl_add_defun(
        toCString(name),
        function.asInstanceOf[Ptr[rl_command_func_t]],
        key
      )
    }

  def rl_set_prompt(prompt: String): Int = Zone {
    all.rl_set_prompt(toCString(prompt))
  }

  def rl_redisplay(): Unit = all.rl_redisplay()

  def rl_insert_text(text: String): Int = Zone {
    all.rl_insert_text(toCString(text))
  }

  def rl_delete_text(start: Int, end: Int): Int =
    all.rl_delete_text(start, end)

  def rl_on_new_line(): Int = all.rl_on_new_line()

  def rl_clear_message(): Int = all.rl_clear_message()

  def rl_set_keymap(keymap: Keymap): Unit = all.rl_set_keymap(keymap)

  def rl_get_keymap(): Keymap = all.rl_get_keymap()

  def rl_make_bare_keymap(): Keymap = all.rl_make_bare_keymap()

  def rl_parse_and_bind(line: String): Int = Zone {
    all.rl_parse_and_bind(toCString(line))
  }

  def using_history(): Unit = all.using_history()
}
