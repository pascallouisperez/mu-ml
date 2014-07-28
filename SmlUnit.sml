structure Assert = struct
  exception Assertion;

  fun assertTrue(b) = if (b) then () else raise Assertion;

  fun assertFalse(b) = if (b) then raise Assertion else ();

  fun assertEquals(eq, v1, v2) = assertTrue(eq(v1, v2));

  fun assertNotEquals(eq, v1, v2) = assertFalse(eq(v1, v2));

  fun assertFails(f) =
      let
        val result = SOME(f ()) handle Assertion => NONE;
      in
        case result
          of SOME(_) => raise Assertion
          | NONE => ()
      end
end

signature TESTRUNNER = sig
  type result

  val runTest : string * (unit -> unit) -> result;

  val runTestCase : (string * (unit -> unit)) list -> result;
end

structure BooleanTestRunner : TESTRUNNER = struct
  type result = bool;

  fun runTest(test : string * (unit -> unit)) =
      let
        val result = (#2 test) ()
      in
        true
      end
      handle _ => false;

  fun runTestCase(testCase) =
      List.all (fn t => runTest(t)) testCase;
end

structure ConsoleTestRunner : TESTRUNNER = struct
  type result = unit;

  val passed_msg = "[ passed ]";
  val failed_msg = "[ FAILED ]";
  val tty_size = 80;
  val passed_msg_size = String.size passed_msg;
  val failed_msg_size = String.size failed_msg;

  fun printOne(message, r) = if (String.size(message) <= tty_size - String.size(r) - 1)
        then print(message ^ " " ^ r ^ "\n")
        else print(String.substring(message, 0, (tty_size - String.size(r) - 4))  ^ "... " ^ r ^ "\n");

  fun runTest(test : string * (unit -> unit)) =
      if(BooleanTestRunner.runTest(test)) then
        printOne(#1 test, passed_msg)
      else
        printOne(#1 test, failed_msg)

  fun runTestCase(testCase) =
      let
        val (messages, results) = ListPair.unzip(List.map
          (fn(x : string * (unit -> unit))  =>
            (#1 x, if BooleanTestRunner.runTest(x) then passed_msg else failed_msg))
          testCase);

        val message_max_size = List.foldl
          (fn(message, max) => if (max < String.size(message)) then String.size(message) else max)
          0
          messages;

        val messages_adjusted = List.map
          (fn(s) => s ^ String.implode(List.tabulate(message_max_size - String.size(s), fn(i) => #" ")))
          messages;

        val show = ListPair.map
          (fn(m, r) => printOne(m, r))
          (messages_adjusted, results);
      in
        (* TODO(pascal): make exit depend on test result *)
        OS.Process.exit(OS.Process.success)
      end;
end
