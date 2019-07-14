# The Defugger

*The Defugger* is a TUI Brain F\*\*k (BF) dubugger written entirely in Haskell using [Brick](https://hackage.haskell.org/package/brick). You can use it as a straight interpreter to run a BF script, or you can run it as an interactive terminal interface to edit, step forward and backward and jump between break points in a BF script. **This is still very much a work in progress and likely to change considerably, but the basic prototype is working**. I am mostly just working on this for my own amusement and to get experience with more complex program architectures, code profiling, performance, testing, etc; however, any feedback and suggestions are welcome!

#### [Installation](#installation)
#### [How it works](#operation)
* [BF format](#format)
* [Interpreter mode](#interpreter)
* [Debugger mode](#debugger)
#### [Performance and halting computations](#performance)
#### [Known issues and to do](#todo)

## Installation<a name="installation"></a>

The Defugger should run on Linux and MacOs; however, it will probably not work on Windows. The Defugger is developed with the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/). To get the Defugger, clone and compile with
```sh
$ git clone https://github.com/MWRuszczycky/defugger.git
$ cd defugger
$ stack build
```
To run the test suites that have been implemented, run
```sh
$ stack test
```
You can now run the Defugger with
```sh
$ stack exec defugger
```
or you can set an alias,
```sh
$ alias defugger='stack exec defugger --'
```
so that the examples below match.

## How it works<a name="operation"></a>

### BF format<a name="format"></a>

The Defugger recognizes standard, single-character BF commands (i.e., `<`, `>`, `+`, etc.) and ignores white space. Any other characters need to be commented out using a `#`. All white space and comments are stripped when the script is opened in the debugger. I actually plan to change many of these format requirements in the future.

### Interpreter mode<a name="interpreter"></a>

If all you want to do is run a BF script with output sent to standard-out, just use
```sh
$ defugger --run myScript.bf
```
You can try this out with the hello-word script (from [Wikipedia](https://en.wikipedia.org/wiki/Brainfuck)) in `tests/files/HelloWorld.bf`. You can also supply your script with an input file using
```sh
$ defugger --run myScript.bf myInput.txt
```
Currently, input has to be supplied as a file, and the Defugger does not support interactive input.

### Debugger mode<a name="debugger"></a>

In order to run a BF script in debugger mode, just leave out the `--run` option,
```sh
$ defugger myScript.bf
```
or if you have an input file,
```sh
$ defugger myScript.bf myInput.txt
```
This will bring up a TUI with four windows and a command/status line as shown here with the [99 Bottles of Beer script](https://sange.fi/esoteric/brainfuck/bf-source/prog/BOTTLES.BF):

![defugger demo](demos/demo_042019f.png)

1. **The Program Window.** Here you can move around your code with a cursor (in green), set break points (in red), add or delete commands and execute the code forwards and backwards stepwise or in jumps between break points (the current position is denoted in yellow).
2. **The Memory Window.** This displays the current memory tape in with each byte represented in decimal form. This will update as you execute or revert execution of the script in the program window. The current memory head is highlighted in yellow.
3. **The Output Window.** This displays the output your script generates. You can toggle between ascii, decimal and hexidecimal formats. Note that in ascii dislay, non-printing characters other than new lines are simply not displayed.
4. **The Input Window.** This displays the remaining input that your script has yet to consume. As in the case of the output window, you can toggle between different number formats.
5. **The Command/Status Line.** Messages are displayed in the bottom row of the TUI. Likewise, you can enter commands here by first pressing `:`.

#### Debugger mode commands and key-bindings <a name="commands"></a>

You can list help information by typing a colon (`:`) and then `help` (i.e., `:help`). The colon enters you into Command Mode where you can run typed commands. The `help` command can also take arguments to provide additional information. For example,
* `:help keys` lists all the current key-bindings.
* `:help commands` lists all the current typed commands available in Command Mode.
* `:help settings` lists all the setable parameters using the `set`/`unset` commands.

The `<esc>` key lets you cancel various actions as well as quit the program:
* During Normal Mode, `<esc>` quits the Defugger program. You can also run `:q` or `:quit` to quit the Defugger.
* When entering a command, `<esc>` cancels the command and returns you back to Normal Mode.
* When running a long or even nonhalting jump through a BF script, `<esc>` will abort the jump (see below).
* During Help Mode, `<esc>` returns you back to Normal Mode (`q` will also do this).

The `<tab>` key is used to cycle focus between the Program, Memory, Output and Input Windows.

## Performance and halting computations<a name="performance"></a>

The Defugger interpreter and debugger execute commands using different algorithms. Therefore, they have different performance characteristics.

The Defugger will correctly execute the [Mandelbrot](https://github.com/pablojorge/brainfuck/blob/master/programs/mandelbrot.bf) script in interpreter mode in about 5 minutes on a Dell Inspiron Core i5 laptop. Not super fast, but at least it doesn't crash or have any space leaks that I can find. In debugger mode, the Mandelbrot script takes much longer (almost three hours on the same computer) to jump to the end; however, it does not crash the computer and does not appear to leak space, though I need to check more carefully. That being said, the Defugger appears to perform just fine with less computationally intensive programs. For example, it quickly (< 1 s) jumps to the end of the reasonably large [99 Bottles of Beer script](https://sange.fi/esoteric/brainfuck/bf-source/prog/BOTTLES.BF) with no problem.

So, jumps through multiple BF statements in the debugger can be very slow or even nonhalting (e.g., `+[]`). However, the debugger runs such jumps in an isolated thread, so you can abort them at any time while the jump is processing by pressing `<esc>`.

## Known issues and to do<a name="todo"></a>

### To do

* Improve help strings and documentation.
* Implement reloading of scripts.
* Implement saving and loading debugger state.
* Implement breaks when running the interpreter and loading the debugger.
* Allow a wider range of BF formats and command naming dictionaries.
* Write a better README.
* Write more tests, especially for the editing transformations.
* Add a command that displays all the current debugger settings.
