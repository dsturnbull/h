# TODO

1. SID
   * wire up synthesizer library

2. Debugging
   o transition to tty-only
   o update screen, don't redraw entire thing
   o disasm mode
   o fix flickering disasm
   o fix keyboard spam problem

3. Refactor
   * minimalise modules
   * clearly separate ASM from decoder (separate project?)

4. exe format
   o code, data segments

5. graphics
   o only output debug info when tty connected
   o remove terminal input from debugger
   o render screen ram to terminal
   o connect terminal input to app
   o move debugger back to terminal
   o use tty as serial port

   * convert to opengl mode