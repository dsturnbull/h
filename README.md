# TODO

1. SID
   * wire up synthesizer library

2. Debugging
   o transition to tty-only
   o update screen, don't redraw entire thing
   o disasm mode
   o fix flickering disasm
   * fix keyboard spam problem

3. Refactor
   * minimalise modules
   * clearly separate ASM from decoder (separate project?)

4. exe format
   * code, data segments

5. graphics
    1. only output debug info when tty connected
    2. remove terminal input from debugger
    3. render screen ram to terminal
    4. connect terminal input to app
    5. convert to opengl mode
    6. move debugger back to terminal
    7. use tty as serial port