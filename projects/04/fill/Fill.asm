// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// SCREEN=16384

@SCREEN
D=A
@pixel
M=D // pixel=SCREEN (i.e., pixel=16384)
@8192 // A=256*512-1=131071
D=D+A   // D=SCREEN+131071
@screen_end
M=D     // screen_end=SCREEN+131071

(LISTEN)
    @KBD
    D=M
    @CLEAR
    D;JEQ
    @BLACKEN
    0;JMP

(BLACKEN)
    @SCREEN
    D=A
    @pixel
    M=D // pixel=SCREEN (i.e., pixel=16384)
(LOOP1)
    @0
    D=!A
    @pixel
    A=M
    M=D
    D=A+1
    @pixel
    M=D
    // Check if done looping
    @24576
    D=D-A
    @LISTEN
    D;JEQ
    @LOOP1
    0;JMP

(CLEAR)
    @SCREEN
    D=A
    @pixel
    M=D // pixel=SCREEN (i.e., pixel=16384)
(LOOP2)
    @pixel
    A=M
    M=0
    D=A+1
    @pixel
    M=D
    // Check if done looping
    @24576
    D=D-A
    @LISTEN
    D;JEQ
    @LOOP2
    0;JMP