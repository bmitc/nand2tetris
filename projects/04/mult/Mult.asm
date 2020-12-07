// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Compute R2=R0*R1

// Initialize R2 to 0
    @R2   // A <- Address[R2]
    M=0   // R2=0

(LOOP)
    // Initilize D to R0. We'll use A for the possible jump location.
    @R0   // A <- Address[R0]
    D=M   // D=R0
    // If R0=0, then we are done multiplying
    @END  // A <- END to prepare for possible jump
    D;JEQ // If D=0, then jump to END
    // Otherwise, add R1 to R2 once
    @R1   // A <- Address[R1]
    D=M   // D=R1
    @R2   // A <- Address[R2]
    M=D+M // R2=R1+R2
    // Decrement R0 by 1
    @R0   // A <- Address[R0]
    M=M-1 // R0=R0-1
    // Continue looping
    @LOOP // A <- LOOP
    0;JMP // Jump to LOOP

(END)
    @END
    0;JMP