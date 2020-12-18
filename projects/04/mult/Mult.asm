// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Compute R2=R0*R1                                  --> comment
                                                  // --> empty
// Initialize R2 to 0                                --> comment
    @R2   // A <- Address[R2]                        --> A-instruction with comment
    M=0   // R2=0                                    --> C-instruction with comment
                                                  // --> empty
(LOOP)                                            // --> L-instruction
    // Initilize D to R0.                            --> comment
    // We'll use A for the possible jump location.   --> comment
    @R0   // A <- Address[R0]                        --> A-instruction with comment
    D=M   // D=R0                                    --> C-instruction with comment
    // If R0=0, then we are done multiplying         --> comment
    @END  // A <- END to prepare for possible jump   --> A-instruction with comment
    D;JEQ // If D=0, then jump to END                --> C-instruction with comment
    // Otherwise, add R1 to R2 once                  --> comment
    @R1   // A <- Address[R1]                        --> A-instruction with comment
    D=M   // D=R1                                    --> C-instruction with comment
    @R2   // A <- Address[R2]                        --> A-instruction with comment
    M=D+M // R2=R1+R2                                --> C-instruction with comment
    // Decrement R0 by 1                             --> comment
    @R0   // A <- Address[R0]                        --> A-instruction with comment
    M=M-1 // R0=R0-1                                 --> C-instruction with comment
    // Continue looping                              --> comment
    @LOOP // A <- LOOP                               --> A-instruction with comment
    0;JMP // Jump to LOOP                            --> C-instruction with comment
                                                  // --> empty
(END)                                             // --> L-instruction
    @END                                          // --> A-instruction
    0;JMP                                         // --> C-instruction