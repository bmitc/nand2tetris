#lang racket

(struct symbol (string))

(struct memory-address (uint16))

(struct a-instruction (symbol memory-address))

(struct c-instruction (comp dest jump))

(struct label (symbol))

(struct source-line (source-string line-number))

;; List of valid strings for the computation field in a C-Instruction.
(define valid-computation-ctrings
  (list "0"
        "1"
        "-1"
        "D"
        "A"
        "!D"
        "!A"
        "-D"
        "-A"
        "D+1"
        "A+1"
        "D-1"
        "A-1"
        "D+A"
        "D-A"
        "A-D"
        "D&A"
        "D|A"
        "M"
        "!M"
        "-M"
        "M+1"
        "M-1"
        "D+M"
        "D-M"
        "M-D"
        "D&M"
        "D|M"))

;; List of valid strings for the destination field in a C-Instruction.
(define valid-destination-strings
  (list "M" "D" "MD" "A" "AM" "AD" "AMD"))

;; List of valid strings for the jump field in a C-Instruction.
(define valid-jump-strings
  (list "JGT" "JEQ" "JGE" "JLT" "JNE" "JLE" "JMP"))

