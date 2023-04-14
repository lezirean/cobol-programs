      ******************************************************************
      * Author: JOHN REIGN M. ENCINA
      * Date: 11-04-2021
      * Purpose: DIAMOND
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DIAMOND.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 GIRDLE PIC 9(2).
       01 TEMP PIC 9(2).
       01 MULT PIC S9(2).
       01 MULT-END PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIAMOND"
            DISPLAY " "
            DISPLAY "ENTER GIRDLE DIAMETER: " NO ADVANCING
            ACCEPT GIRDLE
            DISPLAY " "
            MOVE GIRDLE TO TEMP

            IF FUNCTION MOD(GIRDLE,2) = 0 THEN
                SET MULT TO 2
                SET MULT-END TO 2
                SUBTRACT 1 FROM TEMP
            ELSE
                SET MULT TO 1
                SET MULT-END TO 1
            END-IF.

            DIVIDE TEMP BY 2 GIVING TEMP
            PERFORM UNTIL MULT>GIRDLE
               PERFORM TEMP TIMES
                   DISPLAY " " NO ADVANCING
               END-PERFORM
               PERFORM MULT TIMES
                   DISPLAY "*" NO ADVANCING
               END-PERFORM
               ADD 2 TO MULT
               SUBTRACT 1 FROM TEMP
               DISPLAY " "
            END-PERFORM.

            SUBTRACT 4 FROM MULT

            PERFORM UNTIL MULT<MULT-END
               PERFORM TEMP TIMES
                   DISPLAY " " NO ADVANCING
               END-PERFORM
               PERFORM MULT TIMES
                   DISPLAY "*" NO ADVANCING
               END-PERFORM
               SUBTRACT 2 FROM MULT
               ADD 1 TO TEMP
               DISPLAY " "
            END-PERFORM.

            STOP RUN.
       END PROGRAM DIAMOND.
