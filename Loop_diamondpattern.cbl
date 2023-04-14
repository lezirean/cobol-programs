      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 I PIC 9(1) VALUE 1.
       01 I2 PIC 9(1).
       01 J PIC 9(1) VALUE 1.
       01 ROWS PIC 9(1) VALUE 3.
       01 BLANKS PIC 9(1).
       01 LOWROW PIC 9(1).
       01 LOWI2 PIC 9(1).
       01 LIN PIC 99 COMP.
       01 COLL PIC 99 COMP.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            COMPUTE BLANKS = ROWS - 1.
            MOVE 08 TO LIN
            MOVE 58 TO COLL

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > ROWS
             PERFORM PRESPACE
              SUBTRACT 1 FROM BLANKS
             PERFORM UPHALFSTAR
             ADD 1 TO LIN
            END-PERFORM.

            SET BLANKS TO 1.
            COMPUTE LOWROW = ROWS - 1.

            PERFORM VARYING I FROM 1 BY 1 UNTIL I > LOWROW
             PERFORM PRESPACE
             ADD 1 TO BLANKS
             PERFORM LOWHALFSTAR
             ADD 1 TO LIN
            END-PERFORM.
            STOP RUN.
      ******************************************************************
       PRESPACE.
            MOVE 58 TO COLL
            PERFORM VARYING J FROM 1 BY 1 UNTIL J > BLANKS
             DISPLAY " " AT LINE LIN COLUMN COLL
             ADD 1 TO COLL
            END-PERFORM.
            EXIT.
      ******************************************************************
       UPHALFSTAR.
            COMPUTE I2 = (2 * I) - 1.
            PERFORM VARYING J FROM 1 BY 1 UNTIL J > I2
             DISPLAY "*" AT LINE LIN COLUMN COLL
             ADD 1 TO COLL
            END-PERFORM.
      *      DISPLAY " ".
            EXIT.
      ******************************************************************
       LOWHALFSTAR.
            COMPUTE LOWI2 = (2 * (ROWS - I)) - 1.
            PERFORM VARYING J FROM 1 BY 1 UNTIL J > LOWI2
             DISPLAY "*" AT LINE LIN COLUMN COLL
             ADD 1 TO COLL
            END-PERFORM.
      *     DISPLAY " ".
            EXIT.

       END PROGRAM YOUR-PROGRAM-NAME.
