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
       01 ROWS PIC 9(1) VALUE 5.
       01 LINESTAR PIC 9(1) VALUE 1.
       01 CUR-STAR PIC 9(1) VALUE 1.
      * 01 WS-OUT PIC X(80) VALUE SPACES.
      * 01 WS-I PIC 9(2) VALUE 0.
      * 01 WS-N PIC 9(2) VALUE 5.
      * 01 WS-CENTER PIC 9(2) VALUE 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
      *      PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-N
      *       MOVE ALL '*' TO WS-OUT(WS-CENTER:WS-I)
      *       DISPLAY WS-OUT
      *      END-PERFORM.

             PERFORM LINE-STAR VARYING CUR-STAR FROM 1 BY 1 UNTIL
               CUR-STAR > ROWS.
      *       END-PERFORM.

             STOP RUN.

       LINE-STAR.
             PERFORM DISPLAY-STAR UNTIL LINESTAR > CUR-STAR.
      *       END-PERFORM.
             SET LINESTAR TO 1.
       DISPLAY-STAR.
             PERFORM
              DISPLAY "*" WITH NO ADVANCING
             END-PERFORM.
              ADD 1 TO LINESTAR.
              ADD 1 TO CUR-STAR.
              DISPLAY "".
      *      STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
