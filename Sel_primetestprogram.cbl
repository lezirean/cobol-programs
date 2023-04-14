      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGM1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  N PIC 9(3)V99 VALUE ZERO.
       77  I PIC 9(3) VALUE ZERO.
       77  REM PIC 9(3) VALUE ZERO.
       77  Q PIC 9 VALUE ZERO.
       PROCEDURE DIVISION.
       MAIN-PARA.
               PERFORM ACCEPT-PARA.
               PERFORM PROCESS-PARA.
               PERFORM DISPLAY-PARA.
               STOP RUN.
       ACCEPT-PARA.
           DISPLAY " ENTER N VALUE : ".
           ACCEPT N.
           IF N IS ALPHABETIC
           DISPLAY N " IS ALPHABETIC : PLEASE ENTER NUMERIC VALUE  "
           ELSE IF N IS NUMERIC
           DISPLAY N " IS NUMERIC "
           ELSE
           DISPLAY N " IS ALPHANUMERIC : PLEASE ENTER NUMERIC VALUE "
           END-IF.
       PROCESS-PARA.
               MOVE 2 TO I.
              PERFORM UNTIL  I  >= N
                DIVIDE N BY I GIVING Q REMAINDER REM
                   IF REM = 0 THEN
                       DISPLAY 'GIVEN NUMBER IS NOT PRIME'
                       STOP RUN
                   END-IF
                ADD 1 TO I
              END-PERFORM.
       DISPLAY-PARA.
               IF N = I THEN
                   DISPLAY 'GIVEN NUMBER IS PRIME'
               END-IF.
            STOP RUN.
       END PROGRAM PGM1.
