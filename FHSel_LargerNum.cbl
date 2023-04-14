      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT LARGERNUM
         ASSIGN TO "C:\cobol_project\FHSel_LargerNum.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD LARGERNUM.
       01 FILE-LARGERNUM.
         05 NUM-A PIC 9(2).
         05 NUM-B PIC 9(2).
         05 LARGERNUMRES PIC X(13).
       WORKING-STORAGE SECTION.
       01 WS-LARGERNUM.
        05 WS-NUM-A PIC 9(2).
        05 WS-NUM-B PIC 9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               DISPLAY "ENTER THE FIRST NUMBER: " ACCEPT WS-NUM-A.
               DISPLAY "ENTER THE SECOND NUMBER: " ACCEPT WS-NUM-B.

            OPEN EXTEND LARGERNUM
               MOVE WS-NUM-A TO NUM-A
               MOVE WS-NUM-B TO NUM-B
               IF WS-NUM-A >= WS-NUM-B
                    MOVE "1ST IS LARGER" TO LARGERNUMRES
               ELSE
                  MOVE "2ND IS LARGER" TO LARGERNUMRES
               END-IF.
            WRITE FILE-LARGERNUM.
            CLOSE LARGERNUM.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
