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
         SELECT ODDOREVEN
         ASSIGN TO "C:\cobol_project\FHSel_OddOrEven.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      *********************************
       DATA DIVISION.
       FILE SECTION.
       FD ODDOREVEN.
       01 FILE-ODDOREVEN.
           05 ODDEVEN PIC S9(2).
           05 OOE-RESULT PIC X(4).
       WORKING-STORAGE SECTION.
       77 WS-ODDEVEN PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER A NUMBER: " ACCEPT WS-ODDEVEN.

            OPEN EXTEND ODDOREVEN
               MOVE WS-ODDEVEN TO ODDEVEN
               IF FUNCTION MOD(WS-ODDEVEN, 2) = 0
                   MOVE "EVEN" TO OOE-RESULT
               ELSE
                   MOVE "ODD" TO OOE-RESULT
               END-IF.
             WRITE FILE-ODDOREVEN.
            CLOSE ODDOREVEN.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
