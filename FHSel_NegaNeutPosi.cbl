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
         SELECT NEGANEUTPOSI
         ASSIGN TO "C:\cobol_project\FHSel_NegaNeutPosi.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD NEGANEUTPOSI.
       01 FILE-NEGANEUTPOSI.
         05 FS-NEGANEUTPOSI PIC S9(2).
         05 FS-NNPRES PIC X(8).
       WORKING-STORAGE SECTION.
       01 WS-NEGANEUTPOSI.
         05 WS-NEGANEUTPOSIINPUT PIC S9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
               DISPLAY "ENTER A NUMBER: " ACCEPT WS-NEGANEUTPOSIINPUT.

            OPEN EXTEND NEGANEUTPOSI
               MOVE WS-NEGANEUTPOSIINPUT TO FS-NEGANEUTPOSI
               IF WS-NEGANEUTPOSIINPUT IS EQUAL TO ZERO
                  MOVE "NEUTRAL" TO FS-NNPRES
               ELSE IF WS-NEGANEUTPOSIINPUT IS GREATER THAN ZERO
                   MOVE "POSITIVE" TO FS-NNPRES
               ELSE
                   MOVE "NEGATIVE" TO FS-NNPRES
               END-IF.
             WRITE FILE-NEGANEUTPOSI.
             CLOSE NEGANEUTPOSI.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
