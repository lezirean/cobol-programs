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
       01 NUM PIC 9(3).
       01 I PIC 9(3).
       01 FLAG PIC 9(1) VALUE 1.
      *01 I PIC 9(3).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER A NUMBER: " ACCEPT NUM.

            IF NUM IS EQUAL TO 0 OR 1
              MOVE 0 TO FLAG
            ELSE
              PERFORM A-PARA VARYING I FROM 2 BY 1 UNTIL I > NUM/2
            END-IF.

      *      IF FLAG IS EQUAL TO 1

       A-PARA.
            IF FUNCTION MOD(NUM, I) = 0
              MOVE 0 TO FLAG
              EXIT
      *     END-IF.

       B-PARA.
            IF FLAG IS EQUAL TO 1






            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
