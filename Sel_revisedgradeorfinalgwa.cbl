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
       01 PRELIM PIC 9(3)V9(2).
       01 MIDTERM PIC 9(3)V9(2).
       01 FINALS PIC 9(3)V9(2).
       01 AVERAGE PIC 9(3)V9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter your prelim grade: " ACCEPT PRELIM.
            DISPLAY "Enter your midterm grade: " ACCEPT MIDTERM.
            DISPLAY "Enter your finals grade: " ACCEPT FINALS.

            COMPUTE AVERAGE ROUNDED = (PRELIM + MIDTERM + FINALS) / 3.

            IF AVERAGE >= 97.00 AND AVERAGE <= 100.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (1.O) PASSED"
            ELSE IF AVERAGE >= 94.00 AND AVERAGE <= 96.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (1.25) PASSED"
            ELSE IF AVERAGE >= 91.00 AND AVERAGE <= 93.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (1.50) PASSED"
            ELSE IF AVERAGE >= 88.00 AND AVERAGE <= 90.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (1.75) PASSED"
            ELSE IF AVERAGE >= 85.00 AND AVERAGE <= 87.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (2.00) PASSED"
            ELSE IF AVERAGE >= 82.00 AND AVERAGE <= 84.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (2.25) PASSED"
            ELSE IF AVERAGE >= 79.00 AND AVERAGE <= 81.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (2.50) PASSED"
            ELSE IF AVERAGE >= 76.00 AND AVERAGE <= 78.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (2.75) PASSED"
            ELSE IF AVERAGE IS EQUAL TO 75.00
             DISPLAY "YOUR AVERAGE IS: " AVERAGE " (3.00) PASSED"
            ELSE
             DISPLAY "YOUR AVERAGE IS: " AVERAGE "(5.00) FAILED"
            END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
