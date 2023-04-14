      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEMESTRAL-GRADE.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 PRELIM PIC 9(3)V9(2).
       01 MIDTERM PIC 9(3)V9(2).
       01 FINALS PIC 9(3)V9(2).
       01 SEM-AVERAGE PIC 9(3)V9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter your prelim grade: " ACCEPT PRELIM.
            DISPLAY "Enter your midterm grade: " ACCEPT MIDTERM.
            DISPLAY "Enter your final grade: " ACCEPT FINALS.

            COMPUTE SEM-AVERAGE = (PRELIM + MIDTERM + FINALS) / 3.

            DISPLAY "Your semestral grade is: " SEM-AVERAGE.
            STOP RUN.
       END PROGRAM SEMESTRAL-GRADE.
