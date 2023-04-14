      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TOTAL-SALES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 FIRST-NAME PIC A(10).
       01 MIDDLE-NAME PIC A(10).
       01 LAST-NAME PIC A(10).
       01 COMPANY-POSITION PIC A(10).
       01 HOURS-WORKED PIC 9(5)V9(2).
       01 RATE-PER-HOUR PIC 9(5)V9(2).
       01 DEDUCTION PIC 9(5)V9(2).
       01 GROSS-PAY PIC 9(5)V9(2).
       01 NET-PAY PIC 9(5)V9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter first name: " ACCEPT FIRST-NAME.
            DISPLAY "Enter middle name: " ACCEPT MIDDLE-NAME.
            DISPLAY "Enter last name: " ACCEPT LAST-NAME.
            DISPLAY "Enter company position name: ".
            ACCEPT COMPANY-POSITION.
            DISPLAY "Enter the hours worked: " ACCEPT HOURS-WORKED.
            DISPLAY "Enter the rate per hour: " ACCEPT RATE-PER-HOUR.
            DISPLAY "Enter the deduction fee: " ACCEPT DEDUCTION.

            COMPUTE GROSS-PAY = HOURS-WORKED * RATE-PER-HOUR.
            COMPUTE NET-PAY = GROSS-PAY - DEDUCTION.

            DISPLAY "Employee's name in LN, FN, MN format: ".
            DISPLAY LAST-NAME ", " FIRST-NAME ", " MIDDLE-NAME.
            DISPLAY "Employee's position is: " COMPANY-POSITION.
            DISPLAY "Employee's gross pay is: " GROSS-PAY.
            DISPLAY "Employee's net pay is: " NET-PAY.

            STOP RUN.
       END PROGRAM TOTAL-SALES.
