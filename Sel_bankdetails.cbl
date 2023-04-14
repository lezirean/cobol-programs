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
       01 BANK-NUM PIC 9(5).
       01 BANK-NAME PIC X(10).
       01 BALANCE PIC 9(6)V9(2).
       01 T-CODE PIC A.
      *T-CODE = TRANSACTION CODE
       01 T-AMOUNT PIC 9(6)V9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER BANK ACCOUNT NUMBER: " ACCEPT BANK-NUM.
            DISPLAY "ENTER BANK ACCOUNT NAME: " ACCEPT BANK-NAME.
            DISPLAY "ENTER BALANCE: " ACCEPT BALANCE.
            DISPLAY "ENTER TRANSACTION CODE "
            "(W FOR WITHDRAW AND D FOR DEPOSIT): " ACCEPT T-CODE.
      *      DISPLAY "ENTER TRANSACTION AMOUNT: " ACCEPT T-AMOUNT.

            IF T-CODE IS EQUAL TO 'D' OR 'd'
             DISPLAY "ENTER DEPOSIT AMOUNT: " ACCEPT T-AMOUNT
             COMPUTE BALANCE = BALANCE + T-AMOUNT
             DISPLAY "YOUR NEW BALANCE IS: " BALANCE

            ELSE IF T-CODE IS EQUAL TO 'W' OR 'w'
             DISPLAY "ENTER WITHDRAWAL AMOUNT: " ACCEPT T-AMOUNT
             COMPUTE BALANCE = BALANCE - T-AMOUNT
             DISPLAY "YOUR NEW BALANCE IS: " BALANCE

            ELSE
             DISPLAY "INVALID ENTRY."
            END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
