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
       01 NAMESALESMAN PIC A(10).
       01 SALESMANNUM PIC 9(4).
       01 UNITSSOLD PIC 9(4).
       01 UNITPRICE PIC 9(5)V9(2).
       01 TOTALSALES PIC 9(5)V9(2).
       01 COMMISSION PIC 9(5)V9(2).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "ENTER SALESMAN'S NAME: " ACCEPT NAMESALESMAN.
            DISPLAY "ENTER SALESMAN'S NUMBER: " ACCEPT SALESMANNUM.
            DISPLAY "ENTER NUMBER OF UNITS SOLD: " ACCEPT UNITSSOLD.
            DISPLAY "ENTER UNIT PRICE: " ACCEPT UNITPRICE.

            COMPUTE TOTALSALES = UNITPRICE * UNITSSOLD.

            IF TOTALSALES <= 10000
             COMPUTE COMMISSION ROUNDED = TOTALSALES * 0.10
             DISPLAY "COMMISSION: " COMMISSION
            ELSE IF TOTALSALES <= 15000
             COMPUTE COMMISSION ROUNDED = TOTALSALES * 0.15
             DISPLAY "COMMISSION: " COMMISSION
            ELSE IF TOTALSALES <= 20000
             COMPUTE COMMISSION ROUNDED = TOTALSALES * 0.20
             DISPLAY "COMMISSION: " COMMISSION
            ELSE
             COMPUTE COMMISSION ROUNDED = TOTALSALES * 0.30
             DISPLAY "COMMISSION: " COMMISSION
            END-IF.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
