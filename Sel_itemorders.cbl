 ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ITEM-ORDERS.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ORDER-NUM PIC 9(4).
       01 ORDER-TOTAL PIC 9(4)V9(2).
       01 AMT-TENDER PIC 9(4)V9(2).
      * AMOUNT TENDERED = IBABAYAD
      * ORDER TOTAL = BABAYARAN
       01 ORDER-CHANGE PIC 9(4)V9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "Enter the order number: " ACCEPT ORDER-NUM.
            DISPLAY "Enter the total amount of orders: ".
            ACCEPT ORDER-TOTAL.
            DISPLAY "Enter the amount tendered: " ACCEPT AMT-TENDER.

            IF AMT-TENDER < ORDER-TOTAL THEN
             DISPLAY "AMOUNT TENDERED MUST BE GREATER THAN TOTAL AMOUNT"
            EXIT
            END-IF.

            COMPUTE ORDER-CHANGE ROUNDED = AMT-TENDER - ORDER-TOTAL.

            DISPLAY "Your change is: " ORDER-CHANGE.

           STOP RUN.
       END PROGRAM ITEM-ORDERS.
