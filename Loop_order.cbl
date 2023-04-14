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
       01 ORDER-NUM PIC 9(4).
       01 ORDER-TOTAL PIC 9(4)V9(2) VALUE 0.
       01 AMT-TENDER PIC 9(4)V9(2).
       01 AMT-ORDER PIC 9(4)V9(2) VALUE 0.
       01 ORDER-CHANGE PIC 9(4)V9(2).
       01 ORDER-ITEM PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
             DISPLAY "ENTER THE ORDER NUMBER: " WITH NO ADVANCING.
             ACCEPT ORDER-NUM.
             DISPLAY "ENTER ORDER ITEM (0 TO TERMINATE): " NO ADVANCING.
             ACCEPT ORDER-ITEM.

             PERFORM WITH TEST AFTER UNTIL ORDER-ITEM = '0'
               DISPLAY "ENTER ORDER ITEM AMOUNT: " WITH NO ADVANCING
              ACCEPT AMT-ORDER
              COMPUTE ORDER-TOTAL = AMT-ORDER + ORDER-TOTAL
            DISPLAY "ENTER ORDER ITEM (0 TO TERMINATE): " NO ADVANCING
             ACCEPT ORDER-ITEM
             END-PERFORM.

            DISPLAY "THE TOTAL AMOUNT OF ORDERS IS: " ORDER-TOTAL.
            STOP RUN.

       GET-ORDER.
            DISPLAY "ENTER ORDER ITEM AMOUNT: " WITH NO ADVANCING.
            ACCEPT AMT-ORDER.
              COMPUTE ORDER-TOTAL = AMT-ORDER + ORDER-TOTAL.
            DISPLAY "ENTER ORDER ITEM (0 TO TERMINATE): " NO ADVANCING.
             ACCEPT ORDER-ITEM.
            EXIT.
       END PROGRAM YOUR-PROGRAM-NAME.
