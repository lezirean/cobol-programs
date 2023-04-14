      ******************************************************************
      * Author: LEILA BORROMEO
      * Date: FEBRUARY 24, 2022
      * Purpose: QUIZ 3 IN PROG 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-QUIZ-3.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ENTER PIC Z.
       01 MMCHOICE PIC X.
       01 CHOICE PIC A.
      ********** MAIN MENU VARIABLES

       01 ITEM-A.
           05 TOTAL PIC 9(3).
           05 TOTAL-DIS PIC ZZZ.
           05 NUM PIC 9(3).
           05 DIVISOR PIC 9(3).

       01 ITEM-B.
           05 NUM-A PIC 9(3).
           05 NUM-B PIC 9(3).
           05 NUM-A-INPUT PIC X(3).
           05 NUM-B-INPUT PIC X(3).
           05 GCD PIC 9(3).
           05 GCD-DIS PIC ZZZ.
           05 TEMP PIC 9(3).

       01 ITEM-C.
           05 UPTONUM-INPUT PIC X(3).
           05 UPTONUM PIC 9(3).
           05 SERIES-TOTAL PIC 9(4).
           05 SERIES-TOTAL-DIS PIC Z(4).
           05 OUTER-VAR PIC 9(3).
           05 INNER-VAR PIC 9(3).

       01 COORDINATES.
           05 ROW PIC 99 VALUE 07.
           05 COLL PIC 99 VALUE 45.

       SCREEN SECTION.
       01 CLRSCR.
          05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN.
            PERFORM UNTIL CHOICE = 'D' OR 'd'
            DISPLAY CLRSCR
            DISPLAY "MAIN MENU" AT 0642
            DISPLAY "[A] - PERFECT NUMBERS BETWEEN 1 AND 500" AT 0742
            DISPLAY "[B] - GREATEST COMMON DIVISOR OF TWO NUMS"
               AT 0842
            DISPLAY "[C] - CALCULATE SUM OF SERIES" AT 0942
            DISPLAY "[D] - EXIT" AT 1042
            DISPLAY "ENTER CHOICE: " AT 1242
            ACCEPT MMCHOICE AT 1259


            MOVE MMCHOICE TO CHOICE

                EVALUATE CHOICE
                   WHEN 'A' WHEN 'a'
                       PERFORM A
                       PERFORM GETCH

                       WHEN 'B' WHEN 'b'
                       PERFORM B
                       PERFORM GETCH

                   WHEN 'C' WHEN 'c'
                       PERFORM C
                       PERFORM GETCH

                   WHEN 'D'WHEN 'd'
                       PERFORM OUT

                   WHEN OTHER
                       DISPLAY "ERROR: INVALID INPUT" AT 1342
                       PERFORM GETCH
                       PERFORM MAIN

                    END-EVALUATE
            PERFORM GETCH
            END-PERFORM.
            STOP RUN.

       A.
           DISPLAY CLRSCR
               DISPLAY "PERFECT NUMBERS BETWEEN 1 AND 500: " AT 0645

            PERFORM VARYING NUM FROM 1 BY 1 UNTIL NUM > 500
               MOVE 0 TO TOTAL
               PERFORM VARYING DIVISOR FROM 1 BY 1 UNTIL DIVISOR > NUM/2
                   IF FUNCTION MOD(NUM, DIVISOR) IS EQUAL TO 0 THEN
                       COMPUTE TOTAL = TOTAL + DIVISOR
                        IF TOTAL IS EQUAL TO NUM AND TOTAL IS NOT EQUAL
                        TO 24 THEN
                        MOVE TOTAL TO TOTAL-DIS
                        DISPLAY TOTAL-DIS AT LINE ROW COL COLL
                        ADD 4 TO COLL
                       END-IF
                   END-IF
               END-PERFORM
            END-PERFORM

               PERFORM GETCH
               PERFORM MAIN
            EXIT.


       B.
            DISPLAY CLRSCR
            DISPLAY "ENTER NUM 1: " AT 0645
            ACCEPT NUM-A-INPUT AT 0658
            DISPLAY "ENTER NUM 2: " AT 0745
            ACCEPT NUM-B-INPUT AT 0758

            MOVE NUM-A-INPUT TO NUM-A
            MOVE NUM-B-INPUT TO NUM-B
            MOVE 1 TO GCD

            PERFORM VARYING TEMP FROM 1 BY 1 UNTIL TEMP > NUM-A AND
            TEMP > NUM-B
               IF FUNCTION MOD(NUM-A, TEMP) IS EQUAL TO 0 AND
               FUNCTION MOD(NUM-B, TEMP) IS EQUAL TO 0 THEN
                   MOVE TEMP TO GCD
               END-IF
            END-PERFORM

            MOVE GCD TO GCD-DIS
            DISPLAY "THE GCD OF THE 2 NUMBERS IS " AT 0845
            GCD-DIS AT 0873

               PERFORM GETCH
               PERFORM MAIN
            EXIT.

       C.
            DISPLAY CLRSCR
            DISPLAY "ENTER A NUMBER: " AT 0645
            ACCEPT UPTONUM-INPUT AT 0661
            MOVE UPTONUM-INPUT TO UPTONUM
            MOVE 0 TO SERIES-TOTAL

            PERFORM VARYING OUTER-VAR FROM 1 BY 1 UNTIL
            OUTER-VAR > UPTONUM
               PERFORM VARYING INNER-VAR FROM 1 BY 1 UNTIL
               INNER-VAR > OUTER-VAR
                   COMPUTE SERIES-TOTAL = SERIES-TOTAL + INNER-VAR
               END-PERFORM
            END-PERFORM

            MOVE SERIES-TOTAL TO SERIES-TOTAL-DIS
            DISPLAY "THE SUM OF THE SERIES IS " AT 0745 SERIES-TOTAL-DIS
            AT 0770

               PERFORM GETCH
               PERFORM MAIN
            EXIT.

       OUT.
           DISPLAY " ".
           EXIT.

       GETCH.
           DISPLAY "PRESS ENTER TO CONTINUE" AT 2042
           ACCEPT ENTER
           EXIT.
       END PROGRAM COBOL-QUIZ-3.
