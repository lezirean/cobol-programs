      ******************************************************************
      * Author: LEILA BORROMEO
      * Date: FEBRUARY 17, 2022
      * Purpose: QUIZ 2 IN PROG 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-QUIZ-2.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ENTER PIC Z.
       01 MMCHOICE PIC X.
       01 CHOICE PIC A.
      ********** MAIN MENU VARIABLES

       01 ITEM-A.
           05 RAD-INPUT PIC X(7).
           05 RAD PIC 9(3)V9(3).
           05 VOLUME PIC 9(5)V9(3).
           05 VOLUME-DIS PIC ZZZZZ.999.
           05 PI-VALUE PIC 9V9(5) VALUE 3.14159.

       01 ITEM-B.
           05 KM-TO-M-CONSTANT PIC 9V9(6) VALUE 0.621371.
           05 KM-INPUT PIC X(9).
           05 KM PIC 9(5)V9(3).
           05 MILES PIC 9(5)V9(3).
           05 MILES-DIS PIC ZZZZ9.999.

       01 ITEM-C.
           05 DIVIDEND PIC 9(6).
           05 DIVIDEND-INPUT PIC X(3).
           05 DIVISOR PIC 9(6).
           05 DIVISOR-INPUT PIC X(6).
           05 QUOTIENT PIC 9(6).
           05 QUOTIENT-DIS PIC ZZZZZZ.
           05 REM PIC 9(4).
           05 REM-DIS PIC ZZZZ.

       SCREEN SECTION.
       01 CLRSCR.
          05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN.
            PERFORM UNTIL CHOICE = 'D' OR 'd'
            DISPLAY CLRSCR
            DISPLAY "MAIN MENU" AT 0642
            DISPLAY "[A] - CALCULATE VOLUME OF A SPHERE" AT 0742
            DISPLAY "[B] - KILOMETERS PER HOUR TO MILES PER HOUR"
               AT 0842
            DISPLAY "[C] - COMPUTE QUOTIENT AND REMAINDER" AT 0942
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
               DISPLAY "ENTER THE RADIUS OF THE SPHERE: " AT 0645
               ACCEPT RAD-INPUT AT 0677
               MOVE RAD-INPUT TO RAD

            COMPUTE VOLUME ROUNDED= (4/3) * PI-VALUE * (RAD * RAD * RAD)
               MOVE VOLUME TO VOLUME-DIS

               DISPLAY "THE VOLUME OF THE SPHERE IS " AT 0745 VOLUME-DIS
                   AT 0772

               PERFORM GETCH
               PERFORM MAIN
            EXIT.


       B.
            DISPLAY CLRSCR
            DISPLAY "ENTER NUMBER OF KILOMETERS PER HOUR: " AT 0645
            ACCEPT KM-INPUT AT 0682
            MOVE KM-INPUT TO KM

            COMPUTE MILES ROUNDED = KM * KM-TO-M-CONSTANT
            MOVE MILES TO MILES-DIS

            DISPLAY "THE VALUE CONVERTED INTO MILES PER HOUR IS: "
             AT 0745 MILES-DIS AT 0787

               PERFORM GETCH
               PERFORM MAIN
            EXIT.

       C.
            DISPLAY CLRSCR
            DISPLAY "ENTER THE DIVIDEND: " AT 0645
            ACCEPT DIVIDEND-INPUT AT 0665
            MOVE DIVIDEND-INPUT TO DIVIDEND

            DISPLAY "ENTER THE DIVISOR: " AT 0745
            ACCEPT DIVISOR-INPUT AT 0764
            MOVE DIVISOR-INPUT TO DIVISOR

            IF DIVISOR IS EQUAL TO 0
                DISPLAY "INVALID DIVISOR" AT 0845
                PERFORM GETCH
                PERFORM C
            END-IF.

            COMPUTE QUOTIENT = DIVIDEND / DIVISOR
            MOVE QUOTIENT TO QUOTIENT-DIS

            COMPUTE REM = FUNCTION MOD(DIVIDEND, DIVISOR)
            MOVE REM TO REM-DIS

            DISPLAY "THE QUOTIENT IS " AT 0945 QUOTIENT-DIS AT 0961
            DISPLAY "THE REMAINDER IS " AT 1045
              REM-DIS AT 1062

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
       END PROGRAM COBOL-QUIZ-2.
