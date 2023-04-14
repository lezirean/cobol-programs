      ******************************************************************
      * Author: LEILA BORROMEO
      * Date: NOVEMBER 25, 2021
      * Purpose: QUIZ 1 IN PROG 3
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ENTER PIC Z.
       01 MMCHOICE PIC X.
       01 CHOICE PIC 9 COMP.
      ********** MAIN MENU VARIABLES
          01 PRE PIC 9(3)V9(2) COMP.
          01 PREIN PIC X(6).
          01 MID PIC 9(3)V9(2) COMP.
          01 MIDIN PIC X(6).
          01 FINALS PIC 9(3)V9(2) COMP.
          01 FINALSIN PIC X(6).
          01 SEMGRADE PIC 9(3)V9(2) COMP.
          01 SEMGRADEDIS PIC ZZZ.99.
          01 STU-NUMIN PIC X(15).
          01 STU-NAMEIN PIC X(15).
       SCREEN SECTION.
       01 CLRSCR.
          05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN.
            DISPLAY CLRSCR
            DISPLAY "MAIN MENU" AT 0642
            DISPLAY "[1] - ENTER STUDENT INFO AND GRADES" AT 0742
            DISPLAY "[2] - COMPUTE SEMESTRAL GRADE AND SHOW REMARKS"
               AT 0842
            DISPLAY "[3] - EXIT" AT 0942
            DISPLAY "ENTER CHOICE: " AT 1042
            ACCEPT MMCHOICE AT 1059


            MOVE MMCHOICE TO CHOICE

            EVALUATE CHOICE
               WHEN 1
                   PERFORM ONE
                   PERFORM GETCH

               WHEN 2
                   PERFORM TWO
                   PERFORM GETCH

               WHEN 3
                   PERFORM OUT

               WHEN OTHER
                   DISPLAY "ERROR: INVALID INPUT" AT 1342
                   PERFORM GETCH
                   PERFORM MAIN

            END-EVALUATE
            PERFORM GETCH
            STOP RUN.

       ONE.
           DISPLAY CLRSCR
               DISPLAY "ENTER STUDENT NUMBER: " AT 0620
               ACCEPT STU-NUMIN AT 0660
               DISPLAY "ENTER STUDENT NAME: " AT 0720
               ACCEPT STU-NAMEIN AT 0760
               DISPLAY "ENTER PRELIM GRADE: " AT 0820
               ACCEPT PREIN AT 0860
               DISPLAY "ENTER MIDTERM GRADE: " AT 0920
               ACCEPT MIDIN AT 0960
             DISPLAY "ENTER YOUR FINAL GRADE: " AT 1020
             ACCEPT FINALSIN  AT 1060

               MOVE PREIN TO PRE
               MOVE MIDIN TO MID
               MOVE FINALSIN TO FINALS

           COMPUTE SEMGRADE ROUNDED = (PRE * 0.3) + (MID * 0.3) +
                                       (FINALS * 0.4)
               MOVE SEMGRADE TO SEMGRADEDIS
           DISPLAY "SEMESTRAL GRADE IS: " AT 1142 SEMGRADEDIS

               PERFORM GETCH
               PERFORM MAIN
            EXIT.


       TWO.
            DISPLAY CLRSCR

            IF SEMGRADE >= 97.00 AND SEMGRADE <= 100.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
              SEMGRADEDIS " (1.O) PASSED" AT 1065

            ELSE IF SEMGRADE >= 94.00 AND SEMGRADE <= 96.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
              SEMGRADEDIS " (1.25) PASSED" AT 1065

            ELSE IF SEMGRADE >= 91.00 AND SEMGRADE <= 93.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (1.50) PASSED" AT 1065

            ELSE IF SEMGRADE >= 88.00 AND SEMGRADE <= 90.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (1.75) PASSED" AT 1065

            ELSE IF SEMGRADE >= 85.00 AND SEMGRADE <= 87.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (2.00) PASSED" AT 1065

            ELSE IF SEMGRADE >= 82.00 AND SEMGRADE <= 84.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (2.25) PASSED" AT 1065

            ELSE IF SEMGRADE >= 79.00 AND SEMGRADE <= 81.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (2.50) PASSED" AT 1065

            ELSE IF SEMGRADE >= 76.00 AND SEMGRADE <= 78.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (2.75) PASSED" AT 1065

            ELSE IF SEMGRADE >= 75.00 AND SEMGRADE <= 75.99
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (3.00) PASSED" AT 1065

            ELSE
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEMGRADEDIS " (5.00) FAILED" AT 1065
            END-IF.

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
       END PROGRAM YOUR-PROGRAM-NAME.
