      ******************************************************************
      * Author: GERVACIO, RANDY S.
      * Date: FEBRUARY 23, 2021
      * Purpose: FINALS IN PROGRAMMING
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN_MENU.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GRADE-FILE ASSIGN TO DISK
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  GRADE-FILE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS "GRADE.DAT"
           BLOCK CONTAINS 1 RECORDS
           RECORD CONTAINS 30 CHARACTERS
           DATA RECORD IS GRADE-REC.
       01  GRADE-REC.
            02   ACCOUNTNUM PIC 9(6).
            02   ACCOUNTNAME PIC A(15).
            02   TRANSCODE PIC A.
            02   TRANS_AMOUNT  PIC 9(10).
            02   NEWBAL PIC ZZZ,ZZZ,ZZ9.99.
            02   BALANCE1 PIC ZZZ,ZZZ,ZZ9.99.


       WORKING-STORAGE SECTION.
       01  SCREEN-COLORS PIC 9(4) COMP-5.
      *BACKGROUND-COLOR / FOREGROUND-COLOR
           78 BLACK VALUE 0.
           78 BLUE VALUE 1.
           78 GREEN VALUE 2.
           78 CYAN VALUE 3.
           78 RED VALUE 4.
           78 MAGENTA VALUE 5.
           78 BROWN VALUE 6.
           78 WHITE VALUE 7.
           78 BRIGHT-BLACK VALUE 8.
           78 BRIGHT-BLUE VALUE 9.
           78 BRIGHT-GREEN VALUE 10.
           78 BRIGHT-CYAN VALUE 11.
           78 BRIGHT-RED VALUE 12.
           78 BRIGHT-MAGENTA VALUE 13.
           78 BRIGHT-BROWN VALUE 14.
           78 BRIGHT-WHITE VALUE 15.

       01 GT PIC Z.
      *MMCHOICE
       01 CHOICE PIC X(2).
      *CHOICE
       01 CHOICEE PIC X9.


      *********************S E Q U E N T I A L*************************
      *OPERA
       01 NUM1 PIC 99.
       01 NUM11 PIC ZZ9.
       01 NUM2 PIC 99.
       01 NUM22 PIC ZZ9.

       01 SUMM PIC 999.
       01 DIFF PIC 999.
       01 PRO PIC 999.
       01 QUO PIC 9(3)V9.
       01 SUMM1 PIC -ZZ9.
       01 DIFF1 PIC -ZZ9.
       01 PRO1 PIC -ZZ9.
       01 QUO1 PIC -ZZ9.9.

      *CIRCLE
       01 R PIC 999.
       01 R1 PIC ZZ9.
       01 PI PIC 9V999 VALUE IS 3.14.
       01 CIRCU PIC 999 VALUE IS 2.
       01 CIRCUM PIC 9999V99.
       01 AREAA PIC 9999V99.
       01 CIRCUM1 PIC ZZZ9.99.
       01 AREAA1 PIC ZZZ9.99.

      *SWAP
       01 S1 PIC 9.
       01 S11 PIC ZZ9.
       01 S2 PIC 9.
       01 S22 PIC ZZ9.

      *FAHRENHEIT
       01 CEL PIC 99.
       01 C1 PIC 99.
       01 CEL1 PIC Z9.
       01 C PIC Z9.

      *COMMISSION
       01 TOTALLL PIC 99999.
      *TOTAL
       01 TOTAL PIC ZZZZZ9.
       01 TOTAL1 PIC 999999.
       01 SOLD PIC 999999.
       01 SOLD1 PIC ZZZZZ9.
       01 PRICE1 PIC ZZZZZ9.
       01 PRICE PIC 999999.
       01 USERR PIC X(25).
       01 USERNUM PIC ZZZZZ.
       01 CC PIC 9(4).

      *MFP
       01 MID PIC 99.
       01 MID1 PIC Z9.
       01 PRE PIC 99.
       01 PRE1 PIC Z9.
       01 FIN PIC 99.
       01 FIN1 PIC Z9.
       01 AVE1 PIC Z9.
       01 AVE PIC 99.

      *TRIANGLE
       01 T1 PIC 99.
       01 T11 PIC Z9.
       01 T2 PIC 99.
       01 T22 PIC Z9.
       01 T3 PIC 99.
       01 T33 PIC Z9.
       01 PERI PIC 999.
       01 PERI1 PIC ZZ9.
       01 AREAAA1 PIC ZZ9.9.
       01 AREAAA PIC 999.9.
       01 H PIC 99.
       01 H1 PIC Z9.

      *********************C O N D I T I O N A L************************

      *POSNEGNEU
       01 P1 PIC X9(2).
       01 P11 PIC -Z9.

      *ODDEVEN
       01  O1 PIC 99.
       01  O11 PIC Z9.
       01  O2 PIC 9(2).
       01  REM PIC 9(2).

      *LARGEST
      *LARGER
       01 LAR1 PIC X9.
       01 LAR2 PIC X9.
       01 LAR3 PIC X9.

      *PRIME
       01 PRIME1 PIC X9.
       01 REM1 PIC 999.
       01 REM2 PIC 999.
       01 QUOO PIC 999.

      *POF
       01 NUM55 PIC ZZ9.
       01 NUM5 PIC 999.

      *VOWEL
       01 LET PIC X(10).
       88 VOWEL VALUE 'A','E','I','O','U'.

      ***************************L O O P S******************************
      *SPACING
       01 SPA PIC 9999.

      *PRINTNUMBERS
       01 NUM PIC 99.
       01 NUMM PIC Z9.
       01 I PIC 99 VALUE 0.
       01 II PIC Z9.

      *ALLEVEN
       01  EE PIC 99.
       01 EE1 PIC Z9.
       01  E1 PIC 99.
       01 E2 PIC Z9.

      *FACTORIAL
       01 FA PIC Z9.
       01 FA1 PIC 9(2).
       01 FAC PIC 9(6) VALUE 1.
       01 FACT PIC ZZZZZ9.
       01 J PIC 9(6) VALUE 1.


      *FIBONACCI
       01 FIB PIC 99.
       01 FIB1 PIC Z9.
       01 FIBO PIC 99.
       01 CTR PIC 9999 VALUE 0.
       01 CTR1 PIC 9999 VALUE 1.
       01 CTR2 PIC 9999 VALUE 0.
       01 CTRR PIC ZZZ9.

      *REVERSE
       01 REV PIC 9(6).
       01 REMM PIC 99.
       01 REV1 PIC ZZZZZ9.
       01 RR PIC 99.
       01 RE PIC ZZZZZ9.
       01 RE1 PIC 9(6).

      *RIGHT TRIANGLE
       01 TRII PIC 999999.
       01 TRI1 PIC ZZZZZ9.
       01 TEMP PIC 999999.
       01 TEMP1 PIC 999999.
       01 CO PIC 99.
       01 LI PIC 99.


      *INVERTED
       01 INV PIC 99.
       01 INV1 PIC Z9.
       01 INVI PIC 9.
       01 INVI1 PIC 9.

      *DIAMOND
       01  N77 PIC Z9.
       01  N7 PIC 99.
       01  A7 PIC 9(2) VALUE 1.
       01  SPACESSSSS PIC 9(10) VALUE 1.
       01  O22 PIC 9(3) VALUE 1.
       01  REMAINDERS1 PIC 9(3).
       01  QUOTIENT1 PIC 9(5).
       01  COLL PIC 99.
       01  LISS PIC 99.


      ****************************FILE HANDLING*************************
       01   BALANCE PIC 9(15) VALUE 1000000.00.
       01   ACCOUNT_NUM PIC X(6).
       01   AMOUNT  PIC X(10).

       SCREEN SECTION.
       01 CLRSCR.
           02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN.
               DISPLAY CLRSCR
            DISPLAY "|=|" AT 0231 FOREGROUND-COLOUR BRIGHT-BLUE
            "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  _ " AT 0231
            FOREGROUND-COLOUR BRIGHT-CYAN
            "|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_||_|" AT 0330
            FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0430 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0471 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0530 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0571 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0630 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0671 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0730 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0771 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0830 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0871 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 0930 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 0971 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1030 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1071 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1130 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1171 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1230 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1271 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1330 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1371 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1430 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1471 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1530 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1571 FOREGROUND-COLOUR BRIGHT-CYAN
            DISPLAY "|=|" AT 1630 FOREGROUND-COLOUR BRIGHT-CYAN
            "|=|" AT 1671 FOREGROUND-COLOUR BRIGHT-CYAN

            "|_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" AT 1632
            FOREGROUND-COLOUR BRIGHT-CYAN
            "|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_||_|" AT 1730
            FOREGROUND-COLOUR BRIGHT-CYAN

            DISPLAY "MAIN MENU" AT 0546 BACKGROUND-COLOUR MAGENTA
            DISPLAY "[A] SEQUENCE" AT 0743 FOREGROUND-COLOUR BROWN
            DISPLAY "[B] SELECTION" AT 0843 FOREGROUND-COLOUR BROWN
            DISPLAY "[C] ITERATION" AT 0943 FOREGROUND-COLOUR BROWN
            DISPLAY "[D] FILE HANDLING" AT 1043 FOREGROUND-COLOUR BROWN
            DISPLAY "[E] EXIT" AT 1143 FOREGROUND-COLOUR BROWN
        DISPLAY "ENTER YOUR CHOICE: " AT 1343 BACKGROUND-COLOUR MAGENTA
            ACCEPT CHOICEE AT 1363 FOREGROUND-COLOUR WHITE

            MOVE CHOICEE TO CHOICE

            EVALUATE CHOICE

            WHEN 'A' WHEN 'a'
               PERFORM SEQ

            WHEN 'B' WHEN 'b'
               PERFORM CONDI

            WHEN 'C' WHEN 'c'
               PERFORM LOOP

            WHEN 'D' WHEN 'd'
               PERFORM FILE_HANDLING

            WHEN 'E' WHEN 'e'
               PERFORM OUT

            WHEN OTHER
               DISPLAY "INVALID" AT 1943
               PERFORM GETCH
               PERFORM MAIN

               END-EVALUATE
               STOP RUN.

       SEQ.
                PERFORM UNTIL CHOICEE = 9
                DISPLAY CLRSCR
                   DISPLAY " " AT 0219 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0219

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0320 FOREGROUND-COLOUR CYAN
            "##" AT 0376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0420 FOREGROUND-COLOUR CYAN
            "##" AT 0476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0520 FOREGROUND-COLOUR CYAN
            "##" AT 0576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0620 FOREGROUND-COLOUR CYAN
            "##" AT 0676 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0720 FOREGROUND-COLOUR CYAN
            "##" AT 0776 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0820 FOREGROUND-COLOUR CYAN
            "##" AT 0876 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0920 FOREGROUND-COLOUR CYAN
            "##" AT 0976 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1020 FOREGROUND-COLOUR CYAN
            "##" AT 1076 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1120 FOREGROUND-COLOUR CYAN
            "##" AT 1176 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1220 FOREGROUND-COLOUR CYAN
            "##" AT 1276 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1320 FOREGROUND-COLOUR CYAN
            "##" AT 1376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1420 FOREGROUND-COLOUR CYAN
            "##" AT 1476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1520 FOREGROUND-COLOUR CYAN
            "##" AT 1576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1620 FOREGROUND-COLOUR CYAN
            "##" AT 1676 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1719 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0719
            FOREGROUND-COLOUR CYAN
        DISPLAY "S E Q U E N C E " AT 0438 BACKGROUND-COLOUR MAGENTA
               DISPLAY "[A] HELLO WORLD" AT 0627 FOREGROUND-COLOUR BROWN
               DISPLAY "[B] OPERATION" AT 0727 FOREGROUND-COLOUR BROWN
                DISPLAY "[C] CIRCLE" AT 0827 FOREGROUND-COLOUR BROWN
                DISPLAY "[D] SWAP" AT 0927 FOREGROUND-COLOUR BROWN
       DISPLAY "[E] FARENHEIT CONVERTER" AT 1027 FOREGROUND-COLOUR BROWN
                DISPLAY "[F] SALESMAN" AT 0657 FOREGROUND-COLOUR BROWN
                DISPLAY "[G] AVERAGE " AT 0757 FOREGROUND-COLOUR BROWN
                DISPLAY "[H] TRIANGLE" AT 0857 FOREGROUND-COLOUR BROWN
                DISPLAY "[I] EXIT" AT 0957 FOREGROUND-COLOUR BROWN

        DISPLAY "ENTER YOUR CHOICE:" AT 1240 BACKGROUND-COLOUR MAGENTA
                ACCEPT CHOICEE AT 1259

                MOVE CHOICEE TO CHOICE

                EVALUATE CHOICE

                WHEN 'A' WHEN 'a'
                   PERFORM HW
                   PERFORM GETCH

                WHEN 'B' WHEN 'b'
                   PERFORM OPERA
                   PERFORM GETCH

                WHEN 'C' WHEN 'c'
                   PERFORM CIRCLE
                   PERFORM GETCH

                WHEN 'D' WHEN 'd'
                   PERFORM SWAP
                   PERFORM GETCH

                WHEN 'E' WHEN 'e'
                  PERFORM FAH
                   PERFORM GETCH

                WHEN 'F' WHEN 'f'
                   PERFORM TOTALL
                   PERFORM GETCH

                WHEN 'G' WHEN 'g'
                   PERFORM MFP
                   PERFORM GETCH

               WHEN 'H' WHEN 'h'
                   PERFORM TRI
                   PERFORM GETCH

                WHEN 'I' WHEN 'i'
                   PERFORM MAIN

                WHEN OTHER
                   DISPLAY "INVALID" AT 1943
                   PERFORM GETCH

                 END-EVALUATE
                 END-PERFORM.
      ******THIS PROGRAM IMMEDIATELY DISPLAYS HELLO-WORLD
       HW.

                  DISPLAY "HELLO WORLD" AT 1443.

       OPERA.
      ******THIS PROGRAM ALLOWS THE USER/S TO ENTER 2 INTEGERS.
      ******THEN THE PROGRAM WILL PERFORM THE SIMPLE ARITHMETIC, SUCH AS:
      ******ADDITION, SUBTRACTION, MULTIPLICATION AND DIVISION.
                    DISPLAY CLRSCR

          DISPLAY "O P E R A T I O N" AT 0438 BACKGROUND-COLOUR MAGENTA.
                    DISPLAY "ENTER FIRST NUMBER: " AT 0627.
                    ACCEPT NUM11 AT 0657.

                    MOVE NUM11 TO NUM1.

                    DISPLAY "ENTER SECOND NUMBER: " AT 0727.
                    ACCEPT NUM22 AT 0757.

                    MOVE NUM22 TO NUM2.

                    DISPLAY " "

                    COMPUTE SUMM= NUM1 + NUM2.
                    COMPUTE DIFF= NUM1 - NUM2.
                    COMPUTE PRO= NUM1 * NUM2.
                    COMPUTE QUO= NUM1 / NUM2.

                    MOVE SUMM TO SUMM1.
                    MOVE DIFF TO DIFF1.
                    MOVE PRO TO PRO1.
                    MOVE QUO TO QUO1.

                    DISPLAY "THE SUM IS : " AT 0927 SUMM1.
                    DISPLAY "THE DIFFERENCE IS : " AT 1027 DIFF1.
                    DISPLAY "THE PRODUCT IS : " AT 1127 PRO1.
                    DISPLAY "THE QUOTIENT IS : " AT 1227 QUO1.

       CIRCLE.
      *THIS PROGRAM WILL PRINT THE AREA AND CIRCUMFERENCE OF A CIRCLE

                    DISPLAY CLRSCR
       DISPLAY"AREA AND CIRCUMFERENCE" AT 0438 BACKGROUND-COLOUR MAGENTA
                    DISPLAY "ENTER NUMBER: " AT 0627.
                    ACCEPT R1 AT 0657.
                    MOVE R1 TO R.

                    COMPUTE CIRCUM= CIRCU * PI * R.
                    COMPUTE AREAA= PI * (R * R).

                    MOVE CIRCUM TO CIRCUM1.
                    MOVE AREAA TO AREAA1.

                    DISPLAY " ".
           DISPLAY "THE CIRCUMFERENCE OF A CIRCLE IS: " AT 0927 CIRCUM1.
           DISPLAY "THE AREA OF A CIRCLE IS: " AT 1027 AREAA1.

       SWAP.
      *THIS PROGRAM ALLOWS THE USER TO ENTER 2 INTEGERS
      *THE PROGRAM WILL AUTOMATICALLY SWAP THE 2 INTEGERS

                    DISPLAY CLRSCR
            DISPLAY "S W A P P I N G" AT 0438 BACKGROUND-COLOUR MAGENTA

                    DISPLAY "ENTER FIRST NUMBER: " AT 0627.
                    ACCEPT S11 AT 0657.
                    MOVE S11 TO S1

                    DISPLAY "ENTER SECOND NUMBER: " AT 0727.
                    ACCEPT S22 AT 0757.
                    MOVE S22 TO S2.

                    COMPUTE S1= S1 + S2.
                    COMPUTE S2= S1 - S2.
                    COMPUTE S1= S1 - S2.

                    DISPLAY "AFTER SWAPPING " AT 0927.

                    DISPLAY "FIRST NUMBER IS: " AT 1127 S1.
                    DISPLAY "SECOND NUMBER IS: " AT 1227 S2.

       FAH.
      *THIS PROGRAM WILL CONVERT THE INPUTTED FAHRENHEIT TO CELCIUS

                    DISPLAY CLRSCR
       DISPLAY "FAHRENHEIT TO CELCIUS" AT 0438 BACKGROUND-COLOUR MAGENTA
                    DISPLAY "ENTER FAHRENHEIT: " AT 0627.
                    ACCEPT C AT 0657.

                    MOVE C TO C1.

                     MOVE CEL1 TO CEL.
                    COMPUTE CEL=  (5/9) * (C1 - 32).


                    DISPLAY "THE CELCIUS IS: " AT 0827 CEL.

       TOTALL.
      *THIS PROGRAM WILL GET THE VALUE OF SOLD AND PRICE

                    DISPLAY CLRSCR
       DISPLAY "SALESMAN INFORMATION" AT 0438 BACKGROUND-COLOUR MAGENTA.

                    DISPLAY "NAME: " AT 0627.
                    ACCEPT USERR AT 0657.
                    DISPLAY "NUMBER: " AT 0727.
                    ACCEPT USERNUM AT 0757.

                    DISPLAY "UNIT PRICE: " AT 0827.
                    ACCEPT PRICE1 AT 0857.

                    MOVE PRICE1 TO PRICE.

                    DISPLAY "UNIT SOLD: " AT 0927.
                    ACCEPT SOLD1 AT 0957.

                    MOVE SOLD1 TO SOLD.


                    COMPUTE TOTAL1= PRICE * SOLD.

                    MOVE TOTAL1 TO TOTAL.
                    DISPLAY "THE TOTAL IS: " AT 1127 TOTAL.

       MFP.
      *THIS PROGRAM WILL GET THE AVERAGE OF INPUTTED NUMBERS

                    DISPLAY CLRSCR
              DISPLAY "A V E R A G E" AT 0438 BACKGROUND-COLOUR MAGENTA
                    DISPLAY "ENTER MIDTERM: " AT 0627.
                    ACCEPT MID1 AT 0657.
                    MOVE MID1 TO MID.

                    DISPLAY "ENTER PRELIM : " AT 0727.
                    ACCEPT PRE1 AT 0757.
                    MOVE PRE1 TO PRE.

                    DISPLAY "ENTER FINAL GRADE: " AT 0827.
                    ACCEPT FIN1 AT 0857.
                    MOVE FIN1 TO FIN.

                    COMPUTE AVE= (MID + PRE + FIN)/3.
                    MOVE AVE TO AVE1.

                    DISPLAY "THE AVERAGE IS: " AT 1027 AVE1.

       TRI.
      *THIS PROGRAM WILL PRINT THE AREA AND PERIMETER OF A TIRANGLE

                     DISPLAY CLRSCR
       DISPLAY "PERIMETER AND AREA" AT 0438 BACKGROUND-COLOUR MAGENTA
                     DISPLAY "ENTER THE BASE: " AT 0627.
                     ACCEPT T11 AT 0657.
                     MOVE T11 TO T1.

                     DISPLAY "ENTER THE SIDE: " AT 0727.
                     ACCEPT T22 AT 0757.
                     MOVE T22 TO T2.

                     DISPLAY "ENTER THE SIDE: " AT 0827.
                     ACCEPT T33 AT 0857.
                     MOVE T33 TO T3.

                     DISPLAY " ".

                     DISPLAY "THE HEIGHT IS: " AT 0927.
                     ACCEPT H1 AT 0957.
                     MOVE H1 TO H.


                     DISPLAY " ".
                     COMPUTE PERI= T1 + T2 +T3.
                     MOVE PERI TO PERI1.

                     DISPLAY "THE PERIMETER IS: " AT 1127 PERI1.
                     COMPUTE AREAAA= 1/2 * (T1 * H).
                     MOVE AREAAA TO AREAAA1.

                     DISPLAY "THE AREA IS: " AT 1227 AREAAA1.

       CONDI.
                PERFORM UNTIL CHOICEE =9
                DISPLAY CLRSCR
                      DISPLAY " " AT 0219 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0219

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0320 FOREGROUND-COLOUR CYAN
            "##" AT 0376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0420 FOREGROUND-COLOUR CYAN
            "##" AT 0476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0520 FOREGROUND-COLOUR CYAN
            "##" AT 0576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0620 FOREGROUND-COLOUR CYAN
            "##" AT 0676 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0720 FOREGROUND-COLOUR CYAN
            "##" AT 0776 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0820 FOREGROUND-COLOUR CYAN
            "##" AT 0876 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0920 FOREGROUND-COLOUR CYAN
            "##" AT 0976 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1020 FOREGROUND-COLOUR CYAN
            "##" AT 1076 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1120 FOREGROUND-COLOUR CYAN
            "##" AT 1176 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1220 FOREGROUND-COLOUR CYAN
            "##" AT 1276 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1320 FOREGROUND-COLOUR CYAN
            "##" AT 1376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1420 FOREGROUND-COLOUR CYAN
            "##" AT 1476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1520 FOREGROUND-COLOUR CYAN
            "##" AT 1576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1620 FOREGROUND-COLOUR CYAN
            "##" AT 1676 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1719 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0719
            FOREGROUND-COLOUR CYAN
       DISPLAY "S E L E C T I O N" AT 0438 BACKGROUND-COLOUR MAGENTA

                DISPLAY "[A]"  AT 0627 BACKGROUND-COLOUR MAGENTA
                DISPLAY "POSITIVE, NEGATIVE OR NEUTRAL" AT 0631
                DISPLAY "[B]" AT 0727 BACKGROUND-COLOUR MAGENTA
                DISPLAY "ODD OR EVEN" AT 0731
                DISPLAY "[C]" AT 0827 BACKGROUND-COLOUR MAGENTA
                DISPLAY "LARGEST AMONG 3 NUMBERS" AT 0831
                DISPLAY "[D]" AT 0927 BACKGROUND-COLOUR MAGENTA
                DISPLAY "LARGER AMONG 2 NUMBERS " AT 0931
                DISPLAY "[E]" AT 1027 BACKGROUND-COLOUR MAGENTA
                DISPLAY "PRIME NUMBER" AT 1031
                DISPLAY "[F]" AT 1127 BACKGROUND-COLOUR MAGENTA
                DISPLAY "PASSED OR FAILED" AT 1131
                DISPLAY "[G]" AT 1227 BACKGROUND-COLOUR MAGENTA
                DISPLAY "COMMISSION OF THE SALESMAN " AT 1231
                DISPLAY "[H]" AT 1327 BACKGROUND-COLOUR MAGENTA
                DISPLAY "VOWEL OR CONSONANT" AT 1331
                DISPLAY "[I]" AT 1427 BACKGROUND-COLOUR MAGENTA
                DISPLAY "EXIT" AT 1431

       DISPLAY "ENTER YOUR CHOICE: " AT 1627 BACKGROUND-COLOUR MAGENTA
                ACCEPT CHOICEE AT 1657

                MOVE CHOICEE TO CHOICE

                EVALUATE CHOICE

                WHEN 'A' WHEN 'a'
                   PERFORM POSNEG
                   PERFORM GETCH

                WHEN 'B' WHEN 'b'
                   PERFORM ODDEVEN
                   PERFORM GETCH

                WHEN 'C' WHEN 'c'
                   PERFORM LARGEST
                   PERFORM GETCH

                WHEN 'D' WHEN 'd'
                   PERFORM LARGER
                   PERFORM GETCH

                WHEN 'E' WHEN 'e'
                   PERFORM PRIMEE
                   PERFORM GETCH

                WHEN 'F' WHEN 'f'
                  PERFORM POF
                  PERFORM GETCH

                WHEN 'G' WHEN 'g'
                  PERFORM COMM
                  PERFORM GETCH

               WHEN 'H' WHEN 'h'
                   PERFORM CONVOW
                   PERFORM GETCH

               WHEN 'I' WHEN 'i'
                   PERFORM MAIN

                WHEN OTHER
                   DISPLAY "INVALID" AT 1943
                   PERFORM GETCH

                END-EVALUATE
                END-PERFORM.


       POSNEG.
      *THIS PROGRAM HAS 3 OPTIONS THE POSITIVE, NEGATIVE AND NEUTRAL
      *ONCE THE USER INPUT THE NUMBER, THE PROGRAM WILL AUTOMATICALLY TELL THE USER
      *WEATHER IT IS A NEUTRAL, NEGATIVE OR A POSITIVE INTEGER.

                   DISPLAY CLRSCR
       DISPLAY"POSITIVE/NEGATIVE/NEUTRAL"AT 0428 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER NUMBER: " AT 0627.
                   ACCEPT P1 AT 0657.

                   IF (P1>0)
                       DISPLAY "IT IS POSITIVE" AT 0827

                   ELSE IF (P1<0)
                       DISPLAY "IT IS NEGATIVE" AT 0827

                   ELSE
                       DISPLAY "IT IS NEUTRAL" AT 0827.

               ODDEVEN.
      *THIS PROGRAM WILL TELL THE USER WEATHER THE NUMBER IS ODD OR EVEN

                   DISPLAY CLRSCR
           DISPLAY "ODD OR EVEN" AT 0428 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER NUMBER: " AT 0627.
                   ACCEPT O11 AT 0657.
                   MOVE O11 TO O1.

                   DIVIDE O1 BY 2 GIVING REM REMAINDER O2
                   IF O2 IS EQUAL TO 0
                       DISPLAY "IT IS EVEN NUMBER" AT 0827

                   ELSE
                       DISPLAY "IT IS ODD NUMBER" AT 0827.

               LARGEST.
      *THIS PROGRAM WILL FIND OUT THE LARGEST INPUTTED NUMBER

                   DISPLAY CLRSCR
                DISPLAY " L A R G E S T" AT 0438 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER FIRST NUMBER: " AT 0627.
                   ACCEPT LAR1 AT 0657.

                   DISPLAY "ENTER SECOND NUMBER: " AT 0727.
                   ACCEPT LAR2 AT 0757.

                   DISPLAY "ENTER THIRD NUMBER: " AT 0827.
                   ACCEPT LAR3 AT 0857.

                   IF (LAR1 > LAR2 AND  LAR1 > LAR3)
                       DISPLAY "THE LARGEST NUMBER IS: " AT 1027 LAR1

                   ELSE IF (LAR2 > LAR1 AND LAR2 > LAR3)
                       DISPLAY "THE LARGEST NUMBER IS: " AT 1027 LAR2

                   ELSE
                       DISPLAY "THE LARGEST NUMBER IS: " AT 1027 LAR3.

               LARGER.
      *THIS PROGRAM WILL FIND OUT THE LARGEST INPUTTED NUMBER

                   DISPLAY CLRSCR
                   DISPLAY "L A R G E R" AT 0438 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER FIRST NUMBER: " AT 0627.
                   ACCEPT LAR1 AT 0657.

                   DISPLAY "ENTER SECOND NUMBER: " AT 0727.
                   ACCEPT LAR2 AT 0757.

                   IF (LAR1 > LAR2)
                       DISPLAY "THE LARGER NUMBER IS: " AT 0927 LAR1

                   ELSE
                       DISPLAY "THE LARGEST NUMBER IS: " AT 0927 LAR2.

              PRIMEE.
      *THIS PROGRAM WILL FIND OUT IF THE NUMBER IS PRIME OR NOT

                   DISPLAY CLRSCR
                   DISPLAY "P R I M E" AT 0438 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER NUMBER: " AT 0627.
                   ACCEPT PRIME1 AT 0657.

                   IF PRIME1 = 0 OR PRIME1= 1 THEN
                       DISPLAY "IT IS NOT A PRIME NUMBER" AT 0827

                   ELSE IF PRIME1 = 2 OR PRIME1= 3 THEN
                       DISPLAY "IT IS A PRIME NUMBER " AT 0827

                   ELSE
                       DIVIDE PRIME1 BY 2 GIVING QUOO REMAINDER REM1
                   DIVIDE PRIME1 BY 3 GIVING QUOO REMAINDER REM2

                   IF REM1=0 OR REM2=0 THEN
                      DISPLAY "IT IS NOT A PRIME NUMBER" AT 0827

                   ELSE
                       DISPLAY "IT IS A PRIME NUMBER" AT 0827

                   END-IF.

               POF.
      *THIS PROGRAM WILL FIND OUT IF THE GRADE YOU ENTER IS PASS OR FAIL

                   DISPLAY CLRSCR
              DISPLAY "PASSED OR FAILED" AT 0438 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER GRADE: " AT 0627.
                   ACCEPT NUM55 AT 0657.
                   MOVE NUM55 TO NUM5.

                   IF (NUM5 <= 100 AND NUM5 >=97)
                       DISPLAY "1.0, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=96 AND NUM5 >=94)
                       DISPLAY "1.25, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=93 AND NUM5 >=91)
                       DISPLAY "1.5, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=90 AND NUM5 >=88)
                       DISPLAY "1.75, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=87 AND NUM5 >=85)
                       DISPLAY "2.0, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=84 AND NUM5 >=82)
                       DISPLAY "2.25, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=81 AND NUM5 >=79)
                       DISPLAY "2.5, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=78 AND NUM5 >=76)
                       DISPLAY "2.75, YOU PASSED" AT 0827

                   ELSE IF (NUM5 = 75)
                       DISPLAY "3.0, YOU PASSED" AT 0827

                   ELSE IF (NUM5 <=74 AND NUM5 >=0)
                       DISPLAY "5.0, YOU FAILED" AT 0827

                   ELSE
                       DISPLAY "INVALID" AT 0827.

               COMM.
      *THIS PROGRAM WILL TELL THE SALESMAN'S INFORMATION
      *WILL GET THE TOTAL OF SOLD AND PRICE AND WILL COMPUTE FOR THE COMMISSION

                    DISPLAY CLRSCR
        DISPLAY "SALESMAN INFORMATION" AT 0428 BACKGROUND-COLOUR BLUE.

                    DISPLAY "NAME: " AT 0627.
                    ACCEPT USERR AT 0657.
                    DISPLAY "NUMBER: " AT 0727.
                    ACCEPT USERNUM AT 0757.

                    DISPLAY "UNIT PRICE: " AT 0827.
                    ACCEPT PRICE1 AT 0857.

                    MOVE PRICE1 TO PRICE.

                    DISPLAY "UNIT SOLD: " AT 0927.
                    ACCEPT SOLD1 AT 0957.

                    MOVE SOLD1 TO SOLD.


                    COMPUTE TOTALLL= PRICE * SOLD.


                    DISPLAY "THE TOTAL IS: " AT 1127 TOTALLL.

                    IF (TOTALLL <= 10000)
                        COMPUTE CC= TOTALLL * 0.1

                   ELSE IF (TOTALLL <=15000)
                        COMPUTE CC= TOTALLL * 0.15

                   ELSE IF (TOTALLL <= 20000)
                       COMPUTE CC= TOTALLL * 0.2

                   ELSE
                       COMPUTE CC= TOTALLL * 0.3.

                   DISPLAY "THE COMMISSION IS: " AT 1227 CC.

                CONVOW.
      *THIS PROGRAM WILL FIND OUT IF THE LETTER IS A VOWEL OR CONSONANT

                   DISPLAY CLRSCR
       DISPLAY "CONSONANT OR VOWEL" AT 0438 BACKGROUND-COLOUR BLUE
                   DISPLAY "ENTER THE LETTER: " AT 0627.
                   ACCEPT LET AT 0657.

                   EVALUATE TRUE

                   WHEN VOWEL
                   DISPLAY "IT IS VOWEL" AT 0827

                   WHEN OTHER
                   DISPLAY "IT IS CONSONANT" AT 0827

                   END-EVALUATE.

       LOOP.
                   PERFORM UNTIL CHOICE= 9
                   DISPLAY CLRSCR

                       DISPLAY " " AT 0219 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0219

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0320 FOREGROUND-COLOUR CYAN
            "##" AT 0376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0420 FOREGROUND-COLOUR CYAN
            "##" AT 0476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0520 FOREGROUND-COLOUR CYAN
            "##" AT 0576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0620 FOREGROUND-COLOUR CYAN
            "##" AT 0676 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0720 FOREGROUND-COLOUR CYAN
            "##" AT 0776 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0820 FOREGROUND-COLOUR CYAN
            "##" AT 0876 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0920 FOREGROUND-COLOUR CYAN
            "##" AT 0976 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1020 FOREGROUND-COLOUR CYAN
            "##" AT 1076 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1120 FOREGROUND-COLOUR CYAN
            "##" AT 1176 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1220 FOREGROUND-COLOUR CYAN
            "##" AT 1276 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1320 FOREGROUND-COLOUR CYAN
            "##" AT 1376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1420 FOREGROUND-COLOUR CYAN
            "##" AT 1476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1520 FOREGROUND-COLOUR CYAN
            "##" AT 1576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1620 FOREGROUND-COLOUR CYAN
            "##" AT 1676 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1719 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0719
            FOREGROUND-COLOUR CYAN
       DISPLAY "I T E R A T I O N "AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "[A]" AT 0627 BACKGROUND-COLOUR RED
                   DISPLAY "NUMBERS FROM 1 TO N" AT 0631
                   DISPLAY "[B]" AT 0727 BACKGROUND-COLOUR RED
                   DISPLAY "ALL EVEN FORM 1 TO N" AT 0731
                   DISPLAY "[C]" AT 0827 BACKGROUND-COLOUR RED
                   DISPLAY "FACTORIAL" AT 0831
                   DISPLAY "[D]" AT 0927 BACKGROUND-COLOUR RED
                   DISPLAY "FIBONACCI" AT 0931
                   DISPLAY "[E]" AT 1027 BACKGROUND-COLOUR RED
                   DISPLAY "REVERSE THE ENTER NUMBER" AT 1031
                   DISPLAY "[F]" AT 1127 BACKGROUND-COLOUR RED
                   DISPLAY "RIGHT TRIANGLE ASTERTISK" AT 1131
                   DISPLAY "[G]" AT 1227 BACKGROUND-COLOUR RED
                   DISPLAY "NUMBERS INVERTED" AT 1231
                   DISPLAY "[H]" AT 1327 BACKGROUND-COLOUR RED
                   DISPLAY "DIAMOND ASTERISK" AT 1331
                   DISPLAY "[I]" AT 1427 BACKGROUND-COLOUR RED
                   DISPLAY "EXIT" AT 1431
       DISPLAY " ENTER YOUR CHOICE:" AT 1627 BACKGROUND-COLOUR RED
                   ACCEPT CHOICEE AT 1657
                   MOVE CHOICEE TO CHOICE

                   EVALUATE CHOICE

                   WHEN 'A' WHEN 'a'
                       PERFORM PRINTNUMBERS
                       PERFORM GETCH

                   WHEN 'B' WHEN 'b'
                       PERFORM ALLEVEN
                       PERFORM GETCH

                   WHEN 'C' WHEN 'c'
                       PERFORM FACTORIAL
                       PERFORM GETCH

                   WHEN 'D' WHEN 'd'
                       PERFORM FIBONACCI
                       PERFORM GETCH

                    WHEN 'E' WHEN 'e'
                       PERFORM REVERSEE
                       PERFORM GETCH

                   WHEN 'F' WHEN 'f'
                       PERFORM RT
                       PERFORM GETCH

                   WHEN 'G' WHEN 'g'
                       PERFORM INVERTED
                       PERFORM GETCH

                   WHEN 'H' WHEN 'h'
                       PERFORM DIAMOND
                       PERFORM GETCH

                   WHEN 'I' WHEN 'i'
                       DISPLAY " "
                       PERFORM MAIN

                   WHEN OTHER
                       DISPLAY "INVALID" AT 1943
                       PERFORM GETCH

                   END-EVALUATE
                   END-PERFORM.


       PRINTNUMBERS.
      *THIS PROGRAM WILL PRINT ALL THE NUMBER FROM 1 TO N.


                   DISPLAY CLRSCR.
                   COMPUTE SPA= 0827

       DISPLAY "PRINT NUMBERS" AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "ENTER NUMBER: " AT 0627.
                   ACCEPT NUMM AT 0657.
                   MOVE NUMM TO NUM

                   PERFORM VARYING I FROM 1 BY 1 UNTIL I > NUM
                   MOVE I TO II
                   DISPLAY II AT SPA
                   ADD 3 TO SPA

                   END-PERFORM.

       ALLEVEN.
      *THIS PROGRAM WILL PRINT ALL THE EVEN NUMBERS FROM 1 TO N.

                   DISPLAY CLRSCR.
                   COMPUTE SPA= 0827

       DISPLAY "ALL EVEN NUMBERS" AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "ENTER NUMBER: " AT 0627.
                   ACCEPT EE1 AT 0657.
                   MOVE EE1 TO EE.


                   PERFORM VARYING E1 FROM 2 BY 2 UNTIL E1 > EE
                   MOVE E1 TO E2
                   DISPLAY E2 AT SPA
                   ADD 5 TO SPA

                   END-PERFORM.

               FACTORIAL.
      *THIS PROGRAM WILL PRINT THE FACTORIAL OF INPUTTED NUMBER

                DISPLAY CLRSCR

       DISPLAY "F A C T O R I A L" AT 0438 BACKGROUND-COLOUR RED

                DISPLAY "ENTER NUMBER: " AT 0627.
                ACCEPT FA AT 0657.

                MOVE FA TO FA1.

                PERFORM VARYING J FROM 1 BY 1 UNTIL J > FA1

                COMPUTE FAC = FAC * J

                MOVE FAC TO FACT

                END-PERFORM.

                DISPLAY " ".

                DISPLAY "THE FACTORIAL IS: " AT 0827 FACT.

                FIBONACCI.
      *THIS PROGRAM WILL PRINT THE SEQUENCE OF NUMBERS IN FIBONACCI FROM 1 TO N.

                   DISPLAY CLRSCR.
                   COMPUTE SPA= 0827.

               DISPLAY "F I B O N A C C I"AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "ENTER NUMBER: " AT 0627
                   ACCEPT FIB1 AT 0657.
                   MOVE FIB1 TO FIB.

                   PERFORM VARYING FIBO FROM 0 BY 1 UNTIL FIBO= FIB

                   COMPUTE CTR2= CTR + CTR1
                   COMPUTE CTR1 = CTR
                   COMPUTE CTR= CTR2

                   MOVE CTR TO CTRR
                   DISPLAY CTRR AT SPA
                   ADD 5 TO SPA

                   END-PERFORM.

               REVERSEE.
      *THIS PROGRAM WILL PRINT ALL THE NUMBER FROM 1 TO N IN REVERSE.

                   DISPLAY CLRSCR
                   COMPUTE SPA= 0857.

                   COMPUTE REV = 0.

                   DISPLAY "R E V E R S E"AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "ENTER NUMBER :" AT 0627.
                   ACCEPT RE AT 0657.
                   MOVE RE TO RE1.

                   PERFORM UNTIL RE1 = 0

                   DIVIDE RE1 BY 10 GIVING RR REMAINDER REMM
                   COMPUTE REV= REV * 10 + REMM
                   COMPUTE RE1= RE1 / 10

                   MOVE REV TO REV1

                   END-PERFORM


               DISPLAY "THE REVERSE NUMBER IS: " AT 0827 REV1 AT SPA.



       RT.
      *THIS PROGRAM WILL PRINT THE ASTERISK TRIANGLE.

                DISPLAY CLRSCR.
                COMPUTE CO= 27.
                COMPUTE LI= 08.

                DISPLAY "RIGHT TRIANGLE" AT 0438 BACKGROUND-COLOUR RED

                DISPLAY "ENTER NUMBER: " AT 0627.
                ACCEPT TRI1 AT 0657.

                MOVE TRI1 TO TRII.

            PERFORM VARYING TEMP FROM 1 BY 1 UNTIL TEMP > TRII
                DISPLAY " "

             PERFORM VARYING TEMP1 FROM 1 BY 1 UNTIL TEMP1 > TEMP
                DISPLAY "*" LINE LI COLUMN CO

                ADD 2 TO CO

             END-PERFORM

                COMPUTE CO=27

                ADD 1 TO LI

            END-PERFORM.

       INVERTED.
      *THIS PROGRAM WILL PRINT ALL THE NUMBER FROM 1 TO N IN INVERTED TRIANGLE FORM.

                DISPLAY CLRSCR.
                COMPUTE CO= 27.
                COMPUTE LI= 08.

               DISPLAY "INVERTED TRIANGLE" AT 0438 BACKGROUND-COLOUR RED

                DISPLAY "ENTER NUMBER: " AT 0627.
                ACCEPT INV1 AT 0657.
                MOVE INV1 TO INV.

                PERFORM VARYING INVI FROM INV BY -1 UNTIL INVI < 1

                PERFORM VARYING INVI1 FROM INVI BY -1 UNTIL INVI1 < 1
                DISPLAY " " INVI LINE LI COLUMN CO

                ADD 2 TO CO

                END-PERFORM

                COMPUTE CO=27

                ADD 1 TO LI

                END-PERFORM.


       DIAMOND.
      *THIS PROGRAM WILL PRINT THE ASTERISK DIAMOND IN ACCORDANCE TO THE INPUTTED NUMBER.

             DISPLAY CLRSCR.
            DISPLAY "D I A M O N D" AT 0438 BACKGROUND-COLOUR RED

            DISPLAY "ENTER NUMBER OF ROWS: " AT 0627.
            ACCEPT N77 AT 0657.

            MOVE N77 TO N7.



            COMPUTE LISS = 08.
            COMPUTE COLL = 27.
            COMPUTE SPACESSSSS = COLL - N7.

            PERFORM VARYING A7 FROM 1 BY 1 UNTIL A7 > N7
            PERFORM VARYING O22 FROM 1 BY 1 UNTIL O22 > SPACESSSSS
                COMPUTE COLL = SPACESSSSS
                DISPLAY " " LINE LISS COLUMN COLL

             END-PERFORM

             COMPUTE SPACESSSSS = SPACESSSSS - 1


            PERFORM VARYING O22 FROM 1 BY 1 UNTIL O22 > 2 * A7 - 1
             DIVIDE  O22 BY 2 GIVING QUOTIENT1 REMAINDER REMAINDERS1

             IF (REMAINDERS1 = 1)
                ADD 2 TO COLL
                DISPLAY "* " LINE LISS COLUMN COLL

            END-PERFORM

            ADD 1 TO LISS
            COMPUTE COLL = SPACESSSSS

            END-PERFORM.

            COMPUTE SPACESSSSS = COLL + N7

            PERFORM VARYING A7 FROM 1 BY 1 UNTIL A7 > N7
            PERFORM VARYING O22 FROM 1 BY 1 UNTIL O22 > SPACESSSSS

             COMPUTE COLL = SPACESSSSS
             DISPLAY " " LINE LISS COLUMN COLL
             END-PERFORM


             ADD 1 TO SPACESSSSS

             COMPUTE COLL = SPACESSSSS - N7 + 1
           PERFORM VARYING O22 FROM 1 BY 1 UNTIL O22 > 2 * (N7 - A7) - 1
             DIVIDE O22 BY 2 GIVING QUOTIENT1 REMAINDER REMAINDERS1

             IF (REMAINDERS1 = 1)
               ADD 2 TO COLL
              DISPLAY "* " LINE LISS COLUMN COLL

             END-PERFORM

             ADD 1 TO LISS

             END-PERFORM.

                  FILE_HANDLING.
                   PERFORM UNTIL CHOICE= 9
                   DISPLAY CLRSCR

                       DISPLAY " " AT 0219 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0219

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0320 FOREGROUND-COLOUR CYAN
            "##" AT 0376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0420 FOREGROUND-COLOUR CYAN
            "##" AT 0476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0520 FOREGROUND-COLOUR CYAN
            "##" AT 0576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0620 FOREGROUND-COLOUR CYAN
            "##" AT 0676 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0720 FOREGROUND-COLOUR CYAN
            "##" AT 0776 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0820 FOREGROUND-COLOUR CYAN
            "##" AT 0876 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0920 FOREGROUND-COLOUR CYAN
            "##" AT 0976 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1020 FOREGROUND-COLOUR CYAN
            "##" AT 1076 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1120 FOREGROUND-COLOUR CYAN
            "##" AT 1176 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1220 FOREGROUND-COLOUR CYAN
            "##" AT 1276 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1320 FOREGROUND-COLOUR CYAN
            "##" AT 1376 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1420 FOREGROUND-COLOUR CYAN
            "##" AT 1476 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1520 FOREGROUND-COLOUR CYAN
            "##" AT 1576 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1620 FOREGROUND-COLOUR CYAN
            "##" AT 1676 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1719 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0719
            FOREGROUND-COLOUR CYAN
       DISPLAY "FILE HANDLING" AT 0438 BACKGROUND-COLOUR RED

                   DISPLAY "[A]" AT 0627 BACKGROUND-COLOUR BROWN
                   DISPLAY "BANK ACCOUNT" AT 0631
                   DISPLAY "[B]" AT 0727 BACKGROUND-COLOUR BROWN
                   DISPLAY "EXIT" AT 0731
       DISPLAY "ENTER YOUR CHOICE:" AT 0927 BACKGROUND-COLOUR RED
                   ACCEPT CHOICEE AT 0957
                   MOVE CHOICEE TO CHOICE

                   EVALUATE CHOICE

                   WHEN 'A' WHEN 'a'
                       PERFORM FH
                       PERFORM GETCH

                   WHEN 'B' WHEN 'b'
                       PERFORM MAIN

                   WHEN OTHER
                       DISPLAY "INVALID" AT 1943
                       PERFORM GETCH

                       END-EVALUATE
                       END-PERFORM.

       FH.
      *THIS PROGRAM WILL ALLOW THE USER TO ENTER HIS/HER BANK INFORMATION
      *AND TELLS THE PROGRAM IF HE/SHE WANTS TO DEPOSIT OR WITHDRAW THE MONEY
      *THEN THE MACHINE WILL TELL THE BALANCE IN HIS/HER BANK ACCOUNT

                 DISPLAY CLRSCR.
              OPEN EXTEND GRADE-FILE.
                WRITE GRADE-REC.
            CLOSE GRADE-FILE.

            PERFORM BANK.
            PERFORM CONFIRM.
            DISPLAY CLRSCR

            IF (CHOICE IS EQUAL TO 'Y') THEN
               MOVE NEWBAL TO BALANCE
               PERFORM BANK
               PERFORM CONFIRM
            ELSE IF (CHOICE IS EQUAL TO 'N') THEN
               PERFORM FILE_HANDLING

            ELSE
                PERFORM FILE_HANDLING

            END-IF.

       BANK.
       DISPLAY "BANK TRANSACTION" AT 0748 BACKGROUND-COLOUR GREEN
            DISPLAY"___________________________________" AT 0840.

            DISPLAY "ENTER BANK ACCOUNT NAME: " AT 1235
            ACCEPT ACCOUNTNAME AT 1265.
            DISPLAY "ENTER BANK ACCOUNT NUMBER: " AT 1335
            ACCEPT ACCOUNT_NUM AT 1365.
            DISPLAY "ENTER TRANSACTION CODE: " AT 1435
            ACCEPT TRANSCODE AT 1465.
            DISPLAY CLRSCR.

            IF (TRANSCODE IS EQUAL TO 'W' OR 'w') THEN
               DISPLAY CLRSCR
               PERFORM WITHDRAW
            ELSE IF (TRANSCODE IS EQUAL TO 'D' OR 'd')
                DISPLAY CLRSCR
               PERFORM DEPOSIT
            ELSE
               DISPLAY "INVALID CODE!"
            END-IF.


       WITHDRAW.
            DISPLAY "WITHDRAW" AT 0750 BACKGROUND-COLOUR GREEN
            DISPLAY"___________________________________" AT 0840.
            MOVE BALANCE TO BALANCE1.
            DISPLAY "YOUR BALANCE IS ", AT 1242 BALANCE1 .
            DISPLAY "ENTER AMOUNT: " AT 1342
            ACCEPT AMOUNT AT 1360.

            MOVE AMOUNT TO TRANS_AMOUNT.
            COMPUTE NEWBAL = BALANCE - TRANS_AMOUNT
            DISPLAY "NEW BALANCE: " AT 1442 NEWBAL.

       DEPOSIT.
            DISPLAY "DEPOSIT" AT 0750 BACKGROUND-COLOUR GREEN
            DISPLAY"___________________________________" AT 0840.
            MOVE BALANCE TO BALANCE1.
            DISPLAY "YOUR BALANCE IS ", AT 1242 BALANCE1.
            DISPLAY "ENTER AMOUNT: " AT 1342
            ACCEPT AMOUNT AT 1360.

            MOVE AMOUNT TO TRANS_AMOUNT.
            COMPUTE NEWBAL = BALANCE + TRANS_AMOUNT
            DISPLAY "NEW BALANCE: " AT 1442 NEWBAL.

       CONFIRM.
            DISPLAY "DO YOU WANT TO CONTINUE?" AT 2045
            DISPLAY "[Y] YES" AT 2145.
            DISPLAY "[N] NO" AT 2155.
       DISPLAY "ENTER YOUR CHOICE: " AT 2345 BACKGROUND-COLOUR MAGENTA
            ACCEPT CHOICE AT 2365.



       OUT.
                  DISPLAY " ".

       GETCH.
       DISPLAY "PRESS ENTER TO CONTINUE"AT 2443 FOREGROUND-COLOUR BROWN.
                  ACCEPT GT.

       END PROGRAM MAIN_MENU.
