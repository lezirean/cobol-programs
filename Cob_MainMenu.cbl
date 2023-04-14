      ******************************************************************
      * Author: LEILA H. BORROMEO
      * Date: 17 NOVEMBER 2021
      * Purpose: SEQ, SEL, AND LOOPS MAIN MENU
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-MENU.
       DATA DIVISION.
       FILE SECTION.
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
      *ENTERR FOR ENTER (Z SUPPRESSES LEADING ZEROS BY TURNING THEM TO SPACE
       01 ENTERR PIC Z.
       01 MMCHOICE PIC X(2).
       01 CHOICE PIC X9.
      ***********************SEQUENTIAL VARIABLES***********************
      ******* ARITHMETIC OPERATIONS VARIABLES
       01 ARITHMETIC-SEQ.
          05 NUM1 PIC 99V99 COMP.
          05 NUM1IN PIC X(5).
          05 NUM2 PIC 99V99 COMP.
          05 NUM2IN PIC X(5).
          05 TOTAL PIC 99V99 COMP.
          05 TOTALDIS PIC ZZ9.99.
          05 DIFF PIC 99V99 COMP.
          05 DIFFDIS PIC ZZ9.99.
          05 QUO PIC 99V99.
          05 QUODIS PIC ZZ9.99.
          05 PROD PIC 99V99.
          05 PRODDIS PIC ZZ9.99.
      ******* CIRCLE VARIABLES
       01 CIRCLE-SEQ.
          05 RADIUS PIC 99V99 COMP.
          05 RADIUSIN PIC X(5).
          05 CIRCUM PIC 9999V99 COMP.
          05 CIRCUMDIS PIC ZZZZ.99.
          05 PI-VALUE PIC 9V99999 VALUE 3.14159.
          05 AREAC PIC 9999V99 COMP.
          05 AREACDIS PIC ZZZZ.99.
      ******* SWAP VARIABLES
       01 SWAP-SEQ.
          05 NUM-1IN PIC X(2).
          05 NUM-1DIS PIC 99.
          05 NUM-1 PIC 99 COMP.
          05 NUM-2IN PIC X(2).
          05 NUM-2DIS PIC 99.
          05 NUM-2 PIC 99 COMP.
          05 TEMPORARY PIC 99 COMP.
      ******* CELSIUS TO FAHRENHEIT
       01 CTOF-SEQ.
          05  CELIN PIC X(7).
          05  CELDIS PIC ---9V999.
          05  FAH PIC S999V999 COMP.
          05  CEL PIC S999V999 COMP.
          05  FAHDIS PIC ---9.999.
      ******* TOTAL SALES AND DESCRIPTION
       01 SALESMAN-SEQ.
          05 FNAME PIC A(10).
          05 MNAME PIC A(10).
          05 LNAME PIC A(10).
          05 CPOS PIC A(10).
          05 HWORK PIC 999 COMP.
          05 HWORKIN PIC X(3).
          05 RPH PIC 999999V99 COMP.
          05 RPHIN PIC X(10).
          05 DED PIC 999999V99 COMP.
          05 DEDIN PIC X(10).
          05 GPAY PIC 999999V99 COMP.
          05 GPAYDIS PIC ZZZ,ZZZ.99.
          05 NPAY PIC 999999V99 COMP.
          05 NPAYDIS PIC ZZZ,ZZZ.99.
      ******** SEMESTRAL AVERAGE
       01 SEMESTRAL-AVERAGE.
          05 PRELIM PIC 999V99 COMP.
          05 PRELIMIN PIC X(6).
          05 MIDTERM PIC 999V99 COMP.
          05 MIDTERMIN PIC X(6).
          05 FINALS PIC 999V99 COMP.
          05 FINALSIN PIC X(6).
          05 SEM-AVERAGE PIC 999V99 COMP.
          05 SEM-AVERAGEDIS PIC ZZZ.99.
      ******** RECTANGLE AREA AND PERIMETER
       01 AREAPERIMRECT.
          05 RLENGTH PIC 999 COMP.
          05 RLENGTHIN PIC X(3).
          05 RWIDTH PIC 999 COMP.
          05 RWIDTHIN PIC X(3).
          05 RAREA PIC 999 COMP.
          05 RAREADIS PIC ZZZ.
          05 RPERIM PIC 999 COMP.
          05 RPERIMDIS PIC ZZZ.
      ********** ITEM ORDERS IN SEQUENCE
       01 ITEMORDERS-SEQ.
          05 ORDERNUMIN PIC X(4).
          05 ORDERTOTAL PIC 9999V99 COMP.
          05 ORDERTOTALIN PIC X(7).
          05 AMTTENDER PIC 9999V99 COMP.
          05 AMTTENDERIN PIC X(7).
          05 ORDERCHANGE PIC 9999V99 COMP.
          05 ORDERCHANGEDIS PIC Z,ZZZ.99.
      ********** QUADRATIC EQUATION
       01 QUAD-ROOTS.
          05 A PIC S9(2) COMP.
          05 A-IN PIC X(4).
          05 A-DIS PIC ---9.
          05 B PIC S9(2) COMP.
          05 B-IN PIC X(4).
          05 B-DIS PIC +++9.
          05 C PIC S9(2) COMP.
          05 C-IN PIC X(4).
          05 C-DIS PIC +++9.
          05 DISCRI PIC S9(3)V9(2).
          05 X1 PIC S9(3)V9(2).
          05 X1DIS PIC ++++9.99.
          05 X2 PIC S9(3)V9(2).
          05 X2DIS PIC ++++9.99.
      ***********************SELECTION VARIABLES************************
      ******** POSITIVE, NEGATIVE, OR NEUTRAL
       01 NUMSIGN.
          05 PNN PIC S99 COMP.
          05 PNN-IN PIC X(2).
          05 PNN-DIS PIC ++9.
      ******** ODD OR EVEN
       01 EVENODD.
          05 INPUT-NUM PIC S99 COMP.
          05 INPUT-NUMX PIC X(2).
      ******** LARGEST AMONG 3 NUMBERS
       01 LARGESTNUM.
          05 LEST1 PIC 999 COMP.
          05 LEST1-IN PIC X(3).
          05 LEST2 PIC 999 COMP.
          05 LEST2-IN PIC X(3).
          05 LEST3 PIC 999 COMP.
          05 LEST3-IN PIC X(3).
      ******** LARGER BETWEEN 2 NUMBERS
       01 LARGERNUM.
          05 LER1 PIC 9(2) COMP.
          05 LER1-IN PIC X(2).
          05 LER2 PIC 9(2) COMP.
          05 LER2-IN PIC X(2).
      *********  DETERMINE IF PRIME
       01 PRIMENUMVARS.
          05 PRIME-N PIC 999V99 COMP.
          05 PRIMEIN PIC X(6).
          05  I PIC 999 VALUE ZERO.
          05  REM PIC 999 VALUE ZERO.
          05  QUOTIENT PIC 9 VALUE ZERO.
      ********** PASS OR FAIL WITH CORRESPONDING GRADE EQUIVALENCE
       01 SELPASSORFAIL.
          05 SEL-PRE PIC 9(3)V9(2) COMP.
          05 SEL-PREIN PIC X(6).
          05 SEL-MID PIC 9(3)V9(2) COMP.
          05 SEL-MIDIN PIC X(6).
          05 SEL-FINALS PIC 9(3)V9(2) COMP.
          05 SEL-FINALSIN PIC X(6).
          05 SEL-AVE PIC 9(3)V9(2) COMP.
          05 SEL-AVEDIS PIC ZZZ.99.
      ********* COMMISSION OF A SALESMAN
       01 SEL-COMMISSION.
          05 NAMESALESMAN PIC X(10).
          05 SALESMANNUM PIC X(4).
          05 UNITSSOLD PIC 9(4) COMP.
          05 UNITSSOLDIN PIC X(4).
          05 UNITPRICE PIC 9(5)V9(2) COMP.
          05 UNITPRICEIN PIC X(8).
          05 TOTALSALES PIC 9(5)V9(2) COMP.
          05 SEL-COMM PIC 9(5)V9(2) COMP.
          05 SEL-COMMDIS PIC ZZ,ZZZ.99.
      *********** VOWEL OR CONSONANT
       01 SEL-VOWELORCONSO.
         02 CHAR PIC X.
          88 VOWEL VALUE 'A', 'E', 'I', 'O', 'U'.
          88 LOWER-VOWEL VALUE 'a', 'e', 'i', 'o', 'u'.
          88 CONSO VALUE 'B' THRU 'D', 'F' THRU 'H', 'J' THRU 'N',
              'P' THRU 'T', 'V' THRU 'Z'.
          88 LOWER-CONSO VALUE 'b' THRU 'd', 'f' THRU 'h', 'j' THRU 'n',
             'p' THRU 't', 'v' THRU 'z'.
          88 VALID-CHAR VALUE 'A' THRU 'Z', 'a' THRU 'z'.
      ********* SELECTION ITEM ORDERS
       01 SEL-AMTTENDER.
          05 SELORDERNUM PIC X(4).
          05 SELORDERTOTAL PIC 9(4)V9(2) COMP.
          05 SELORDERTOTALIN PIC X(7).
          05 SELAMTTENDER PIC 9(4)V9(2) COMP.
          05 SELAMTTENDERIN PIC X(7).
          05 SELORDERCHANGE PIC 9(4)V9(2) COMP.
          05 SELORDERCHANGEDIS PIC Z,ZZZ.99.
      *********** BANK DEPOSIT OR WITHDRAW
       01 BANKDEPORWITH.
          05 BANKNUM PIC X(5).
          05 BANKNAME PIC X(10).
          05 BALANCE PIC 9(6)V9(2) COMP.
          05 BALANCEIN PIC X(9).
          05 BALANCEDIS PIC ZZZ,ZZZ.99.
          05 T-CODE PIC X.
      *T-CODE = TRANSACTION CODE
          05 T-AMOUNT PIC 9(6)V9(2) COMP.
          05 T-AMOUNTIN PIC X(9).

      ***********************ITERATION VARIABLES************************
      ******* SPACING
       01 SHOWSPACE PIC 9999.
      ********** PRINT 1 TO N NATURAL NUMBERS
       01 1TONNATURALNUMS.
          05 LAST1 PIC 9(3) COMP.
          05 LAST1IN PIC X(3).
          05 NAT-CTR PIC 999.
          05 NAT-CTRDIS PIC ZZ9.
      ******** PRINT EVEN NUMBERS FORM 1 TO N
       01 1TONEVENNUMS.
          05 LASNUM PIC 9(3) COMP.
          05 LASNUMIN PIC X(3).
          05 EVENCTR PIC 9(3) VALUE 1.
          05 EVENCTRDIS PIC ZZ9.
      ********** FACTORIAL
       01 FACTORIALVARS.
          05 NUMINPUTFACTO PIC 9(2) COMP.
          05 NUMINPUTFACTOIN PIC X(2).
          05 NUMINPUTFACTODIS PIC Z9.
          05 CTRFACTO PIC 9(2) VALUE 0 COMP.
          05 PRODFACTO PIC 9(6) VALUE 1 COMP.
          05 PRODFACTODIS PIC ZZZZZ9.
      *********** FIBONACCI VARIABLES
       01 FIBVARS.
          05 NUM-A PIC 9(3) VALUE 1 COMP.
          05 NUM-B PIC S9(3) VALUE -1.
          05 NEXT-NUM PIC 9(3).
          05 DIS-NUM PIC ZZ9.
          05 FIBINPUTNUM PIC 9(2) COMP.
          05 FIBINPUTNUMIN PIC X(2).
      ************* REVERSE NUMBER AND CHECK IF PALINDROME
       01 REVERSEPALINDROME.
          05 PALINPUTNUM PIC 9(7) COMP.
          05 PALINPUTNUMIN PIC X(7).
          05 HOLDNUM PIC 9(7) COMP.
          05 TEMP PIC 9(7) COMP.
          05 TEMPDIS PIC ZZZZZZ9.
          05 REMAINDERNUM PIC 9 COMP.
          05 REVERSENUM PIC 9(7) VALUE ZEROS COMP.
          05 REVERSENUMDIS PIC ZZZZZZ9.
      ************ LINE AND COLUMN COORDINATES
       01 LINEE PIC 99 COMP.
       01 COLUMNN PIC 99 COMP.
      ************ RIGHT TRIANGLE ASTERISK PATTERN VARIABLES
       01 RTSTARPATTERN.
          05 ROWS PIC 9(1) VALUE 5 COMP.
          05 LINESTAR PIC 9(1) VALUE 1 COMP.
          05 CUR-STAR PIC 9(1) VALUE 1 COMP.
      *********** INVERTED RIGHT TRIANGLE NUMBER PATTERN
       01 INVERTEDNUM.
          05 INVERTROWS PIC 9(1) VALUE 5 COMP.
          05 LINENUM PIC 9(1) VALUE 5 COMP.
          05 LINENUMDIS PIC Z.
          05 CUR-NUM PIC 9(1) VALUE 1 COMP.
      *********** DIAMOND PATTERN
       01 DIAPATTERN.
          05  N77 PIC Z9.
          05  N7 PIC 99.
          05  A7 PIC 9(2) VALUE 1.
          05  SPACESSSSS PIC 9(10) VALUE 1.
          05  O22 PIC 9(3) VALUE 1.
          05  REMAINDERS1 PIC 9(3).
          05  QUOTIENT1 PIC 9(5).
          05  COLL PIC 99.
          05  LISS PIC 99.
      ********** LOOP ORDER
       01 LOOPORDERTERMINATE.
          05 ORDER-NUM PIC X(4).
          05 ORDER-TOTAL PIC 9(4)V9(2) VALUE 0 COMP.
          05 ORDER-TOTALDIS PIC Z,ZZ9.99.
          05 AMTTENDERLOOP PIC 9(4)V9(2) COMP.
          05 AMT-ORDER PIC 9(4)V9(2) VALUE 0 COMP.
          05 AMT-ORDERIN PIC X(7).
          05 ORDER-CHANGE PIC 9(4)V9(2) COMP.
          05 ORDER-ITEM PIC X(10).
      ***********************CONSOLE CLEAR SCREEN***********************
       SCREEN SECTION.
       01 CLRSCR.
           05 BLANK SCREEN.
      ******************************************************************
       PROCEDURE DIVISION.
       MAIN.
             DISPLAY CLRSCR
            DISPLAY "|=|" AT 0241 FOREGROUND-COLOUR CYAN
            "_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _  _ " AT 0241
            FOREGROUND-COLOUR CYAN
            "|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_||_|" AT 0340
            FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0440 FOREGROUND-COLOUR CYAN
            "|=|" AT 0481 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0540 FOREGROUND-COLOUR CYAN
            "|=|" AT 0581 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0640 FOREGROUND-COLOUR CYAN
            "|=|" AT 0681 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0740 FOREGROUND-COLOUR CYAN
            "|=|" AT 0781 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0840 FOREGROUND-COLOUR CYAN
            "|=|" AT 0881 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 0940 FOREGROUND-COLOUR CYAN
            "|=|" AT 0981 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1040 FOREGROUND-COLOUR CYAN
            "|=|" AT 1081 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1140 FOREGROUND-COLOUR CYAN
            "|=|" AT 1181 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1240 FOREGROUND-COLOUR CYAN
            "|=|" AT 1281 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1340 FOREGROUND-COLOUR CYAN
            "|=|" AT 1381 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1440 FOREGROUND-COLOUR CYAN
            "|=|" AT 1481 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1540 FOREGROUND-COLOUR CYAN
            "|=|" AT 1581 FOREGROUND-COLOUR CYAN
            DISPLAY "|=|" AT 1640 FOREGROUND-COLOUR CYAN
            "|=|" AT 1681 FOREGROUND-COLOUR CYAN

            "|_ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _" AT 1642
            FOREGROUND-COLOUR CYAN
            "|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_||_|" AT 1740
            FOREGROUND-COLOUR CYAN

            DISPLAY "MAIN MENU" AT 0558 BACKGROUND-COLOUR RED
            DISPLAY "[A] SEQUENCE" AT 0755 FOREGROUND-COLOUR GREEN
            DISPLAY "[B] SELECTION" AT 0855 FOREGROUND-COLOUR GREEN
            DISPLAY "[C] ITERATION" AT 0955 FOREGROUND-COLOUR GREEN
            DISPLAY "[D] EXIT" AT 1055 FOREGROUND-COLOUR GREEN
            DISPLAY "ENTER YOUR CHOICE:" AT 1353 BACKGROUND-COLOUR RED
            ACCEPT CHOICE AT 1372 FOREGROUND-COLOUR WHITE

            MOVE CHOICE TO MMCHOICE

            EVALUATE MMCHOICE

            WHEN 'A' WHEN 'a'
               PERFORM SEQ

            WHEN 'B' WHEN 'b'
               PERFORM CONDI

            WHEN 'C' WHEN 'c'
               PERFORM LOOP

            WHEN 'D' WHEN 'd'
               PERFORM OUT

            WHEN OTHER
               DISPLAY "INVALID" AT 1955
               PERFORM GETCH
               PERFORM MAIN

               END-EVALUATE

           STOP RUN.
      ****************************** S E Q U E N C E   F U N C T I O N S
       SEQ.
      ********* PARA MAULIT-ULIT YUNG MENU NG SEQUENCE
            PERFORM UNTIL CHOICE = 9
                DISPLAY CLRSCR
                   DISPLAY " " AT 0230 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0230
            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0331 FOREGROUND-COLOUR CYAN
            "##" AT 0387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0431 FOREGROUND-COLOUR CYAN
            "##" AT 0487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0531 FOREGROUND-COLOUR CYAN
            "##" AT 0587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0631 FOREGROUND-COLOUR CYAN
            "##" AT 0687 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0731 FOREGROUND-COLOUR CYAN
            "##" AT 0787 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0831 FOREGROUND-COLOUR CYAN
            "##" AT 0887 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0931 FOREGROUND-COLOUR CYAN
            "##" AT 0987 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1031 FOREGROUND-COLOUR CYAN
            "##" AT 1087 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1131 FOREGROUND-COLOUR CYAN
            "##" AT 1187 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1231 FOREGROUND-COLOUR CYAN
            "##" AT 1287 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1331 FOREGROUND-COLOUR CYAN
            "##" AT 1387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1431 FOREGROUND-COLOUR CYAN
            "##" AT 1487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1531 FOREGROUND-COLOUR CYAN
            "##" AT 1587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1631 FOREGROUND-COLOUR CYAN
            "##" AT 1687 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1730 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0730
            FOREGROUND-COLOUR CYAN
        DISPLAY "S E Q U E N C E " AT 0452 BACKGROUND-COLOUR MAGENTA
               DISPLAY "[A] HELLO WORLD" AT 0637 FOREGROUND-COLOUR BROWN
               DISPLAY "[B] OPERATIONS" AT 0737 FOREGROUND-COLOUR BROWN
                DISPLAY "[C] CIRCLE" AT 0837 FOREGROUND-COLOUR BROWN
                DISPLAY "[D] SWAP" AT 0937 FOREGROUND-COLOUR BROWN
       DISPLAY "[E] FARENHEIT CONVERTER" AT 1037 FOREGROUND-COLOUR BROWN
                DISPLAY "[F] SALESMAN" AT 0668 FOREGROUND-COLOUR BROWN
                DISPLAY "[G] AVERAGE " AT 0768 FOREGROUND-COLOUR BROWN
                DISPLAY "[H] RECTANGLE" AT 0868 FOREGROUND-COLOUR BROWN
                DISPLAY "[I] ORDER ITEM" AT 0968 FOREGROUND-COLOUR BROWN
                DISPLAY "[J] QUADRATIC" AT 1068 FOREGROUND-COLOUR BROWN
                DISPLAY "[K] EXIT" AT 1168 FOREGROUND-COLOUR BROWN

          DISPLAY "ENTER YOUR CHOICE:" AT 1450 BACKGROUND-COLOUR MAGENTA
                ACCEPT CHOICE AT 1469

                MOVE CHOICE TO MMCHOICE

                EVALUATE MMCHOICE

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
                  PERFORM CTOF
                   PERFORM GETCH

                WHEN 'F' WHEN 'f'
                   PERFORM SALESMAN
                   PERFORM GETCH

                WHEN 'G' WHEN 'g'
                   PERFORM SEQAVE
                   PERFORM GETCH

               WHEN 'H' WHEN 'h'
                   PERFORM REC
                   PERFORM GETCH

               WHEN 'I' WHEN 'i'
                   PERFORM AMT-TENDER
                   PERFORM GETCH

               WHEN 'J' WHEN 'j'
                   PERFORM QUADRA
                   PERFORM GETCH

                WHEN 'K' WHEN 'k'
                   PERFORM MAIN

                WHEN OTHER
                   DISPLAY "INVALID INPUT!" AT 1952 FOREGROUND-COLOR RED
                   PERFORM GETCH

                 END-EVALUATE
           END-PERFORM.
      ********** HELLO WORLD
       HW.
             DISPLAY CLRSCR
             DISPLAY "HELLO WORLD" AT 1454.
      ********** ARITHMETIC OPERATIONS
       OPERA.
            DISPLAY CLRSCR
            DISPLAY "ENTER TWO NUMBERS: " AT 0642
            ACCEPT NUM1IN AT 0670
            ACCEPT NUM2IN AT 0674

            MOVE NUM1IN TO NUM1
            MOVE NUM2IN TO NUM2

            IF NUM1 < NUM2 THEN
              DISPLAY "NUM 2 HAS TO BE GREATER THAN NUM 1 FOR DIVISION"
                AT 0842
              EXIT PARAGRAPH
            END-IF.

            COMPUTE TOTAL ROUNDED = NUM1 + NUM2.
            COMPUTE DIFF ROUNDED = NUM1 - NUM2.
            COMPUTE QUO ROUNDED = NUM1 / NUM2.
            COMPUTE PROD ROUNDED = NUM1 * NUM2.

            MOVE TOTAL TO TOTALDIS
            MOVE DIFF TO DIFFDIS
            MOVE QUO TO QUODIS
            MOVE PROD TO PRODDIS

            DISPLAY "SUM: " AT 0942 TOTALDIS
            DISPLAY "DIFFERENCE: " AT 1042 DIFFDIS
            DISPLAY "QUOTIENT: " AT 1142 QUODIS
            DISPLAY "PRODUCT: " AT 1242 PRODDIS
           EXIT.
      ******* AREA AND CIRCUMFERENCE OF A CIRCLE
       CIRCLE.
            DISPLAY CLRSCR
            DISPLAY "ENTER RADIUS OF CIRCLE: " AT 0642
            ACCEPT RADIUSIN AT 0666
            MOVE RADIUSIN TO RADIUS

            MOVE ZERO TO AREAC
            MOVE ZERO TO CIRCUM
            COMPUTE AREAC ROUNDED = PI-VALUE * (RADIUS * RADIUS)
            COMPUTE CIRCUM ROUNDED = 2 * PI-VALUE * RADIUS
            MOVE AREAC TO AREACDIS
            MOVE CIRCUM TO CIRCUMDIS

            DISPLAY " "
            DISPLAY "THE CIRCLE'S AREA: " AT 0942 AREACDIS
            DISPLAY "The CIRCLE'S CIRCUMFERENCE: "AT 1042 CIRCUMDIS
            EXIT.
      ******* SWAP FUNCTION
       SWAP.
           DISPLAY CLRSCR
           DISPLAY "ENTER NUM 1: " AT 0642 ACCEPT NUM-1IN AT 0666
           DISPLAY "ENTER NUM 2: " AT 0742 ACCEPT NUM-2IN AT 0766

            MOVE NUM-1IN TO NUM-1
            MOVE NUM-2IN TO NUM-2

            MOVE NUM-1 TO TEMPORARY.
            MOVE NUM-2 TO NUM-1.
            MOVE TEMPORARY TO NUM-2.

            MOVE NUM-1 TO NUM-1DIS
            MOVE NUM-2 TO NUM-2DIS

           DISPLAY "NUM 1 AFTER THE SWAP: " AT 1142 NUM-1DIS.
           DISPLAY "NUM 2 AFTER THE SWAP: " AT 1242 NUM-2DIS.
      ********* CELSIUS TO FAHRENHEIT
       CTOF.
           DISPLAY CLRSCR
           DISPLAY "ENTER THE TEMPERATURE IN CELSIUS: " AT 0642
           ACCEPT CELIN AT 0678
           MOVE CELIN TO CEL
           MOVE ZERO TO FAH
           COMPUTE FAH ROUNDED = (CEL * 01.80 ) + 32.00.

           MOVE FAH TO FAHDIS
           MOVE CEL TO CELDIS

           DISPLAY CELDIS "C IN FAHRENHEIT IS: "AT 0842 FAHDIS "F"
           EXIT.
      ******* TOTAL SALES AND DESCRIPTION
       SALESMAN.
           DISPLAY CLRSCR
            DISPLAY "Enter first name: " AT 0642 ACCEPT FNAME AT 0660
            DISPLAY "Enter middle name: "AT 0742 ACCEPT MNAME AT 0761
            DISPLAY "Enter last name: " AT 0842 ACCEPT LNAME AT 0860
            DISPLAY "Enter company position name: " AT 0942
            ACCEPT CPOS AT 0971
           DISPLAY "Enter hours worked: " AT 1042 ACCEPT HWORKIN AT 1062
            DISPLAY "Enter rate per hour: " AT 1142 ACCEPT RPHIN AT 1163
            DISPLAY "Enter deduction fee: " AT 1242 ACCEPT DEDIN AT 1264

            MOVE HWORKIN TO HWORK
            MOVE RPHIN TO RPH
            MOVE DEDIN TO DED
            MOVE ZERO TO GPAY
            MOVE ZERO TO NPAY
            COMPUTE GPAY ROUNDED = HWORK * RPH
            COMPUTE NPAY ROUNDED = GPAY - DED

            MOVE GPAY TO GPAYDIS
            MOVE NPAY TO NPAYDIS

            DISPLAY "Employee's gross pay is: " AT 1442 GPAYDIS.
            DISPLAY "Employee's net pay is: " AT 1542 NPAYDIS.
      ******** PRELIM, MIDTERM, AND FINALS AVERAGE
       SEQAVE.
            DISPLAY CLRSCR
            DISPLAY "Enter your prelim grade:" AT 0642
            ACCEPT PRELIMIN AT 0668
            DISPLAY "Enter your midterm grade: " AT 0742
            ACCEPT MIDTERMIN AT 0769
            DISPLAY "Enter your final grade: " AT 0842
            ACCEPT FINALSIN AT 0867

            MOVE PRELIMIN TO PRELIM
            MOVE MIDTERMIN TO MIDTERM
            MOVE FINALSIN TO FINALS

           COMPUTE SEM-AVERAGE ROUNDED = (PRELIM + MIDTERM + FINALS) / 3
            MOVE SEM-AVERAGE TO SEM-AVERAGEDIS

            DISPLAY "Your semestral grade is: " AT 1042 SEM-AVERAGEDIS
            EXIT.
      ********* RECTANGLE AREA AND PERIMETER
       REC.
           DISPLAY CLRSCR
           DISPLAY "Enter the length of the rectangle: " AT 0642
           ACCEPT RLENGTHIN AT 0678
           DISPLAY "Enter the width of the rectangle:  " AT 0742
           ACCEPT RWIDTHIN AT 0778

           MOVE RLENGTHIN TO RLENGTH
           MOVE RWIDTHIN TO RWIDTH

           COMPUTE RPERIM ROUNDED = (2 * RLENGTH) + (2 * RWIDTH)
           COMPUTE RAREA ROUNDED = RLENGTH * RWIDTH
           MOVE RPERIM TO RPERIMDIS
           MOVE RAREA TO RAREADIS

           DISPLAY "The perimeter of the rectangle: "AT 0942 RPERIMDIS
           DISPLAY "The area of the rectangle: " AT 1042 RAREADIS
           EXIT.

      ********* AMOUNT TENDER
       AMT-TENDER.
            DISPLAY CLRSCR
            DISPLAY "Enter the order number: " AT 0642
            ACCEPT ORDERNUMIN AT 0666
            DISPLAY "Enter the total amount of orders: " AT 0742
            ACCEPT ORDERTOTALIN AT 0776
            DISPLAY "Enter the amount tendered: " AT 0842
            ACCEPT AMTTENDERIN AT 0870

            MOVE AMTTENDERIN TO AMTTENDER
            MOVE ORDERTOTALIN TO ORDERTOTAL

            COMPUTE ORDERCHANGE ROUNDED = AMTTENDER - ORDERTOTAL
            MOVE ORDERCHANGE TO ORDERCHANGEDIS

            DISPLAY "Your change is: " AT 1042 ORDERCHANGEDIS.
            EXIT.

      ********** ROOTS OF QUADRATIC EQUATION
       QUADRA.
            DISPLAY CLRSCR
            DISPLAY "Enter the first constant: "AT 0642
            ACCEPT A-IN AT 0668
            DISPLAY "Enter the second constant: " AT 0742
            ACCEPT B-IN AT 0770
            DISPLAY "Enter the third constant: " AT 0842
            ACCEPT C-IN AT 0869
            MOVE A-IN TO A-DIS
            MOVE B-IN TO B-DIS
            MOVE C-IN TO C-DIS

            DISPLAY "Quadratic equation: " AT 1042 A-DIS AT 1062 "x^2"
             AT 1066 B-DIS at 1069 "x" AT 1073 C-DIS

            MOVE A-IN TO A
            MOVE B-IN TO B
            MOVE C-IN TO C

            COMPUTE DISCRI = (B*B) - (4*A*C).
            COMPUTE X1 = (-B + FUNCTION SQRT(DISCRI)) / (2*A).
            COMPUTE X2 = (-B - FUNCTION SQRT(DISCRI)) / (2*A).
            MOVE X1 TO X1DIS
            MOVE X2 TO X2DIS

            DISPLAY "x1: " AT 1142 X1DIS AT 1147 " x2: " AT 1160 X2DIS
            EXIT.
      ************************ C O N D I T I O N A L   F U N C T I O N S
       CONDI.
             PERFORM UNTIL CHOICE = 9
                DISPLAY CLRSCR
                      DISPLAY " " AT 0230 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0230

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0331 FOREGROUND-COLOUR CYAN
            "##" AT 0387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0431 FOREGROUND-COLOUR CYAN
            "##" AT 0487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0531 FOREGROUND-COLOUR CYAN
            "##" AT 0587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0631 FOREGROUND-COLOUR CYAN
            "##" AT 0687 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0731 FOREGROUND-COLOUR CYAN
            "##" AT 0787 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0831 FOREGROUND-COLOUR CYAN
            "##" AT 0887 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0931 FOREGROUND-COLOUR CYAN
            "##" AT 0987 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1031 FOREGROUND-COLOUR CYAN
            "##" AT 1087 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1131 FOREGROUND-COLOUR CYAN
            "##" AT 1187 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1231 FOREGROUND-COLOUR CYAN
            "##" AT 1287 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1331 FOREGROUND-COLOUR CYAN
            "##" AT 1387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1431 FOREGROUND-COLOUR CYAN
            "##" AT 1487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1531 FOREGROUND-COLOUR CYAN
            "##" AT 1587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1631 FOREGROUND-COLOUR CYAN
            "##" AT 1687 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1730 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0730
            FOREGROUND-COLOUR CYAN
       DISPLAY "S E L E C T I O N" AT 0451 BACKGROUND-COLOUR MAGENTA

                DISPLAY "[A]"  AT 0634 FOREGROUND-COLOR BROWN
                DISPLAY "NUMBER SIGN" AT 0638 FOREGROUND-COLOR BROWN
                DISPLAY "[B]" AT 0734 FOREGROUND-COLOR BROWN
                DISPLAY "ODD OR EVEN" AT 0738 FOREGROUND-COLOR BROWN
                DISPLAY "[C]" AT 0834 FOREGROUND-COLOR BROWN
           DISPLAY "LARGEST IN 3 NUMBERS" AT 0838 FOREGROUND-COLOR BROWN
                DISPLAY "[D]" AT 0934 FOREGROUND-COLOR BROWN
           DISPLAY "LARGER IN 2 NUMBERS " AT 0938 FOREGROUND-COLOR BROWN
                DISPLAY "[E]" AT 1034 FOREGROUND-COLOR BROWN
                DISPLAY "PRIME NUMBER" AT 1038 FOREGROUND-COLOR BROWN
                DISPLAY "[F]" AT 0663 FOREGROUND-COLOR BROWN
               DISPLAY "PASSED OR FAILED" AT 0667 FOREGROUND-COLOR BROWN
                DISPLAY "[G]" AT 0763 FOREGROUND-COLOR BROWN
                DISPLAY "COMMISSION" AT 0767 FOREGROUND-COLOR BROWN
                DISPLAY "[H]" AT 0863 FOREGROUND-COLOR BROWN
                DISPLAY "VOWEL/CONSONANT" AT 0867 FOREGROUND-COLOR BROWN
                DISPLAY "[I]" AT 0963 FOREGROUND-COLOR BROWN
                DISPLAY "AMOUNT TENDERED" AT 0967 FOREGROUND-COLOR BROWN
                DISPLAY "[J]" AT 1063 FOREGROUND-COLOR BROWN
                DISPLAY "BANK DETAILS" AT 1067 FOREGROUND-COLOR BROWN
                DISPLAY "[K]" AT 1163 FOREGROUND-COLOR BROWN
                DISPLAY "EXIT" AT 1167 FOREGROUND-COLOR BROWN


         DISPLAY "ENTER YOUR CHOICE:" AT 1550 BACKGROUND-COLOUR MAGENTA
                ACCEPT CHOICE AT 1569

                MOVE CHOICE TO MMCHOICE

                EVALUATE MMCHOICE

                WHEN 'A' WHEN 'a'
                   PERFORM POSNEGNEUT
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
                   PERFORM PRIMENUM
                   PERFORM GETCH

                WHEN 'F' WHEN 'f'
                  PERFORM PASSFAIL
                  PERFORM GETCH

                WHEN 'G' WHEN 'g'
                  PERFORM COMMISSION
                  PERFORM GETCH

               WHEN 'H' WHEN 'h'
                   PERFORM VOWCON
                   PERFORM GETCH

               WHEN 'I' WHEN 'i'
                   PERFORM IFAMTTENDER
                   PERFORM GETCH

               WHEN 'J' WHEN 'j'
                   PERFORM BANKDETAILS
                   PERFORM GETCH

               WHEN 'K' WHEN 'k'
                   PERFORM MAIN

                WHEN OTHER
                   DISPLAY "INVALID INPUT!" AT 1952 FOREGROUND-COLOR RED
                   PERFORM GETCH

                END-EVALUATE
            END-PERFORM.

      ********* DETERMINE NUMBER IF POSITIVE, NEGATIVE, OR NEUTRAL
       POSNEGNEUT.
            DISPLAY CLRSCR
            DISPLAY "ENTER A NUMBER: " AT 0642 ACCEPT PNN-IN AT 0659
            MOVE PNN-IN TO PNN

           IF PNN IS EQUAL TO ZERO
               DISPLAY "THE NUMBER IS NEUTRAL" AT 0842
           ELSE IF PNN IS GREATER THAN ZERO
               DISPLAY "THE NUMBER IS POSITIVE" AT 0842
           ELSE
               DISPLAY "THE NUMBER IS NEGATIVE" AT 0842
           END-IF.

      ********* ODD OR EVEN
       ODDEVEN.
           DISPLAY CLRSCR
           DISPLAY "ENTER A NUMBER: " AT 0642 ACCEPT INPUT-NUMX AT 0670
           MOVE INPUT-NUMX TO INPUT-NUM

           IF FUNCTION MOD(INPUT-NUM, 2) = 0
               DISPLAY "THE NUMBER IS EVEN" AT 0842
           ELSE
               DISPLAY "THE NUMBER IS ODD" AT 0842
           END-IF.

      ********** LARGEST OF 3 NUMS
       LARGEST.
            DISPLAY CLRSCR
            DISPLAY "Enter the first number: " AT 0642
            ACCEPT LEST1-IN AT 0667
            DISPLAY "Enter the second number: " AT 0742
            ACCEPT LEST2-IN AT 0768
            DISPLAY "Enter the third number: " AT 0842
            ACCEPT LEST3-IN AT 0867
            MOVE LEST1-IN TO LEST1
            MOVE LEST2-IN TO LEST2
            MOVE LEST3-IN TO LEST3

            IF LEST1 >= LEST2 AND LEST1 >= LEST3 THEN
                DISPLAY "Num 1 is the largest number." AT 1042
            ELSE IF LEST2 >= LEST1 AND LEST2 >= LEST3 THEN
                DISPLAY "Num 2 is the largest number." AT 1042
            ELSE IF LEST3 >= LEST1 AND LEST3 >= LEST2 THEN
                DISPLAY "Num 3 is the largest number." AT 1042
            END-IF.

      ********** LARGER OF 2 NUMS
       LARGER.
            DISPLAY CLRSCR
           DISPLAY "ENTER THE FIRST NUMBER: " AT 0642
           ACCEPT LER1-IN AT 0667
           DISPLAY "ENTER THE SECOND NUMBER: " AT 0742
           ACCEPT LER2-IN AT 0768
           MOVE LER1-IN TO LER1
           MOVE LER2-IN TO LER2

           IF LER1 >= LER2
               DISPLAY "NUM 1 IS THE LARGER NUMBER" AT 0942
           ELSE
               DISPLAY "NUM 2 IS THE LARGER NUMBER" AT 0942
           END-IF.

      *********** DETERMINE IF PRIME NUM
       PRIMENUM.
            DISPLAY CLRSCR
            DISPLAY "ENTER N VALUE: " AT 0642 ACCEPT PRIMEIN AT 0658
            MOVE PRIMEIN TO PRIME-N

            IF PRIME-N < 2
                DISPLAY "ENTERED NUMBER IS NEITHER PRIME NOR COMPOSITE"
                 AT 0842
                EXIT
            END-IF.

              MOVE 2 TO I.
              PERFORM UNTIL  I  >= PRIME-N
                DIVIDE PRIME-N BY I GIVING QUOTIENT REMAINDER REM
                   IF REM = 0 THEN
                       DISPLAY 'GIVEN NUMBER IS NOT PRIME' AT 0842
                   EXIT PERFORM
                   END-IF
                ADD 1 TO I
              END-PERFORM.

             IF PRIME-N = I THEN
                   DISPLAY "GIVEN NUMBER IS PRIME" AT 0842
             END-IF.

      *********** PASS OR FAIL GRADE
       PASSFAIL.
            DISPLAY CLRSCR
            DISPLAY "Enter your prelim grade: " AT 0642
            ACCEPT SEL-PREIN AT 0668
            DISPLAY "Enter your midterm grade: " AT 0742
            ACCEPT SEL-MIDIN AT 0769
            DISPLAY "Enter your finals grade: " AT 0842
            ACCEPT SEL-FINALSIN AT 0868
            MOVE SEL-PREIN TO SEL-PRE
            MOVE SEL-MIDIN TO SEL-MID
            MOVE SEL-FINALSIN TO SEL-FINALS

          COMPUTE SEL-AVE ROUNDED = (SEL-PRE + SEL-MID + SEL-FINALS) / 3
           MOVE SEL-AVE TO SEL-AVEDIS

            IF SEL-AVE >= 97.00 AND SEL-AVE <= 100.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
              SEL-AVEDIS " (1.O) PASSED" AT 1065
            ELSE IF SEL-AVE >= 94.00 AND SEL-AVE <= 96.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
              SEL-AVEDIS " (1.25) PASSED" AT 1065
            ELSE IF SEL-AVE >= 91.00 AND SEL-AVE <= 93.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (1.50) PASSED" AT 1065
            ELSE IF SEL-AVE >= 88.00 AND SEL-AVE <= 90.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (1.75) PASSED" AT 1065
            ELSE IF SEL-AVE >= 85.00 AND SEL-AVE <= 87.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (2.00) PASSED" AT 1065
            ELSE IF SEL-AVE >= 82.00 AND SEL-AVE <= 84.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (2.25) PASSED" AT 1065
            ELSE IF SEL-AVE >= 79.00 AND SEL-AVE <= 81.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (2.50) PASSED" AT 1065
            ELSE IF SEL-AVE >= 76.00 AND SEL-AVE <= 78.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (2.75) PASSED" AT 1065
            ELSE IF SEL-AVE IS EQUAL TO 75.00
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (3.00) PASSED" AT 1065
            ELSE
             DISPLAY "YOUR AVERAGE IS: " AT 1042
             SEL-AVEDIS " (5.00) FAILED" AT 1065
            END-IF.

      *********** COMMISSION OF SALESMAN
       COMMISSION.
           DISPLAY CLRSCR
            DISPLAY "ENTER SALESMAN'S NAME: " AT 0642
            ACCEPT NAMESALESMAN AT 0666
            DISPLAY "ENTER SALESMAN'S NUMBER: " AT 0742
            ACCEPT SALESMANNUM AT 0768
            DISPLAY "ENTER NUMBER OF UNITS SOLD: " AT 0842
            ACCEPT UNITSSOLDIN AT 0871
            DISPLAY "ENTER UNIT PRICE: " AT 0942
            ACCEPT UNITPRICEIN AT 0961
            MOVE UNITSSOLDIN TO UNITSSOLD
            MOVE UNITPRICEIN TO UNITPRICE

            COMPUTE TOTALSALES = UNITPRICE * UNITSSOLD.

            IF TOTALSALES <= 10000
             COMPUTE SEL-COMM ROUNDED = TOTALSALES * 0.10
             MOVE SEL-COMM TO SEL-COMMDIS
             DISPLAY "COMMISSION: " AT 1142 SEL-COMMDIS
            ELSE IF TOTALSALES <= 15000
             COMPUTE SEL-COMM ROUNDED = TOTALSALES * 0.15
             MOVE SEL-COMM TO SEL-COMMDIS
             DISPLAY "COMMISSION: " AT 1142 SEL-COMMDIS
            ELSE IF TOTALSALES <= 20000
             COMPUTE SEL-COMM ROUNDED = TOTALSALES * 0.20
             MOVE SEL-COMM TO SEL-COMMDIS
             DISPLAY "COMMISSION: " AT 1142 SEL-COMMDIS
            ELSE
             COMPUTE SEL-COMM ROUNDED = TOTALSALES * 0.30
             MOVE SEL-COMM TO SEL-COMMDIS
             DISPLAY "COMMISSION: " AT 1142 SEL-COMMDIS
            END-IF.

      *********** DETERMINE IF LETTER IS VOWEL OR CONSONANT
       VOWCON.
            DISPLAY CLRSCR
            DISPLAY "ENTER A LETTER: " AT 0642 ACCEPT CHAR AT 0659

            EVALUATE TRUE
             WHEN VOWEL DISPLAY "VOWEL" AT 0842
             WHEN LOWER-VOWEL DISPLAY "VOWEL" AT 0842
             WHEN CONSO DISPLAY "CONSONANT" AT 0842
             WHEN LOWER-CONSO DISPLAY "CONSONANT" AT 0842
             WHEN OTHER DISPLAY "CHARACTER IS NOT A LETTER" AT 0842
            END-EVALUATE.

      ********* INVALID TRANSACTION IF AMT TOTAL > AMT TENDER
       IFAMTTENDER.
            DISPLAY CLRSCR
            DISPLAY "Enter the order number: " AT 0642
            ACCEPT SELORDERNUM AT 0667
            DISPLAY "Enter the total amount of orders: " AT 0742
            ACCEPT SELORDERTOTALIN AT 0777
            DISPLAY "Enter the amount tendered: " AT 0842
            ACCEPT SELAMTTENDERIN AT 0870
            MOVE SELORDERTOTALIN TO SELORDERTOTAL
            MOVE SELAMTTENDERIN TO SELAMTTENDER

             IF SELAMTTENDER < SELORDERTOTAL THEN
             DISPLAY "AMOUNT TENDERED MUST BE GREATER THAN TOTAL AMOUNT"
              AT 1042
             EXIT PARAGRAPH
            END-IF.

           COMPUTE SELORDERCHANGE ROUNDED = SELAMTTENDER - SELORDERTOTAL
           MOVE SELORDERCHANGE TO SELORDERCHANGEDIS

            DISPLAY "Your change is: " AT 1042 SELORDERCHANGEDIS
            EXIT.

      ********** DEPOSIT OR WITHDRAW AND SHOW BALANCE
       BANKDETAILS.
            DISPLAY CLRSCR
            DISPLAY "ENTER BANK ACCOUNT NUMBER: " AT 0642
            ACCEPT BANKNUM AT 0670
            DISPLAY "ENTER BANK ACCOUNT NAME: " AT 0742
            ACCEPT BANKNAME AT 0768
            DISPLAY "ENTER BALANCE: " AT 0842 ACCEPT BALANCEIN AT 0858
            DISPLAY "ENTER TRANSACTION CODE " AT 0942
            "(WITHDRAW [W] OR DEPOSIT [D]): " AT 0965
             ACCEPT T-CODE AT 0996

             MOVE BALANCEIN TO BALANCE

            IF T-CODE IS EQUAL TO 'D' OR 'd'
             DISPLAY "ENTER DEPOSIT AMOUNT: " AT 1042
             ACCEPT T-AMOUNTIN AT 1064
             MOVE T-AMOUNTIN TO T-AMOUNT
             COMPUTE BALANCE = BALANCE + T-AMOUNT
             MOVE BALANCE TO BALANCEDIS
             DISPLAY "YOUR NEW BALANCE IS: " AT 1242 BALANCEDIS

            ELSE IF T-CODE IS EQUAL TO 'W' OR 'w'
             DISPLAY "ENTER WITHDRAWAL AMOUNT: " AT 1042
             ACCEPT T-AMOUNTIN AT 1067
             MOVE T-AMOUNTIN TO T-AMOUNT
             COMPUTE BALANCE = BALANCE - T-AMOUNT
             MOVE BALANCE TO BALANCEDIS
             DISPLAY "YOUR NEW BALANCE IS: " AT 1242 BALANCEDIS

            ELSE
             DISPLAY "INVALID ENTRY." AT 1242
            END-IF.

      *************************************** L O O P  F U N C T I O N S
       LOOP.
            PERFORM UNTIL CHOICE = 9
            DISPLAY CLRSCR

             DISPLAY " " AT 0230 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0230

            FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0331 FOREGROUND-COLOUR CYAN
            "##" AT 0387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0431 FOREGROUND-COLOUR CYAN
            "##" AT 0487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0531 FOREGROUND-COLOUR CYAN
            "##" AT 0587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0631 FOREGROUND-COLOUR CYAN
            "##" AT 0687 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0731 FOREGROUND-COLOUR CYAN
            "##" AT 0787 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0831 FOREGROUND-COLOUR CYAN
            "##" AT 0887 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 0931 FOREGROUND-COLOUR CYAN
            "##" AT 0987 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1031 FOREGROUND-COLOUR CYAN
            "##" AT 1087 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1131 FOREGROUND-COLOUR CYAN
            "##" AT 1187 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1231 FOREGROUND-COLOUR CYAN
            "##" AT 1287 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1331 FOREGROUND-COLOUR CYAN
            "##" AT 1387 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1431 FOREGROUND-COLOUR CYAN
            "##" AT 1487 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1531 FOREGROUND-COLOUR CYAN
            "##" AT 1587 FOREGROUND-COLOUR CYAN
            DISPLAY "##" AT 1631 FOREGROUND-COLOUR CYAN
            "##" AT 1687 FOREGROUND-COLOUR CYAN
            DISPLAY " " AT 1730 FOREGROUND-COLOUR CYAN
           "##########################################################" AT 0730
            FOREGROUND-COLOUR CYAN
       DISPLAY "I T E R A T I O N "AT 0451 BACKGROUND-COLOUR MAGENTA

            DISPLAY "[A]" AT 0635 FOREGROUND-COLOR BROWN
            DISPLAY "PRINT NUMS (1 TO N)" AT 0639 FOREGROUND-COLOR BROWN
            DISPLAY "[B]" AT 0735 FOREGROUND-COLOR BROWN
            DISPLAY "PRINT EVEN (1 TO N)" AT 0739 FOREGROUND-COLOR BROWN
            DISPLAY "[C]" AT 0835 FOREGROUND-COLOR BROWN
            DISPLAY "FACTORIAL" AT 0839 FOREGROUND-COLOR BROWN
            DISPLAY "[D]" AT 0935 FOREGROUND-COLOR BROWN
            DISPLAY "FIBONACCI" AT 0939 FOREGROUND-COLOR BROWN
            DISPLAY "[E]" AT 1035 FOREGROUND-COLOR BROWN
            DISPLAY "PALINDROME" AT 1039 FOREGROUND-COLOR BROWN
            DISPLAY "[F]" AT 0665 FOREGROUND-COLOR BROWN
            DISPLAY "RIGHT TRIANGLE" AT 0669 FOREGROUND-COLOR BROWN
            DISPLAY "[G]" AT 0765 FOREGROUND-COLOR BROWN
            DISPLAY "NUMBERS INVERTED" AT 0769 FOREGROUND-COLOR BROWN
            DISPLAY "[H]" AT 0865 FOREGROUND-COLOR BROWN
            DISPLAY "DIAMOND ASTERISK" AT 0869 FOREGROUND-COLOR BROWN
            DISPLAY "[I]" AT 0965 FOREGROUND-COLOR BROWN
            DISPLAY "ORDER ITEMS" AT 0969 FOREGROUND-COLOR BROWN
            DISPLAY "[J]" AT 1065 FOREGROUND-COLOR BROWN
            DISPLAY "EXIT" AT 1069 FOREGROUND-COLOR BROWN
       DISPLAY " ENTER YOUR CHOICE:" AT 1450 BACKGROUND-COLOUR MAGENTA
                   ACCEPT CHOICE AT 1470
                   MOVE CHOICE TO MMCHOICE

                   EVALUATE MMCHOICE

                   WHEN 'A' WHEN 'a'
                       PERFORM PRINT1TON
                       PERFORM GETCH

                   WHEN 'B' WHEN 'b'
                       PERFORM PRINTEVEN1TON
                       PERFORM GETCH

                   WHEN 'C' WHEN 'c'
                       PERFORM FACTORIAL
                       PERFORM GETCH

                   WHEN 'D' WHEN 'd'
                       PERFORM FIBONACCI
                       PERFORM GETCH

                    WHEN 'E' WHEN 'e'
                       PERFORM PALINDROME
                       PERFORM GETCH

                   WHEN 'F' WHEN 'f'
                       PERFORM RTSTAR
                       PERFORM GETCH

                   WHEN 'G' WHEN 'g'
                       PERFORM INVERTEDRTNUM
                       PERFORM GETCH

                   WHEN 'H' WHEN 'h'
                       PERFORM DIAMOND
                       PERFORM GETCH

                   WHEN 'I' WHEN 'i'
                       PERFORM LOOPORDER
                       PERFORM GETCH

                   WHEN 'J' WHEN 'j'
                       DISPLAY " "
                       PERFORM MAIN

                  WHEN OTHER
                   DISPLAY "INVALID INPUT!" AT 1952 FOREGROUND-COLOR RED
                       PERFORM GETCH

                   END-EVALUATE
             END-PERFORM.

      ******** PRINT NUMBERS FROM 1 TO N
       PRINT1TON.
            DISPLAY CLRSCR
            COMPUTE SHOWSPACE = 0857
            DISPLAY "PRINTING VALUES FROM 1 TO N. ENTER N: " AT 0642
            ACCEPT LAST1IN AT 0680
            MOVE LAST1IN TO LAST1
           PERFORM VARYING NAT-CTR FROM 1 BY 1 UNTIL NAT-CTR > LAST1
             MOVE NAT-CTR TO NAT-CTRDIS
             DISPLAY NAT-CTRDIS AT SHOWSPACE
             ADD 100 TO SHOWSPACE
           END-PERFORM.

      ******** PRINT EVEN NUMBERS FROM 1 TO N
       PRINTEVEN1TON.
            DISPLAY CLRSCR
            DISPLAY "PRINTING EVEN NUMBERS FROM 1 TO N. ENTER N: "
             AT 0642
            ACCEPT LASNUMIN AT 0686
            MOVE LASNUMIN TO LASNUM
            COMPUTE SHOWSPACE = 0862

           PERFORM VARYING EVENCTR FROM 1 BY 1 UNTIL EVENCTR > LASNUM
             IF FUNCTION MOD(EVENCTR, 2) = ZERO
                MOVE EVENCTR TO EVENCTRDIS
                DISPLAY EVENCTRDIS AT SHOWSPACE
                ADD 100 TO SHOWSPACE
             END-IF
           END-PERFORM.

      ******** FACTORIAL
       FACTORIAL.
            DISPLAY CLRSCR
            DISPLAY "ENTER A NUMBER TO COMPUTE FOR ITS FACTORIAL: "
             AT 0642
            ACCEPT NUMINPUTFACTOIN AT 0687
            MOVE NUMINPUTFACTOIN TO NUMINPUTFACTO

            PERFORM UNTIL CTRFACTO = NUMINPUTFACTO
              ADD 1 TO CTRFACTO
              COMPUTE PRODFACTO = PRODFACTO * CTRFACTO
            END-PERFORM.
            MOVE PRODFACTO TO PRODFACTODIS
            MOVE NUMINPUTFACTO TO NUMINPUTFACTODIS
           DISPLAY "THE FACTORIAL OF " AT 0842 NUMINPUTFACTODIS
            " IS " AT  0864 PRODFACTODIS

           EXIT.

      ******** FIBONACCI STARTING FROM 0
       FIBONACCI.
            DISPLAY CLRSCR
            COMPUTE SHOWSPACE = 0853
            MOVE 1 TO NUM-A.
            MOVE -1 TO NUM-B.
            DISPLAY "ENTER N VALUE: " AT 0647
            ACCEPT FIBINPUTNUMIN AT 0664
            MOVE FIBINPUTNUMIN TO FIBINPUTNUM

            PERFORM FIBINPUTNUM TIMES
               COMPUTE NEXT-NUM = NUM-A + NUM-B
               MOVE NEXT-NUM TO DIS-NUM
               DISPLAY DIS-NUM AT SHOWSPACE
               ADD 100 TO SHOWSPACE
               MOVE NUM-B TO NUM-A
               MOVE NEXT-NUM TO NUM-B
            END-PERFORM.

      ******** REVERSE NUMBER AND CHECK IF PALINDROME
       PALINDROME.
            DISPLAY CLRSCR
            DISPLAY "ENTER NUMBER (7 DIGITS MAX): " AT 0642
            ACCEPT PALINPUTNUMIN AT 0673
            MOVE PALINPUTNUMIN TO PALINPUTNUM
            MOVE PALINPUTNUM TO HOLDNUM
            MOVE PALINPUTNUM TO TEMP

            PERFORM UNTIL PALINPUTNUM = 0
            DIVIDE PALINPUTNUM BY 10 GIVING PALINPUTNUM
              REMAINDER REMAINDERNUM
            COMPUTE REVERSENUM = REVERSENUM * 10 + REMAINDERNUM
            END-PERFORM.
            MOVE REVERSENUM TO REVERSENUMDIS
            MOVE TEMP TO TEMPDIS

            DISPLAY "REVERSE OF " AT 0842 TEMPDIS " IS " AT 0860
              REVERSENUMDIS

            IF REVERSENUM IS EQUAL TO HOLDNUM
                DISPLAY "PALINDROME NUMBER" AT 0942
            ELSE
                DISPLAY "NOT A PALINDROME" AT 0942
            END-IF.
      ********* RIGHT TRIANGLE ASTERISK PATTERN
       RTSTAR.
           DISPLAY CLRSCR
           COMPUTE LINEE = 11
           COMPUTE COLUMNN = 59
           PERFORM VARYING LINESTAR FROM 1 BY 1 UNTIL
                LINESTAR > ROWS
             MOVE 59 TO COLUMNN
             PERFORM UNTIL CUR-STAR > LINESTAR
             DISPLAY "*" AT LINE LINEE COLUMN COLUMNN
             ADD 1 TO CUR-STAR
             ADD 1 TO COLUMNN
              END-PERFORM
             DISPLAY " "
             SET CUR-STAR TO 1
             ADD 1 TO LINEE
            END-PERFORM.
            EXIT.
      ********* INVERTED RIGHT TRIANGLE NUMBER PATTERN
       INVERTEDRTNUM.
            DISPLAY CLRSCR
            MOVE 11 TO LINEE
            MOVE 59 TO COLUMNN
            PERFORM VARYING LINENUM FROM 5 BY -1 UNTIL
             LINENUM = 0
               MOVE 59 TO COLUMNN
               PERFORM UNTIL CUR-NUM > LINENUM
                   MOVE LINENUM TO LINENUMDIS
                   DISPLAY LINENUMDIS AT LINE LINEE COLUMN COLUMNN
                   ADD 1 TO COLUMNN
                   ADD 1 TO CUR-NUM
               END-PERFORM
                   DISPLAY " "
                   SET CUR-NUM TO 1
                   ADD 1 TO LINEE
             END-PERFORM.

      ********* DIAMOND ASTERISK PATTERN
       DIAMOND.
             DISPLAY CLRSCR.

            DISPLAY "ENTER NUMBER OF ROWS: " AT 0642.
            ACCEPT N77 AT 0665.

            MOVE N77 TO N7.



            COMPUTE LISS = 08.
            COMPUTE COLL = 59.
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

      ******** ITEM ORDER LOOP
       LOOPORDER.
            DISPLAY CLRSCR
            MOVE 08 TO LINEE
            MOVE 42 TO COLUMNN
         DISPLAY "ENTER THE ORDER NUMBER: " AT 0642
             ACCEPT ORDER-NUM AT 0667
             DISPLAY "ENTER ORDER ITEM (0 TO TERMINATE): " AT 0742
             ACCEPT ORDER-ITEM AT 0777

           PERFORM WITH TEST AFTER UNTIL ORDER-ITEM = '0'
            MOVE 42 TO COLUMNN
        DISPLAY "ENTER ORDER ITEM AMOUNT: " AT LINE LINEE COLUMN COLUMNN
              ADD 25 TO COLUMNN
              ACCEPT AMT-ORDERIN AT LINE LINEE COLUMN COLUMNN
              MOVE AMT-ORDERIN TO AMT-ORDER
              COMPUTE ORDER-TOTAL = AMT-ORDER + ORDER-TOTAL
              ADD 1 TO LINEE
              MOVE 42 TO COLUMNN
            DISPLAY "ENTER ORDER ITEM (0 TO TERMINATE): " AT LINE LINEE
               COLUMN COLUMNN
             ADD 35 TO COLUMNN
             ACCEPT ORDER-ITEM AT LINE LINEE COLUMN COLUMNN
               ADD 1 TO LINEE
           END-PERFORM.

            MOVE ORDER-TOTAL TO ORDER-TOTALDIS
            ADD 2 TO LINEE
            DISPLAY "THE TOTAL AMOUNT OF ORDERS IS: "
              AT LINE LINEE COLUMN 42 ORDER-TOTALDIS

            EXIT.

      ******* GETCH FUNCTION
       GETCH.
       DISPLAY "PRESS ENTER TO CONTINUE" AT 2450 FOREGROUND-COLOUR BROWN
                  ACCEPT ENTERR
           EXIT.
      ********* OUT FUNCTION
       OUT.
            DISPLAY " ".
            EXIT.

       END PROGRAM MAIN-MENU.
