      ******************************************************************
      * Author: LEILA BORROMEO
      * Date: MARCH 03, 2022
      * Purpose: COBOL QUIZ 4
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-QUIZ-4.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT BANKINFO
         ASSIGN TO "C:\cobol_project\Quiz4FH.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **************************************
       DATA DIVISION.
       FILE SECTION.
       FD BANKINFO.
       01 FILE-BANKINFO.
          05 EMP-ID PIC X(5).
          05 EMP-LNAME PIC X(10).
          05 EMP-FNAME PIC X(10).
          05 EMP-MNAME PIC X(10).
          05 EMP-PCODE PIC X(9).
          05 EMP-RATEPERHOUR PIC 999,999.99.
          05 EMP-HRSWORKED PIC 9(3).
          05 EMP-TAX PIC 9,999,999,999.99.
          05 EMP-LATEANDABSENCES PIC 9(3).
          05 EMP-TTLDEDUCTION PIC 9,999,999,999.99.
          05 EMP-GROSSPAY PIC 9,999,999,999.99.
          05 EMP-NETPAY PIC 9,999,999,999.99.
      *****************************************
       WORKING-STORAGE SECTION.
       01 WS-BANKINFO.
          05 WS-ID PIC X(5).
          05 WS-LNAME PIC X(10).
          05 WS-FNAME PIC X(10).
          05 WS-MNAME PIC X(10).
          05 WS-PCODE PIC X.
          05 WS-RATEPERHOUR PIC 9(6)V99.
          05 WS-RATEPERHOURIN PIC X(9).
          05 WS-HRSWORKED PIC 9(3).
          05 WS-HRSWORKEDIN PIC X(3).
          05 WS-TAX PIC 9(10)V99.
          05 WS-LATEANDABSENCES PIC 9(3).
          05 WS-LATEANDABSENCESIN PIC X(3).
          05 WS-TTLDEDUCTION PIC 9(10)V99.
          05 WS-TTLDEDUCTIONDIS PIC Z,ZZZ,ZZZ,ZZ9.99.
          05 WS-GROSSPAY PIC 9(10)V99.
          05 WS-GROSSPAYDIS PIC Z,ZZZ,ZZZ,ZZ9.99.
          05 WS-NETPAY PIC 9(10)V99.
          05 WS-NETPAYDIS PIC Z,ZZZ,ZZZ,ZZ9.99.
          05 WS-SSS PIC 9(10)V99.

       77 ENTER PIC X.

       SCREEN SECTION.
       01 CLRSCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "EMPLOYEE ID: " AT 0645 ACCEPT WS-ID AT 0658
            DISPLAY "EMPLOYEE SURNAME: " AT 0745 ACCEPT WS-LNAME AT 0763
            DISPLAY "EMPLOYEE FIRST NAME: " AT 0845
            ACCEPT WS-FNAME AT 0866
            DISPLAY "EMPLOYEE MIDDLE NAME: " AT 0945
            ACCEPT WS-MNAME AT 0967
            DISPLAY "POSITION CODE: " AT 1045
            ACCEPT WS-PCODE AT 1060
            DISPLAY "RATE PER HOUR: " AT 1145
            ACCEPT WS-RATEPERHOURIN AT 1160
            MOVE WS-RATEPERHOURIN TO WS-RATEPERHOUR
            DISPLAY "NO. OF HOURS WORKED: " AT 1245
            ACCEPT WS-HRSWORKEDIN AT 1267
            MOVE WS-HRSWORKEDIN TO WS-HRSWORKED
            DISPLAY "NO. OF HOURS LATE/ABSENT: " AT 1345
            ACCEPT WS-LATEANDABSENCESIN AT 1371
            MOVE WS-LATEANDABSENCESIN TO WS-LATEANDABSENCES

            COMPUTE WS-GROSSPAY ROUNDED = WS-RATEPERHOUR * WS-HRSWORKED

            IF WS-GROSSPAY IS LESS THAN OR EQUAL TO 20833.33 THEN
                SET WS-TAX TO 0.0
            ELSE IF WS-GROSSPAY > 20833.33 AND WS-GROSSPAY <= 33333.33
            THEN
                COMPUTE WS-TAX ROUNDED = 0.20 * WS-GROSSPAY
            ELSE IF WS-GROSSPAY > 33333.33 AND WS-GROSSPAY <= 66666.67
            THEN
                COMPUTE WS-TAX ROUNDED = (0.25 * WS-GROSSPAY) + 30000
            ELSE IF WS-GROSSPAY > 66666.67 AND WS-GROSSPAY <= 166666.67
            THEN
                COMPUTE WS-TAX ROUNDED = (0.30 * WS-GROSSPAY) + 130000
            ELSE IF WS-GROSSPAY > 166666.67 AND WS-GROSSPAY <= 666666.67
            THEN
                COMPUTE WS-TAX ROUNDED = (0.32 * WS-GROSSPAY) + 490000
            ELSE IF WS-GROSSPAY > 666666.67 THEN
                COMPUTE WS-TAX ROUNDED = (0.35 * WS-GROSSPAY) + 2410000
            END-IF.

            COMPUTE WS-TTLDEDUCTION = WS-TAX + WS-LATEANDABSENCES +
                                      (WS-GROSSPAY * .0450)

            COMPUTE WS-NETPAY = WS-GROSSPAY - WS-TTLDEDUCTION
            PERFORM WRITE-FILE
            PERFORM CONSOLE-OUTPUT

            STOP RUN.

       CONSOLE-OUTPUT.
            DISPLAY CLRSCR
               IF WS-PCODE IS EQUAL TO 'R' OR 'r' THEN
                   DISPLAY "POSITION: REGULAR" AT 0645
               ELSE IF WS-PCODE IS EQUAL TO 'T' OR 't' THEN
                   DISPLAY "POSITION: TEMPORARY" AT 0645
               ELSE IF WS-PCODE IS EQUAL TO 'P' OR 'p' THEN
                   DISPLAY "POSITION: PART-TIME" AT 0645
               ELSE
                  DISPLAY "INVALID CODE" AT 0645
               END-IF.

               MOVE WS-GROSSPAY TO WS-GROSSPAYDIS
               MOVE WS-TTLDEDUCTION TO WS-TTLDEDUCTIONDIS
               MOVE WS-NETPAY TO WS-NETPAYDIS

               DISPLAY "GROSS PAY: " AT 0745
               DISPLAY WS-GROSSPAYDIS AT 0756
               DISPLAY "DEDUCTION: " AT 0845
               DISPLAY WS-TTLDEDUCTIONDIS AT 0856
               DISPLAY "NET PAY: " AT 0945
               DISPLAY WS-NETPAYDIS AT 0954
               PERFORM GETCH
            EXIT.

       WRITE-FILE.
            OPEN EXTEND BANKINFO
               MOVE WS-ID TO EMP-ID
               MOVE WS-LNAME TO EMP-LNAME
               MOVE WS-FNAME TO EMP-FNAME
               MOVE WS-MNAME TO EMP-MNAME

               IF WS-PCODE IS EQUAL TO 'R' OR 'r' THEN
                   MOVE "REGULAR" TO EMP-PCODE
               ELSE IF WS-PCODE IS EQUAL TO 'T' OR 't' THEN
                   MOVE "TEMPORARY" TO EMP-PCODE
               ELSE IF WS-PCODE IS EQUAL TO 'P' OR 'p' THEN
                   MOVE "PART-TIME" TO EMP-PCODE
               ELSE
                   MOVE "INVALID" TO EMP-PCODE
               END-IF.

               MOVE WS-RATEPERHOUR TO EMP-RATEPERHOUR
               MOVE WS-HRSWORKED TO EMP-HRSWORKED
               MOVE WS-TAX TO EMP-TAX
               MOVE WS-LATEANDABSENCES TO EMP-LATEANDABSENCES
               MOVE WS-TTLDEDUCTION TO EMP-TTLDEDUCTION
               MOVE WS-GROSSPAY TO EMP-GROSSPAY
               MOVE WS-NETPAY TO EMP-NETPAY
            WRITE FILE-BANKINFO
            CLOSE BANKINFO.
            EXIT.

       GETCH.
           DISPLAY "PRESS ENTER TO CONTINUE" AT 2142
           ACCEPT ENTER
           EXIT.
       END PROGRAM COBOL-QUIZ-4.
