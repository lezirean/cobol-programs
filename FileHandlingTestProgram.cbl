      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
         FILE-CONTROL.
         SELECT STUDENT ASSIGN TO 'Application.dat'
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.

       DATA DIVISION.
         FILE SECTION.
         FD STUDENT.
      ************ KUNG ANO LANG ANG SPECIFIED LENGTH, YUN LANG TATANGGAPIN
         01 STUDENT-FILE.
          88 END-OF-APP-FILE VALUE HIGH-VALUES.
          02 STUDENT-ID PIC 9(5).
          02 STUDENT-NAME.
            03 STUDENT-FNAME PIC X(10).
            03 STUDENT-MNAME PIC X(10).
            03 STUDENT-LNAME PIC X(15).
          02 BDAY.
            03 STUDENT-YOB PIC 9(4).
            03 STUDENT-MOB PIC 99.
            03 STUDENT-DOB PIC 99.

       WORKING-STORAGE SECTION.
         01 WS-STUDENT.
      *    88 WS-EOAF VALUE HIGH-VALUES.
          02 WS-STUDENT-ID PIC 9(5).
          02 WS-NAME.
           03 WS-FNAME PIC X(10).
           03 WS-MNAME PIC X(10).
           03 WS-LNAME PIC X(15).
          02 WS-BDAY.
           03 WS-YOB PIC 9(4).
           03 WS-MOB PIC 99.
           03 WS-DOB PIC 99.

         01 WS-EOF PIC A(1).
         01 WS-FOUND-FLAG PIC 9 VALUE 0.
         77 WS-CHOICEEVAL PIC X(2).
         77 ENTER PIC Z.

         01 WS-STUDENT-PORTAL.
          05 APPLCT-FNAME PIC X(15).
          05 APPLCT-MNAME PIC X(15).
          05 APPLCT-LNAME PIC X(15).
          05 APPLCT-BYEAR PIC 9(4).
          05 APPLCT-BMONTH PIC 99.
          05 APPLCT-BDAY PIC 99.

         01 HOLD-STD-ID PIC 9(5).

       SCREEN SECTION.
          01 CLRSCR.
           05 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY CLRSCR
            DISPLAY "LANI APPLICATION FORM (ATTEMPT)" AT 0546
            DISPLAY "[A] - STUDENT PORTAL" AT 0752
            DISPLAY "[B] - ADMIN PORTAL" AT 0852
            DISPLAY "[C] - EXIT PROGRAM" AT 0952
            DISPLAY "ENTER YOUR CHOICE: " AT 1152
            ACCEPT WS-CHOICEEVAL AT 1171

            EVALUATE WS-CHOICEEVAL
                WHEN 'A' WHEN 'a'
                   PERFORM STUDENT-PORTAL
                WHEN 'B' WHEN 'b'
                   PERFORM ADMIN-PORTAL
                WHEN 'C' WHEN 'c'
                   PERFORM EXIT-PROGRAM
                WHEN 'D' WHEN 'd'
                   PERFORM TESTING
                WHEN OTHER
                   DISPLAY "INVALID CHOICE!" AT 1252
                   PERFORM GETCH
                   PERFORM MAIN-PROCEDURE
            END-EVALUATE.

            STOP RUN.

      *********** WRITING TO THE DAT FILE
       TESTING.
           DISPLAY CLRSCR.
             OPEN EXTEND STUDENT
              MOVE 00002 TO STUDENT-ID.
              MOVE "ELLARIE" TO STUDENT-FNAME.
              MOVE "BORROMEO" TO STUDENT-LNAME.
              MOVE 2003 TO STUDENT-YOB.
              MOVE 09 TO STUDENT-MOB.
              MOVE 14 TO STUDENT-DOB.
             WRITE STUDENT-FILE
             END-WRITE.
            CLOSE STUDENT.

            OPEN INPUT STUDENT.
            READ STUDENT
               AT END SET END-OF-APP-FILE TO TRUE
            END-READ
            PERFORM UNTIL END-OF-APP-FILE
               DISPLAY STUDENT-FILE
               PERFORM GETCH
               READ STUDENT
                   AT END SET END-OF-APP-FILE TO TRUE
               END-READ
            END-PERFORM
            CLOSE STUDENT.

      *      OPEN INPUT STUDENT.
      *       PERFORM UNTIL WS-EOF='Y' OR WS-FOUND-FLAG = 1
      *         READ STUDENT INTO WS-STUDENT
      *           AT END
      *             MOVE 'Y' TO WS-EOF
      *           NOT AT END
      *             IF STUDENT-LNAME EQUALS "BORROMEO"
      *                 DISPLAY WS-STUDENT AT 0220
      *                 PERFORM GETCH
      *                 MOVE 1 TO WS-FOUND-FLAG
      *             END-IF
      *             DISPLAY WS-STUDENT
      *         END-READ
      *       END-PERFORM.
      *      CLOSE STUDENT.
            EXIT.

       STUDENT-PORTAL.
           DISPLAY CLRSCR.
           DISPLAY "STUDENT APPLICATION DETAILS" AT 0546
               DISPLAY "ENTER FIRST NAME: " AT 0746
               ACCEPT APPLCT-FNAME AT 0764
               DISPLAY "ENTER MIDDLE NAME: " AT 0846
               ACCEPT APPLCT-MNAME AT 0865
               DISPLAY "ENTER LAST NAME: " AT 0946
               ACCEPT APPLCT-LNAME AT 0963
               DISPLAY "ENTER BIRTH YEAR: " AT 1046
               ACCEPT APPLCT-BYEAR AT 1064
               DISPLAY "ENTER BIRTH MONTH: " AT 1146
               ACCEPT APPLCT-BMONTH AT 1165
               DISPLAY "ENTER BIRTH DAY: " AT 1246
               ACCEPT APPLCT-BDAY AT 1263

               PERFORM CHECK-FILE
               IF WS-FOUND-FLAG EQUALS 1
                   DISPLAY "RECORD ALREADY EXISTS" AT 1146
                   DISPLAY "CANNOT MAKE A NEW ACCOUNT" AT 1246
                   PERFORM GETCH
               ELSE
                   PERFORM APPEND-RECORD
                   DISPLAY "NEW RECORD HAS BEEN MADE" AT 1346
                   PERFORM GETCH
               END-IF.
           EXIT.


       ADMIN-PORTAL.
           DISPLAY CLRSCR.
           DISPLAY "ENTER FIRST NAME: " AT 1040
           EXIT.

       EXIT-PROGRAM.
           DISPLAY " ".
           EXIT.

       CHECK-FILE.
            MOVE 0 TO HOLD-STD-ID.
            OPEN INPUT STUDENT.
            READ STUDENT
               AT END SET END-OF-APP-FILE TO TRUE
            END-READ
            PERFORM UNTIL END-OF-APP-FILE
      *        DISPLAY STUDENT-FILE
      *         PERFORM GETCH
      *         NOT AT END
                ADD 1 TO HOLD-STD-ID
                   IF APPLCT-FNAME EQUALS STUDENT-FNAME AND APPLCT-LNAME
                    EQUALS STUDENT-LNAME THEN
                       MOVE 1 TO WS-FOUND-FLAG
                       SET END-OF-APP-FILE TO TRUE
                   END-IF
               READ STUDENT
                   AT END
                       SET END-OF-APP-FILE TO TRUE
               END-READ
            END-PERFORM
            CLOSE STUDENT.
            EXIT.

       APPEND-RECORD.
              OPEN EXTEND STUDENT
              COMPUTE HOLD-STD-ID = HOLD-STD-ID + 1
              MOVE HOLD-STD-ID TO STUDENT-ID.
              MOVE APPLCT-FNAME TO STUDENT-FNAME.
              MOVE APPLCT-MNAME TO STUDENT-MNAME.
              MOVE APPLCT-LNAME TO STUDENT-LNAME.
              MOVE APPLCT-BYEAR TO STUDENT-YOB.
              MOVE APPLCT-BMONTH TO STUDENT-MOB.
              MOVE APPLCT-BDAY TO STUDENT-DOB.
             WRITE STUDENT-FILE
             END-WRITE.
            CLOSE STUDENT.

       GETCH.
           DISPLAY "PRESS ENTER TO CONTINUE" AT 1652
           ACCEPT ENTER
           EXIT.


       END PROGRAM YOUR-PROGRAM-NAME.
