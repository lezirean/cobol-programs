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
      * 77 A PIC 999 VALUE 1.
      * 77 B PIC S999 VALUE -1.
      * 77 C PIC 999.
      * 77 C1 PIC ZZ9.
      * 77 N PIC 99.
       01 NUM-A PIC 9(3) VALUE 1.
       01 NUM-B PIC S9(3) VALUE -1.
       01 NEXT-NUM PIC 9(3).
       01 DIS-NUM PIC 9(3).
       01 INPUT-NUM PIC 9(2).
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
            MOVE 1 TO NUM-A.
            MOVE -1 TO NUM-B.
            DISPLAY "ENTER N VALUE".
            ACCEPT INPUT-NUM.

            PERFORM INPUT-NUM TIMES
               COMPUTE NEXT-NUM = NUM-A + NUM-B
               MOVE NEXT-NUM TO DIS-NUM
               DISPLAY DIS-NUM
               MOVE NUM-B TO NUM-A
               MOVE NEXT-NUM TO NUM-B
            END-PERFORM.

            STOP RUN.

       DISP-PARA.
            COMPUTE NEXT-NUM = NUM-A + NUM-B.
            MOVE NEXT-NUM TO DIS-NUM.
            DISPLAY DIS-NUM.
            MOVE NUM-B TO NUM-A.
            MOVE NEXT-NUM TO NUM-B.

       END PROGRAM YOUR-PROGRAM-NAME.
