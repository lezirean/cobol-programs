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
         SELECT RECTANGLE
         ASSIGN TO "C:\cobol_project\FHSeq_RectAreaAndPerim.dat"
         ORGANIZATION IS LINE SEQUENTIAL
         ACCESS IS SEQUENTIAL.
      **********************************
       DATA DIVISION.
       FILE SECTION.
        FD RECTANGLE.
        01 FILE-RECTANGLE.
          05 RECT-LENGTH PIC 9(3).99.
          05 RECT-WIDTH PIC 9(3).99.
          05 RECT-AREA PIC 9(3).99.
          05 RECT-PERIMETER PIC 9(3).99.
      *************************************
       WORKING-STORAGE SECTION.
       01 WS-RECTANGLE.
          05 WS-RECT-LENGTH PIC 9(3)V99.
          05 WS-RECT-LENGTHINPUT PIC X(6).
          05 WS-RECT-WIDTH PIC 9(3)V99.
          05 WS-RECT-WIDTHINPUT PIC X(6).
          05 WS-RECT-AREA PIC 9(3).99.
          05 WS-RECT-PERIMETER PIC 9(3).99.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ENTER THE LENGTH OF RECTANGLE: ".
           ACCEPT WS-RECT-LENGTHINPUT.
           DISPLAY "ENTER THE WIDTH OF RECTANGLE:  ".
           ACCEPT WS-RECT-WIDTHINPUT.
           MOVE WS-RECT-LENGTHINPUT TO WS-RECT-LENGTH.
           MOVE WS-RECT-WIDTHINPUT TO WS-RECT-WIDTH.

           COMPUTE WS-RECT-PERIMETER =
             (2 * WS-RECT-LENGTH) + (2 * WS-RECT-WIDTH)
           COMPUTE WS-RECT-AREA = WS-RECT-LENGTH * WS-RECT-WIDTH

           OPEN EXTEND RECTANGLE
               MOVE WS-RECT-LENGTH TO RECT-LENGTH
               MOVE WS-RECT-WIDTH TO RECT-WIDTH
               MOVE WS-RECT-AREA TO RECT-AREA
               MOVE WS-RECT-PERIMETER TO RECT-PERIMETER
           WRITE FILE-RECTANGLE
           CLOSE RECTANGLE.
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
