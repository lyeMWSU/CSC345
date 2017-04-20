      *
      * Author: Elayne Colvin
      * Date: March 30, 2017
      * Program #06 Due: April 3, 2017 PTS:
      * Description: SORTING
      *

       IDENTIFICATION DIVISION.

       PROGRAM-ID. Program06.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE
               ASSIGN "ADV6MSTR.TXT"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN "SORT-WORK".

           SELECT SORTED-FILE
              ASSIGN "MSTR.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS SEQUENTIAL
              RECORD KEY IS SF-NUM
              FILE STATUS IS FILE-STATUS.


           SELECT PRINT-FILE
      *         ASSIGN DISPLAY.
               ASSIGN TO "PROG06.TXT".

       DATA DIVISION.

       FILE SECTION.

       FD  INPUT-FILE.
       01  INPUT-REC                   PIC X(80).

       SD  SORT-FILE.
       01  SORT-REC.
           05  SR-NUM                  PIC 999.
           05                          PIC X(77).

       FD  SORTED-FILE.
       01  SORTED-REC.
           05  SF-NUM                  PIC 999.
           05                          PIC X(77).

       FD  PRINT-FILE.
       01  PRINT-REC                   PIC X(80).

       WORKING-STORAGE SECTION.

       01  SWITCHES.
           05  FILE-STATUS             PIC XX.
           05  IR-END-OF-FILE          PIC X.
           05  SF-END-OF-FILE          PIC X.

       01  IR-INPUT-REC.
           05  IR-PROD-NUM             PIC 999.
           05  IR-PROD-DESC            PIC X(27).
           05                          PIC X(10).
           05  IR-UNIT-PRICE           PIC 999V99.
           05                          PIC X(4).
           05  IR-MIN-ORDER            PIC 999.
           05  IR-QTY-DISCOUNT-LEVEL   PIC 999.
           05                          PIC X(5).
           05  IR-DISCOUNT-PERCENT     PIC V999.

       01  SW-SORTED-WORK.
           05  SW-PROD-NUM             PIC 999.
           05  SW-PROD-DESC            PIC X(27).
           05  SW-UNIT-PRICE           PIC 999V99.
           05  SW-MIN-ORDER            PIC 999.
           05  SW-QTY-DISCOUNT-LEVEL   PIC 999.
           05  SW-DISCOUNT-PERCENT     PIC V999.
           05  SW-TOTAL                PIC 9(7).

       01  DL-DETAIL-LINE.
           05  DL-INPUT-REC            PIC X(63).
           05                          PIC X(4).
           05  DL-MESSAGE              PIC X(10).

       PROCEDURE DIVISION.

       000-MAINLINE.
      *INITIALIZATION SECTION
           PERFORM 100-INITIALIZE
           SORT SORT-FILE
                 ASCENDING KEY SR-NUM
                 INPUT PROCEDURE IS 400-SORT
                 OUTPUT PROCEDURE IS 410-OUT-SORT
      *TERMINATION SECTION
           STOP RUN.
      *

       100-INITIALIZE.
           MOVE "N" TO IR-END-OF-FILE
           MOVE "N" TO SF-END-OF-FILE
      *Write PRINT-REC
           MOVE SPACES TO PRINT-REC.

       400-SORT.
           OPEN INPUT INPUT-FILE
                 OUTPUT PRINT-FILE

           PERFORM UNTIL IR-END-OF-FILE = "Y"
               READ INPUT-FILE INTO IR-INPUT-REC
                   AT END
                       MOVE "Y" TO IR-END-OF-FILE
                   NOT AT END
                       PERFORM 405-PROCESSING
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE.

       405-PROCESSING.
           MOVE IR-PROD-NUM TO SW-PROD-NUM
           MOVE IR-PROD-DESC TO SW-PROD-DESC
           MOVE IR-UNIT-PRICE TO SW-UNIT-PRICE
           MOVE IR-MIN-ORDER TO SW-MIN-ORDER
           MOVE IR-QTY-DISCOUNT-LEVEL TO SW-QTY-DISCOUNT-LEVEL
           MOVE IR-DISCOUNT-PERCENT TO SW-DISCOUNT-PERCENT

           MOVE IR-INPUT-REC TO DL-INPUT-REC
           WRITE PRINT-REC FROM DL-DETAIL-LINE
               AFTER 2

           MOVE SW-SORTED-WORK TO SORT-REC
           RELEASE SORT-REC.

       410-OUT-SORT.
           OPEN OUTPUT SORTED-FILE
           MOVE "N" TO SF-END-OF-FILE
           RETURN SORT-FILE INTO SORTED-REC
               AT END
                   MOVE "Y" TO SF-END-OF-FILE
           END-RETURN

           PERFORM 420-CHECK-SORTS
               UNTIL SF-END-OF-FILE = "Y"
           CLOSE SORTED-FILE
               PRINT-FILE.

       420-CHECK-SORTS.
           WRITE SORTED-REC
           MOVE SORT-REC TO DL-INPUT-REC

           IF FILE-STATUS = "00"
               MOVE "WRITTEN" TO DL-MESSAGE
           ELSE
               IF FILE-STATUS = "22"
                   MOVE "DUPLICATE" TO DL-MESSAGE
               ELSE
                   DISPLAY "FILE STATUS ERROR"
                   DISPLAY FILE-STATUS
               END-IF
           END-IF

           WRITE PRINT-REC FROM DL-DETAIL-LINE
               AFTER 2

           RETURN SORT-FILE INTO SORTED-REC
               AT END
                   MOVE "Y" TO SF-END-OF-FILE.
