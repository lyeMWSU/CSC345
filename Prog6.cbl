      ******************************************************************
      * Author: Antonio Cristobal
      * Date: 04/03/2017
      * Program: #6        Due: 04/03/2017     PTS:
      * Purpose: Practice with VSAM
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG6.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-IN
               ASSIGN TO "MSTRISAM.TXT"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT SORT-FILE
               ASSIGN TO SYSWORK.

           SELECT DATA-OUT
               ASSIGN TO "ADV6MSTR.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS SR-ISAM-NUM
               FILE STATUS IS FILE-STATUS.

           SELECT PRINT-DATA
               ASSIGN TO DISPLAY.


       DATA DIVISION.
       FILE SECTION.
       FD  DATA-IN.
       01  UNSORTED-REC                PIC X(80).

       SD  SORT-FILE.
       01  TEMP-SORT-REC.
           05 SR-PRO-NUM               PIC 9(3).
           05 FILLER                   PIC X(77).

       FD  DATA-OUT.
       01  SR-SORTED-REC.
           05 SR-ISAM-NUM              PIC 9(3).
           05 FILLER                   PIC X(77).

       FD  PRINT-DATA.
       01  PRINT-REC                  PIC X(80).



       WORKING-STORAGE SECTION.
       01  NEEDED-VARIABLES.
           05 END-OF-FILE              PIC XXX.
           05 FILE-STATUS              PIC XX.
           05 SORT-END-OF-FILE         PIC XXX.

       01  INPUT-REC.
           05 PROD-NUM-IN              PIC 999.
           05 PROD-DESCR-IN            PIC X(27).
           05 FILLER                   PIC X(10).
           05 UNIT-PRICE-IN            PIC 9(3)V99.
           05 FILLER                   PIC X(4).
           05 MIN-ORDER-IN             PIC 9(3).
           05 QTY-DISC-IN              PIC 9(3).
           05 FILLER                   PIC X(5).
           05 DISC-PERCENT-IN          PIC V9(3).
           05 FILLER                   PIC X(16).


       01  SORT-WORK.
           05 PROD-NUM                 PIC 999.
           05 PROD-DESCR               PIC X(27).
           05 UNIT-PRICE               PIC 9(3)V99.
           05 MIN-ORDER                PIC 9(3).
           05 QTY-DISC                 PIC 9(3).
           05 DISC-PERCENT             PIC V9(3).
           05 TOTAL-SOLD               PIC 9(7).

       01  HL-HEADING-LINE              VALUE SPACES.
           05                           PIC X(15).
           05  HL-HEADING               PIC X(16).


       01  DETAIL-LINE.
           05 DL-RECORD                PIC X(51).
           05 FILLER                   PIC X(19).
           05 DL-MESSAGE               PIC X(10).

       PROCEDURE DIVISION.
       100-MAIN-MODULE.
           SORT SORT-FILE
               ON ASCENDING KEY SR-PRO-NUM
               INPUT PROCEDURE IS 200-PRIOR-TO-RUN
               OUTPUT PROCEDURE IS 300-AFTER-RUN
           STOP RUN

           .

       200-PRIOR-TO-RUN.

           OPEN INPUT DATA-IN
               OUTPUT PRINT-DATA

           MOVE "NO" TO END-OF-FILE
           PERFORM UNTIL END-OF-FILE = "YES"
               READ DATA-IN INTO INPUT-REC
                   AT END
                       MOVE "YES" TO END-OF-FILE
                   NOT AT END
                       PERFORM 210-PRIOR-TO-RUN-CHANGES
               END-READ
           END-PERFORM
           CLOSE DATA-IN.


       210-PRIOR-TO-RUN-CHANGES.

           MOVE PROD-NUM-IN TO PROD-NUM
           MOVE PROD-DESCR-IN TO PROD-DESCR
           MOVE UNIT-PRICE-IN TO UNIT-PRICE
           MOVE MIN-ORDER-IN TO MIN-ORDER
           MOVE QTY-DISC-IN TO QTY-DISC
           MOVE DISC-PERCENT-IN TO DISC-PERCENT
           MOVE ZEROS TO TOTAL-SOLD

           MOVE SORT-WORK TO DL-RECORD

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2

           RELEASE TEMP-SORT-REC FROM SORT-WORK.


       300-AFTER-RUN.

           OPEN OUTPUT DATA-OUT

           MOVE "OUTPUT PROCEDURE" TO HL-HEADING
           MOVE SPACES TO PRINT-REC
           WRITE PRINT-REC AFTER PAGE
           WRITE PRINT-REC FROM HL-HEADING-LINE AFTER 2
           MOVE "NO" TO SORT-END-OF-FILE

      *Crashes at this for some reason
           RETURN SORT-FILE INTO SR-SORTED-REC
               AT END
                   MOVE "YES" TO SORT-END-OF-FILE
           END-RETURN

           PERFORM 310-AFTER-RUN-CHANGES
               UNTIL SORT-END-OF-FILE = "YES"

           CLOSE DATA-OUT
                 PRINT-DATA.


       310-AFTER-RUN-CHANGES.

           WRITE SR-SORTED-REC

           MOVE TEMP-SORT-REC TO DL-RECORD

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

           WRITE PRINT-REC FROM DETAIL-LINE AFTER 2

           RETURN SORT-FILE INTO SR-SORTED-REC
               AT END MOVE "YES" TO SORT-END-OF-FILE.
