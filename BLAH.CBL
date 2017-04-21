      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM07.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT TRANSACTION-FILE
           ASSIGN TO "TRNSISAM.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT MASTER-FILE
           ASSIGN "VSAM.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS MR-ID
           FILE STATUS IS MASTER-SWITCH.

           SELECT OUTPUT-FILE
           ASSIGN TO "P7REPORT.TXT".
      *     ASSIGN TO DISPLAY.

       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TF-ID                        PIC X(80).

       FD  MASTER-FILE.
       01  MASTER-REC.
           05  MR-ID                   PIC X(5).
           05                          PIC X(75).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(132).

       WORKING-STORAGE SECTION.

       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

      *INPUT-RECORD...
       01  TRANSACTION-RECORD.
           05  TR-CUS-NAME             PIC X(25).
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  TR-QTY-PCH              PIC 999.
           05  FILLER                  PIC X VALUE SPACES.
           05  TR-PRO-NUM              PIC 999.

       01  MASTER-RECORD.
           05  MR-PRO-NUM              PIC 999.
           05  MR-PRO-DES              PIC X(27).
           05  MR-PRICE                PIC 999V99.
           05  MR-MIN-ORD              PIC 999.
           05  MR-QTY-DIS              PIC 999.
           05  MR-DIS-PER              PIC V999.
           05  MR-TOTAL                PIC 9(7).

      *CALCULATIONS...
       01  DC-DETAIL-CALCS.
           05  DC-GROSS-PAY            PIC X(5).

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-CUS-NAME             PIC X(25).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-QTY-PCH              PIC ZZ9.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-PRO-NUM              PIC 999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-PRO-DES              PIC X(27).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-PRICE                PIC $999.99.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-GRO-PRI              PIC $9999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-SAL-TAX              PIC 9999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-DIS-PER              PIC 9999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-DIS-AMT              PIC 9999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-NET-PRI              PIC 9999.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-MESSAGE              PIC X(11) VALUE "NOT ON FILE".


       01  DR-RECORD                   VALUE SPACES.
           05  DR-REC                  PIC X(63).
           05  FILLER                  PIC X(5).
           05  DR-MESSAGE              PIC X(10).

      *HEADING AREAS...
       01  PC-PAGE-CONTROLS.
           05  PC-PAGE-NUM             PIC S99  VALUE 0.
           05  PC-LINES-USED           PIC S99  VALUE 0.
           05  PC-LINES-PER-PAGE       PIC S99  VALUE 27.

       01  HL-HEADING-LINE.
           05  HL-DATE.
               10 HL-MO                PIC XX/.
               10 HL-DAY               PIC XX/.
               10 HL-YY                PIC XX.
           05  FILLER                  PIC X(3).
           05  HL-HEAD-TITLE           PIC X(20).
           05  FILLER                  PIC X(5).
           05                          PIC X(4) VALUE "PAGE".
           05  HL-PAGE-NUM             PIC Z9.

       01  SWITCHES.
           05  EOD                     PIC XXX.
           05  INPUT-SWITCH            PIC XXX VALUE "NO".
           05  MASTER-SWITCH           PIC XX.
           05  OUTPUT-SWITCH           PIC XXX VALUE "NO".

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 020-OPEN
           PERFORM 100-INITIALIZE
           PERFORM 850-HEADINGS
           PERFORM 800-READ-TRANS
           PERFORM 810-READ-MASTER
           PERFORM 200-PROCESSING
               UNTIL EOD = "YES"

           PERFORM 030-CLOSE.

       020-OPEN.
           OPEN INPUT      TRANSACTION-FILE
                OUTPUT     OUTPUT-FILE

           OPEN I-O MASTER-FILE.

       030-CLOSE.
           CLOSE     TRANSACTION-FILE
                     OUTPUT-FILE

           CLOSE MASTER-FILE
           STOP RUN.


       100-INITIALIZE.
           MOVE "NO " TO EOD
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD.


       200-PROCESSING.
           IF MASTER-SWITCH = "00"
               PERFORM 210-MATCH
           ELSE
               PERFORM 220-NOT-ON-MASTER
           END-IF.

           PERFORM 800-READ-TRANS
           IF EOD = "NO"
               PERFORM 810-READ-MASTER
           END-IF.

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.

       210-MATCH.
           MOVE TR-CUS-NAME TO DL-CUS-NAME
           MOVE TR-QTY-PCH TO DL-QTY-PCH
           MOVE TR-PRO-NUM TO DL-PRO-NUM
           MOVE MR-PRO-DES TO DL-PRO-DES

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.

       220-NOT-ON-MASTER.


       800-READ-TRANS.
           READ TRANSACTION-FILE INTO TRANSACTION-RECORD
               AT END MOVE "YES" TO EOD
           END-READ.

       810-READ-MASTER.
           MOVE TF-ID TO MR-ID
           READ MASTER-FILE INTO MASTER-RECORD
               AT END MOVE "YES" TO EOD
           END-READ.

       850-HEADINGS.
           MOVE "TRANSACTION REPORT" TO HL-HEAD-TITLE
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD
           ADD 5 TO PC-LINES-USED.
