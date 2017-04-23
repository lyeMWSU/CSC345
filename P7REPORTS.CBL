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
           ASSIGN TO "VSAM.TXT"
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
           05  MR-ID                   PIC X(3).
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
           05  FILLER                  PIC X(43) VALUE SPACES.

       01  MASTER-RECORD.
           05  MR-PRO-NUM              PIC 999.
           05  MR-PRO-DES              PIC X(27).
           05  MR-PRICE                PIC 999V99.
           05  MR-MIN-ORD              PIC 999.
           05  MR-QTY-DIS              PIC 999.
           05  MR-DIS-PER              PIC V999.
           05  MR-TOTAL                PIC 9(7).
           05  FILLER                  PIC X(29).

      *CALCULATIONS...
       01  DC-DETAIL-CALCS.
           05  DC-GRO-PRI              PIC 9(6)V99.
           05  DC-SAL-TAX              PIC 9(6)V99.
           05  DC-DIS-PER              PIC 99V9.
           05  DC-DIS-AMT              PIC 9(3)V99.
           05  DC-NET-PRI              PIC 9(5)V99.



      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-CUS-NAME             PIC X(20).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL-PRO-NUM              PIC ZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL-PRO-DES              PIC X(23).
           05  DL-PRO-DES-X
               REDEFINES DL-PRO-DES    PIC X(23).
           05  DL-QTY-PCH              PIC ZZ9.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-PRICE                PIC $ZZ9.99.
           05  DL-PRICE-X
               REDEFINES DL-PRICE      PIC X(7).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL-GRO-PRI              PIC $ZZ,ZZ9.99.
           05  DL-GRO-PRI-X
               REDEFINES DL-GRO-PRI    PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL-SAL-TAX              PIC $ZZ9.99.
           05  DL-SAL-TAX-X
               REDEFINES DL-SAL-TAX    PIC X(7).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL-DIS-PER              PIC ZZ9.
           05  DL-DIS-PER-X
               REDEFINES DL-DIS-PER    PIC X(3).
           05  DL-PERCENTAGE           PIC X.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-DIS-AMT              PIC $Z,ZZ9.99.
           05  DL-DIS-AMT-X
               REDEFINES DL-DIS-AMT    PIC X(9).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-NET-PRI              PIC $Z,Z99.99.
           05  DL-NET-PRI-X
               REDEFINES DL-NET-PRI    PIC X(9).
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL-MESSAGE              PIC X(11) VALUE SPACES.

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

       01  HEADING-TWO.
           05                          PIC X(8) VALUE "CUSTOMER".
           05  FILLER                  PIC X(10) VALUE SPACES.
           05                          PIC X(7) VALUE "PRODUCT".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05                          PIC X(7) VALUE "PRODUCT".
           05  FILLER                  PIC X(16) VALUE SPACES.
           05                          PIC X(4) VALUE "QTY.".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(4) VALUE "UNIT".
           05  FILLER                  PIC X(6) VALUE SPACES.
           05                          PIC X(5) VALUE "GROSS".
           05  FILLER                  PIC X(5) VALUE SPACES.
           05                          PIC X(6) VALUE "SALES".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(4) VALUE "DIS.".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(8) VALUE "DISCOUNT".
           05  FILLER                  PIC X(4) VALUE SPACES.
           05                          PIC X(3) VALUE "NET".
           05  FILLER                  PIC X(9) VALUE SPACES.
           05                          PIC X(5) VALUE "ERROR".

       01  HEADING-THREE.
           05                          PIC X(4) VALUE "NAME".
           05                          PIC X(14) VALUE SPACES.
           05                          PIC X(6) VALUE "NUMBER".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(11) VALUE "DESCRIPTION".
           05  FILLER                  PIC X(12) VALUE SPACES.
           05                          PIC X(4) VALUE "PCH.".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(5) VALUE "PRICE".
           05  FILLER                  PIC X(5) VALUE SPACES.
           05                          PIC X(5) VALUE "PRICE".
           05  FILLER                  PIC X(5) VALUE SPACES.
           05                          PIC X(3) VALUE "TAX".
           05  FILLER                  PIC X(6) VALUE SPACES.
           05                          PIC X(3) VALUE "PER".
           05  FILLER                  PIC X(4) VALUE SPACES.
           05                          PIC X(7) VALUE "AMOUNT".
           05  FILLER                  PIC X(5) VALUE SPACES.
           05                          PIC X(5) VALUE "PRICE".
           05  FILLER                  PIC X(7) VALUE SPACES.
           05                          PIC X(7) VALUE "MESSAGE".

       01  HEADING-FOUR.
           05                          PIC X(17) VALUE ALL "_".
           05                          PIC X(1) VALUE SPACES.
           05                          PIC X(7) VALUE ALL "_".
           05                          PIC X(2) VALUE SPACES.
           05                          PIC X(20) VALUE ALL "_".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(4) VALUE ALL "_".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05                          PIC X(7) VALUE ALL "_".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05                          PIC X(10) VALUE ALL "_".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05                          PIC X(7) VALUE ALL "_".
           05  FILLER                  PIC X(2) VALUE SPACES.
           05                          PIC X(4) VALUE ALL "_".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(9) VALUE ALL "_".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(9) VALUE ALL "_".
           05  FILLER                  PIC X(3) VALUE SPACES.
           05                          PIC X(11) VALUE ALL "_".


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
               UNTIL EOD = "YES"
           PERFORM 030-CLOSE.

       020-OPEN.

           OPEN INPUT      TRANSACTION-FILE
                OUTPUT     OUTPUT-FILE
                I-O        MASTER-FILE.

       030-CLOSE.
           CLOSE     TRANSACTION-FILE
                     OUTPUT-FILE
                     MASTER-FILE
           STOP RUN.


       100-INITIALIZE.
           MOVE "NO " TO EOD
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD

           INITIALIZE DC-DETAIL-CALCS
           INITIALIZE DL-DETAIL-LINE.

       200-PROCESSING.
           MOVE TR-PRO-NUM TO MR-ID
           READ MASTER-FILE INTO MASTER-RECORD


           MOVE TR-CUS-NAME TO DL-CUS-NAME
           MOVE TR-QTY-PCH TO DL-QTY-PCH
           MOVE TR-PRO-NUM TO DL-PRO-NUM
           MOVE MR-PRO-DES TO DL-PRO-DES
           MOVE MR-PRICE TO DL-PRICE
           MOVE ALL "%" TO DL-PERCENTAGE

           MULTIPLY MR-PRICE BY TR-QTY-PCH GIVING DC-GRO-PRI
           MULTIPLY MR-DIS-PER BY 100 GIVING DC-DIS-PER
           MULTIPLY .07 BY DC-GRO-PRI GIVING DC-SAL-TAX
           COMPUTE DC-DIS-AMT = DC-GRO-PRI * MR-DIS-PER
           COMPUTE DC-NET-PRI ROUNDED = (DC-GRO-PRI -
                       DC-DIS-AMT) + DC-SAL-TAX

           MOVE DC-GRO-PRI TO DL-GRO-PRI
           MOVE DC-DIS-PER TO DL-DIS-PER
           MOVE DC-DIS-AMT TO DL-DIS-AMT
           MOVE DC-SAL-TAX TO DL-SAL-TAX
           MOVE DC-NET-PRI TO DL-NET-PRI


           IF MASTER-SWITCH = 00

               MOVE MASTER-SWITCH TO DL-MESSAGE

           ELSE
               MOVE ALL "*" TO DL-NET-PRI-X
               MOVE ALL "*" TO DL-PRICE-X
               MOVE SPACES TO DL-PRO-DES-X
               MOVE SPACES TO DL-GRO-PRI-X
               MOVE SPACES TO DL-SAL-TAX-X
               MOVE SPACES TO DL-DIS-PER-X
               MOVE SPACES TO DL-DIS-AMT-X
               MOVE SPACES TO DL-PERCENTAGE
               MOVE "NOT ON FILE" TO DL-MESSAGE
           END-IF.


           WRITE PRINT-RECORD FROM DL-DETAIL-LINE AFTER 1.
       800-READ-TRANS.
           MOVE "NO" TO EOD
           READ TRANSACTION-FILE INTO TRANSACTION-RECORD
               AT END
                   MOVE "YES" TO EOD
               NOT AT END
                   PERFORM 200-PROCESSING
           END-READ.

       850-HEADINGS.
           MOVE "TRANSACTION REPORT" TO HL-HEAD-TITLE
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER PAGE
           WRITE PRINT-RECORD FROM HEADING-TWO AFTER 1
           WRITE PRINT-RECORD FROM HEADING-THREE AFTER 1
           WRITE PRINT-RECORD FROM HEADING-FOUR AFTER 1
           MOVE SPACES TO PRINT-RECORD
           WRITE PRINT-RECORD
           ADD 5 TO PC-LINES-USED.
