*
      * Author: Elayne Colvin
      * Date: April 17, 2017
      * Program #07 Due: April 20, 2017 PTS:
      * Description: SORTING PART 2
      *

       IDENTIFICATION DIVISION.

       PROGRAM-ID. Program07.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT TRANSACTION-FILE
               ASSIGN "TRNSISAM.TXT"
               ORGANIZATION LINE SEQUENTIAL.

           SELECT MASTER-FILE
              ASSIGN "VSAM.TXT"
              ORGANIZATION IS INDEXED
              ACCESS MODE IS RANDOM
              RECORD KEY IS MR-NUM
              FILE STATUS IS FILE-STATUS.


           SELECT PRINT-FILE
              ASSIGN TO "PROG07.TXT".

       DATA DIVISION.

       FILE SECTION.

       FD  TRANSACTION-FILE.
       01  TRANSACTION-REC             PIC X(80).

       FD  MASTER-FILE.
       01  MASTER-REC.
           05  MR-NUM                  PIC XXX.
           05                          PIC X(77).

       FD  PRINT-FILE.
       01  PRINT-REC                   PIC X(132).

       WORKING-STORAGE SECTION.

       01  SWITCHES.
           05  END-OF-DATA             PIC XXX.
           05  FILE-STATUS             PIC XX.
           05  MR-END-OF-FILE          PIC XXX.
           05  TR-END-OF-FILE          PIC XXX.

       01  CALCS.
           05  GROSS-PRICE             PIC S9(6)V99.
           05  SALES-TAX               PIC 999V999.
           05  DISCOUNT-AMT            PIC S9(6)V99.
           05  DISCOUNT-PERCENT        PIC 999V99.

       01  IR-INPUT-REC.
           05  IR-PROD-NUM             PIC 999.
           05  IR-PROD-DESC            PIC X(27).
           05  IR-UNIT-PRICE           PIC 999V99.
           05  IR-MIN-ORDER            PIC 999.
           05  IR-QTY-DISCOUNT-LEVEL   PIC 999.
           05  IR-DISCOUNT-PERCENT     PIC V999.
           05  IR-TOTAL                PIC 9(7).

       01  TR-TRANS-REC.
           05  TR-CUS-NAME             PIC X(25).
           05                          PIC X(5).
           05  TR-QTY-PURCHASED        PIC 999.
           05                          PIC X.
           05  TR-PROD-NUM             PIC 999.

       01  DL-DETAIL-LINE.
           05  DL-CUS-NAME             PIC X(18).
           05                          PIC X.
           05  DL-PROD-NUM             PIC ZZ9.
           05                          PIC X.
           05  DL-PROD-DESC            PIC X(23).
           05                          PIC X.
           05  DL-QTY-PURCHASED        PIC ZZ9.
           05                          PIC X.
           05  DL-UNIT-PRICE           PIC $ZZZ.ZZ-.
           05  DL-UNIT-PRICE-X
               REDEFINES DL-UNIT-PRICE PIC X(8).
           05                          PIC X.
           05  DL-GROSS-PRICE          PIC $ZZZ,ZZZ.ZZ-.
           05                          PIC X.
           05  DL-SALES-TAX            PIC $ZZZ.ZZZ.
           05                          PIC XX.
           05  DL-DISCOUNT-PERCENT     PIC ZZZ.ZZ.
           05                          PIC X.
           05  DL-DISCOUNT-AMOUNT      PIC $ZZZ,ZZZ.ZZ-.
           05                          PIC X.
           05  DL-NET-PRICE            PIC $ZZ,ZZZ,ZZZ.ZZ-.
           05  DL-NET-PRICE-X
               REDEFINES DL-NET-PRICE  PIC X(14).

       01  CH-COLUMN-HEADINGS.
           05  CH-CUS-NAME             PIC X(13) VALUE "CUSTOMER NAME".
           05                          PIC XX.
           05  CH-PROD-NUM             PIC X(8) VALUE "PROD NUM".
           05                          PIC XXXX.
           05  CH-PROD-DESC            PIC X(9) VALUE "PROD DESC".
           05                          PIC X(11).
           05  CH-QTY-PURCH            PIC X(3) VALUE "QTY".
           05                          PIC XX.
           05  CH-UNIT-PRICE           PIC X(6) VALUE "UNIT $".
           05                          PIC X(5).
           05  CH-GROSS-PRICE          PIC X(7) VALUE "GROSS $".
           05                          PIC XX.
           05  CH-SALES-TAX            PIC X(9) VALUE "SALES TAX".
           05                          PIC XX.
           05  CH-DIS-PERCENT          PIC X(8) VALUE "DISCOUNT".
           05                          PIC XX.
           05  CH-DIS-AMT              PIC X(12) VALUE "DISCOUNT AMT".
           05                          PIC XXX.
           05  CH-NET-PRICE            PIC X(9) VALUE "NET PRICE".

       01  MESSAGES.
           05                          PIC X(102).
           05  DL-MESSAGE              PIC X(30).

       PROCEDURE DIVISION.

       000-MAINLINE.
           OPEN INPUT TRANSACTION-FILE
                 OUTPUT PRINT-FILE

           OPEN I-O MASTER-FILE

      *INITIALIZATION SECTION
           PERFORM 100-INITIALIZE

      *PROCESSING SECTION
           PERFORM UNTIL TR-END-OF-FILE = "YES"
               READ TRANSACTION-FILE INTO TR-TRANS-REC
                   AT END
                       MOVE "YES" TO TR-END-OF-FILE
                   NOT AT END
                       PERFORM 200-PROCESSING
           END-PERFORM

      *TERMINATION SECTION
           CLOSE MASTER-FILE
               TRANSACTION-FILE
               PRINT-FILE

           STOP RUN.
      *

       100-INITIALIZE.
           INITIALIZE CALCS
           MOVE "NO " TO END-OF-DATA
           MOVE "NO " TO MR-END-OF-FILE
           MOVE "NO " TO TR-END-OF-FILE
      *Write PRINT-REC
           MOVE SPACES TO PRINT-REC
           WRITE PRINT-REC FROM CH-COLUMN-HEADINGS.

       200-PROCESSING.
           MOVE TR-PROD-NUM TO MR-NUM
           READ MASTER-FILE INTO IR-INPUT-REC
           IF FILE-STATUS = "00"
               PERFORM 300-MATCH
           ELSE
               PERFORM 310-NOT-ON-MASTER
               DISPLAY FILE-STATUS
           END-IF.

       300-MATCH.
           INITIALIZE DL-DETAIL-LINE
           INITIALIZE CALCS
           INITIALIZE MESSAGES
           MOVE TR-CUS-NAME TO DL-CUS-NAME
           MOVE TR-PROD-NUM TO DL-PROD-NUM
           MOVE IR-PROD-DESC TO DL-PROD-DESC
           MOVE TR-QTY-PURCHASED TO DL-QTY-PURCHASED
           MOVE IR-UNIT-PRICE TO DL-UNIT-PRICE
           IF TR-QTY-PURCHASED NOT < IR-MIN-ORDER
               IF TR-QTY-PURCHASED >= IR-QTY-DISCOUNT-LEVEL


                   MULTIPLY IR-UNIT-PRICE BY TR-QTY-PURCHASED
                       GIVING GROSS-PRICE
                   MOVE GROSS-PRICE TO DL-GROSS-PRICE

                   MULTIPLY IR-DISCOUNT-PERCENT BY 100
                       GIVING DISCOUNT-PERCENT
                   MOVE DISCOUNT-PERCENT TO DL-DISCOUNT-PERCENT

                   MULTIPLY .07 BY GROSS-PRICE
                       GIVING SALES-TAX
                   MOVE SALES-TAX TO DL-SALES-TAX

                   COMPUTE DISCOUNT-AMT = GROSS-PRICE
                       * IR-DISCOUNT-PERCENT

                   MOVE DISCOUNT-AMT TO DL-DISCOUNT-AMOUNT

                   COMPUTE DL-NET-PRICE ROUNDED = (GROSS-PRICE -
                       DISCOUNT-AMT) + SALES-TAX

               ELSE
                   MOVE ALL "*" TO DL-NET-PRICE-X
                   MOVE "BELOW MIN. ORDER" TO DL-MESSAGE

               END-IF
           END-IF
           WRITE PRINT-REC FROM DL-DETAIL-LINE AFTER 2
           WRITE PRINT-REC FROM MESSAGES AFTER 1
           ADD TR-QTY-PURCHASED TO IR-TOTAL
           REWRITE MASTER-REC FROM IR-INPUT-REC
               INVALID KEY
                   MOVE "REWRITE NOT SUCCESSFUL" TO DL-MESSAGE
           END-REWRITE.

       310-NOT-ON-MASTER.
           MOVE ALL "*" TO DL-UNIT-PRICE-X, DL-NET-PRICE-X.
