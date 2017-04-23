      ******************************************************************
      * Author: AWAIS KHAN
      * Date: APRIL 20 2017
      * Purpose: TO SORT A MASTER FILE, PART 1
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG07.
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
               ASSIGN "AFTER_EDIT.TXT".


       DATA DIVISION.
       FILE SECTION.

       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD                  PIC X(80).

       FD  MASTER-FILE.
       01  MASTER-RECORD.
           05  MR-NUM                          PIC XXX.
           05                                  PIC X(77).

       FD  PRINT-FILE.
       01  PRINT-RECORD                        PIC X(132).


       WORKING-STORAGE SECTION.
      *Switch statement is for turning on and off a loop****
       01  SWITCHES.
           05  FILE-STATUS              PIC XX.
           05  TRANS-EOF                PIC XXX VALUE 'NO'.
           05  MAS-EOF                  PIC XXX VALUE 'NO'.
           05  END-DATA                 PIC XXX VALUE 'NO'.

       01  TC-CALS VALUE ZEROS.
           05  TC-DISCOUNT-AMT    PIC S9(6)V99.
           05  TC-DIS-PERCENT     PIC S9(3)V99.
           05  TC-GROSS           PIC S9(6)V99.
           05  TC-AMT             PIC S9(3)V99.
           05  TC-NET-PRICE       PIC S9(5)V99.
           05  TC-SALES-TAX       PIC SV99 VALUE .07.
           05  TC-SALES-AMT       PIC S9(4)V99.
           05  TC-GROSS-TAXED     PIC S9(6)V99.
           05  TC-AFTER-TAX       PIC S9(5)V99.

       01  IR-INPUT-RECORD.
           05  IR-PROD-NUM              PIC 9(3).
           05  IR-PROD-DES              PIC X(27).
           05  IR-UNIT-PRICE            PIC S9(3)V99.
           05  IR-MIN-ORDER             PIC S9(3).
           05  IR-QTY-DIS-LEVEL         PIC S9(3).
           05  IR-DIS-PERCENT           PIC SV9(3).
           05  IR-TOTAL                 PIC S9(7).

       01  TR-TRANS-REC.
           05 TR-CUS-NAME               PIC X(25).
           05 FILLER                    PIC X(5).
           05 TR-QTY-PURCHASED          PIC 999.
           05 FILLER                    PIC X.
           05 TR-PROD-NUM               PIC 999.

       01  DL-DETAIL-LINE.
           05  DL-CUS-NAME              PIC X(18).
           05  DL-PROD-NUM              PIC ZZ9.
           05  FILLER                   PIC X(2).
           05  DL-PROD-DES              PIC X(23).
           05  DL-QTY-PUR               PIC ZZ9.
           05  FILLER                   PIC X(2).
           05  DL-UNIT-PRICE            PIC $ZZ9.99-.
           05  DL-UNIT-PRICE-X REDEFINES DL-UNIT-PRICE
                                        PIC X(8).
           05  DL-GROSS-PRICE           PIC $ZZZ,ZZZ.ZZ-.
           05  FILLER                   PIC X(1).
           05  DL-SALES-TAX             PIC $ZZZ.ZZ.
           05  FILLER                   PIC X(2).
           05  DL-DIS-PERCENT           PIC ZZZ.ZZ.
           05  DL-PERCENT-SIGN          PIC X VALUE '%'.
           05  FILLER                   PIC X(2).
           05  DL-DIS-AMT               PIC $Z,ZZZ.ZZ.
           05  FILLER                   PIC X(1).
           05  DL-NET-PRICE             PIC $Z,ZZZ,ZZZ.ZZ-.
           05  DL-NET-PRICE-X REDEFINES DL-NET-PRICE
                                        PIC X(14).
           05  FILLER                   PIC X(2).
           05  DL-MESSAGE               PIC X(15).

       01  CL-COL-HEADINGS.
           05                          PIC X(13) VALUE "CUSTOMER NAME".
           05  FILLER                  PIC X(4).
           05                          PIC X(5) VALUE "PROD#".
           05  FILLER                  PIC X(2).
           05                          PIC X(12) VALUE "PRODUCT DESC".
           05  FILLER                  PIC X(10).
           05                          PIC X(3) VALUE "QTY".
           05  FILLER                  PIC X(3).
           05                          PIC X(5) VALUE "PRICE".
           05  FILLER                  PIC X(6).
           05                          PIC X(5) VALUE "GROSS".
           05  FILLER                  PIC X(3).
           05                          PIC X(9) VALUE "SALES TAX".
           05  FILLER                  PIC X(3).
           05                          PIC X(5) VALUE "DIS %".
           05  FILLER                  PIC X(4).
           05                          PIC X(7) VALUE "DIS AMT".
           05  FILLER                  PIC X(5).
           05                          PIC X(9) VALUE "NET PRICE".
           05  FILLER                  PIC X(6).
           05                          PIC X(7) VALUE "MESSAGE".


       PROCEDURE DIVISION.
       000-MAINLINE.
           OPEN INPUT TRANSACTION-FILE
                OUTPUT PRINT-FILE
                I-O MASTER-FILE

           PERFORM 900-HEADINGS
           MOVE 'NO' TO END-DATA

           PERFORM UNTIL END-DATA = 'YES'
               READ TRANSACTION-FILE INTO TR-TRANS-REC
                   AT END
                       MOVE 'YES' TO END-DATA
                   NOT AT END
                       PERFORM 200-PROCESSING
           END-PERFORM
           CLOSE MASTER-FILE
                 TRANSACTION-FILE
                 PRINT-FILE
           STOP RUN.


       200-PROCESSING.
           MOVE TR-PROD-NUM TO MR-NUM
           READ MASTER-FILE INTO IR-INPUT-RECORD
           IF FILE-STATUS = "00"
             DISPLAY FILE-STATUS, "ERROR"
             PERFORM 210-MATCH
           ELSE
             DISPLAY FILE-STATUS, "ERROR"
           END-IF.

       210-MATCH.
           INITIALIZE DL-DETAIL-LINE
           INITIALIZE TC-CALS
           MOVE TR-CUS-NAME TO DL-CUS-NAME
           MOVE MR-NUM TO DL-PROD-NUM
           MOVE IR-PROD-DES TO DL-PROD-DES
           MOVE TR-QTY-PURCHASED TO DL-QTY-PUR
           MOVE IR-UNIT-PRICE  TO DL-UNIT-PRICE

           IF TR-QTY-PURCHASED >= IR-MIN-ORDER
               IF TR-QTY-PURCHASED > IR-QTY-DIS-LEVEL
                   MULTIPLY TR-QTY-PURCHASED BY IR-UNIT-PRICE
                        GIVING TC-GROSS
                   MULTIPLY IR-DIS-PERCENT BY 100
                       GIVING TC-DIS-PERCENT
                   MULTIPLY TC-GROSS BY IR-DIS-PERCENT
                       GIVING TC-DISCOUNT-AMT
                   COMPUTE TC-GROSS-TAXED = (TC-GROSS - TC-DISCOUNT-AMT)
                       * 0.07
                   COMPUTE TC-SALES-AMT = TC-GROSS * .07

                   COMPUTE TC-NET-PRICE = TC-GROSS - TC-DISCOUNT-AMT
                           + TC-GROSS-TAXED
                   MOVE TC-GROSS TO DL-GROSS-PRICE
                   MOVE TC-DIS-PERCENT TO DL-DIS-PERCENT
                   MOVE TC-DISCOUNT-AMT TO DL-DIS-AMT
                   MOVE TC-NET-PRICE   TO DL-NET-PRICE
                   MOVE TC-SALES-AMT TO DL-SALES-TAX
                   MOVE SPACES TO DL-MESSAGE
                END-IF
           ELSE
               MOVE ALL '*' TO DL-UNIT-PRICE-X
               MOVE ALL '*' TO DL-NET-PRICE-X
               MOVE "BELOW MIN ORDER" TO DL-MESSAGE
           END-IF

           WRITE PRINT-RECORD FROM DL-DETAIL-LINE
           AFTER 1 LINE
           ADD TR-QTY-PURCHASED TO IR-TOTAL
           REWRITE MASTER-RECORD FROM IR-INPUT-RECORD
               INVALID KEY
                   MOVE "REWRITE NOT SUCESSFUL" TO DL-MESSAGE
           END-REWRITE.

       900-HEADINGS.
           WRITE PRINT-RECORD FROM CL-COL-HEADINGS
           AFTER ADVANCING 1 LINE.
