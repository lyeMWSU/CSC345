      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM06.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE
           ASSIGN TO "ADV6MSTR.TXT"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT SORT-FILE
           ASSIGN TO "SORT-WORK".

           SELECT SORTED-FILE
           ASSIGN "MSTRISAM.TXT"
           ORGANIZATION IS INDEXED
           ACCESS MODE IS SEQUENTIAL
           RECORD KEY IS SR-ISAM-NUM
           FILE STATUS IS SORT-SWITCH.

           SELECT OUTPUT-FILE
           ASSIGN TO DISPLAY.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  IR-RECORD                   PIC X(80).

       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SR-NUM                  PIC X(3).
           05                          PIC X(35).

       FD  SORTED-FILE.
       01  SR-SORTED-RECORD.
           05  SR-ISAM-NUM             PIC XXX.
           05                          PIC X(35).

       FD  OUTPUT-FILE.
       01  PRINT-RECORD                PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-DATE.
           05  WS-YY                   PIC XX.
           05  WS-MO                   PIC XX.
           05  WS-DAY                  PIC XX.

       01  INPUT-RECORD.
           05  IR-PRO-NUM              PIC 999.
           05  IR-PRO-DES              PIC X(27).
           05  FILLER                  PIC X(10).
           05  IR-PRICE                PIC S999V99.
           05  FILLER                  PIC X(4).
           05  IR-MIN-ORD              PIC 999.
           05  IR-QTY-DIS              PIC 999.
           05  FILLER                  PIC X(5).
           05  IR-DIS-PER              PIC SV999.

      *OUTPUT...
       01  DL-DETAIL-LINE.
           05  DL-PRO-NUM              PIC XXX.
           05  DL-PRO-DES              PIC X(27).
           05  DL-PRICE                PIC S999V99.
           05  DL-MIN-ORD              PIC XXX.
           05  DL-QTY-DIS              PIC XXX.
           05  DL-DIS-PER              PIC SV999.
           05  DL-ZEROS                PIC 9(7).

       01  DR-RECORD.
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
           05  FILLER                  PIC X(5).
           05  HL-HEADING              PIC X(20) VALUE SPACES.
           05  FILLER                  PIC X(5).
           05                          PIC X(4) VALUE "PAGE".
           05  HL-PAGE-NUM             PIC Z9.

       01  SWITCHES.
           05  IR-SWITCH               PIC XXX.
           05  SORT-SWITCH             PIC XX.
           05  OR-SWITCH               PIC XXX.

       PROCEDURE DIVISION.
       000-MAINLINE.
           PERFORM 100-INITIALIZE
           SORT SORT-FILE
               ON ASCENDING KEY SR-NUM
      *>          USING   INPUT-FILE
      *>          GIVING  OUTPUT-FILE
               INPUT PROCEDURE IS 400-INPUT
               OUTPUT PROCEDURE IS 410-OUTPUT
           STOP RUN.
       100-INITIALIZE.
           MOVE "NO " TO IR-SWITCH
           MOVE "NO" TO OR-SWITCH
      *DATE
           ACCEPT WS-DATE FROM DATE
           MOVE WS-MO TO HL-MO
           MOVE WS-DAY TO HL-DAY
           MOVE WS-YY TO HL-YY
           MOVE SPACES TO PRINT-RECORD.

       200-PROCESSING.
           MOVE IR-PRO-NUM TO DL-PRO-NUM
           MOVE IR-PRO-DES TO DL-PRO-DES
           MOVE IR-PRICE TO DL-PRICE
           MOVE IR-MIN-ORD TO DL-MIN-ORD
           MOVE IR-QTY-DIS TO DL-QTY-DIS
           MOVE IR-DIS-PER TO DL-DIS-PER
           MOVE ZEROS TO DL-ZEROS

           MOVE DL-DETAIL-LINE TO DR-REC
           WRITE PRINT-RECORD FROM DR-RECORD AFTER PAGE.

           RELEASE SORT-RECORD FROM DL-DETAIL-LINE.

      *>      MOVE DL-DETAIL-LINE TO SORT-RECORD
      *>      RELEASE SORT-RECORD.

       210-PROCESSING.
           WRITE SR-SORTED-RECORD
           MOVE SORT-RECORD TO DR-REC

           IF SORT-SWITCH = "00"
               MOVE "WRITTEN" TO DR-MESSAGE
           ELSE
               IF SORT-SWITCH = "22"
                   MOVE "DUPLICATE" TO DR-MESSAGE
               ELSE
                   DISPLAY "FILE STATUS ERROR"
                   DISPLAY SORT-SWITCH
               END-IF
           END-IF

           WRITE PRINT-RECORD FROM DR-RECORD AFTER PAGE.

           RETURN SORT-FILE INTO SR-SORTED-RECORD
               AT END MOVE "YES" TO OR-SWITCH.

       400-INPUT.
           OPEN INPUT INPUT-FILE
                OUTPUT OUTPUT-FILE

           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           MOVE "INPUT PROCEDURE" TO HL-HEADING
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER 1

           PERFORM UNTIL IR-SWITCH = "YES"
               READ INPUT-FILE INTO INPUT-RECORD
                   AT END
                       MOVE "YES" TO IR-SWITCH
                   NOT AT END
                       PERFORM 200-PROCESSING
               END-READ
           END-PERFORM
           CLOSE INPUT-FILE.

       410-OUTPUT.
           ADD 1 TO PC-PAGE-NUM
           MOVE PC-PAGE-NUM TO HL-PAGE-NUM
           MOVE "OUTPUT PROCEDURE" TO HL-HEADING
           WRITE PRINT-RECORD FROM HL-HEADING-LINE AFTER 1

           OPEN OUTPUT SORTED-FILE
           MOVE "NO" TO OR-SWITCH
           MOVE ZEROS TO DL-ZEROS
           RETURN SORT-FILE INTO SR-SORTED-RECORD
               AT END
                   MOVE "YES" TO OR-SWITCH
           END-RETURN

           PERFORM 210-PROCESSING
               UNTIL OR-SWITCH = "YES"
           CLOSE SORTED-FILE
                 OUTPUT-FILE.
