      ****************************************************************
      * PROGRAM:  SAMII1
      *           Sample program for the VS COBOL II Compiler
      *
      * AUTHOR :  Doug Stout
      *           IBM PD TOOLS
      *
      * READS A SEQUENTIAL FILE AND WRITES A REPORT
      * PROCESSING IS CONTROLLED BY A TRANSACTION FILE
      *
      * THIS EXAMPLE APPLICATION IS A TEACHING AID.  INTENDED USES ARE:
      *   FOR DEBUG TOOL WORKSHOP:
      *      - DETERMINE WHY MAX VALUE IS INCORRECT IN THE REPORT
      *      - INTERCEPT THE S0C7 ABEND THAT CAN OCCUR IN PROGRAM SAMII2
      *   FOR FAULT ANALYZER WORKSHOP:
      *      - DETERMINE WHY THE SAMII2 PROGRAM ABENDS IN SOME CASES
      *   FOR APPLICATION PERFORMANCE ANALYZER WORKSHOP:
      *      - DETERMINE WHERE THE PROGRAM IS SPENDING THE MOST TIME
      *****************************************************************
      *
      * Transaction file record descriptions:
      *     0    1    1    2    2    3    3    4    4    5    5    6    6
      * ....5....0....5....0....5....0....5....0....5....0....5....0....5
      * *        <== an asterisk in first column is a comment
      * PRINT    <== produces a detail report
      * TOTALS   <== produces a summary report
      * ABEND    <== force a divide by zero abend
      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMII1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CUSTOMER-FILE ASSIGN TO CUSTFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-CUSTFILE-STATUS.

            SELECT TRANSACTION-FILE ASSIGN TO TRANFILE
               ACCESS IS SEQUENTIAL
               FILE STATUS  IS  WS-TRANFILE-STATUS.

            SELECT REPORT-FILE      ASSIGN TO CUSTRPT
               FILE STATUS  IS  WS-REPORT-STATUS.

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.

       FD  CUSTOMER-FILE
           RECORDING MODE IS F.
       COPY CUST2COB REPLACING ==:TAG:== BY ==CUST==.

       FD  TRANSACTION-FILE
           RECORDING MODE IS F.
       COPY TRANRCOB.

       FD  REPORT-FILE
           RECORDING MODE IS F.
       01  REPORT-RECORD              PIC X(132).

      *****************************************************************
       WORKING-STORAGE SECTION.
      *****************************************************************
      *
       01  SYSTEM-DATE-AND-TIME.
           05  CURRENT-DATE.
               10  CURRENT-YEAR            PIC 9(2).
               10  CURRENT-MONTH           PIC 9(2).
               10  CURRENT-DAY             PIC 9(2).
           05  CURRENT-TIME.
               10  CURRENT-HOUR            PIC 9(2).
               10  CURRENT-MINUTE          PIC 9(2).
               10  CURRENT-SECOND          PIC 9(2).
               10  CURRENT-HNDSEC          PIC 9(2).
      *
       01  WS-FIELDS.
           05  WS-CUSTFILE-STATUS      PIC X(2)  VALUE SPACES.
           05  WS-TRANFILE-STATUS      PIC X(2)  VALUE SPACES.
           05  WS-REPORT-STATUS        PIC X(2)  VALUE SPACES.
           05  WS-TRAN-FILE-EOF        PIC X     VALUE SPACES.
           05  WS-TRAN-OK              PIC X     VALUE 'N'.
           05  WS-CUST-FILE-EOF        PIC X     VALUE 'N'.
           05  WS-TRAN-MSG             PIC X(50) VALUE SPACES.
      *
       01  WORK-VARIABLES.
           05  RESULT                PIC S9(7)   COMP-3  VALUE +0.
           05  NUM-A                 PIC S9(7)   COMP-3.
           05  NUM-B                 PIC S9(7)   COMP-3.
      *
       01  TOTALS-VARS.
           05  NUM-TRANFILE-RECS     PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TRAN-ERRORS       PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TRANSACTIONS      PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-CUSTFILE-RECS     PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-CUSTOMER-RECS     PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-PRODUCT-RECS      PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-DETAIL-LINES      PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-PRINT-REQUESTS    PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-PRINT-COMPLETED   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TOTALS-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-TOTALS-COMPLETED  PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-ABEND-REQUESTS    PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-ABEND-COMPLETED   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-CRUNCH-REQUESTS   PIC S9(9)   COMP-3  VALUE +0.
           05  NUM-CRUNCH-COMPLETED  PIC S9(9)   COMP-3  VALUE +0.
      *
       01  CUSTOMER-BALANCE-STATS.
           05  BALANCE-COUNT         PIC S9(7)V99 COMP-3 VALUE +0.
           05  BALANCE-TOT           PIC S9(7)V99 COMP-3 VALUE +0.
           05  BALANCE-MIN           PIC S9(7)V99 COMP-3 VALUE +0.
           05  BALANCE-MAX           PIC S9(7)V99 COMP-3 VALUE +0.
           05  BALANCE-RANGE         PIC S9(7)V99 COMP-3 VALUE +0.
           05  BALANCE-AVG           PIC S9(7)V99 COMP-3 VALUE +0.
      *
       01  PRODUCT-STATS.
           05  SERV-CALLS-COUNT      PIC S9(7)    COMP-3 VALUE +0.
           05  SERV-CALLS-TOTAL      PIC S9(7)    COMP-3 VALUE +0.
           05  SERV-CALLS-MIN        PIC S9(7)    COMP-3 VALUE +0.
           05  SERV-CALLS-MAX        PIC S9(7)    COMP-3 VALUE +0.
           05  SERV-CALLS-RANGE      PIC S9(7)    COMP-3 VALUE +0.
           05  SERV-CALLS-AVG        PIC S9(7)V99 COMP-3 VALUE +0.
      *
      *        *******************
      *            report lines
      *        *******************
       01 RPT-HEADER1.
           05  FILLER                     PIC X(40)
                     VALUE 'CUSTOMER FILE REPORT              DATE: '.
           05  RPT-MM                     PIC 99.
           05  FILLER                     PIC X     VALUE '/'.
           05  RPT-DD                     PIC 99.
           05  FILLER                     PIC X     VALUE '/'.
           05  RPT-YY                     PIC 99.
           05  FILLER                     PIC X(20)
                          VALUE ' (mm/dd/yy)   TIME: '.
           05  RPT-HH                     PIC 99.
           05  FILLER                     PIC X     VALUE ':'.
           05  RPT-MIN                    PIC 99.
           05  FILLER                     PIC X     VALUE ':'.
           05  RPT-SS                     PIC 99.
           05  FILLER                     PIC X(55) VALUE SPACES.
       01 RPT-HEADER2.
           05  FILLER PIC X(5)  VALUE 'ID   '.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(17) VALUE 'CUSTOMER NAME    '.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(30) VALUE 'OCCUPATION                    '.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(10) VALUE '   BALANCE'.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(10) VALUE 'ORDERS-YTD'.
           05  FILLER PIC X(56) VALUE SPACES.
       01 RPT-HEADER3.
           05  FILLER PIC X(5)  VALUE ALL '-'.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(17) VALUE ALL '-'.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(30) VALUE ALL '-'.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(10) VALUE ALL '-'.
           05  FILLER PIC X     VALUE ' '.
           05  FILLER PIC X(10) VALUE ALL '-'.
           05  FILLER PIC X(56) VALUE SPACES.
       01 RPT-DETAIL.
           05  RPT-CUST-ID            PIC X(5).
           05  FILLER                 PIC X     VALUE ' '.
           05  RPT-CUST-NAME          PIC X(17) VALUE SPACES.
           05  FILLER                 PIC X     VALUE ' '.
           05  RPT-CUST-OCCUPATION    PIC X(30) VALUE SPACES.
           05  FILLER                 PIC X     VALUE ' '.
           05  RPT-CUST-ACCT-BALANCE  PIC ZZZ,ZZ9.99.
           05  FILLER                 PIC X     VALUE ' '.
           05  RPT-CUST-ORDERS-YTD    PIC ZZ,ZZZ,ZZ9.
           05  FILLER                 PIC X(56) VALUE SPACES.
       01  RPT-TRAN-DETAIL.
           05  RPT-TRAN-MSG1      PIC X(31)
                        VALUE ' Transaction:                 '.
           05  RPT-TRAN-RECORD            PIC X(80)  VALUE SPACES.
           05  FILLER                     PIC X(21)  VALUE SPACES.
       01  ERR-MSG-BAD-TRAN.
           05  FILLER PIC X(31)
                        VALUE '    Transaction Error:        '.
           05  ERR-MSG-DATA1              PIC X(35)  VALUE SPACES.
           05  ERR-MSG-DATA2              PIC X(66)  VALUE SPACES.
       01  RPT-TOTALS-HDR1.
           05  FILLER PIC X(26) VALUE 'TOTALS REPORT             '.
           05  FILLER PIC X(107) VALUE SPACES.
       01  RPT-TOTALS-HDR2.
           05  FILLER PIC X(100)  VALUE ALL '-'.
           05  FILLER PIC X(32) VALUE SPACES.
       01  RPT-TOTALS-DETAIL.
           05  FILLER              PIC XX       VALUE SPACES.
           05  RPT-TOTALS-TYPE     PIC X(15).
           05  FILLER              PIC X(4)     VALUE ':   '.
           05  RPT-TOTALS-ITEM1    PIC X(11).
           05  RPT-TOTALS-VALUE1   PIC ZZZ,ZZZ,ZZ9.
           05  RPT-TOTALS-VALUE1D  REDEFINES RPT-TOTALS-VALUE1
                                   PIC ZZZZZZZZ.99.
           05  FILLER              PIC X(3)     VALUE SPACES.
           05  RPT-TOTALS-ITEM2    PIC X(11).
           05  RPT-TOTALS-VALUE2   PIC ZZZ,ZZZ,ZZ9.
           05  RPT-TOTALS-VALUE2D  REDEFINES RPT-TOTALS-VALUE2
                                   PIC ZZZZZZZZ.99.
           05  FILLER              PIC X(3)     VALUE SPACES.
           05  RPT-TOTALS-ITEM3    PIC X(11).
           05  RPT-TOTALS-VALUE3   PIC ZZZ,ZZZ,ZZ9.
           05  RPT-TOTALS-VALUE3D  REDEFINES RPT-TOTALS-VALUE3
                                   PIC ZZZZZZZZ.99.
           05  FILLER              PIC X(36)    VALUE SPACES.
       01 RPT-ABEND-TRAN.
           05  FILLER PIC X(30) VALUE ' ABEND TRANSACTION PROCESSED.'.
           05  FILLER PIC X(102)  VALUE SPACES.
       01  RPT-SPACES.
           05  FILLER              PIC X(132)   VALUE SPACES.

      *****************************************************************
       PROCEDURE DIVISION.
      *****************************************************************

       000-MAIN.
      *    ACCEPT CURRENT-DATE FROM DATE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE(9:14) TO CURRENT-TIME.
      *    ACCEPT CURRENT-TIME FROM TIME.
           DISPLAY 'SAMII1 STARTED DATE = ' CURRENT-MONTH '/'
                  CURRENT-DAY '/' CURRENT-YEAR '  (mm/dd/yy)'.
           DISPLAY '             TIME = ' CURRENT-HOUR ':'
                  CURRENT-MINUTE ':' CURRENT-SECOND.

           PERFORM 900-OPEN-TRAN-AND-RPT-FILES.
           PERFORM 800-INIT-REPORT .

           PERFORM 100-PROCESS-TRANSACTIONS
                   UNTIL WS-TRAN-FILE-EOF = 'Y' .

           PERFORM 905-CLOSE-TRAN-AND-RPT-FILES.

           GOBACK .

       100-PROCESS-TRANSACTIONS.
           PERFORM 700-READ-TRAN-FILE.
           IF WS-TRAN-FILE-EOF NOT = 'Y'
             IF TRAN-COMMENT NOT = '*'
               WRITE REPORT-RECORD FROM RPT-SPACES AFTER 1
               MOVE TRAN-RECORD TO RPT-TRAN-RECORD
               WRITE REPORT-RECORD FROM RPT-TRAN-DETAIL
               MOVE 'Y' TO WS-TRAN-OK
               EVALUATE TRAN-CODE
                  WHEN 'PRINT '
                      PERFORM 200-PROCESS-PRINT-TRAN
                  WHEN 'TOTALS'
                      PERFORM 300-PROCESS-TOTALS-TRAN
                  WHEN 'ABEND '
                      PERFORM 400-PROCESS-ABEND-TRAN
                  WHEN OTHER
                      MOVE 'INVALID TRAN CODE:' TO ERR-MSG-DATA1
                      MOVE TRAN-CODE TO ERR-MSG-DATA2
                      PERFORM 820-REPORT-BAD-TRAN
                      ADD +1 TO NUM-TRANSACTIONS
               END-EVALUATE
             END-IF
           END-IF .

       200-PROCESS-PRINT-TRAN.
           ADD +1 TO NUM-PRINT-REQUESTS.
           ADD +1 TO NUM-TRANSACTIONS.
           WRITE REPORT-RECORD FROM RPT-SPACES  AFTER 1.
           WRITE REPORT-RECORD FROM RPT-HEADER2.
           WRITE REPORT-RECORD FROM RPT-HEADER3.
           PERFORM 910-OPEN-CUST-FILE.
           PERFORM 210-PROCESS-CUSTFILE-RECORD
               WITH TEST BEFORE
               UNTIL WS-CUST-FILE-EOF = 'Y'.
           IF WS-TRAN-OK = 'Y'
                   ADD +1 TO NUM-PRINT-COMPLETED.
           PERFORM 915-CLOSE-CUST-FILE.

       210-PROCESS-CUSTFILE-RECORD.
           PERFORM 730-READ-CUSTOMER-FILE.
           IF WS-CUST-FILE-EOF NOT = 'Y'
             IF CUST-RECORD-TYPE = 'C'
               ADD +1 TO NUM-CUSTOMER-RECS
      *        SUBROUTINE SAMII2 WILL COLLECT CUSTOMER STATISTICS
               CALL 'SAMII2' USING CUST-REC,
                       CUSTOMER-BALANCE-STATS
               MOVE CUST-ID           TO RPT-CUST-ID
               MOVE CUST-NAME         TO RPT-CUST-NAME
               MOVE CUST-OCCUPATION   TO RPT-CUST-OCCUPATION
               MOVE CUST-ACCT-BALANCE TO RPT-CUST-ACCT-BALANCE
               MOVE CUST-ORDERS-YTD   TO RPT-CUST-ORDERS-YTD
               WRITE REPORT-RECORD FROM RPT-DETAIL AFTER 1
               ADD +1 TO NUM-DETAIL-LINES
             END-IF
             IF CUST-RECORD-TYPE = 'P'
               ADD +1 TO NUM-PRODUCT-RECS
      *        SUBROUTINE SAMII3 WILL COLLECT PRODUCT STATISTICS
               CALL 'SAMII3' USING CUST-REC,
                       PRODUCT-STATS
             END-IF
           END-IF .

       300-PROCESS-TOTALS-TRAN.
           ADD +1 TO NUM-TOTALS-REQUESTS .
           ADD +1 TO NUM-TRANSACTIONS.
           WRITE REPORT-RECORD FROM RPT-SPACES      AFTER 1.
           WRITE REPORT-RECORD FROM RPT-TOTALS-HDR1.
           WRITE REPORT-RECORD FROM RPT-TOTALS-HDR2.
           IF NUM-PRINT-COMPLETED > 0
               MOVE SPACES            TO RPT-TOTALS-DETAIL
               MOVE 'Acct Balance:  ' TO RPT-TOTALS-TYPE
               MOVE '     Total:'     TO RPT-TOTALS-ITEM1
               MOVE BALANCE-TOT       TO RPT-TOTALS-VALUE1D
               MOVE '       Max:'     TO RPT-TOTALS-ITEM2
               MOVE BALANCE-MAX       TO RPT-TOTALS-VALUE2D
               MOVE '   Average:'     TO RPT-TOTALS-ITEM3
               MOVE BALANCE-AVG       TO RPT-TOTALS-VALUE3D
               WRITE REPORT-RECORD FROM RPT-TOTALS-DETAIL
      *
               MOVE SPACES            TO RPT-TOTALS-DETAIL
               MOVE 'Cust Records:  ' TO RPT-TOTALS-TYPE
               MOVE '      Read:'     TO RPT-TOTALS-ITEM1
               MOVE NUM-CUSTFILE-RECS TO RPT-TOTALS-VALUE1
               WRITE REPORT-RECORD FROM RPT-TOTALS-DETAIL
      *
               ADD +1 TO NUM-TOTALS-COMPLETED
           ELSE
               MOVE 'CANNOT COMPLETE TOTALS TRAN.' TO ERR-MSG-DATA1
               MOVE 'A PRINT tran must be requested/processed first.'
                   TO ERR-MSG-DATA2
               PERFORM 820-REPORT-BAD-TRAN
           END-IF .
      *
           MOVE SPACES            TO RPT-TOTALS-DETAIL.
           MOVE 'Transactions:  ' TO RPT-TOTALS-TYPE.
           MOVE '     Count:'     TO RPT-TOTALS-ITEM1.
           MOVE NUM-TRANSACTIONS  TO RPT-TOTALS-VALUE1.
           MOVE ' Processed:'     TO RPT-TOTALS-ITEM2.
           COMPUTE RPT-TOTALS-VALUE2 =
               NUM-TRANSACTIONS - NUM-TRAN-ERRORS.
           MOVE '    Errors:'     TO RPT-TOTALS-ITEM3.
           MOVE NUM-TRAN-ERRORS   TO RPT-TOTALS-VALUE3.
           WRITE REPORT-RECORD FROM RPT-TOTALS-DETAIL.

       400-PROCESS-ABEND-TRAN.
           ADD +1 TO NUM-ABEND-REQUESTS .
           ADD +1 TO NUM-TRANSACTIONS.
           MOVE +999 TO RESULT.
           MOVE +100 TO NUM-A.
           MOVE +0   TO NUM-B.
           COMPUTE RESULT =
               NUM-A / NUM-B.
           ADD +1 TO NUM-ABEND-COMPLETED.
           WRITE REPORT-RECORD FROM RPT-ABEND-TRAN  AFTER 2.
           WRITE REPORT-RECORD FROM RPT-SPACES.

       700-READ-TRAN-FILE.
           READ TRANSACTION-FILE
             AT END MOVE 'Y' TO WS-TRAN-FILE-EOF .
           EVALUATE      WS-TRANFILE-STATUS
              WHEN '00'
                  COMPUTE NUM-TRANFILE-RECS = NUM-TRANFILE-RECS + 1
                  CONTINUE
              WHEN '10'
                  MOVE 'Y' TO WS-TRAN-FILE-EOF
              WHEN OTHER
                  MOVE 'Error on tran file read.  Code:'
                              TO ERR-MSG-DATA1
                  MOVE WS-CUSTFILE-STATUS TO ERR-MSG-DATA2
                  PERFORM 820-REPORT-BAD-TRAN
                  MOVE 'Y' TO WS-TRAN-FILE-EOF
           END-EVALUATE .

       730-READ-CUSTOMER-FILE.
           READ CUSTOMER-FILE
             AT END MOVE 'Y' TO WS-CUST-FILE-EOF .
           EVALUATE WS-CUSTFILE-STATUS
              WHEN '00'
              WHEN '04'
                  ADD +1 TO NUM-CUSTFILE-RECS
                  CONTINUE
              WHEN '10'
                  MOVE 'Y' TO WS-CUST-FILE-EOF
              WHEN OTHER
                  MOVE 'Y' TO WS-CUST-FILE-EOF
                  MOVE 'Customer input File I/O Error on Read.  RC: '
                              TO ERR-MSG-DATA1
                  MOVE WS-CUSTFILE-STATUS TO ERR-MSG-DATA2
                  PERFORM 820-REPORT-BAD-TRAN
           END-EVALUATE .

       800-INIT-REPORT.
           MOVE CURRENT-YEAR   TO RPT-YY.
           MOVE CURRENT-MONTH  TO RPT-MM.
           MOVE CURRENT-DAY    TO RPT-DD.
           MOVE CURRENT-HOUR   TO RPT-HH.
           MOVE CURRENT-MINUTE TO RPT-MIN.
           MOVE CURRENT-SECOND TO RPT-SS.
           WRITE REPORT-RECORD FROM RPT-HEADER1 AFTER PAGE.

       820-REPORT-BAD-TRAN.
           ADD +1 TO NUM-TRAN-ERRORS.
           MOVE 'N' TO WS-TRAN-OK.
           WRITE REPORT-RECORD FROM ERR-MSG-BAD-TRAN.
           WRITE REPORT-RECORD FROM RPT-SPACES.

       900-OPEN-TRAN-AND-RPT-FILES.
           OPEN INPUT    TRANSACTION-FILE
                OUTPUT   REPORT-FILE .
           IF WS-TRANFILE-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING TRAN FILE. RC:' WS-TRANFILE-STATUS
             DISPLAY 'ENDING PROGRAM DUE TO FILE ERROR'
             MOVE 16 TO RETURN-CODE
             MOVE 'Y' TO WS-TRAN-FILE-EOF
           END-IF .
           IF WS-REPORT-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING REPORT FILE. RC:' WS-REPORT-STATUS
             DISPLAY 'ENDING PROGRAM DUE TO FILE ERROR'
             MOVE 16 TO RETURN-CODE
             MOVE 'Y' TO WS-TRAN-FILE-EOF
           END-IF .

       905-CLOSE-TRAN-AND-RPT-FILES.
           CLOSE TRANSACTION-FILE .
           CLOSE REPORT-FILE .

       910-OPEN-CUST-FILE.
           OPEN INPUT    CUSTOMER-FILE .
           IF WS-CUSTFILE-STATUS NOT = '00'
             DISPLAY 'ERROR OPENING CUSTOMER INPUT FILE. RC:'
                     WS-CUSTFILE-STATUS
             DISPLAY 'ENDING PROGRAM DUE TO FILE ERROR'
             MOVE 16 TO RETURN-CODE
             MOVE 'Y' TO WS-TRAN-FILE-EOF
           END-IF .

       915-CLOSE-CUST-FILE.
           CLOSE CUSTOMER-FILE .

      * END OF PROGRAM SAMII1