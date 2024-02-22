       IDENTIFICATION DIVISION.
      ******************************************************
      *  PROGRAM NAME: ATCDEMO  Version TDM                *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL WITH A MUILTIPLE *
      *       COMPILE UNIT TEST CASE                       *
      *                                                    *
      ******************************************************
       PROGRAM-ID.             ATCDEMO.
       AUTHOR.                 TIM MAGEE.
           DATE-WRITTEN.       03/22/02.
           DATE-COMPILED.      CURRENT-DATE.
           INSTALLATION.       IBM LEXINGTON.
           REMARKS.
              PURPOSE.
              THIS PROGRAM IS DEFINED TO TEST A NUMBER OF THE
              APPLICATION TESTING COLLECTION AND DEBUG TOOL FUNCTONS
              IT WILL READ AN INPUT FILE, PROCESS EACH RECORD WITH
              OUTPUT BEING GENERATED BY THE "DISPLAY" STATEMENT.
              INPUT FILE:  QSAMIN
      *    SKIP3
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-370.
       OBJECT-COMPUTER.        IBM-370.
      *    EJECT
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT QSAMIN
                       ASSIGN TO   INPUT1
                       FILE STATUS IS QSAMIN-STATUS.
      *    SKIP2
      *    EJECT
       DATA DIVISION.
       FILE SECTION.

      **************************************************************
      *  FILE DEFINITION BLOCK                                     *
      **************************************************************
       FD  QSAMIN
           RECORDING MODE IS F
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORDS ARE STANDARD.
       01  INPUT-RECORD                PIC X(80).

      *    EJECT
       WORKING-STORAGE SECTION.

      **************************************************************
      *  DATE SYSTEM DATE (FOUR DIGIT YEAR)                        *
      **************************************************************
       01  MY-DATE.
           05  DATE-YEAR               PIC 9(4).
           05  DATE-MONTH              PIC 9(2).
           05  DATE-DAY                PIC 9(2).

      **************************************************************
      *  CURRENT-DATE SYSTEM DATE (TWO DIGIT YEAR)                 *
      **************************************************************
       01  CURRENT-DATE.
           05  CURRENT-YEAR            PIC 9(2).
           05  CURRENT-MONTH           PIC 9(2).
           05  CURRENT-DAY             PIC 9(2).

       01  TEST-DATE.
           05 TODAY-DATE  PIC X(8)  VALUE SPACES.
      *************************************************************
      *  SYSTEM TIME                                              *
      *************************************************************
       01  CURRENT-TIME.
           05  CURRENT-HOUR            PIC 9(2).
           05  CURRENT-MINUTE          PIC 9(2).
           05  CURRENT-SECOND          PIC 9(2).
           05  CURRENT-HNDSEC          PIC 9(2).

      *************************************************************
      *  I/O BUFFER                                               *
      *************************************************************
       01  WS-INPUT-RECORD.
           05  WS-REC-TYPE             PIC X(2).
           05  WS-FILLER               PIC X(78).

       COPY RECBUF1.
       COPY RECBUF2.

      ************************************************************
      *  PARM BUFFER PASSED TO ATCDEM5                           *
      ************************************************************
        01 PARM-VALS.
           05  DEMO-TYPE               PIC X(2).
           05  RECORD-TYPE1-YEAR       PIC 9(2).

      ************************************************************
      *  DATE SWITCH                                             *
      ************************************************************
       01  DATE-SW                     PIC X    VALUE 'N'.

       01  TEST-BUF                    PIC X(196608) VALUE SPACES.

       01  QSAMIN-STATUS               PIC X(2) VALUE SPACES.
       01  RF-STATUS                   PIC X(2) VALUE SPACES.

       01  SW-SWITCHES-AREA.
           05 SW-EOF-QSAMIN            PIC X    VALUE 'N'.

       01  WS-COUNTERS.
           05  WS-COUNT-101            PIC 9(6) VALUE 0.
           05  WS-COUNT-102            PIC 9(6) VALUE 0.
           05  WS-COUNT-103            PIC 9(6) VALUE 0.
           05  WS-COUNT-104            PIC 9(6) VALUE 0.
           05  WS-COUNT-105            PIC 9(6) VALUE 0.
           05  WS-COUNT-106            PIC 9(6) VALUE 0.
           05  WS-COUNT-107            PIC 9(6) VALUE 0.
           05  WS-COUNT-108            PIC 9(6) VALUE 0.
           05  WS-COUNT-109            PIC 9(6) VALUE 0.
           05  WS-COUNT-110            PIC 9(6) VALUE 0.
           05  WS-COUNT-111            PIC 9(6) VALUE 0.
           05  WS-COUNT-112            PIC 9(6) VALUE 0.
           05  WS-COUNT-113            PIC 9(6) VALUE 0.
           05  WS-COUNT-114            PIC 9(6) VALUE 0.
           05  WS-COUNT-115            PIC 9(6) VALUE 0.
           05  WS-COUNT-116            PIC 9(6) VALUE 0.
           05  WS-COUNT-117            PIC 9(6) VALUE 0.

      *    SKIP2
      *    EJECT
      *    SKIP2
       01  XX-WORKING-STORAGE-END      PIC X(50)        VALUE
               '************END  WORKING STORAGE *****************'.

      *    EJECT

      **********************************************************
      *  LINKAGE FOR PASSED PROGRAM PARAMITER                  *
      **********************************************************
       LINKAGE SECTION.
        01  PD-TOOL-DEMO.
           05  PARM-LENGTH             PIC 9(2).
           05  PD-TOOL-DEMO-TYPE       PIC X(2).

      **********************************************************
      *  PROCEDURE DIVISON FOR MAIN PROGRAM                    *
      **********************************************************
       PROCEDURE DIVISION USING PD-TOOL-DEMO.

       PG000-MAIN-LOGIC.

      **********************************************************
      *  GET THE SYSTEM DATE AND TIME                          *
      **********************************************************
           ACCEPT MY-DATE FROM DATE YYYYMMDD.
           ACCEPT CURRENT-DATE FROM DATE.
           ACCEPT CURRENT-TIME FROM TIME.

      **********************************************************
      *  OUTPUT THE SYSTM DATE AND TIME                        *
      **********************************************************
           DISPLAY 'ATCDEMO RUNTIME DATE = ' CURRENT-MONTH ':'
                             CURRENT-DAY ':' CURRENT-YEAR.

           DISPLAY 'ATCDEMO RUNTIME TIME = ' CURRENT-HOUR ':'
                             CURRENT-MINUTE ':' CURRENT-SECOND.

      **********************************************************
      *  OPEN THE INPUT FILE                                   *
      **********************************************************
           MOVE 0000 TO RETURN-CODE.
           OPEN INPUT QSAMIN.
           DISPLAY 'FILE-STATUS ON QSAMIN OPEN = ' QSAMIN-STATUS.

      **********************************************************
      *  READ AND PROCESS EACH INPUT FILE RECORD               *
      **********************************************************
           PERFORM 900-READ-QSAMIN.
           PERFORM 100-PROCESS-INPUT-FILE
              THRU 100-EXIT
                 UNTIL SW-EOF-QSAMIN = 'Y'.
           PERFORM 200-PRINT-REPORT.

      **********************************************************
      *  CLOSE THE INPUT FILE                                  *
      **********************************************************
       800-CLOSE.
           CLOSE QSAMIN.
           PERFORM 999-STOP-RUN.

     *     EJECT

      **********************************************************
      *  THE FOLLOWING IS THE PROCESSING LOGIC FOR EACH INPUT  *
      *  RECORD.                                               *
      **********************************************************
       100-PROCESS-INPUT-FILE.

      **********************************************************
      *  MOVE THE I/O BUFFER TO THE CORRECT RECORD BUFFER      *
      **********************************************************
           IF WS-REC-TYPE = '01'
               MOVE WS-INPUT-RECORD TO WS-INPUT-RECORD-1
               PERFORM PROCESS-REC-01
             ELSE
               MOVE WS-INPUT-RECORD TO WS-INPUT-RECORD-2
               PERFORM PROCESS-REC-02
           END-IF.

       PROCESS-REC-01.
      ********************************************************
      *  MOVE DEMO TYPE AND RECORD YEAR INTO PARM BUFFER     *
      ********************************************************
           MOVE PD-TOOL-DEMO-TYPE TO DEMO-TYPE.
           MOVE RECORD-YEAR1 TO RECORD-TYPE1-YEAR.

      ********************************************************
      *  COMPARE THE SYSTEM DATE WITH THE INPUT RECORD DATE  *
      ********************************************************
           IF CURRENT-DATE = RECORD-DATE1
               ADD 10 TO RECORD-DAY1
             ELSE
               ADD 5 TO RECORD-DAY1
           END-IF.

      **********************************************************
      *  COMPARE THE SYSTEM DATE IS LESS THAN THE RECORD DATE  *
      **********************************************************
           IF CURRENT-DATE LESS THAN RECORD-DATE1
              CALL 'ATCDEM2'
              ADD 10 TO RECORD-DAY1
             ELSE
              ADD 10 TO RECORD-YEAR1
           END-IF.

      **********************************************************
      *  COMPARE THE ELEMENTS OF THE SYSTEM DATE WITH THE      *
      *  ELEMENTS OF THE RECORD DATE                           *
      **********************************************************
           IF CURRENT-DAY = RECORD-DAY1
              CALL 'ATCDEM2'
             ELSE
              CALL 'ATCDEM5' USING PARM-VALS
           END-IF.

           IF CURRENT-MONTH = RECORD-MONTH1
              CALL 'ATCDEM4'
             ELSE
              CALL 'ATCDEM5' USING PARM-VALS
           END-IF.

      **********************************************************
      *  THIS BLOCK WILL NEVER BE EXECUTED, IT CONTAINS DATE   *
      *  IMPACTED LOGIC.                                       *
      **********************************************************
           IF DATE-SW = 'Y'
               MOVE RECORD-YEAR1 TO CURRENT-YEAR
               MOVE RECORD-DATE1 TO CURRENT-DATE
           END-IF.

      **********************************************************
      *  COMPARE THE SYSTEM DATE TO THE RECORD DATE            *
      *  IMPACTED LOGIC.                                       *
      **********************************************************
           IF CURRENT-YEAR LESS THAN RECORD-YEAR1
              DISPLAY 'YEAR = ' CURRENT-YEAR ' < ' RECORD-YEAR1
           ELSE
              DISPLAY 'YEAR = ' RECORD-YEAR1 ' < ' CURRENT-YEAR
           END-IF.

      **********************************************************
      *  CHECK EACH RECORD KEY VALUE AND SET THE COUNTERS      *
      **********************************************************
           IF WS-KEY-CASE1 = '4a' PERFORM 101-CASE
           ELSE
           IF WS-KEY-CASE1 = '7n' PERFORM 102-CASE
           ELSE
           IF WS-KEY-CASE1 = '6r' PERFORM 103-CASE
           ELSE
           IF WS-KEY-CASE1 = '2o' PERFORM 104-CASE
           ELSE
           IF WS-KEY-CASE1 = '3m' PERFORM 105-CASE
           ELSE
           IF WS-KEY-CASE1 = '9a' PERFORM 106-CASE
           ELSE
           IF WS-KEY-CASE1 = '1z' PERFORM 107-CASE
           ELSE
           IF WS-KEY-CASE1 = '5i' PERFORM 108-CASE
           ELSE
           IF WS-KEY-CASE1 = '8l' PERFORM 109-CASE
           ELSE
           IF WS-KEY-CASE1 = '4b' PERFORM 110-CASE
           ELSE
           IF WS-KEY-CASE1 = '6x' PERFORM 111-CASE
           ELSE
           IF WS-KEY-CASE1 = '2j' PERFORM 112-CASE
           ELSE
           IF WS-KEY-CASE1 = '8f' PERFORM 113-CASE
           ELSE
           IF WS-KEY-CASE1 = '9v' PERFORM 114-CASE
           ELSE
           IF WS-KEY-CASE1 = '4w' PERFORM 115-CASE
           ELSE
           IF WS-KEY-CASE1 = '5p' PERFORM 116-CASE
           ELSE
           IF WS-KEY-CASE1 = '7q' PERFORM 117-CASE.

           PERFORM 900-READ-QSAMIN.
       100-EXIT. EXIT.
     *     EJECT

      ********************************************************
      *  THE FOLLOWING WILL INCREMENT THE COUNTERS DEPENDING *
      *  ON THE SELECTED KEY VALUES                          *
      ********************************************************
       101-CASE.
           ADD 1 TO WS-COUNT-101.

       102-CASE.
           ADD 1 TO WS-COUNT-102.

       103-CASE.
           ADD 1 TO WS-COUNT-103.

       104-CASE.
           ADD 1 TO WS-COUNT-104.

       105-CASE.
           ADD 1 TO WS-COUNT-105.

       106-CASE.
           ADD 1 TO WS-COUNT-106.

       107-CASE.
           ADD 1 TO WS-COUNT-107.

       108-CASE.
           ADD 1 TO WS-COUNT-108.

       109-CASE.
           ADD 1 TO WS-COUNT-109.

       110-CASE.
           ADD 1 TO WS-COUNT-110.

       111-CASE.
           ADD 1 TO WS-COUNT-111.

       112-CASE.
           ADD 1 TO WS-COUNT-112.

       113-CASE.
           ADD 1 TO WS-COUNT-113.

       114-CASE.
           ADD 1 TO WS-COUNT-114.

       115-CASE.
           ADD 1 TO WS-COUNT-115.

       116-CASE.
           ADD 1 TO WS-COUNT-116.

       117-CASE.
           ADD 1 TO WS-COUNT-117.

       PROCESS-REC-02.
      ********************************************************
      *  BYPASS ALL RECORD TYPE 02                           *
      ********************************************************
           DISPLAY 'REC TYPE 02 DROPPED, KEY = ' WS-KEY-CASE2.

      *************************************************************
      *    200-PRINT-REPORT WILL DISPLAY TOTAL KEY COUNTS ONLY    *
      *************************************************************
       200-PRINT-REPORT.
           DISPLAY '*************************************'.
           DISPLAY '*          KEY CASE COUNTERS        *'.
           DISPLAY '*************************************'.
           DISPLAY 'KEY = 4a:' WS-COUNT-101.
           DISPLAY 'KEY = 7n:' WS-COUNT-102.
           DISPLAY 'KEY = 6r:' WS-COUNT-103.
           DISPLAY 'KEY = 2o:' WS-COUNT-104.
           DISPLAY 'KEY = 3m:' WS-COUNT-105.
           DISPLAY 'KEY = 9s:' WS-COUNT-106.
           DISPLAY 'KEY = 1z:' WS-COUNT-107.
           DISPLAY 'KEY = 5i:' WS-COUNT-108.
           DISPLAY 'KEY = 8l:' WS-COUNT-109.
           DISPLAY 'KEY = 4b:' WS-COUNT-110.
           DISPLAY 'KEY = 6x:' WS-COUNT-111.
           DISPLAY 'KEY = 2j:' WS-COUNT-112.
           DISPLAY 'KEY = 8f:' WS-COUNT-113.
           DISPLAY 'KEY = 9v:' WS-COUNT-114.
           DISPLAY 'KEY = 4w:' WS-COUNT-115.
           DISPLAY 'KEY = 5p:' WS-COUNT-116.
           DISPLAY 'KEY = 7q:' WS-COUNT-117.

      **********************************************************
      *  THE FOLLOWING IS THE FILE READ LOGIC                  *
      **********************************************************
       900-READ-QSAMIN.
           READ  QSAMIN  INTO  WS-INPUT-RECORD
               AT END  MOVE  'Y'  TO  SW-EOF-QSAMIN.
      *    DISPLAY 'FILE STATUS ON QSAMIN READ = ' QSAMIN-STATUS.

       999-STOP-RUN.
           STOP RUN.

      **********************************************************
      *  THE FOLLOWING IS THE FILE READ LOGIC                  *
      **********************************************************

       100-AM-DEMO.
      **********************************************************
      *  IBM APPLICATION MONITOR DEMO                          *
      **********************************************************
           IF WS-REC-TYPE = '01'
               MOVE WS-INPUT-RECORD TO WS-INPUT-RECORD-1
               PERFORM PROCESS-REC-01
             ELSE
               MOVE WS-INPUT-RECORD TO WS-INPUT-RECORD-2
               PERFORM PROCESS-REC-02
           END-IF.

      *    EJECT
      **********************************************************
       FILE-SET.
      **********************************************************
      *  IBM APPLICATION MONITOR DEMO                          *
      **********************************************************
           IF CURRENT-YEAR = '06'
             ADD 1 TO DATE-YEAR
             ADD 1 TO CURRENT-YEAR
           END-IF.
      *    EJECT