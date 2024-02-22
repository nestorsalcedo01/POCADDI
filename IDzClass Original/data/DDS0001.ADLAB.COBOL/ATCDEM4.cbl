       IDENTIFICATION DIVISION.
      ******************************************************
      *  PROGRAM NAME: DTDEM4  VERSION TDM                 *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL                  *
      *                                                    *
      ******************************************************
       PROGRAM-ID.             ATCDEM4.
       AUTHOR.                 TIM MAGEE.
           DATE-WRITTEN.       03/22/02.
           DATE-COMPILED.      CURRENT-DATE.
           INSTALLATION.       IBM LEXINGTON.
           REMARKS.
              PURPOSE.
              THIS PROGRAM IS DEFINED TO TEST A NUMBER OF THE
              APPLICATION TESTING COLLECTION AND VA/COBOL TOOLS
      *    SKIP3
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.        IBM-370.
       OBJECT-COMPUTER.        IBM-370.
      *    EJECT

       DATA DIVISION.

       WORKING-STORAGE SECTION.
       01 TCPARM1      PIC 99 VALUE 5.
       01 TCPARM2      PIC 99 VALUE 2.
       01 ATCDEM5      PIC X(7) VALUE 'ATCDEM5'.
       01 P1PARM1      PIC 99 VALUE 0.

       PROCEDURE DIVISION.

       PROGC.
           PERFORM WITH TEST BEFORE UNTIL TCPARM1 = 5
      * THIS PERFORM NOT EXECUTED
             SUBTRACT 1 FROM TCPARM1
             CALL 'ATCDEM5'
           END-PERFORM

      * THIS IF EXECUTED
           IF TCPARM2 = 0
             PERFORM PROCC
           END-IF

      * THIS PERFORM EXECUTED
           PERFORM WITH TEST BEFORE UNTIL TCPARM2 = 0
             SUBTRACT 1 FROM TCPARM2
           END-PERFORM
           .

       PROCC.
           MOVE 10 TO P1PARM1
      * IF NEVER EXECUTED
           IF TCPARM2 = 99
             CALL 'ATCDEM5'
           END-IF
           .


