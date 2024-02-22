       IDENTIFICATION DIVISION.
       PROGRAM-ID. TDM01B.
      ******************************************************
      *  PROGRAM NAME: TDM01B                              *
      *                                                    *
      *  PROGRAMMER: TIMOTHY DAVID MAGEE                   *
      *              LEXINGTON, KENTUCKY                   *
      *                                                    *
      *  DATE: 02/28/2005                                  *
      *                                                    *
      *  OBJECTIVES OF TESTCASE:                           *
      *                                                    *
      *       DEMO INTERACTIVE DEBUG TOOL WITH A MUILTIPLE *
      *       COMPILE OS/VS COBOL TEST CASE                *
      *                                                    *
      *  FUNCTION OF THIS COMPILE UNIT:                    *
      *                                                    *
      *       THIS CU IS CALLED BY THE MAIN DRIVER AND     *
      *       IT WILL UPDATE A NUMBER OF DATA VARIABLES    *
      *       AND CALL SECONDARY CU, TDM01C                *
      ******************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.

      ******************************************************
      * WORKING STORAGE                                    *
      ******************************************************
       WORKING-STORAGE SECTION.
       01  DTPARM1      PIC 99 VALUE 5.
       01  DTPARM2      PIC 99 VALUE 0.
       01  TDM01C       PIC X(6) VALUE 'TDM01C'.
       01  P1PARM1      PIC 99 VALUE 0.
       01  CALL-COUNT   PIC 99 VALUE 99.

       PROCEDURE DIVISION.

      ******************************************************
      * MAIN LINE PROGRAM                                  *
      ******************************************************
       PROGB.
           PERFORM LOOP1 UNTIL DTPARM1 = 0

           IF DTPARM2 = 0  THEN
              PERFORM PROCB
           OTHERWISE
              SUBTRACT 1 FROM CALL-COUNT.


      ******************************************************
      * THIS CONDITION WILL NEVER BE TRUE AND NEVER EXCUTE *
      ******************************************************
           IF  DTPARM2 = 2  THEN
              SUBTRACT 1 FROM DTPARM2.
           GOBACK
           .

      ******************************************************
      * PROCB: INTERNAL PROCEEDURE                         *
      ******************************************************
       PROCB.
           MOVE 10 TO P1PARM1
           .
       LOOP1.
           IF DTPARM1 > 0 THEN
              SUBTRACT 1 FROM DTPARM1.
           CALL 'TDM01C'
           .

       THE-END.
             STOP RUN.