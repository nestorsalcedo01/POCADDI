       IDENTIFICATION DIVISION.                                         $CI01310
                                                                        $CI01240
      ******************************************************            $CI01120
      *  PROGRAM NAME: DTDEMO  Version TDM                 *            $CI01130
      *                                                    *            $CI01220
      *  OBJECTIVES OF TESTCASE:                           *            $CI01160
      *                                                    *            $CI01170
      *       DEMO INTERACTIVE DEBUG TOOL                  *            $CI01180
      *                                                    *            $CI01220
      ******************************************************            $CI01230
                                                                        $CI01240
       PROGRAM-ID. DTDEMO.                                              $CI01320
       ENVIRONMENT DIVISION.                                            $CI01330
       DATA DIVISION.                                                   $CI01340
       WORKING-STORAGE SECTION.                                         $CI01350
                                                                        $CI01420
       77  PROGRAM-STATUS  PIC X(20) VALUE 'DUMMY'.                     $CI01460
                                                                        $CI01470
       01  ACCUMS  COMP.
           04  ACCUM-A    PIC S9(4).
           04  ACCUM-B    PIC S9(4).

       01  TEST-GROUP.
           10  FIRST-FIELD   PIC 9(10) VALUE 1234567890.
           10  NEXT-FIELD    PIC X(10) VALUE 'ABCDEFGHIJ'.

       PROCEDURE DIVISION.
                                                                        $CI01630
           MOVE  'PROGRAM STARTING' TO PROGRAM-STATUS
           MOVE ZERO TO ACCUMS

           PERFORM  2  TIMES
               ADD  3  TO  ACCUM-A
           END-PERFORM

           PERFORM  ACCUM-A  TIMES
               ADD  4  TO  ACCUM-B
           END-PERFORM

           ADD  ACCUM-A  TO  ACCUM-B
           SUBTRACT 6 FROM  ACCUM-A
           DIVIDE ACCUM-A INTO  ACCUM-B
           ADD  4  TO  ACCUM-B
           STOP RUN.
       END PROGRAM DTDEMO.