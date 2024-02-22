000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. NACT04.
000300 INSTALLATION. IBM HURSLEY.
000310 DATE-WRITTEN. AUGUST 1999.
000320 DATE-COMPILED.
000330*
000341*-------------------------------------------------------------*
000342*                                                             *
000343*               @BANNER_START@                                *
000344*      nact04.cbl                                             *
000345*      (C) Copyright IBM Corp. 2000. All Rights Reserved.     *
000346*                                                             *
000347* Element of Designing and Programming CICS Applications book *
000348*               @BANNER_END@                                  *
000349*                                                             *
000350*-------------------------------------------------------------*
000351*
000352***************************************************************
000353*    DESCRIPTION
000360*
000361* This program provides the error and ABEND handling logic
000362* for CICS Application Design and Programming book sample
000363* application. It issues several CICS commands which are
000364* classified as being part of the System Programming Interface
000365* (SPI); as a result it needs to be transalated with the 'SP'
000366* option which can be specified as an input parameter to the
000367* translator (preferred) or specified in the source code by
000368* including a 'CBL XOPTS(SP)' statement beginning in column 8
000369* of the first line. Note that this program can have control
000370* control transferred to it (via a CICS XCTL command), called
000371* (via a CICS LINK or a COBOL Dynamic CALL) or entered as a
000380* result of CICS System Recovery processing. The program needs
000390* to distinguish between these different possibilities.
000392*
000393***************************************************************
000394*     AMENDMENT HISTORY
000395*
000396*      DATE         AUTHOR          DESCRIPTION
000397*
000398*
000399***************************************************************
000400*     FILES
000401*
000402***************************************************************
000403*     CICS RESOURCES
000404*
000405*     TD QUEUE - QUEUE(WS-LITS-ERROR-QUEUE)
000406*           WRITEQ TD
000408*
000409***************************************************************
000410*     UTILITIES
000411*
000412***************************************************************
000413*     COPYBOOKS
000414*
000415*     NACWLITS - Common working storage.
000416*     NACTSET  - The mapsets.
000424*     NACCERRH - The commarea layout of the data passed to
000425*                this program from the other programs in the
000426*                suite.
000427*
000430***************************************************************
001700*
001800 ENVIRONMENT DIVISION.
001900 CONFIGURATION SECTION.
002000 DATA DIVISION.
002010*
002100 WORKING-STORAGE SECTION.
002210*
002220*    Store eye catcher details to aid dump reading
002230*
002240 01  WS-DEBUG-DETAILS.
002250     05  FILLER              PIC X(32)
002260           VALUE 'NACT04-------WORKING STORAGE  '.
002270     05  DEBUG-EYE.
002280         10  DEBUG-TRANID    PIC X(4) VALUE SPACES.
002290         10  DEBUG-TERMID    PIC X(4) VALUE SPACES.
002291         10  DEBUG-TASKNO    PIC 9(7) VALUE ZERO.
002292     05  FILLER              PIC X    VALUE SPACE.
002293     05  DEBUG-COMMAREA-ADDR USAGE IS POINTER.
002296*
002300* The following fields contain data obtained from CICS. Some of
002400* these are used to determine the type of actions that the logic
002500* needs to perform under the various conditions. However,
002600* several are not actually used but are included to illustrate
002700* some additional data that may be useful in an ABEND/Error
002800* handling program.
002900*
003000 01  AA-ASSIGN-AREAS.
003100     05  AA-ABCODE.
003200         10  FILLER          PIC X(01) VALUE SPACES.
003300         10  AA-AC234        PIC X(03) VALUE SPACES.
003400             88  AA-DO-NOT-SEND        VALUE 'TNI'
003410                                             'TND'
003420                                             'KCT'.
003500             88  AA-TO-DISCONNECT      VALUE 'KCT'.
003600     05  AA-ABPROGRAM        PIC X(08) VALUE SPACES.
003700     05  AA-ASRAINTRPT.
003800         10  AA-AI-LTH       PIC S9(4) COMP VALUE ZERO.
003900         10  AA-AI-IC        PIC S9(4) COMP VALUE ZERO.
004000         10  FILLER REDEFINES AA-AI-IC.
004100             15  FILLER      PIC X(01).
004200             15  AA-AI-IC-LO PIC X(01).
004300         10  FILLER          PIC X(04) VALUE SPACES.
004400     05  AA-ASRAKEY          PIC S9(8) COMP VALUE ZERO.
004500     05  AA-ASRAPSW.
004600         10  FILLER          PIC X(04) VALUE SPACES.
004700         10  AA-ASRAPSW-NSI  PIC S9(8) COMP VALUE ZERO.
004800         10  AA-AP-NSI REDEFINES AA-ASRAPSW-NSI.
004900             15  AA-AP-HIGH  PIC X(01).
005000             15  FILLER      PIC X(03).
005100     05  AA-ASRAREGS.
005200         10  AA-REG          PIC X(04) OCCURS 16
005300                             INDEXED BY AA-REG-IX.
005400     05  AA-ASRASPC          PIC S9(8) COMP VALUE ZERO.
005500     05  AA-ASRASTG          PIC S9(8) COMP VALUE ZERO.
005600     05  AA-FCI              PIC X(01) VALUE SPACE.
005700         88  AA-TERM-STARTED VALUE X'01'.
005800     05  AA-INVOKINGPROG     PIC X(08) VALUE SPACES.
005900     05  AA-NETNAME          PIC X(08) VALUE SPACES.
006000     05  AA-PROGRAM          PIC X(08) VALUE SPACES.
006100     05  AA-RETURNPROG       PIC X(08) VALUE SPACES.
006200     05  AA-STARTCODE        PIC X(02) VALUE SPACES.
006300     05  AA-TERMCODE         PIC X(01) VALUE SPACES.
006400     05  AA-USERID           PIC X(08) VALUE SPACES.
006500     05  AA-USERNAME         PIC X(20) VALUE SPACES.
006600*
006700* For program interrupts and operating system ABENDs, the logic
006800* calculates the offset into the program where the error was
006900* encountered in order to expedite problem analysis. This can
007000* be used to relate the problem back to the source code of the
007100* program in error without needing to examine a dump. These
007200* fields are used in this calculation process.
007300*
007320 01  FILLER                  PIC X(36) VALUE
007330         '********  WF-WORK-FIELDS    ********'.
007340*
007400 01  WF-WORK-FIELDS.
007500     05  WF-ENTRY-POINT      PIC S9(8) COMP VALUE ZERO.
007600     05  FILLER REDEFINES WF-ENTRY-POINT.
007700         10  WF-EP-HIGH      PIC X(01).
007800         10  FILLER          PIC X(03).
007900     05  WF-LOAD-POINT       PIC S9(8) COMP VALUE ZERO.
008000     05  WF-LENGTH           PIC S9(8) COMP VALUE ZERO.
008100     05  WF-END-POINT        PIC S9(8) COMP VALUE ZERO.
008200     05  WF-AP-HALF          PIC S9(4) COMP VALUE ZERO.
008300     05  FILLER REDEFINES WF-AP-HALF.
008400         10  WF-AP-HIGH      PIC X(01).
008500         10  WF-AP-LOW       PIC X(01).
008600     05  WF-ABSTIME          PIC X(08) VALUE SPACES.
008610     05  WF-ERRH-NUMBER      PIC 9(04) VALUE ZERO.
008700*
008800* Various indices are required during message output building.
008900*
008920 01  FILLER                  PIC X(36) VALUE
008930         '********  WS-INDEXES        ********'.
008940*
009000 01  WS-INDEXES.
009100     05  WS-IX               PIC S9(4) COMP VALUE ZERO.
009200     05  WS-IH               PIC S9(4) COMP VALUE ZERO.
009300     05  WS-IL               PIC S9(4) COMP VALUE ZERO.
009400*
009500* A dump is produced in case further anlysis of the problem is
009600* required. The code used is based on the CICS or application
009700* problem encountered.
009800*
009900 01  DUMP-CODE.
010000     05  DUMP-CODE-PREFIX    PIC X(01) VALUE SPACES.
010100     05  DUMP-CODE-SUFFIX    PIC X(03) VALUE SPACES.
010200*
010300* The following areas are used to create the
010400* messages documenting the problem.
010500*
010520 01  FILLER                  PIC X(36) VALUE
010530         '********  MA-MESSAGE-AREA   ********'.
010540*
010600 01  MA-MESSAGE-AREA.
010700     05  MA-STD-INFO.
010800         10  MA-SI-ID        PIC X(03) VALUE SPACES.
010900         10  FILLER          PIC X(06) VALUE 'EH001'.
011100         10  MA-SI-DATE      PIC X(10) VALUE SPACES.
011200         10  FILLER          PIC X(01) VALUE SPACE.
011300         10  MA-SI-TIME      PIC X(08) VALUE SPACES.
011400         10  MA-SI-SCREEN.
011500             15  FILLER      PIC X(22) VALUE
011600                 ' Error in transaction'.
011700             15  MA-TRANSID  PIC X(04) VALUE SPACES.
011800             15  FILLER      PIC X(10) VALUE
011900                 ', program'.
012000             15  MA-ABPROGRAM PIC X(08) VALUE SPACES.
012100             15  FILLER      PIC X(01) VALUE '.'.
012300     05  MA-XTR-INFO.
012400         10  MA-XI-ID        PIC X(03) VALUE SPACES.
012500         10  FILLER          PIC X(06) VALUE 'EH002'.
012700         10  MA-XI-DATE      PIC X(10) VALUE SPACES.
012800         10  FILLER          PIC X(01) VALUE SPACE.
012900         10  MA-XI-TIME      PIC X(08) VALUE SPACES.
013000         10  MA-XI-SCREEN.
013100             15  FILLER      PIC X(09) VALUE ' Type is '.
013300             15  MA-XI-VAR   PIC X(70) VALUE SPACES.
013400     05  FILLER.
013500         10  MA-AB.
013600             15  MA-AB-TYPE  PIC X(05) VALUE SPACES.
013700             15  FILLER      PIC X(10) VALUE '. Code is '.
013900             15  MA-ABCODE   PIC X(04) VALUE SPACES.
014000             15  FILLER      PIC X(51) VALUE '.'.
014200         10  MA-TR.
014300             15  MA-TR-TYPE  PIC X(04) VALUE SPACES.
014400             15  FILLER      PIC X(14) VALUE
014500                 '. Response is '.
014600             15  MA-RESP     PIC X(12) VALUE SPACES.
014700             15  FILLER      PIC X(12) VALUE
014800                 ', Reason is '.
014900             15  MA-REAS     PIC X(04) VALUE SPACES.
015000             15  FILLER      PIC X(03) VALUE ' - '.
015200             15  MA-CMD      PIC X(20) VALUE SPACES.
015300             15  FILLER      PIC X(01) VALUE '.'.
015400*
015500     05  MA-ASRA-DATA.
015600         10  MA-AD-ID        PIC X(03) VALUE SPACES.
015700         10  FILLER          PIC X(06) VALUE 'EH003'.
015900         10  MA-AD-DATE      PIC X(10) VALUE SPACES.
016000         10  FILLER          PIC X(01) VALUE SPACE.
016100         10  MA-AD-TIME      PIC X(08) VALUE SPACES.
016200         10  MA-AD-SCREEN.
016300             15  FILLER      PIC X(22) VALUE
016400                 ' Problem is at offset'.
016500             15  MA-AD-OFF   PIC X(09) VALUE SPACES.
016600             15  FILLER      PIC X(12) VALUE
016700                 '. Exception'.
016800             15  MA-AD-IC    PIC 9(04) VALUE ZERO.
016900             15  FILLER      PIC X(03) VALUE ' - '.
017100             15  MA-AD-EXPLAIN
017200                             PIC X(21) VALUE SPACES.
017300*
017400* For program interrupts, the logic includes an analysis of
017500* the type of error and includes it in the output messages.
017600*
017700 01  EXCEPTION-EXPLANATIONS.
017800     05  FILLER              PIC X(21) VALUE
017900         'Operation'.
018000     05  FILLER              PIC X(21) VALUE
018100         'Privileged-operation'.
018200     05  FILLER              PIC X(21) VALUE
018300         'Execute'.
018400     05  FILLER              PIC X(21) VALUE
018500         'Protection'.
018600     05  FILLER              PIC X(21) VALUE
018700         'Addressing'.
018800     05  FILLER              PIC X(21) VALUE
018900         'Specification'.
019000     05  FILLER              PIC X(21) VALUE
019100         'Data'.
019200     05  FILLER              PIC X(21) VALUE
019300         'Fixed-point overflow'.
019400     05  FILLER              PIC X(21) VALUE
019500         'Fixed-point divide'.
019600     05  FILLER              PIC X(21) VALUE
019700         'Decimal-overflow'.
019800     05  FILLER              PIC X(21) VALUE
019900         'Decimal-divide'.
020000     05  FILLER              PIC X(21) VALUE
020100         'Exponent-overflow'.
020200     05  FILLER              PIC X(21) VALUE
020300         'Exponent-underflow'.
020400     05  FILLER              PIC X(21) VALUE
020500         'Significance'.
020600     05  FILLER              PIC X(21) VALUE
020700         'Floating-point divide'.
020800 01  FILLER REDEFINES EXCEPTION-EXPLANATIONS.
020900     05  EXC-EXPLAIN         PIC X(21) OCCURS 15.
021000*
021100* The offset calculated needs to be expressed in hexadecimal
021200* format in order to make it useful for relating back to source
021300* code. These definitions provide the necessary conversion table
021400* for that process.
021500*
021600 01  HEX-TABLE               PIC X(16) VALUE
021700     '0123456789ABCDEF'.
021800 01  FILLER REDEFINES HEX-TABLE.
021900     05  HEX-CHAR            PIC X(01) OCCURS 16.
022000*
022100* For unexpected exceptional condition handling, the output
022200* messages include an explanation of the problem in words
022300* rather than in codes wherever possible. These definitions
022400* provide the words for the codes which are converted. These
022500* are all of the exception codes that can occur through
022600* CICS/ESA V5.3 (The CICS COmponent of Transaction Server 1.3).
022700*
022800 01  RESPONSE-TABLE.
022900     05  RESP01              PIC X(12)     VALUE 'ERROR'.
023000     05  RESP02              PIC X(12)     VALUE 'RDATT'.
023100     05  RESP03              PIC X(12)     VALUE 'WRBRK'.
023200     05  RESP04              PIC X(12)     VALUE 'EOF'.
023300     05  RESP05              PIC X(12)     VALUE 'EODS'.
023400     05  RESP06              PIC X(12)     VALUE 'EOC'.
023500     05  RESP07              PIC X(12)     VALUE 'INBFMH'.
023600     05  RESP08              PIC X(12)     VALUE 'ENDINPT'.
023700     05  RESP09              PIC X(12)     VALUE 'NONVAL'.
023800     05  RESP10              PIC X(12)     VALUE 'NOSTART'.
023900     05  RESP11              PIC X(12)     VALUE 'TERMIDERR'.
024000     05  RESP12              PIC X(12)     VALUE 'FILENOTFOUND'.
024100     05  RESP13              PIC X(12)     VALUE 'NOTFND'.
024200     05  RESP14              PIC X(12)     VALUE 'DUPREC'.
024300     05  RESP15              PIC X(12)     VALUE 'DUPKEY'.
024400     05  RESP16              PIC X(12)     VALUE 'INVREQ'.
024500     05  RESP17              PIC X(12)     VALUE 'IOERR'.
024600     05  RESP18              PIC X(12)     VALUE 'NOSPACE'.
024700     05  RESP19              PIC X(12)     VALUE 'NOTOPEN'.
024800     05  RESP20              PIC X(12)     VALUE 'ENDFILE'.
024900     05  RESP21              PIC X(12)     VALUE 'ILLOGIC'.
025000     05  RESP22              PIC X(12)     VALUE 'LENGERR'.
025100     05  RESP23              PIC X(12)     VALUE 'QZERO'.
025200     05  RESP24              PIC X(12)     VALUE 'SIGNAL'.
025300     05  RESP25              PIC X(12)     VALUE 'QBUSY'.
025400     05  RESP26              PIC X(12)     VALUE 'ITEMERR'.
025500     05  RESP27              PIC X(12)     VALUE 'PGMIDERR'.
025600     05  RESP28              PIC X(12)     VALUE 'TRANSIDERR'.
025700     05  RESP29              PIC X(12)     VALUE 'ENDDATA'.
025800     05  RESP30              PIC X(12)     VALUE 'INVTSREQ'.
025900     05  RESP31              PIC X(12)     VALUE 'EXPIRED'.
026000     05  RESP32              PIC X(12)     VALUE 'RETPAGE'.
026100     05  RESP33              PIC X(12)     VALUE 'RTEFAIL'.
026200     05  RESP34              PIC X(12)     VALUE 'RTESOME'.
026300     05  RESP35              PIC X(12)     VALUE 'TSIOERR'.
026400     05  RESP36              PIC X(12)     VALUE 'MAPFAIL'.
026500     05  RESP37              PIC X(12)     VALUE 'INVERRTERM'.
026600     05  RESP38              PIC X(12)     VALUE 'INVMPSZ'.
026700     05  RESP39              PIC X(12)     VALUE 'IGREQID'.
026800     05  RESP40              PIC X(12)     VALUE 'OVERFLOW'.
026900     05  RESP41              PIC X(12)     VALUE 'INVLDC'.
027000     05  RESP42              PIC X(12)     VALUE 'NOSTG'.
027100     05  RESP43              PIC X(12)     VALUE 'JIDERR'.
027200     05  RESP44              PIC X(12)     VALUE 'QIDERR'.
027300     05  RESP45              PIC X(12)     VALUE 'NOJBUFSP'.
027400     05  RESP46              PIC X(12)     VALUE 'DSSTAT'.
027500     05  RESP47              PIC X(12)     VALUE 'SELNERR'.
027600     05  RESP48              PIC X(12)     VALUE 'FUNCERR'.
027700     05  RESP49              PIC X(12)     VALUE 'UNEXPIN'.
027800     05  RESP50              PIC X(12)     VALUE 'NOPASSBKRD'.
027900     05  RESP51              PIC X(12)     VALUE 'NOPASSBKWR'.
028000     05  RESP52              PIC X(12)     VALUE '*NOT VALID*'.
028100     05  RESP53              PIC X(12)     VALUE 'SYSIDERR'.
028200     05  RESP54              PIC X(12)     VALUE 'ISCINVREQ'.
028300     05  RESP55              PIC X(12)     VALUE 'ENQBUSY'.
028400     05  RESP56              PIC X(12)     VALUE 'ENVDEFERR'.
028500     05  RESP57              PIC X(12)     VALUE 'IGREQCD'.
028600     05  RESP58              PIC X(12)     VALUE 'SESSIONERR'.
028700     05  RESP59              PIC X(12)     VALUE 'SYSBUSY'.
028800     05  RESP60              PIC X(12)     VALUE 'SESSBUSY'.
028900     05  RESP61              PIC X(12)     VALUE 'NOTALLOC'.
029000     05  RESP62              PIC X(12)     VALUE 'CBIDERR'.
029100     05  RESP63              PIC X(12)     VALUE 'INVEXITREQ'.
029200     05  RESP64              PIC X(12)     VALUE 'INVPARTNSET'.
029300     05  RESP65              PIC X(12)     VALUE 'INVPARTN'.
029400     05  RESP66              PIC X(12)     VALUE 'PARTNFAIL'.
029500     05  RESP67              PIC X(12)     VALUE '*NOT VALID*'.
029600     05  RESP68              PIC X(12)     VALUE '*NOT VALID*'.
029700     05  RESP69              PIC X(12)     VALUE 'USERIDERR'.
029800     05  RESP70              PIC X(12)     VALUE 'NOTAUTH'.
029900     05  RESP71              PIC X(12)     VALUE 'VOLIDERR'.
030000     05  RESP72              PIC X(12)     VALUE 'SUPPRESSED'.
030100     05  RESP73              PIC X(12)     VALUE '*NOT VALID*'.
030200     05  RESP74              PIC X(12)     VALUE '*NOT VALID*'.
030300     05  RESP75              PIC X(12)     VALUE 'RESIDERR'.
030400     05  RESP76              PIC X(12)     VALUE '*NOT VALID*'.
030500     05  RESP77              PIC X(12)     VALUE '*NOT VALID*'.
030600     05  RESP78              PIC X(12)     VALUE '*NOT VALID*'.
030700     05  RESP79              PIC X(12)     VALUE '*NOT VALID*'.
030800     05  RESP80              PIC X(12)     VALUE 'NOSPOOL'.
030900     05  RESP81              PIC X(12)     VALUE 'TERMERR'.
031000     05  RESP82              PIC X(12)     VALUE 'ROLLEDBACK'.
031100     05  RESP83              PIC X(12)     VALUE 'END'.
031200     05  RESP84              PIC X(12)     VALUE 'DISABLED'.
031300     05  RESP85              PIC X(12)     VALUE 'ALLOCERR'.
031400     05  RESP86              PIC X(12)     VALUE 'STRELERR'.
031500     05  RESP87              PIC X(12)     VALUE 'OPENERR'.
031600     05  RESP88              PIC X(12)     VALUE 'SPOLBUSY'.
031700     05  RESP89              PIC X(12)     VALUE 'SPOLERR'.
031800     05  RESP90              PIC X(12)     VALUE 'NODEIDER'.
031900     05  RESP91              PIC X(12)     VALUE 'TASKIDERR'.
032000     05  RESP92              PIC X(12)     VALUE 'TCIDERR'.
032100     05  RESP93              PIC X(12)     VALUE 'DSNNOTFOUND'.
032200     05  RESP94              PIC X(12)     VALUE 'LOADING'.
032300     05  RESP95              PIC X(12)     VALUE 'MODELIDERR'.
032400     05  RESP96              PIC X(12)     VALUE 'OUTDESCRERR'.
032500     05  RESP97              PIC X(12)     VALUE 'PARTNERIDERR'.
032600     05  RESP98              PIC X(12)     VALUE 'PROFILEIDERR'.
032700     05  RESP99              PIC X(12)     VALUE 'NETNAMEIDERR'.
032800     05  RESP100             PIC X(12)     VALUE 'LOCKED'.
032900     05  RESP101             PIC X(12)     VALUE 'RECORDBUSY'.
033000 01  FILLER                  REDEFINES RESPONSE-TABLE.
033100     05  RESPONSE-VALUE      OCCURS 101
033200                             PIC X(12).
033210*
033300 01  FILLER.
033400     05  MAXIMUM-RESPONSE    PIC 9(4)      VALUE 0101.
033500     05  NOTPOSS-RESPONSE    PIC 9(4)      VALUE 0255.
033600     05  NOTPOSS-RESP-VAL    PIC X(12)     VALUE 'NOTPOSS'.
033700     05  BAD-RESPONSE-VALUE  PIC X(12)     VALUE '**INVALID**'.
033800*
033900* For unexpected exceptional condition handling, the output
034000* messages include an explanation of the problem in words
034100* rather than in codes wherever possible. These definitions
034200* provide the words for the functions which are converted.
034300* These are all of the function values that can occur through
034400* CICS/ESA V5.3 (The CICS COmponent of Transaction Server 1.3).
034500*
034600 01  VAL-COMMAND-VALUES.
034700     05  VAL-0202        PIC 9(5) VALUE 00514.
034800     05  VAL-0204        PIC 9(5) VALUE 00516.
034900     05  VAL-0206        PIC 9(5) VALUE 00518.
035000     05  VAL-0208        PIC 9(5) VALUE 00520.
035100     05  VAL-020A        PIC 9(5) VALUE 00522.
035200     05  VAL-020C        PIC 9(5) VALUE 00524.
035300     05  VAL-020E        PIC 9(5) VALUE 00526.
035400     05  VAL-0210        PIC 9(5) VALUE 00528.
035500     05  VAL-0402        PIC 9(5) VALUE 01026.
035600     05  VAL-0404        PIC 9(5) VALUE 01028.
035700     05  VAL-0406        PIC 9(5) VALUE 01030.
035800     05  VAL-0408        PIC 9(5) VALUE 01032.
035900     05  VAL-040A        PIC 9(5) VALUE 01034.
036000     05  VAL-040C        PIC 9(5) VALUE 01036.
036100     05  VAL-040E        PIC 9(5) VALUE 01038.
036200     05  VAL-0410        PIC 9(5) VALUE 01040.
036300     05  VAL-0412        PIC 9(5) VALUE 01042.
036400     05  VAL-0414        PIC 9(5) VALUE 01044.
036500     05  VAL-0416        PIC 9(5) VALUE 01046.
036600     05  VAL-0418        PIC 9(5) VALUE 01048.
036700     05  VAL-041A        PIC 9(5) VALUE 01050.
036800     05  VAL-041C        PIC 9(5) VALUE 01052.
036900     05  VAL-041E        PIC 9(5) VALUE 01054.
037000     05  VAL-0420        PIC 9(5) VALUE 01056.
037100     05  VAL-0422        PIC 9(5) VALUE 01058.
037200     05  VAL-0424        PIC 9(5) VALUE 01060.
037300     05  VAL-0426        PIC 9(5) VALUE 01062.
037400     05  VAL-0428        PIC 9(5) VALUE 01064.
037500     05  VAL-042A        PIC 9(5) VALUE 01066.
037600     05  VAL-042C        PIC 9(5) VALUE 01068.
037700     05  VAL-042E        PIC 9(5) VALUE 01070.
037800     05  VAL-0430        PIC 9(5) VALUE 01072.
037900     05  VAL-0432        PIC 9(5) VALUE 01074.
038000     05  VAL-0434        PIC 9(5) VALUE 01076.
038100     05  VAL-0436        PIC 9(5) VALUE 01078.
038200     05  VAL-0438        PIC 9(5) VALUE 01080.
038300     05  VAL-043A        PIC 9(5) VALUE 01082.
038400     05  VAL-043C        PIC 9(5) VALUE 01084.
038500     05  VAL-043E        PIC 9(5) VALUE 01086.
038600     05  VAL-0602        PIC 9(5) VALUE 01538.
038700     05  VAL-0604        PIC 9(5) VALUE 01540.
038800     05  VAL-0606        PIC 9(5) VALUE 01542.
038900     05  VAL-0608        PIC 9(5) VALUE 01544.
039000     05  VAL-060A        PIC 9(5) VALUE 01546.
039100     05  VAL-060C        PIC 9(5) VALUE 01548.
039200     05  VAL-060E        PIC 9(5) VALUE 01550.
039300     05  VAL-0610        PIC 9(5) VALUE 01552.
039400     05  VAL-0612        PIC 9(5) VALUE 01554.
039500     05  VAL-0614        PIC 9(5) VALUE 01556.
039600     05  VAL-0802        PIC 9(5) VALUE 02050.
039700     05  VAL-0804        PIC 9(5) VALUE 02052.
039800     05  VAL-0806        PIC 9(5) VALUE 02054.
039900     05  VAL-0A02        PIC 9(5) VALUE 02562.
040000     05  VAL-0A04        PIC 9(5) VALUE 02564.
040100     05  VAL-0A06        PIC 9(5) VALUE 02566.
040200     05  VAL-0C02        PIC 9(5) VALUE 03074.
040300     05  VAL-0C04        PIC 9(5) VALUE 03076.
040400     05  VAL-0E02        PIC 9(5) VALUE 03586.
040500     05  VAL-0E04        PIC 9(5) VALUE 03588.
040600     05  VAL-0E06        PIC 9(5) VALUE 03590.
040700     05  VAL-0E08        PIC 9(5) VALUE 03592.
040800     05  VAL-0E0A        PIC 9(5) VALUE 03594.
040900     05  VAL-0E0C        PIC 9(5) VALUE 03596.
041000     05  VAL-0E0E        PIC 9(5) VALUE 03598.
041100     05  VAL-1002        PIC 9(5) VALUE 04098.
041200     05  VAL-1004        PIC 9(5) VALUE 04100.
041300     05  VAL-1006        PIC 9(5) VALUE 04102.
041400     05  VAL-1008        PIC 9(5) VALUE 04104.
041500     05  VAL-100A        PIC 9(5) VALUE 04106.
041600     05  VAL-100C        PIC 9(5) VALUE 04108.
041700     05  VAL-1202        PIC 9(5) VALUE 04610.
041800     05  VAL-1204        PIC 9(5) VALUE 04612.
041900     05  VAL-1206        PIC 9(5) VALUE 04614.
042000     05  VAL-1208        PIC 9(5) VALUE 04616.
042100     05  VAL-1402        PIC 9(5) VALUE 05122.
042200     05  VAL-1404        PIC 9(5) VALUE 05124.
042300     05  VAL-1406        PIC 9(5) VALUE 05126.
042400     05  VAL-1408        PIC 9(5) VALUE 05128.
042500     05  VAL-1602        PIC 9(5) VALUE 05634.
042600     05  VAL-1604        PIC 9(5) VALUE 05636.
042700     05  VAL-1802        PIC 9(5) VALUE 06146.
042800     05  VAL-1804        PIC 9(5) VALUE 06148.
042900     05  VAL-1806        PIC 9(5) VALUE 06150.
043000     05  VAL-1808        PIC 9(5) VALUE 06152.
043100     05  VAL-180A        PIC 9(5) VALUE 06154.
043200     05  VAL-180C        PIC 9(5) VALUE 06156.
043300     05  VAL-180E        PIC 9(5) VALUE 06158.
043400     05  VAL-1810        PIC 9(5) VALUE 06160.
043500     05  VAL-1812        PIC 9(5) VALUE 06162.
043600     05  VAL-1C02        PIC 9(5) VALUE 07170.
043700     05  VAL-1E02        PIC 9(5) VALUE 07682.
043800     05  VAL-1E04        PIC 9(5) VALUE 07684.
043900     05  VAL-1E06        PIC 9(5) VALUE 07686.
044000     05  VAL-1E08        PIC 9(5) VALUE 07688.
044100     05  VAL-1E0A        PIC 9(5) VALUE 07690.
044200     05  VAL-1E0C        PIC 9(5) VALUE 07692.
044300     05  VAL-1E0E        PIC 9(5) VALUE 07694.
044400     05  VAL-1E10        PIC 9(5) VALUE 07696.
044500     05  VAL-1E12        PIC 9(5) VALUE 07698.
044600     05  VAL-1E14        PIC 9(5) VALUE 07700.
044700     05  VAL-2002        PIC 9(5) VALUE 08194.
044800     05  VAL-2202        PIC 9(5) VALUE 08706.
044900     05  VAL-2204        PIC 9(5) VALUE 08708.
045000     05  VAL-2206        PIC 9(5) VALUE 08710.
045100     05  VAL-3002        PIC 9(5) VALUE 12290.
045200     05  VAL-3004        PIC 9(5) VALUE 12292.
045300     05  VAL-3006        PIC 9(5) VALUE 12294.
045400     05  VAL-3008        PIC 9(5) VALUE 12296.
045500     05  VAL-300A        PIC 9(5) VALUE 12298.
045600     05  VAL-300C        PIC 9(5) VALUE 12300.
045700     05  VAL-300E        PIC 9(5) VALUE 12302.
045800     05  VAL-3010        PIC 9(5) VALUE 12304.
045900     05  VAL-3012        PIC 9(5) VALUE 12306.
046000     05  VAL-3014        PIC 9(5) VALUE 12308.
046100     05  VAL-3016        PIC 9(5) VALUE 12310.
046200     05  VAL-3018        PIC 9(5) VALUE 12312.
046300     05  VAL-301A        PIC 9(5) VALUE 12314.
046400     05  VAL-301C        PIC 9(5) VALUE 12316.
046500     05  VAL-301E        PIC 9(5) VALUE 12318.
046600     05  VAL-3020        PIC 9(5) VALUE 12320.
046700     05  VAL-3022        PIC 9(5) VALUE 12322.
046800     05  VAL-3024        PIC 9(5) VALUE 12324.
046900     05  VAL-3026        PIC 9(5) VALUE 12326.
047000     05  VAL-3028        PIC 9(5) VALUE 12328.
047100     05  VAL-302A        PIC 9(5) VALUE 12330.
047200     05  VAL-302C        PIC 9(5) VALUE 12332.
047300     05  VAL-302E        PIC 9(5) VALUE 12334.
047400     05  VAL-3030        PIC 9(5) VALUE 12336.
047500     05  VAL-3A02        PIC 9(5) VALUE 14850.
047600     05  VAL-4202        PIC 9(5) VALUE 16898.
047700     05  VAL-4210        PIC 9(5) VALUE 16912.
047800     05  VAL-4402        PIC 9(5) VALUE 17410.
047900     05  VAL-4410        PIC 9(5) VALUE 17424.
048000     05  VAL-4602        PIC 9(5) VALUE 17922.
048100     05  VAL-4610        PIC 9(5) VALUE 17936.
048200     05  VAL-4802        PIC 9(5) VALUE 18434.
048300     05  VAL-4804        PIC 9(5) VALUE 18436.
048400     05  VAL-4A02        PIC 9(5) VALUE 18946.
048500     05  VAL-4A04        PIC 9(5) VALUE 18948.
048600     05  VAL-4C02        PIC 9(5) VALUE 19458.
048700     05  VAL-4C04        PIC 9(5) VALUE 19460.
048800     05  VAL-4C10        PIC 9(5) VALUE 19472.
048900     05  VAL-4E02        PIC 9(5) VALUE 19970.
049000     05  VAL-4E04        PIC 9(5) VALUE 19972.
049100     05  VAL-4E10        PIC 9(5) VALUE 19984.
049200     05  VAL-5002        PIC 9(5) VALUE 20482.
049300     05  VAL-5004        PIC 9(5) VALUE 20484.
049400     05  VAL-5010        PIC 9(5) VALUE 20496.
049500     05  VAL-5202        PIC 9(5) VALUE 20994.
049600     05  VAL-5204        PIC 9(5) VALUE 20996.
049700     05  VAL-5206        PIC 9(5) VALUE 20998.
049800     05  VAL-5208        PIC 9(5) VALUE 21000.
049900     05  VAL-5210        PIC 9(5) VALUE 21008.
050000     05  VAL-5212        PIC 9(5) VALUE 21010.
050100     05  VAL-5214        PIC 9(5) VALUE 21012.
050200     05  VAL-5216        PIC 9(5) VALUE 21014.
050300     05  VAL-5402        PIC 9(5) VALUE 21506.
050400     05  VAL-5404        PIC 9(5) VALUE 21508.
050500     05  VAL-5602        PIC 9(5) VALUE 22018.
050600     05  VAL-5604        PIC 9(5) VALUE 22020.
050700     05  VAL-5606        PIC 9(5) VALUE 22022.
050800     05  VAL-5610        PIC 9(5) VALUE 22032.
050900     05  VAL-5802        PIC 9(5) VALUE 22530.
051000     05  VAL-5804        PIC 9(5) VALUE 22532.
051100     05  VAL-5806        PIC 9(5) VALUE 22534.
051200     05  VAL-5810        PIC 9(5) VALUE 22544.
051300     05  VAL-5A02        PIC 9(5) VALUE 23042.
051400     05  VAL-5A04        PIC 9(5) VALUE 23044.
051500     05  VAL-5C02        PIC 9(5) VALUE 23554.
051600     05  VAL-5C04        PIC 9(5) VALUE 23556.
051700     05  VAL-5C10        PIC 9(5) VALUE 23568.
051800     05  VAL-5E02        PIC 9(5) VALUE 24066.
051900     05  VAL-5E04        PIC 9(5) VALUE 24068.
052000     05  VAL-5E06        PIC 9(5) VALUE 24070.
052100     05  VAL-5E08        PIC 9(5) VALUE 24072.
052200     05  VAL-5E12        PIC 9(5) VALUE 24082.
052300     05  VAL-5E14        PIC 9(5) VALUE 24084.
052400     05  VAL-5E18        PIC 9(5) VALUE 24088.
052500     05  VAL-5E1A        PIC 9(5) VALUE 24090.
052600     05  VAL-5E1C        PIC 9(5) VALUE 24092.
052700     05  VAL-5E22        PIC 9(5) VALUE 24098.
052800     05  VAL-5E32        PIC 9(5) VALUE 24114.
052900     05  VAL-6002        PIC 9(5) VALUE 24578.
053000     05  VAL-6004        PIC 9(5) VALUE 24580.
053100     05  VAL-6010        PIC 9(5) VALUE 24592.
053200     05  VAL-6012        PIC 9(5) VALUE 24594.
053300     05  VAL-6014        PIC 9(5) VALUE 24596.
053400     05  VAL-6202        PIC 9(5) VALUE 25090.
053500     05  VAL-6204        PIC 9(5) VALUE 25092.
053600     05  VAL-6402        PIC 9(5) VALUE 25602.
053700     05  VAL-6602        PIC 9(5) VALUE 26114.
053800     05  VAL-6604        PIC 9(5) VALUE 26116.
053900     05  VAL-6612        PIC 9(5) VALUE 26130.
054000     05  VAL-6614        PIC 9(5) VALUE 26132.
054100     05  VAL-6622        PIC 9(5) VALUE 26146.
054200     05  VAL-6624        PIC 9(5) VALUE 26148.
054300     05  VAL-6802        PIC 9(5) VALUE 26626.
054400     05  VAL-6804        PIC 9(5) VALUE 26628.
054500     05  VAL-6812        PIC 9(5) VALUE 26642.
054600     05  VAL-6814        PIC 9(5) VALUE 26644.
054700     05  VAL-6822        PIC 9(5) VALUE 26658.
054800     05  VAL-6824        PIC 9(5) VALUE 26660.
054900     05  VAL-6826        PIC 9(5) VALUE 26662.
055000     05  VAL-6A02        PIC 9(5) VALUE 27138.
055100     05  VAL-6C02        PIC 9(5) VALUE 27650.
055200     05  VAL-6C12        PIC 9(5) VALUE 27666.
055300     05  VAL-6E02        PIC 9(5) VALUE 28162.
055400     05  VAL-6E04        PIC 9(5) VALUE 28164.
055500     05  VAL-7002        PIC 9(5) VALUE 28674.
055600     05  VAL-7004        PIC 9(5) VALUE 28676.
055700     05  VAL-7006        PIC 9(5) VALUE 28678.
055800     05  VAL-7008        PIC 9(5) VALUE 28680.
055900     05  VAL-7012        PIC 9(5) VALUE 28690.
056000     05  VAL-7014        PIC 9(5) VALUE 28692.
056100     05  VAL-7202        PIC 9(5) VALUE 29186.
056200     05  VAL-7402        PIC 9(5) VALUE 29698.
056300     05  VAL-7404        PIC 9(5) VALUE 29670.
056400     05  VAL-7406        PIC 9(5) VALUE 29672.
056500     05  VAL-7408        PIC 9(5) VALUE 29674.
056600     05  VAL-7602        PIC 9(5) VALUE 30210.
056700     05  VAL-7802        PIC 9(5) VALUE 30722.
056800     05  VAL-7804        PIC 9(5) VALUE 30724.
056900     05  VAL-7812        PIC 9(5) VALUE 30738.
057000     05  VAL-7814        PIC 9(5) VALUE 30740.
057100     05  VAL-7822        PIC 9(5) VALUE 30754.
057200     05  VAL-7824        PIC 9(5) VALUE 30756.
057300     05  VAL-7A02        PIC 9(5) VALUE 31234.
057400     05  VAL-7A04        PIC 9(5) VALUE 31236.
057500     05  VAL-7C02        PIC 9(5) VALUE 31746.
057600     05  VAL-7E02        PIC 9(5) VALUE 32258.
057700     05  VAL-7E04        PIC 9(5) VALUE 32260.
057800     05  VAL-8002        PIC 9(5) VALUE 32770.
057900     05  VAL-8004        PIC 9(5) VALUE 32772.
058000     05  VAL-8012        PIC 9(5) VALUE 32786.
058100     05  VAL-8014        PIC 9(5) VALUE 32788.
058200     05  VAL-801A        PIC 9(5) VALUE 32794.
058300     05  VAL-8022        PIC 9(5) VALUE 32802.
058400     05  VAL-8030        PIC 9(5) VALUE 32816.
058500     05  VAL-8602        PIC 9(5) VALUE 34306.
058600 01  FILLER              REDEFINES VAL-COMMAND-VALUES.
058700     05  COMMAND-VAL     PIC 9(5) OCCURS 239.
058800*
058810 01  FILLER.
058900     05  MAX-COMMANDS    PIC 9(4) COMP VALUE 239.
058910*
059000 01  NAME-COMMAND-NAMES.
059100     05  NAME-0202       PIC X(20) VALUE 'ADDRESS'.
059200     05  NAME-0204       PIC X(20) VALUE 'HANDLE CONDITION'.
059300     05  NAME-0206       PIC X(20) VALUE 'HANDLE AID'.
059400     05  NAME-0208       PIC X(20) VALUE 'ASSIGN'.
059500     05  NAME-020A       PIC X(20) VALUE 'IGNORE CONDITION'.
059600     05  NAME-020C       PIC X(20) VALUE 'PUSH'.
059700     05  NAME-020E       PIC X(20) VALUE 'POP'.
059800     05  NAME-0210       PIC X(20) VALUE 'ADDRESS SET'.
059900     05  NAME-0402       PIC X(20) VALUE 'RECEIVE'.
060000     05  NAME-0404       PIC X(20) VALUE 'SEND'.
060100     05  NAME-0406       PIC X(20) VALUE 'CONVERSE'.
060200     05  NAME-0408       PIC X(20) VALUE 'ISSUE EODS'.
060300     05  NAME-040A       PIC X(20) VALUE 'ISSUE COPY'.
060400     05  NAME-040C       PIC X(20) VALUE 'WAIT TERMINAL'.
060500     05  NAME-040E       PIC X(20) VALUE 'ISSUE LOAD'.
060600     05  NAME-0410       PIC X(20) VALUE 'WAIT SIGNAL'.
060700     05  NAME-0412       PIC X(20) VALUE 'ISSUE RESET'.
060800     05  NAME-0414       PIC X(20) VALUE 'ISSUE DISCONNECT'.
060900     05  NAME-0416       PIC X(20) VALUE 'ISSUE ENDOUTPUT'.
061000     05  NAME-0418       PIC X(20) VALUE 'ISSUE ERASEUP'.
061100     05  NAME-041A       PIC X(20) VALUE 'ISSUE ENDFILE'.
061200     05  NAME-041C       PIC X(20) VALUE 'ISSUE PRINT'.
061300     05  NAME-041E       PIC X(20) VALUE 'ISSUE SIGNAL'.
061400     05  NAME-0420       PIC X(20) VALUE 'ALLOCATE'.
061500     05  NAME-0422       PIC X(20) VALUE 'FREE'.
061600     05  NAME-0424       PIC X(20) VALUE 'POINT'.
061700     05  NAME-0426       PIC X(20) VALUE 'BUILD ATTACH'.
061800     05  NAME-0428       PIC X(20) VALUE 'EXTRACT ATTACH'.
061900     05  NAME-042A       PIC X(20) VALUE 'EXTRACT TCT'.
062000     05  NAME-042C       PIC X(20) VALUE 'WAIT CONVID'.
062100     05  NAME-042E       PIC X(20) VALUE 'EXTRACT PROCESS'.
062200     05  NAME-0430       PIC X(20) VALUE 'ISSUE ABEND'.
062300     05  NAME-0432       PIC X(20) VALUE 'CONNECT PROCESS'.
062400     05  NAME-0434       PIC X(20) VALUE 'ISSUE CONFIRMATION'.
062500     05  NAME-0436       PIC X(20) VALUE 'ISSUE ERROR'.
062600     05  NAME-0438       PIC X(20) VALUE 'ISSUE PREPARE'.
062700     05  NAME-043A       PIC X(20) VALUE 'ISSUE PASS'.
062800     05  NAME-043C       PIC X(20) VALUE 'EXTRACT LOGONMSG'.
062900     05  NAME-043E       PIC X(20) VALUE 'EXTRACT ATTRIBUTES'.
063000     05  NAME-0602       PIC X(20) VALUE 'READ'.
063100     05  NAME-0604       PIC X(20) VALUE 'WRITE'.
063200     05  NAME-0606       PIC X(20) VALUE 'REWRITE'.
063300     05  NAME-0608       PIC X(20) VALUE 'DELETE'.
063400     05  NAME-060A       PIC X(20) VALUE 'UNLOCK'.
063500     05  NAME-060C       PIC X(20) VALUE 'STARTBR'.
063600     05  NAME-060E       PIC X(20) VALUE 'READNEXT'.
063700     05  NAME-0610       PIC X(20) VALUE 'READPREV'.
063800     05  NAME-0612       PIC X(20) VALUE 'ENDBR'.
063900     05  NAME-0614       PIC X(20) VALUE 'RESETBR'.
064000     05  NAME-0802       PIC X(20) VALUE 'WRITEQ TD'.
064100     05  NAME-0804       PIC X(20) VALUE 'READQ TD'.
064200     05  NAME-0806       PIC X(20) VALUE 'DELETEQ TD'.
064300     05  NAME-0A02       PIC X(20) VALUE 'WRITEQ TS'.
064400     05  NAME-0A04       PIC X(20) VALUE 'READQ TS'.
064500     05  NAME-0A06       PIC X(20) VALUE 'DELETEQ TS'.
064600     05  NAME-0C02       PIC X(20) VALUE 'GETMAIN'.
064700     05  NAME-0C04       PIC X(20) VALUE 'FREEMAIN'.
064800     05  NAME-0E02       PIC X(20) VALUE 'LINK'.
064900     05  NAME-0E04       PIC X(20) VALUE 'XCTL'.
065000     05  NAME-0E06       PIC X(20) VALUE 'LOAD'.
065100     05  NAME-0E08       PIC X(20) VALUE 'RETURN'.
065200     05  NAME-0E0A       PIC X(20) VALUE 'RELEASE'.
065300     05  NAME-0E0C       PIC X(20) VALUE 'ABEND'.
065400     05  NAME-0E0E       PIC X(20) VALUE 'HANDLE ABEND'.
065500     05  NAME-1002       PIC X(20) VALUE 'ASKTIME'.
065600     05  NAME-1004       PIC X(20) VALUE 'DELAY'.
065700     05  NAME-1006       PIC X(20) VALUE 'POST'.
065800     05  NAME-1008       PIC X(20) VALUE 'START'.
065900     05  NAME-100A       PIC X(20) VALUE 'RETRIEVE'.
066000     05  NAME-100C       PIC X(20) VALUE 'CANCEL'.
066100     05  NAME-1202       PIC X(20) VALUE 'WAIT EVENT'.
066200     05  NAME-1204       PIC X(20) VALUE 'ENQ'.
066300     05  NAME-1206       PIC X(20) VALUE 'DEQ'.
066400     05  NAME-1208       PIC X(20) VALUE 'SUSPEND'.
066500     05  NAME-1402       PIC X(20) VALUE 'WRITE JOURNALNUM'.
066600     05  NAME-1404       PIC X(20) VALUE 'WAIT JOURNALNUM'.
066700     05  NAME-1406       PIC X(20) VALUE 'WRITE JOURNALNAME'.
066800     05  NAME-1408       PIC X(20) VALUE 'WAIT JOURNALNAME'.
066900     05  NAME-1602       PIC X(20) VALUE 'SYNCPOINT'.
067000     05  NAME-1604       PIC X(20) VALUE 'RESYNC ENTRYNAME'.
067100     05  NAME-1802       PIC X(20) VALUE 'RECEIVE MAP'.
067200     05  NAME-1804       PIC X(20) VALUE 'SEND MAP'.
067300     05  NAME-1806       PIC X(20) VALUE 'SEND TEXT'.
067400     05  NAME-1808       PIC X(20) VALUE 'SEND PAGE'.
067500     05  NAME-180A       PIC X(20) VALUE 'PURGE MESSAGE'.
067600     05  NAME-180C       PIC X(20) VALUE 'ROUTE'.
067700     05  NAME-180E       PIC X(20) VALUE 'RECEIVE PARTN'.
067800     05  NAME-1810       PIC X(20) VALUE 'SEND PARTNSET'.
067900     05  NAME-1812       PIC X(20) VALUE 'SEND CONTROL'.
068000     05  NAME-1C02       PIC X(20) VALUE 'DUMP'.
068100     05  NAME-1E02       PIC X(20) VALUE 'ISSUE ADD'.
068200     05  NAME-1E04       PIC X(20) VALUE 'ISSUE ERASE'.
068300     05  NAME-1E06       PIC X(20) VALUE 'ISSUE REPLACE'.
068400     05  NAME-1E08       PIC X(20) VALUE 'ISSUE ABORT'.
068500     05  NAME-1E0A       PIC X(20) VALUE 'ISSUE QUERY'.
068600     05  NAME-1E0C       PIC X(20) VALUE 'ISSUE END'.
068700     05  NAME-1E0E       PIC X(20) VALUE 'ISSUE RECEIVE'.
068800     05  NAME-1E10       PIC X(20) VALUE 'ISSUE NOTE'.
068900     05  NAME-1E12       PIC X(20) VALUE 'ISSUE WAIT'.
069000     05  NAME-1E14       PIC X(20) VALUE 'ISSUE SEND'.
069100     05  NAME-2002       PIC X(20) VALUE 'BIF DEEDIT'.
069200     05  NAME-2202       PIC X(20) VALUE 'ENABLE PROGRAM'.
069300     05  NAME-2204       PIC X(20) VALUE 'DISABLE PROGRAM'.
069400     05  NAME-2206       PIC X(20) VALUE 'EXTRACT EXIT'.
069500     05  NAME-3002       PIC X(20) VALUE 'CREATE PROGRAM'.
069600     05  NAME-3004       PIC X(20) VALUE 'CREATE MAPSET'.
069700     05  NAME-3006       PIC X(20) VALUE 'CREATE PARTITIONSET'.
069800     05  NAME-3008       PIC X(20) VALUE 'CREATE TRANSACTION'.
069900     05  NAME-300A       PIC X(20) VALUE 'CREATE PROFILE'.
070000     05  NAME-300C       PIC X(20) VALUE 'CREATE TYPETERM'.
070100     05  NAME-300E       PIC X(20) VALUE 'CREATE CONNECTION'.
070200     05  NAME-3010       PIC X(20) VALUE 'CREATE TERMINAL'.
070300     05  NAME-3012       PIC X(20) VALUE 'CREATE SESSIONS'.
070400     05  NAME-3014       PIC X(20) VALUE 'CREATE FILE'.
070500     05  NAME-3016       PIC X(20) VALUE 'CREATE LSRPOOL'.
070600     05  NAME-3018       PIC X(20) VALUE 'CREATE PARTNER'.
070700     05  NAME-301A       PIC X(20) VALUE 'CREATE TRANCLASS'.
070800     05  NAME-301C       PIC X(20) VALUE 'CREATE TDQUEUE'.
070900     05  NAME-301E       PIC X(20) VALUE 'CREATE JOURNALMODEL'.
071000     05  NAME-3020       PIC X(20) VALUE 'CREATE DB2CONN'.
071100     05  NAME-3022       PIC X(20) VALUE 'CREATE DB2ENTRY'.
071200     05  NAME-3024       PIC X(20) VALUE 'CREATE DB2TRAN'.
071300     05  NAME-3026       PIC X(20) VALUE 'CREATE PROCESSTYPE'.
071400     05  NAME-3028       PIC X(20) VALUE 'CREATE TSMODEL'.
071500     05  NAME-302A       PIC X(20) VALUE 'CREATE ENQMODEL'.
071600     05  NAME-302C       PIC X(20) VALUE 'CREATE REQUESTMODEL'.
071700     05  NAME-302E       PIC X(20) VALUE 'CREATE DOCTEMPLATE'.
071800     05  NAME-3030       PIC X(20) VALUE 'CREATE TCPIPSERVICE'.
071900     05  NAME-3A02       PIC X(20) VALUE 'INQUIRE RRMS'.
072000     05  NAME-4202       PIC X(20) VALUE 'INQUIRE AUTINSTMODEL'.
072100     05  NAME-4210       PIC X(20) VALUE 'DISCARD AUTINSTMODEL'.
072200     05  NAME-4402       PIC X(20) VALUE 'INQUIRE PARTNER'.
072300     05  NAME-4410       PIC X(20) VALUE 'DISCARD PARTNER'.
072400     05  NAME-4602       PIC X(20) VALUE 'INQUIRE PROFILE'.
072500     05  NAME-4610       PIC X(20) VALUE 'DISCARD PROFILE'.
072600     05  NAME-4802       PIC X(20) VALUE 'ENTER TRACENUM'.
072700     05  NAME-4804       PIC X(20) VALUE 'MONITOR POINT'.
072800     05  NAME-4A02       PIC X(20) VALUE 'ASKTIME ABSTIME'.
072900     05  NAME-4A04       PIC X(20) VALUE 'FORMATTIME'.
073000     05  NAME-4C02       PIC X(20) VALUE 'INQUIRE FILE'.
073100     05  NAME-4C04       PIC X(20) VALUE 'SET FILE'.
073200     05  NAME-4C10       PIC X(20) VALUE 'DISCARD FILE'.
073300     05  NAME-4E02       PIC X(20) VALUE 'INQUIRE PROGRAM'.
073400     05  NAME-4E04       PIC X(20) VALUE 'SET PROGRAM'.
073500     05  NAME-4E10       PIC X(20) VALUE 'DISCARD PROGRAM'.
073600     05  NAME-5002       PIC X(20) VALUE 'INQUIRE TRANSACTION'.
073700     05  NAME-5004       PIC X(20) VALUE 'SET TRANSACTION'.
073800     05  NAME-5010       PIC X(20) VALUE 'DISCARD TRANSACTION'.
073900     05  NAME-5202       PIC X(20) VALUE 'INQUIRE TERMINAL'.
074000     05  NAME-5204       PIC X(20) VALUE 'SET TERMINAL'.
074100     05  NAME-5206       PIC X(20) VALUE 'INQUIRE NETNAME'.
074200     05  NAME-5208       PIC X(20) VALUE 'SET NETNAME'.
074300     05  NAME-5210       PIC X(20) VALUE 'DISCARD TERMINAL'.
074400     05  NAME-5212       PIC X(20) VALUE 'INQUIRE TERMINAL'.
074500     05  NAME-5214       PIC X(20) VALUE 'SET TERMINAL'.
074600     05  NAME-5216       PIC X(20) VALUE 'INQUIRE NETNAME'.
074700     05  NAME-5402       PIC X(20) VALUE 'INQUIRE SYSTEM'.
074800     05  NAME-5404       PIC X(20) VALUE 'SET SYSTEM'.
074900     05  NAME-5602       PIC X(20) VALUE 'SPOOLOPEN'.
075000     05  NAME-5604       PIC X(20) VALUE 'SPOOLREAD'.
075100     05  NAME-5606       PIC X(20) VALUE 'SPOOLWRITE'.
075200     05  NAME-5610       PIC X(20) VALUE 'SPOOLCLOSE'.
075300     05  NAME-5802       PIC X(20) VALUE 'INQUIRE CONNECTION'.
075400     05  NAME-5804       PIC X(20) VALUE 'SET CONNECTION'.
075500     05  NAME-5806       PIC X(20) VALUE 'PERFORM ENDAFFINITY'.
075600     05  NAME-5810       PIC X(20) VALUE 'DISCARD CONNECTION'.
075700     05  NAME-5A02       PIC X(20) VALUE 'INQUIRE MODENAME'.
075800     05  NAME-5A04       PIC X(20) VALUE 'SET MODENAME'.
075900     05  NAME-5C02       PIC X(20) VALUE 'INQUIRE TDQUEUE'.
076000     05  NAME-5C04       PIC X(20) VALUE 'SET TDQUEUE'.
076100     05  NAME-5C10       PIC X(20) VALUE 'DISCARD TDQUEUE'.
076200     05  NAME-5E02       PIC X(20) VALUE 'INQUIRE TASK'.
076300     05  NAME-5E04       PIC X(20) VALUE 'SET TASK'.
076400     05  NAME-5E06       PIC X(20) VALUE 'CHANGE TASK'.
076500     05  NAME-5E08       PIC X(20) VALUE 'INQUIRE STORAGE'.
076600     05  NAME-5E12       PIC X(20) VALUE 'INQUIRE TCLASS'.
076700     05  NAME-5E14       PIC X(20) VALUE 'SET TCLASS'.
076800     05  NAME-5E18       PIC X(20) VALUE 'DISCARD TRANCLASS'.
076900     05  NAME-5E1A       PIC X(20) VALUE 'INQUIRE TRANCLASS'.
077000     05  NAME-5E1C       PIC X(20) VALUE 'SET TRANCLASS'.
077100     05  NAME-5E22       PIC X(20) VALUE 'WAIT EXTERNAL'.
077200     05  NAME-5E32       PIC X(20) VALUE 'WAITCICS'.
077300     05  NAME-6002       PIC X(20) VALUE 'INQUIRE JOURNALNUM'.
077400     05  NAME-6004       PIC X(20) VALUE 'SET JOURNALNUM'.
077500     05  NAME-6010       PIC X(20) VALUE 'DISCARD JOURNALNAME'.
077600     05  NAME-6012       PIC X(20) VALUE 'INQUIRE JOURNALNAME'.
077700     05  NAME-6014       PIC X(20) VALUE 'SET JOURNALNAME'.
077800     05  NAME-6202       PIC X(20) VALUE 'INQUIRE VOLUME'.
077900     05  NAME-6204       PIC X(20) VALUE 'SET VOLUME'.
078000     05  NAME-6402       PIC X(20) VALUE 'PERFORM SECURITY'.
078100     05  NAME-6602       PIC X(20) VALUE 'INQUIRE DUMPDS'.
078200     05  NAME-6604       PIC X(20) VALUE 'SET DUMPDS'.
078300     05  NAME-6612       PIC X(20) VALUE 'INQUIRE TRANDUMPCODE'.
078400     05  NAME-6614       PIC X(20) VALUE 'SET TRANDUMPCODE'.
078500     05  NAME-6622       PIC X(20) VALUE 'INQUIRE SYSDUMPCODE'.
078600     05  NAME-6624       PIC X(20) VALUE 'SET SYSDUMPCODE'.
078700     05  NAME-6802       PIC X(20) VALUE 'INQUIRE VTAM'.
078800     05  NAME-6804       PIC X(20) VALUE 'SET VTAM'.
078900     05  NAME-6812       PIC X(20) VALUE 'INQUIRE AUTOINSTALL'.
079000     05  NAME-6814       PIC X(20) VALUE 'SET AUTOINSTALL'.
079100     05  NAME-6822       PIC X(20) VALUE 'INQUIRE DELETSHIPPED'.
079200     05  NAME-6824       PIC X(20) VALUE 'SET DELETSHIPPED'.
079300     05  NAME-6826       PIC X(20) VALUE 'PERFORM DELETSHIPPED'.
079400     05  NAME-6A02       PIC X(20) VALUE 'QUERY SECURITY'.
079500     05  NAME-6C02       PIC X(20) VALUE 'WRITE OPERATOR'.
079600     05  NAME-6C12       PIC X(20) VALUE 'ISSUE DFHWTO'.
079700     05  NAME-6E02       PIC X(20) VALUE 'INQUIRE IRC'.
079800     05  NAME-6E04       PIC X(20) VALUE 'SET IRC'.
079900     05  NAME-7002       PIC X(20) VALUE 'INQUIRE STATISTICS'.
080000     05  NAME-7004       PIC X(20) VALUE 'SET STATISTICS'.
080100     05  NAME-7006       PIC X(20) VALUE 'PERFORM STATISTICS'.
080200     05  NAME-7008       PIC X(20) VALUE 'COLLECT STATISTICS'.
080300     05  NAME-7012       PIC X(20) VALUE 'INQUIRE MONITOR'.
080400     05  NAME-7014       PIC X(20) VALUE 'SET MONITOR'.
080500     05  NAME-7202       PIC X(20) VALUE 'PERFORM RESETTIME'.
080600     05  NAME-7402       PIC X(20) VALUE 'SIGNON'.
080700     05  NAME-7404       PIC X(20) VALUE 'SIGNOFF'.
080800     05  NAME-7406       PIC X(20) VALUE 'VERIFY PASSWORD'.
080900     05  NAME-7408       PIC X(20) VALUE 'CHANGE PASSWORD'.
081000     05  NAME-7602       PIC X(20) VALUE 'PERFORM SHUTDOWN'.
081100     05  NAME-7802       PIC X(20) VALUE 'INQUIRE TRACEDEST'.
081200     05  NAME-7804       PIC X(20) VALUE 'SET TRACEDEST'.
081300     05  NAME-7812       PIC X(20) VALUE 'INQUIRE TRACEFLAG'.
081400     05  NAME-7814       PIC X(20) VALUE 'SET TRACEFLAG'.
081500     05  NAME-7822       PIC X(20) VALUE 'INQUIRE TRACETYPE'.
081600     05  NAME-7824       PIC X(20) VALUE 'SET TRACETYPE'.
081700     05  NAME-7A02       PIC X(20) VALUE 'INQUIRE DSNAME'.
081800     05  NAME-7A04       PIC X(20) VALUE 'SET DSNAME'.
081900     05  NAME-7C02       PIC X(20) VALUE 'INQUIRE EXCI'.
082000     05  NAME-7E02       PIC X(20) VALUE 'DUMP TRANSACTION'.
082100     05  NAME-7E04       PIC X(20) VALUE 'PERFORM DUMP'.
082200     05  NAME-8002       PIC X(20) VALUE 'INQUIRE TSQUEUE'.
082300     05  NAME-8004       PIC X(20) VALUE 'SET TSQUEUE'.
082400     05  NAME-8012       PIC X(20) VALUE 'INQUIRE TSQNAME'.
082500     05  NAME-8014       PIC X(20) VALUE 'SET TSQNAME'.
082600     05  NAME-801A       PIC X(20) VALUE 'INQUIRE TSPOOL'.
082700     05  NAME-8022       PIC X(20) VALUE 'INQUIRE TSMODEL'.
082800     05  NAME-8030       PIC X(20) VALUE 'DISCARD TSMODEL'.
082900     05  NAME-8602       PIC X(20) VALUE 'ACQUIRE TERMINAL'.
083000 01  FILLER              REDEFINES NAME-COMMAND-NAMES.
083100     05  COMMAND-NAME    PIC X(20) OCCURS 239.
083110*
083200 01  FILLER.
083300     05  UNKNOWN-COMMAND PIC X(20) VALUE '**UNKNOWN COMMAND**'.
083400*
083500* Various values which you might wish to modify are placed in one
083600* copy book in order to make those sorts of changes more easily.
083700*
083720 01  FILLER.
083721     05  FILLER          PIC X(36) VALUE
083730         '********  NACWLITS COPYBOOK ********'.
083900     COPY NACWLITS.
084000*
084100* The generated symbolic map must be included in the program.
084200*
084210 01  FILLER.
084220     05  FILLER          PIC X(36) VALUE
084230         '********  NACTSET COPYBOOK  ********'.
084300     COPY NACTSET.
084400*
084420 01  FILLER              PIC X(36) VALUE
084430         '********  LINKAGE SECTION   ********'.
084500 EJECT.
084600*
084800 LINKAGE SECTION.
084810*
084820* The interface to this program is described in
084830* a copy book in order to ensure consistency.
084840*
084900 01  DFHCOMMAREA.
085000     COPY NACCERRH.
085010*
085020 EJECT.
085030*
085900 PROCEDURE DIVISION.
085910*
085920* First we obtain various information from CICS about the
085930* environment and the problem. We use this to determine
085940* what actions to take and whether the program was entered
085950* via CICS ABEND handling or whether an application program
085960* transferred control to it (via a CICS XCTL) or called it
085970* (via a CICS LINK or COBOL Dynamic CALL).
085980*
086000 NACT04-MAIN SECTION.
086001*
086002 NACT04-010.
086010*
086011*  Set up values in the eye-catcher
086012*
086013     MOVE EIBTRNID TO DEBUG-TRANID.
086014     MOVE EIBTRMID TO DEBUG-TERMID.
086015     MOVE EIBTASKN TO DEBUG-TASKNO.
086016*
086017*  Set up the commarea address
086018*
086019     SET DEBUG-COMMAREA-ADDR TO ADDRESS OF DFHCOMMAREA.
086020*
086030 NACT04-020.
086100     EXEC CICS ASSIGN NOHANDLE
086200               ABCODE(AA-ABCODE)
086300               ABPROGRAM(AA-ABPROGRAM)
086400               ASRAINTRPT(AA-ASRAINTRPT)
086500               ASRAKEY(AA-ASRAKEY)
086600               ASRAPSW(AA-ASRAPSW)
086700               ASRAREGS(AA-ASRAREGS)
086800               ASRASPC(AA-ASRASPC)
086900               ASRASTG(AA-ASRASTG)
087000               FCI(AA-FCI)
087100               INVOKINGPROG(AA-INVOKINGPROG)
087200               NETNAME(AA-NETNAME)
087300               PROGRAM(AA-PROGRAM)
087400               RETURNPROG(AA-RETURNPROG)
087500               STARTCODE(AA-STARTCODE)
087600               TERMCODE(AA-TERMCODE)
087700     END-EXEC.
087710*
087720 NACT04-030.
087800*
087900* A maximum of 16 parameters can be specified on one
088000* ASSIGN command, so we need to issue two.
088100*
088200     EXEC CICS ASSIGN NOHANDLE
088300               USERID(AA-USERID)
088400               USERNAME(AA-USERNAME)
088500     END-EXEC.
088510*
088520 NACT04-040.
088600*
088700* Here we initialize various fields, primarily from the constant
088800* values included in the common copy book set up for this purpose.
088900*
089000     MOVE LOW-VALUES           TO ACCTERRO.
089100     MOVE WS-LITS-DUMP-PREFIX  TO DUMP-CODE-PREFIX.
089200     MOVE WS-LITS-ERROR-PREFIX TO MA-SI-ID.
089300     MOVE WS-LITS-ERROR-PREFIX TO MA-XI-ID.
089400     MOVE WS-LITS-ERROR-PREFIX TO MA-AD-ID.
089500     MOVE EIBTRNID             TO MA-TRANSID.
089510*
089520 NACT04-050.
089600*
089700* If there is no ABEND code, then this program was invoked
089800* by either a CICS XCTL, a CICS LINK or a COBOL Dynamic CALL.
089900*
090000     IF  AA-ABCODE = SPACES
090010         PERFORM A-FIRST-ABEND
090011         PERFORM C-NORMAL-STUFF
090020     ELSE
090021         IF  AA-ABCODE NOT = WS-LITS-SPECIAL
090030             PERFORM B-SECOND-ABEND
090040             PERFORM C-NORMAL-STUFF
090100         END-IF
090101     END-IF.
090102*
090103 NACT04-060.
090110*
090120* If the return program identified by CICS contains spaces,
090130* that means we have come all the way up the LINK chain (if
090131* there was one) and so we should simply return to CICS.
090132* If the return and invoking program identified by CICS
090133* are the same, then that means this program was LINKed to
090134* and so we should simply return to it.
090135*
090136     IF  AA-RETURNPROG = SPACES
090137     OR  AA-RETURNPROG = AA-INVOKINGPROG
090138         EXEC CICS RETURN
090139         END-EXEC
090140*
090141* If the return program identified by CICS did not contain
090142* spaces and the invoking program identified by CICS is not
090143* the same, then that means this program is executing at a
090144* lower logical level and the invoker does not expect to be
090145* returned to. (Either this was transferred control to (via a
090146* CICS XCTL) or entered via CICS ABend Handling.) So we should
090147* propagate the ABEND up the LINK chain with the special code.
090148*
090149     ELSE
090150         EXEC CICS ABEND
090151                   ABCODE(WS-LITS-SPECIAL)
090152         END-EXEC
090153     END-IF.
090154*
090155 END-NACT04-MAIN.
090156     EXIT.
090157     EJECT.
090158*
090159 A-FIRST-ABEND SECTION.
090160*
090200* When the program is not entered as a result of CICS ABEND
090300* handling, we expect an interface area of a particular size
090400* and format. If it is not, then we have no option but to
090500* ABEND ourselves.
090600*
090610 A-010.
090700     IF  (EIBCALEN NOT = LENGTH OF DFHCOMMAREA)
090800     OR  (NOT CA-ERRH-CORRECT-VERSION)
090900         EXEC CICS ABEND
091000                   ABCODE(WS-LITS-ABEND-ERRH-IF)
091100         END-EXEC
091200     END-IF.
091300*
091310 A-020.
091400* Now we set up the message fields, etc., to indicate that
091500* the error was trapped by the application program logic.
091600*
091700     MOVE SPACES             TO MA-ASRA-DATA.
091800     MOVE CA-ERRH-PROGRAM    TO MA-ABPROGRAM.
091900     MOVE CA-ERRH-ERROR(2:3) TO DUMP-CODE-SUFFIX.
092000     MOVE 'TRAP'             TO MA-TR-TYPE.
092100     MOVE CA-ERRH-REASON-X   TO MA-REAS.
092200*
092210 A-030.
092300* If the passed error code is numeric, then it is one
092400* originally from CICS, so we need to set up the message
092500* wording based on the error trapped. However we must also
092600* allow for the fact that the interface to this was not
092700* correctly constructed by the calling program.
092800*
092900     IF  CA-ERRH-ERROR-X IS NUMERIC
093000         IF  CA-ERRH-ERROR > 0000
093100         AND CA-ERRH-ERROR <= MAXIMUM-RESPONSE
093200             MOVE RESPONSE-VALUE(CA-ERRH-ERROR) TO MA-RESP
093300         ELSE
093400             IF  CA-ERRH-ERROR = NOTPOSS-RESPONSE
093500                 MOVE NOTPOSS-RESP-VAL TO MA-RESP
093600             ELSE
093700                 MOVE BAD-RESPONSE-VALUE TO MA-RESP
093800             END-IF
093900         END-IF
094000         PERFORM TEST BEFORE VARYING WS-IX FROM 1 BY 1
094100          UNTIL  CA-ERRH-CICS-FUNCTION = COMMAND-VAL(WS-IX)
094200           OR    WS-IX > MAX-COMMANDS
094300         END-PERFORM
094400         IF  WS-IX > MAX-COMMANDS
094500             MOVE UNKNOWN-COMMAND  TO MA-CMD
094600         ELSE
094700             MOVE COMMAND-NAME(WS-IX) TO MA-CMD
094800         END-IF
094900*
095000* If the passed error code is not numeric, then it is one
095100* generated by the internal application logic.
095200*
095300     ELSE
095400         MOVE CA-ERRH-ERROR-X TO MA-RESP
095500         MOVE 'INTERNAL'      TO MA-CMD
095600     END-IF.
095700*
095800* Repeat some common information and complete the error
095900* messages and output them as appropriate to the environment.
096000*
096100     MOVE MA-TR          TO MA-XI-VAR.
096201*
096202 END-A-FIRST-ABEND.
096203     EXIT.
096204     EJECT.
096205*
096210 B-SECOND-ABEND SECTION.
096300*
096400* If there is an ABEND code, then this program was invoked
096500* by CICS ABEND Handling.
096900* In order to ensure that the quiesce of the task after handling
097000* an ABEND does not produce erroneous results, the ABEND must be
097100* propagated up the LINK chain (if any). Because we only wish to
097200* perform the various error handling actions once per ABENDing
097300* task, we need to identify that we have entered this logic
097400* previously for the same task. This is done by using a special
097500* ABEND code when terminating the handler at lower logical levels
097600* of a LINK chain via a propagating ABEND. We now check for that.
097700* If that is the case we discover, we bypass all of the error
097800* processing actions, otherwise we take actions specific to the
097900* type of ABEND encountered.
098000*
098010 B-010.
098200     MOVE AA-ABPROGRAM TO MA-ABPROGRAM.
098300*
098400* There are a series of System Recovery ABENDs which must all
098500* be handled in a similar manner. This involves identifying
098600* the program in error and calculating an offset into it for
098700* rapid relating to source code.
098800*
098900     IF  AA-ABCODE(1:3) = 'ASR'
098910         PERFORM BA-ADDITIONAL-ASR-PROCESS
113500*
113600* If the ABEND is not one of the System Recovery ones,
113700* then we produce a different type of message.
113800*
113900     ELSE
114000         MOVE SPACES TO MA-ASRA-DATA
114100     END-IF.
114110*
114120 B-020.
114200*
114300* We then finish off the variable part of the messages
114400* and output them as appropriate to the environment.
114500*
114600     MOVE AA-ABCODE(2:3) TO DUMP-CODE-SUFFIX.
114700     MOVE 'ABEND'        TO MA-AB-TYPE.
114800     MOVE AA-ABCODE      TO MA-ABCODE.
114900     MOVE MA-AB          TO MA-XI-VAR.
115100*
115110 END-B-SECOND-ABEND.
115200     EXIT.
115201     EJECT.
115202*
115210 BA-ADDITIONAL-ASR-PROCESS SECTION.
115220*
115300* Because a COBOL Dynamic CALL employs COBOL (LE) services to
115400* transfer control from the calling program to the called program,
115500* CICS is unaware of its involvement in the task as an executable
115600* module. To allow for this possibility we need to determine if
115700* the error actually occurred in the module CICS thinks it did.
115800* We do this by testing the Program Status Word (PSW) Next
115900* Sequential Instruction (NSI) address (the point of interrupt)
116000* is in the range of the program CICS has identified as the one
116100* in error. If it is not, then we need to search for the program
116200* in which the PSW NSI refers.
116300*
116310 BA-010.
116400     EXEC CICS INQUIRE
116500               PROGRAM(AA-ABPROGRAM)
116600               ENTRYPOINT(WF-ENTRY-POINT)
116700               LOADPOINT(WF-LOAD-POINT)
116800               LENGTH(WF-LENGTH)
116900               NOHANDLE
117000     END-EXEC.
117010*
117100     ADD WF-LENGTH TO WF-LOAD-POINT
117200               GIVING WF-END-POINT.
117210*
117220 BA-020.
117300*
117400* Note that the PSW NSI contains an indicator in the high-order
117500* bit along with a 31-bit address. If on, the high-order bit
117600* would cause the PSW NSI to be treated as a negative number
117700* (an impossibility for an address). So we need to manipulate
117800* the high-order byte of the PSW NSI in order to eliminate this
117900* possibility. Since COBOL has no bit manipulation capabilities
118000* the code to do this is a bit convoluted.
118100*
118200     MOVE AA-AP-HIGH TO WF-AP-LOW.
118210*
118300     IF  WF-AP-HALF >= 128
118400         SUBTRACT 128  FROM WF-AP-HALF
118500     END-IF.
118510*
118600     MOVE WF-AP-LOW  TO AA-AP-HIGH
118610*
118700     IF  AA-ASRAPSW-NSI > WF-END-POINT
118800     OR  AA-ASRAPSW-NSI < WF-LOAD-POINT
118900         MOVE '*UNKNOWN'     TO MA-ABPROGRAM
119000*
119010* In the event that the error is not in the program CICS has
119020* identified as being in error, we need to browse all of the
119030* programs known to CICS. We initiate this process by an
119040* INQUIRE PROGRAM START command followed by a loop containing
119050* an INQUIRE PROGRAM( ) NEXT command until we exhaust the list
119060* or we find the program with the problem.
119070*
119080         EXEC CICS INQUIRE PROGRAM START
119090                              NOHANDLE
119091         END-EXEC
119092         PERFORM UNTIL (MA-ABPROGRAM NOT = '*UNKNOWN')
119093                 OR   (EIBRESP = DFHRESP(END))
119094            EXEC CICS INQUIRE
119095                      PROGRAM(AA-ABPROGRAM) NEXT
119096                      ENTRYPOINT(WF-ENTRY-POINT)
119097                      LOADPOINT(WF-LOAD-POINT)
119098                      LENGTH(WF-LENGTH)
119099                      NOHANDLE
119100            END-EXEC
119101            ADD WF-LENGTH TO     WF-LOAD-POINT
119102                          GIVING WF-END-POINT
119103            IF  AA-ASRAPSW-NSI < WF-END-POINT
119104            AND AA-ASRAPSW-NSI > WF-LOAD-POINT
119105                MOVE AA-ABPROGRAM   TO MA-ABPROGRAM
119106            END-IF
119107        END-PERFORM
119108*
119109* When we have exhausted the list or have found the program
119110* with the problem, we need to terminate the browse.
119111*
119112        EXEC CICS INQUIRE PROGRAM END
119113                          NOHANDLE
119114        END-EXEC
119115     ELSE
119116        MOVE AA-ABPROGRAM   TO MA-ABPROGRAM
119117     END-IF.
119118*
119119 BA-030.
119120*
119121* Next we need to convert the calculated offset to hexadecimal
119122* display data. We initialize fields for this purpose to contain
119123* the value in the form "X'nnnnnn'".
119124*
119125     MOVE 'X'   TO MA-AD-OFF(1:1)
119126     MOVE X'7D' TO MA-AD-OFF(2:1)
119127     MOVE X'7D' TO MA-AD-OFF(9:1)
119128*
119129     IF  MA-ABPROGRAM = '*UNKNOWN'
119130         MOVE 'FFFFFF' TO MA-AD-OFF(3:6)
119131     ELSE
119132*
119133* The architecture of the System/390 means that the PSW NSI
119134* usually identifies the instruction after the one in error,
119135* so we need to subtract the instruction length from it first.
119136*
119137         SUBTRACT AA-AI-LTH FROM AA-ASRAPSW-NSI
119138*
119139* Note that like the PSW NSI, the ENtry Point contains an
119140* indicator in the high-order bit along with a 31-bit address.
119141* If on, the high-order bit would cause the Entry Point to be
119142* treated as a negative number (an impossibility for an address).
119143* So we need to manipulate the high-order byte of the Entry
119144* Point in order to eliminate this possibility. Since COBOL
119145* has no bit manipulation capabilities the code to do this is
119146* a bit convoluted.
119147*
119148         MOVE WF-EP-HIGH TO WF-AP-LOW
119149         IF  WF-AP-HALF >= 128
119150             SUBTRACT 128  FROM WF-AP-HALF
119151         END-IF
119152         MOVE WF-AP-LOW  TO WF-EP-HIGH
119153*
119154* Finally we can calculate the offset of the error instruction.
119155*
119156         SUBTRACT WF-ENTRY-POINT
119157           FROM   AA-ASRAPSW-NSI
119158*
119159* Then we can convert it to hexadecimal display format.
119160* We convert the last 3 bytes of the value only since
119161* no offset can exceed that size.
119162*
119163         PERFORM TEST BEFORE
119164                 VARYING WS-IX FROM 2 BY 1 UNTIL WS-IX > 4
119165             MOVE AA-AP-NSI(WS-IX:1) TO WF-AP-LOW
119166             MOVE LOW-VALUE       TO WF-AP-HIGH
119167             MOVE ZERO            TO WS-IH
119168             MOVE WF-AP-HALF      TO WS-IL
119169             PERFORM UNTIL WS-IL < 16
119170                 SUBTRACT 16 FROM WS-IL
119171                 ADD       1  TO  WS-IH
119172             END-PERFORM
119173             ADD 1 TO WS-IH
119174             ADD 1 TO WS-IL
119175             MOVE HEX-CHAR(WS-IH)
119176               TO  MA-AD-OFF(((WS-IX - 2) * 2) + 3:1)
119177             MOVE HEX-CHAR(WS-IL)
119178               TO  MA-AD-OFF(((WS-IX - 2) * 2) + 4:1)
119179         END-PERFORM
119180     END-IF.
119181*
119182 BA-040.
119183*
119184* Next we put in the interrupt code and an explanation of it.
119185*
119186     MOVE AA-AI-IC    TO MA-AD-IC.
119187     MOVE AA-AI-IC-LO TO WF-AP-LOW.
119188     MOVE LOW-VALUE   TO WF-AP-HIGH.
119189*
119190     IF  WF-AP-HALF > 0
119191     AND WF-AP-HALF < 16
119192         MOVE EXC-EXPLAIN(WF-AP-HALF)
119193          TO  MA-AD-EXPLAIN
119194     END-IF.
119195*
119196 END-BA-ADDITIONAL-ASR-PROCESS.
119197     EXIT.
119198     EJECT.
119199*
119200 C-NORMAL-STUFF SECTION.
119201*
119202* When no ABEND handler is active, CICS performs some default
119203* actions when an ABEND is encountered. If an ABEND handler is
119204* active, then CICS leaves all of these actions up to the handler.
119205* The default actions are to: (1) record information about the
119206* problem via a dump; (2) backout any changes made in the current
119207* Unit Of Work (UOW); (3) send messages to the user (if associated
119208* with a terminal), to transient data and (optionally) to the
119209* machine operator; and (4) terminate the task. As a result we
119210* perform similar actions in this program.
119211*
119220*
119300* We take a dump but with a code based on the type of error we
119400* have encountered. This way we ensure we do not conflict with
119500* any CICS dump codes. Note that if these are not desired, they
119600* can be suppressed by operational procedures.
119700*
119710 C-010.
119800     EXEC CICS DUMP TRANSACTION
119900               DUMPCODE(DUMP-CODE)
120000               NOHANDLE
120100     END-EXEC.
120200*
120300* We backout by instructing CICS to roll the current UOW back.
120400*
120500     EXEC CICS SYNCPOINT ROLLBACK
120600     END-EXEC.
120601*
120610 C-020.
120700*
120800* We want to include a date and time in our messages so we get
120900* CICS to tell us the current time and ask it to format it.
121000*
121100     EXEC CICS ASKTIME
121200               ABSTIME(WF-ABSTIME)
121300     END-EXEC.
121310*
121400     EXEC CICS FORMATTIME
121500               ABSTIME(WF-ABSTIME)
121600               DDMMYYYY(MA-SI-DATE)
121700               DATESEP
121800               TIME(MA-SI-TIME)
121900               TIMESEP
122000     END-EXEC.
122010*
122100     MOVE MA-SI-DATE TO MA-XI-DATE
122200                        DATEEO.
122300     MOVE MA-SI-TIME TO MA-XI-TIME
122400                        TIMEEO.
122410*
122420 C-030.
122500*
122600* By default we produce two message to transient data.
122700* We also want to display these at the terminal if we have one.
122800*
122900     MOVE 2 TO WF-ERRH-NUMBER.
122910*
123000     EXEC CICS WRITEQ TD
123100               QUEUE(WS-LITS-ERROR-QUEUE)
123200               FROM(MA-STD-INFO)
123300               NOHANDLE
123400     END-EXEC.
123610*
123700     EXEC CICS WRITEQ TD
123800               QUEUE(WS-LITS-ERROR-QUEUE)
123900               FROM(MA-XTR-INFO)
124000               NOHANDLE
124100     END-EXEC.
124310*
124320 C-040.
124400*
124500* By writing these messages out to the operator, we enable
124600* automated operations policies to be implemented.
124700* You might not want to do this.
124800*
124900     EXEC CICS WRITE OPERATOR
125000               TEXT(MA-STD-INFO)
125100     END-EXEC.
125110*
125200     EXEC CICS WRITE OPERATOR
125300               TEXT(MA-XTR-INFO)
125400     END-EXEC.
125410*
125420 C-050.
125500*
125600* If this was a System Recovery type of problem,
125700* there is a third message to output.
125800*
125900     IF  MA-ASRA-DATA NOT = SPACES
126000         MOVE MA-SI-DATE TO MA-AD-DATE
126100         MOVE MA-SI-TIME TO MA-AD-TIME
126200         EXEC CICS WRITEQ TD
126300                   QUEUE(WS-LITS-ERROR-QUEUE)
126400                   FROM(MA-ASRA-DATA)
126500                   NOHANDLE
126600         END-EXEC
126700         MOVE 3            TO WF-ERRH-NUMBER
127000         EXEC CICS WRITE OPERATOR
127100                   TEXT(MA-ASRA-DATA)
127200         END-EXEC
127300     END-IF.
127301*
127302 C-060.
127303*
127304* Only set values in the commarea when the commarea is
127305* the correct length.
127306*
127307     IF  EIBCALEN = LENGTH OF DFHCOMMAREA
127308         MOVE WF-ERRH-NUMBER TO CA-ERRH-NUMBER
127309         MOVE MA-STD-INFO    TO CA-ERRH-MESSAGE(1)
127310         MOVE MA-XTR-INFO    TO CA-ERRH-MESSAGE(2)
127311         MOVE MA-ASRA-DATA   TO CA-ERRH-MESSAGE(3)
127312     END-IF.
127313*
127320 C-070.
127400*
127500* If there is a terminal associated with this task,
127600* we want to inform the end-user of the problem as well.
127700*
127800     IF  AA-TERM-STARTED
127900*
128000* However there are some conditions where the terminal is
128100* unusable, so we don't want to send anything to it under
128200* those circumstances.
128300*
128400         IF  NOT AA-DO-NOT-SEND
128420             MOVE MA-SI-SCREEN TO MSGEO(1)
128430             MOVE MA-XI-SCREEN TO MSGEO(2)
128440             MOVE MA-AD-SCREEN TO MSGEO(3)
128500             EXEC CICS SEND
128600                       MAP('ACCTERR')
128700                       MAPSET(WS-LITS-MAPSET)
128800                       FREEKB
128900                       ERASE
129000                       NOHANDLE
129100             END-EXEC
129200         END-IF
129300*
129400* There is one condition where we need to drop
129500* the session with the end user's terminal.
129600*
129700         IF  AA-TO-DISCONNECT
129800             EXEC CICS ISSUE
129900                       DISCONNECT
130000                       NOHANDLE
130100             END-EXEC
130200         END-IF
130300     END-IF.
130400*
130500 END-C-NORMAL-STUFF.
130600     EXIT.
