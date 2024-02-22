/* REXX */
   /*
     REXX BIND processor sample
    */
   trace o
   Arg PACKAGE DBRM

   rcode = 0

   /* Set BIND options */
   SYSTEM    = 'DSN9'

   i = Pos('(', DBRM)
   len = Length(DBRM)
   LIBRARY = Substr(DBRM, 1, i - 1)
   MEMBER = Substr(DBRM, i + 1, len - i - 1) 

   OWNER     = 'DEVDBA'
   ACTION    = 'REPLACE'
   VALIDATE  = 'RUN'
   ISOLATION = 'CS'
   EXPLAIN   = 'NO'
   QUALIFIER = 'DEVDBA'

   Call Bind_it

Exit rcode

Bind_it:

   /*  Create a bind control statement as a single long line.  Nothing
       Else seems to work  */

   DB2_Line = "BIND PACKAGE("PACKAGE")"            ||,
                  " LIBRARY('"LIBRARY"')"          ||,
                  " MEMBER("MEMBER")"              ||,
                  " OWNER("OWNER")"                ||,
                  " ACTION("ACTION")"              ||,
                  " VALIDATE("VALIDATE")"          ||,
                  " ISOLATION("ISOLATION")"        ||,
                  " EXPLAIN("EXPLAIN")"            ||,
                  " QUALIFIER("QUALIFIER")"

   /*  Write the bind control statement to the data queue and execute  */
   /*  DB2I to perform the bind.                                       */

   queue DB2_Line
   queue "End"
   Address TSO "DSN SYSTEM("SYSTEM")"
   rcode = RC

Return
