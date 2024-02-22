/* REXX */
/*
 * BNDPROC
 */
   trace o

   /*
    * DB2 BIND does not work when DBRMLIB DD is allocated
    * with BPXWDYN, which RTC uses.  Following lines
    * reallocates DBRMLIB DD using TSO ALLOC command.
    */
   Call BPXWDYN("info dd(DBRMLIB) inrtdsn(dsnvar)")
   Call BPXWDYN("free dd(DBRMLIB)")
   "ALLOC dd(DBRMLIB) da('"dsnvar"') shr"

   "EXECIO 0 DISKR INDD (OPEN"
   
   tsocmd = ''
   cmd = ''
   eof = 'NO'

   DO WHILE eof = 'NO'

      "EXECIO 1 DISKR INDD (STEM LINE."

      IF RC = 2 THEN
         eof = 'YES'
      ELSE
      DO
         line.1 = STRIP(line.1)
         IF tsocmd = '' THEN
            tsocmd = line.1
         ELSE
            IF RIGHT(line.1, 1) = '-' THEN
            DO
               Call Remove_cont_char
               Call Add_cmd
            END
            ELSE
            DO
               Call Add_cmd
               queue cmd
               cmd = ''
            END
      END
   END

   "EXECIO 0 DISKR INDD (FINIS"
   
   Address TSO tsocmd
   rcode = RC

Exit rcode

Remove_cont_char:
   len = LENGTH(line.1)
   line.1 = LEFT(line.1, len - 1)
   line.1 = STRIP(line.1)
Return

Add_cmd:
   If cmd = '' THEN
      cmd = line.1
   ELSE
      cmd = cmd || ' ' || line.1
Return