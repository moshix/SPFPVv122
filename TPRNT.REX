/*  TPRNT.REX  */

/*
   Sample Tritus SPF logical printer REXX program.
   This program is called to execute the PRINT command when the
   active logical printer has a REXX program name of TPRNT.

   The following fields in the logical printer definition are used -
   ZlprDevn - Device or filename
   ZlprSets - Setup string
   ZlprRess - Reset string
   ZlprIffe - Initial form feed eject
   ZlprFffe - Final form feed eject
   All other fields in the logical printer definition are ignored.
*/

                                   /* Get definition fields.         */
'ISPEXEC VGET ZlprDevn ZlprSets ZlprRess ZlprIffe ZlprFffe'
                                   /* Setup string.                  */
CALL PSTRING ZlprSets
                                   /* Initial form feed eject.       */
IF ( ZlprIffe == 'Y' ) THEN
   CALL CHAROUT ZlprDevn, '0C'X
                                   /* Process data to be printed.    */
'ISPEXEC LPRTGET DataLine'

DO WHILE ( Rc = 0 )
   CALL LINEOUT ZlprDevn, DataLine
   'ISPEXEC LPRTGET DataLine'
END
                                   /* Final form feed eject.         */
IF ( ZlprFffe == 'Y' ) THEN
   CALL CHAROUT ZlprDevn, '0C'X
                                   /* Handle the reset string.       */
CALL PSTRING ZlprRess
                                   /* Close the printer file.        */
CALL LINEOUT ZlprDevn

EXIT

/* Subroutine to handle the setup string or reset string             */
PSTRING:
   PARSE ARG Arg1
   Done = 0
   IF ( TRANSLATE(SUBSTR(Arg1,1,2)) == 'F=' ) THEN
      DO                           /* Copy file one char at a time.  */
         Arg1 = RIGHT( Arg1, LENGTH(Arg1)-2 )
         Char = CHARIN( Arg1 )
         DO WHILE ( LENGTH(Char) == 1 )
            CALL CHAROUT ZlprDevn, Char
            Char = CHARIN( Arg1 )
         END
         Done = 1
      END
   IF ( TRANSLATE(SUBSTR(Arg1,1,2)) == 'D=' ) THEN
      DO                           /* Strip leading two characters.  */
         Arg1 = RIGHT( Arg1, LENGTH(Arg1)-2 )
      END
   IF ( Done == 0 ) THEN
      DO                           /* Copy the string.               */
         CALL CHAROUT ZlprDevn, Arg1
      END
   RETURN

