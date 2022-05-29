/*  TPRINT.REX  */

/* trace ?i */

/*
   This program performs a PRINT operation within Tritus SPF using
   the complete set of fields for the active logical printer
   definition.  It can be modified to support additional print
   requirements of your installation.  You may also want to refer
   to TPRNT.REX, which is a similar program, but it implements
   only a subset of the active logical printer definition.  Since
   TPRNT.REX is a much smaller REXX program, it is easier to follow.

   The variable names and descriptions for a logical printer
   definition are -

   ZlprName - Name of the active logical printer
   ZlprDesc - Logical printer description
   ZlprRexx - NONE or name of REXX program to drive printer
   ZlprDevn - Device or filename
   ZlprIffe - Form feed before printing
   ZlprFffe - Form feed after printing
   ZlprSubU - Substitute for unprintable characters
   ZlprSets - Setup string
   ZlprRess - Reset string
   ZlprRows - Number of lines per page
   ZlprCols - Printer width
   ZlprPffe - Form feed between pages
   ZlprTmgn - Top margin
   ZlprBmgn - Bottom margin
   ZlprLmgn - Left margin
   ZlprRmgn - Right margin
   ZlprHdro - Header contents
   ZlprHdrs - Header spacing
   ZlprFtro - Footer contents
   ZlprFtrs - Footer spacing
*/
                                   /* Get filename.                  */
PARSE ARG FileName
                                   /* Get definition fields.         */
'ISPEXEC VGET ZlprName ZlprDesc ZlprRexx ZlprDevn ZlprIffe ZlprFffe'
'ISPEXEC VGET ZlprSubU ZlprSets ZlprRess ZlprRows ZlprCols ZlprPffe'
'ISPEXEC VGET ZlprTmgn ZlprBmgn ZlprLmgn ZlprRmgn'
'ISPEXEC VGET ZlprHdro ZlprHdrs ZlprFtro ZlprFtrs'
                                   /* Build translate table.         */
XlatTable = XRANGE('00'X,'FF'X)
IF ( (LENGTH(ZlprSubU) == 1) & (ZlprSubU <> ' ') ) THEN
   DO
      XlatTable = OVERLAY( ZlprSubU, XlatTable, 1, 32, ZlprSubU )
      XlatTable = OVERLAY( ZlprSubU, XlatTable, 128, 129, ZlprSubU )
   END
                                   /* Print setup string.            */
CALL SR_STRING ZlprSets
                                   /* Form feed before printing.     */
IF ( ZlprIffe == 'Y' ) THEN
   CALL CHAROUT ZlprDevn, '0C'X
                                   /* Process data to be printed.    */
IF ( ZlprRows = 0 ) THEN
   DO                              /* (Unformatted).                 */
      'ISPEXEC LPRTGET DataLine'
      DO WHILE ( Rc = 0 )
         DataLine = TRANSLATE( DataLine, XlatTable )
         CALL LINEOUT ZlprDevn, DataLine
         'ISPEXEC LPRTGET DataLine'
      END
   END
ELSE
   DO                              /* (Formatted).                   */
      CALL PRT_FORMAT
   END
                                   /* Form feed after printing.      */
IF ( ZlprFffe == 'Y' ) THEN
   CALL CHAROUT ZlprDevn, '0C'X
                                   /* Print reset string.            */
CALL SR_STRING ZlprRess
                                   /* Close the printer file.        */
CALL LINEOUT ZlprDevn

EXIT

/* Subroutine to handle the setup string or reset string.            */
SR_STRING:
   PARSE ARG SrString
   Done = 0
   IF ( TRANSLATE(SUBSTR(SrString,1,2)) == 'F=' ) THEN
      DO                           /* Copy file one char at a time.  */
         SrString = RIGHT( SrString, LENGTH(SrString)-2 )
         Char = CHARIN( SrString )
         DO WHILE ( LENGTH(Char) == 1 )
            CALL CHAROUT ZlprDevn, Char
            Char = CHARIN( SrString )
         END
         Done = 1
      END
   IF ( TRANSLATE(SUBSTR(SrString,1,2)) == 'D=' ) THEN
      DO                           /* Strip leading two characters.  */
         SrString = RIGHT( SrString, LENGTH(SrString)-2 )
      END
   IF ( Done == 0 ) THEN
      DO                           /* Copy the string.               */
         CALL CHAROUT ZlprDevn, SrString
      END
   RETURN

/* Handle formatted print. */
PRT_FORMAT:
   PageNbr = 0
   PageResid = 0
                                   /* Establish date and time.       */
   'ISPEXEC VGET ZintDtFm ZintDtSe ZintTmFm ZintTmSe'
   Date = DATE( 'S' )
   Time = TIME()
   Year = SUBSTR( Date, 3, 2 )
   Month = SUBSTR( Date, 5, 2 )
   Day = SUBSTR( Date, 7, 2 )
   SELECT
      WHEN ( ZintDtFm = 1 ) THEN
         Date = Day || ZintDtSe || Month || ZintDtSe || Year
      WHEN ( ZintDtFm = 2 ) THEN
         Date = Year || ZintDtSe || Month || ZintDtSe || Day
      OTHERWISE
         Date = Month || ZintDtSe || Day || ZintDtSe || Year
   END
   Hour = SUBSTR(Time,1,2)
   AmPm = ''
   IF ( ZintTmFm <> 1 ) THEN
      DO
         AmPm = 'a'
         IF ( Hour >= 12 ) THEN
            DO
               Hour = Hour - 12
               AmPm = 'p'
            END
         IF ( Hour = 0 ) THEN
            Hour = 12
         Hour = Hour + 0
      END
   Time = STRIP( Hour || ZintTmSe || SUBSTR(Time,4,2) || AmPm, 'B' )
                                   /* Validate depth.                */
   DataRows = ZlprRows - (ZlprTmgn + ZlprBmgn + ZlprHdrs + ,
         ZlprFtrs + (ZlprHdrO <> '') + (ZlprFtrO <> ''))
   IF ( DataRows < 1 ) THEN
      DO
         SAY "Too many blank lines on page, there is no room on the" ,
               "page for a data line."
         SAY "Check logical printer definition."
         EXIT
      END
                                   /* Validate width.                */
   DataCols = 0
   IF ( ZlprCols > 0 ) THEN
      DO
         DataCols = ZlprCols - (ZlprLmgn + ZlprRmgn)
         IF ( DataCols < 1 ) THEN
            DO
               SAY "Too many blank columns on line, " ,
                     "there is no room on the line for data."
               SAY "Check logical printer definition."
               EXIT
            END
      END
                                   /* Finally, handle data lines.    */
   'ISPEXEC LPRTGET DataLine'
   DO WHILE ( Rc = 0 )
      DataLine = TRANSLATE( DataLine, XlatTable )
      IF ( ZlprCols = 0 ) THEN
         CALL PRT_FRAG DataLine
      ELSE
         DO
            IF ( LENGTH(DataLine) = 0 ) THEN
               DataLine = ' '
            DO LeftCol = 1 TO LENGTH(DataLine) BY DataCols
               CALL PRT_FRAG SUBSTR( DataLine, LeftCol, DataCols )
            END
         END
      'ISPEXEC LPRTGET DataLine'
   END
                                   /* Blank lines for last page.     */
   IF ( PageNbr > 0 ) THEN
      DO
         DO i = 1 TO PageResid
            CALL LINEOUT ZlprDevn, ''
         END
         CALL PRT_END
      END

   RETURN

/* Subroutine to print a data line fragment. */
PRT_FRAG:
   PARSE ARG Frag
   Frag = STRIP( Frag, 'T' )
   IF ( PageResid <= 0 ) THEN
      DO
         IF ( PageNbr > 0 ) THEN
            DO
               CALL PRT_END
               IF ( ZlprPffe == 'Y' ) THEN
                  CALL CHAROUT ZlprDevn, '0C'X
            END
         PageNbr = PageNbr + 1
         CALL PRT_TOP
         PageResid = DataRows
      END
   Line = STRIP( COPIES( ' ', ZlprLmgn ) || Frag, 'T' )
   CALL LINEOUT ZlprDevn, Line
   PageResid = PageResid - 1
   RETURN

/* Subroutine to print top of page. */
PRT_TOP:
   DO i = 1 TO ZlprTmgn
      CALL LINEOUT ZlprDevn, ''
   END
   CALL PRT_HDR_FTR ZlprHdrO
   DO i = 1 TO ZlprHdrs
      CALL LINEOUT ZlprDevn, ''
   END
   RETURN

/* Subroutine to print end of page. */
PRT_END:
   DO i = 1 TO ZlprFtrS
      CALL LINEOUT ZlprDevn, ''
   END
   CALL PRT_HDR_FTR ZlprFtrO
   DO i = 1 TO ZlprBmgn
      CALL LINEOUT ZlprDevn, ''
   END
   RETURN

/* Subroutine to print header or footer line. */
PRT_HDR_FTR:
   PARSE UPPER ARG HdrFtr
   IF ( HdrFtr == '' ) THEN
      RETURN
   Data.L = ''
   Data.C = ''
   Data.R = ''
   DataVar = 'L'
   DO i = 1 TO LENGTH( HdrFtr )
      Char = SUBSTR( HdrFtr, i, 1 )
      SELECT
         WHEN ( (Char == 'L') | (Char = 'C') | (Char == 'R') ) THEN
            DO
               IF ( ZlprCols > 0 ) THEN
                  DataVar = Char
            END
         WHEN ( Char == 'N' ) THEN
            IF ( LENGTH(FileName) > 0 ) THEN
               Data.DataVar = Data.DataVar || FileName || '  '
         WHEN ( Char == 'D' ) THEN
            Data.DataVar = Data.DataVar || Date || '  '
         WHEN ( Char == 'T' ) THEN
            Data.DataVar = Data.DataVar || Time || '  '
         WHEN ( Char == 'P' ) THEN
            Data.DataVar = Data.DataVar || 'Page' PageNbr || '  '
         OTHERWISE
            NOP
      END
   END
   Data.L = STRIP( Data.L, 'T' )
   Data.C = STRIP( Data.C, 'T' )
   Data.R = STRIP( Data.R, 'T' )
                                   /* Check for overlapping fields.  */
   IF ( ZlprCols > 0 ) THEN
      DO
         Overlap = 0
         Width = ZlprCols - ZlprLmgn - ZlprRmgn
         IF ( LENGTH(Data.C) > 0 ) THEN
            DO
               OrgC = (Width - LENGTH(Data.C)) % 2
               EndC = OrgC + LENGTH(Data.C)
               BlanksLC = OrgC - LENGTH(Data.L)
               IF ( LENGTH(Data.L) == 0 ) THEN
                  DO
                     IF ( BlanksLC < 0 ) THEN
                        Overlap = 1
                  END
               ELSE
                  DO
                     IF ( BlanksLC < 2 ) THEN
                        Overlap = 1
                  END
               BlanksCR = Width - LENGTH(Data.R) - EndC
               IF ( LENGTH(Data.R) == 0 ) THEN
                  DO
                     IF ( BlanksCR < 0 ) THEN
                        Overlap = 1
                  END
               ELSE
                  DO
                     IF ( BlanksCR < 2 ) THEN
                        Overlap = 1
                  END
            END
         ELSE
            DO
               BlanksLC = 0
               BlanksCR = Width - LENGTH(Data.L) - LENGTH(Data.R)
               IF ( (LENGTH(Data.L) = 0) | (LENGTH(Data.R) = 0) ) THEN
                  DO
                     IF ( BlanksCR < 0 ) THEN
                        Overlap = 1
                  END
               ELSE
                  DO
                     IF ( BlanksCR < 2 ) THEN
                        Overlap = 1
                  END
            END
         IF ( Overlap > 0 ) THEN
            DO
               BlanksLC = ((LENGTH(Data.L) > 0) & ,
                     ((LENGTH(Data.C)+LENGTH(Data.R)) > 0)) * 2
               BlanksCR = ((LENGTH(Data.C) > 0) & ,
                     (LENGTH(Data.R) > 0)) * 2
            END
         ELSE
            DO
            END
      END
   ELSE
      DO
         BlanksLC = 0
         BlanksCR = 0
      END
                                   /* Collect pieces and print.      */
   Line = COPIES( ' ', ZlprLmgn ) || Data.L || ,
          COPIES( ' ', BlanksLC ) || Data.C || ,
          COPIES( ' ', BlanksCR ) || Data.R
   IF ( ZlprCols > 0 ) THEN
      DO
         MaxLen = ZlprCols - ZlprRmgn
         IF ( LENGTH(Line) > MaxLen ) THEN
            Line = LEFT( Line, MaxLen )
      END
   Line = TRANSLATE( Line, XlatTable )
   CALL LINEOUT ZlprDevn, Line
   RETURN

