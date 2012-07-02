      SUBROUTINE SNDBG (IFM,ICLASS,IVAL,RVAL,KVAL)
      IMPLICIT NONE
      INTEGER           IFM,ICLASS,IVAL
      REAL*8                            RVAL(*)
      CHARACTER*(*)                          KVAL
C     ------------------------------------------------------------------
C          ECRITURE DE CE QUE L'ON A TROUVE (NIVEAU SN)
C     ------------------------------------------------------------------
C IN  ICLASS   CLASSE  DE CE QUE L'ON A TROUVE
C     ------------------------------------------------------------------
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DE FICHIER
C       0   ERREUR           CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL(1) DE TYPE REAL*8
C       3   IDENTIFICATEUR   CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            CVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   COMPLEXE         RVAL(1),RVAL(2)
C       6   BOOLEEN          IVAL  = 1 = VRAI , 0 = FAUX
C
C           SEPARATEUR       CVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C       7   '('
C       8   ')'
C       9   ','
C      10   ':'
C      11   '='
C      12   ';'
C      13   SEPARATEUR INDEFINI
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         -
C     ROUTINE(S) FORTRAN     :
C         WRITE
C     ------------------------------------------------------------------
C FIN SNDBG
C     ------------------------------------------------------------------
C
      CHARACTER*12 PGM
      CHARACTER*80 CVAL
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CVAL = KVAL
      PGM = ' <SNDBG >:  '
      WRITE(IFM,'(1X,72(''-''))')
      IF (ICLASS.EQ.-1) THEN
         WRITE(IFM,*) PGM,'EOF    : "FIN DE FICHIER"'
      ELSEIF(ICLASS.EQ.0) THEN
         WRITE(IFM,*) PGM,'ERREUR : "'//CVAL(:IVAL)//'"'
      ELSEIF(ICLASS.EQ.1) THEN
         WRITE(IFM,*) PGM,'ENTIER :', IVAL
      ELSEIF(ICLASS.EQ.2) THEN
         WRITE(IFM,*) PGM,'REEL   :', RVAL(1)
      ELSEIF(ICLASS.EQ.3) THEN
         WRITE(IFM,*) PGM,'IDENT  : "'//CVAL(:IVAL)//'"'
      ELSEIF(ICLASS.EQ.4) THEN
         WRITE(IFM,*) PGM,'TEXTE  : "'//CVAL(:IVAL)//'"'
      ELSEIF(ICLASS.EQ.5) THEN
         WRITE(IFM,*) PGM,'CMPLX  : (',RVAL(1),',',RVAL(2),')'
      ELSEIF(ICLASS.EQ.6) THEN
         WRITE(IFM,*) PGM,'BOOLEAN: ',IVAL
      ELSEIF(ICLASS.GT.6.AND.ICLASS.LT.13) THEN
         WRITE(IFM,*) PGM,ICLASS,'  : "'//CVAL(:IVAL)//'"'
      ELSEIF(ICLASS.EQ.13) THEN
         WRITE(IFM,*) PGM,'UNDEF  : "'//CVAL(:IVAL)//'"'
      ELSE
         WRITE(IFM,*) PGM,'CLASSE INDEFINIE ',ICLASS
      ENDIF
      END
