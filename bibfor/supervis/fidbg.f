      SUBROUTINE FIDBG(IFM,ICLASS,IVAL,RVAL,KVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IFM,ICLASS,IVAL
      REAL*8 RVAL(*)
      CHARACTER*(*) KVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 12/01/2000   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ECRITURE DE CE QUE L'ON A TROUVE (NIVEAU FI) DEBUG
C     ------------------------------------------------------------------
C IN  ICLASS   CLASSE  DE CE QUE L'ON A TROUVE
C     ------------------------------------------------------------------
C           -- TYPE -----    ---- INFORMATION --------------------------
C      -1   FIN DE FICHIER
C       0   ERREUR           KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       1   ENTIER           IVAL DE TYPE INTEGER
C       2   REEL             RVAL(1) DE TYPE REAL*8
C       3   IDENTIFICATEUR   KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       4   TEXTE            KVAL DE TYPE CHARACTER*(*) DE LONGUEUR IVAL
C       5   COMPLEXE         RVAL(1),RVAL(2)
C       6   BOOLEEN          IVAL  = 1 = VRAI , 0 = FAUX

C           SEPARATEUR       KVAL DE TYPE CHARACTER*(*) DE LONGUEUR 1
C       7   '('
C       8   ')'
C       9   ','
C      10   ':'
C      11   '='
C      12   ';'
C      13   '*'
C      14   '/'
C      15   '+'
C      16   '-'
C      17   SEPARATEUR INDEFINI
C     ------------------------------------------------------------------
C     AUTEUR : D.SELIGMANN
C     ------------------------------------------------------------------

      CHARACTER*12 PGM
      PGM = ' <FIDBG >:  '
      WRITE (IFM,'(1X,72(''-''))')
      IF (ICLASS.EQ.-1) THEN
        WRITE (IFM,*) PGM,'EOF    : "FIN DE FICHIER"'
      ELSE IF (ICLASS.EQ.0) THEN
        WRITE (IFM,*) PGM,'ERREUR : "',KVAL(:IVAL),'"'
      ELSE IF (ICLASS.EQ.1) THEN
        WRITE (IFM,*) PGM,'ENTIER :',IVAL
      ELSE IF (ICLASS.EQ.2) THEN
        WRITE (IFM,*) PGM,'REEL   :',RVAL(1)
      ELSE IF (ICLASS.EQ.3) THEN
        WRITE (IFM,*) PGM,'IDENT  : "',KVAL(:IVAL),'"'
      ELSE IF (ICLASS.EQ.4) THEN
        WRITE (IFM,*) PGM,'TEXTE  : "',KVAL(:IVAL),'"'
      ELSE IF (ICLASS.EQ.5) THEN
        WRITE (IFM,*) PGM,'CMPLX  : (',RVAL(1),',',RVAL(2),')'
      ELSE IF (ICLASS.EQ.6) THEN
        WRITE (IFM,*) PGM,'BOOLEAN: ',IVAL
      ELSE IF (ICLASS.GT.6 .AND. ICLASS.LT.17) THEN
        WRITE (IFM,*) PGM,ICLASS,'  : "',KVAL(:IVAL),'"'
      ELSE IF (ICLASS.EQ.17) THEN
        WRITE (IFM,*) PGM,ICLASS,' UNDEF  "',KVAL(:IVAL),'"'
      ELSE
        WRITE (IFM,*) PGM,'CLASSE INDEFINIE ',ICLASS
      END IF
      END
