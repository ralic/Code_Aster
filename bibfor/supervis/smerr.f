      SUBROUTINE SMERR (IETAT,ICLASS,IVAL,RVAL,CVAL)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           IETAT,ICLASS,IVAL
      REAL*8                              RVAL(*)
      CHARACTER*(*)                            CVAL
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C     ECRITURE D'UN MESSAGE D'ERREUR LIE A L'ANALYSEUR "SM"
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         LXINFO  UTMESS  PRSOUL
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN SMERR
C     ------------------------------------------------------------------
      INTEGER        JLIG, JCOL , LG
      CHARACTER*72   CHAINE, LIGNE
C     ------------------------------------------------------------------
      CHAINE = CVAL
      LG     = MIN (72,IVAL)
C
      CALL LXINFO(LIGNE,JLIG,JCOL)
C
      IFE = IUNIFI('ERREUR')
C     IFM ETAIT MESSAGE, IL DEVIENT RESULTAT LE 18 01 91
      IFM = IUNIFI('RESULTAT')
      IF (IFE.GT.0)
     +             WRITE(IFE,'(1X,I5,''!'',A,''!'')' ) JLIG, LIGNE
      IF ( IFM.GT.0 .AND. IFE.NE.IFM )
     +             WRITE(IFM,'(1X,I5,''!'',A,''!'')' ) JLIG, LIGNE
C
      IF ( IETAT .EQ. 0 ) THEN
         CALL PRSOUL('*', JCOL-IVAL,IVAL)
         CALL UTMESS('E','SMERR','ERREUR SYNTAXIQUE GRAVE : '//
     +                   '"'//CHAINE(1:LG)//'" CHAINE NON ATTENDUE')
      ELSEIF ( ICLASS .EQ. 00 ) THEN
         CALL PRSOUL('*', JCOL-IVAL,IVAL)
         CALL UTMESS('E','SMERR','ERREUR SYNTAXIQUE GRAVE : '//
     +                   '"'//CHAINE(1:LG)//'" CHAINE INCOHERENTE')
      ENDIF
C
      END
