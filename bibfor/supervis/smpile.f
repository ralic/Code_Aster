      SUBROUTINE SMPILE(CH6,ICLASS,IVAL,RVAL,CVAL,ENCORE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     CH6,                 CVAL
      INTEGER               ICLASS,IVAL
      REAL*8                            RVAL(*)
      LOGICAL                                     ENCORE
C     ------------------------------------------------------------------
C     GESTION DE LA PILE DE TYPE FIFO POUR L'ANALYSEUR SYNTAXIQUE
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
C     ------------------------------------------------------------------
C     ROUTINE(S) UTILISEE(S) :
C         UTMESS
C     ROUTINE(S) FORTRAN     :
C         -
C     ------------------------------------------------------------------
C FIN SMPILE
C     ------------------------------------------------------------------
C
C     ------- COMMUN DEBUG SUPERVISEUR ---------------------------------
      LOGICAL         LDBG
      INTEGER                IFV
      COMMON /CXSU00/ LDBG , IFV
C     ------------------------------------------------------------------
C
      PARAMETER     (MXPILE = 10 )
      INTEGER      PILECL(0:MXPILE), PILEIV(0:MXPILE)
      REAL*8                         PILERV(2,0:MXPILE)
      CHARACTER*80                   PILECV(0:MXPILE)
C
      SAVE PILECL, PILEIV, PILERV, PILECV
      SAVE ILIBRE, IFIRST
C
      INTEGER      NEXT
      LOGICAL      PILVID, PLEIN
C
C     FONCTIONS ON-LINE
      NEXT(I)     = MOD(I+1,MXPILE)
      PILVID(JFIRST,JLIBRE) = JFIRST .EQ. JLIBRE
      PLEIN (JFIRST,JLIBRE) = JFIRST .EQ. JLIBRE+1
C     ------------------------------------------------------------------
      IF (LDBG) WRITE(IFV,*) ' <SMPILE>: OPTION :',CH6(1:6)
      IF ( CH6.EQ.'EMPILE' ) THEN
         IF ( PLEIN(IFIRST,ILIBRE) ) THEN
            CALL UTMESS('F','PROBLEME D''ANALYSEUR','PILE PLEINE')
         ELSE
             PILECL(ILIBRE) = ICLASS
             PILEIV(ILIBRE) = IVAL
             PILECV(ILIBRE) = CVAL
             PILERV(1,ILIBRE) = RVAL(1)
             PILERV(2,ILIBRE) = RVAL(2)
             ILIBRE         = NEXT(ILIBRE)
         ENDIF
C
      ELSEIF ( CH6.EQ.'DEPILE' ) THEN
         IF ( PILVID(IFIRST,ILIBRE) ) THEN
            CALL UTMESS('F','PROBLEME D''ANALYSEUR','PILE VIDE')
            ICLASS  = 99
         ELSE
             ICLASS = PILECL(IFIRST)
             IVAL   = PILEIV(IFIRST)
             CVAL   = PILECV(IFIRST)
             RVAL(1)= PILERV(1,IFIRST)
             RVAL(2)= PILERV(2,IFIRST)
             IFIRST = NEXT(IFIRST)
             ENCORE = .NOT. PILVID(IFIRST,ILIBRE)
         ENDIF
      ELSEIF ( CH6.EQ.'INITPI' ) THEN
         IFIRST = 0
         ILIBRE = 0
      ELSE
         WRITE(06,*) ' * ERREUR * <SMPILE> ',CH6(1:6),' OPTION INVALIDE'
      ENDIF
      END
