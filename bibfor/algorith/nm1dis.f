      SUBROUTINE NM1DIS(TM,T,E,ET,ALPH,SY,
     >                  SIGM,DEPS,EPSPM,PM,
     >                  SIG,EPSP,P,DSDEM,DSDEP)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 12/05/98   AUTEUR UFBHHLL C.CHAVANT 
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
C ----------------------------------------------------------------------
C
      IMPLICIT NONE
C ----------------------------------------------------------------------
C          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
C          ON PEUT AVOIR T0 DIFF TREF
C
C
C IN  T        : TEMPERATURE PLUS
C IN  TM       : TEMPERATURE MOINS
C IN  E        : MODULE D YOUNG
C IN  ET       : PENTE D ECROUISSAGE
C IN  ALPH     : COEF DILAT THERMIQUE
C IN  SY       : LIMITE D ELASTICITE INITIALE
C
C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C               UTILISE UNIQUEMENT POUR EVALUER DSDEM
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
C IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS
C
C OUT SIG     : CONTRAINTES PLUS
C OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
C OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSDEM   : DSIG/DEPS TEMPS MOINS
C OUT DSDEP   : DSIG/DEPS TEMPS PLUS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8        T,TM,E,ET,ALPH,SY
      REAL*8        SIGM,DEPS,EPSPM,PM
      REAL*8        SIG,EPSP,P,DSDEM,DSDEP
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8        BETA,EMETSE,DEPMEC,RM,SIGE,S
C     ------------------------------------------------------------------
C     VARIABLES INTERMEDIAIRES
C     ------------------------------------------------------------------
C
      BETA   = E*ET/(E-ET)
      EMETSE = (E-ET)/E
C     ------------------------------------------------------------------
C     DELTA DEFORMATION MECANIQUE
C     ------------------------------------------------------------------
C
      DEPMEC = DEPS-ALPH*(T-TM)
C     ------------------------------------------------------------------
C     LIMITE ELSATIQUE AU TEMPS MOINS
C     ------------------------------------------------------------------
C
      RM = BETA*PM+SY
C     ------------------------------------------------------------------
C     ESTIMATION ELASTIQUE
C     ------------------------------------------------------------------
      SIGE=SIGM+E*DEPMEC
C     ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C     ------------------------------------------------------------------
      IF (ABS(SIGE).LE.RM) THEN
       EPSP = EPSPM
       P = PM
       SIG = SIGE
       DSDEP = E
      ELSE
       IF (SIGE.GT.RM) THEN
        S = 1.D0
       ELSE
        S = -1.D0
       ENDIF
       P = EMETSE*(PM+(S*SIGE-SY)/E)
       EPSP    = EPSPM+S*(P-PM)
       SIG = S*(BETA*P+SY)
       DSDEP = ET
      ENDIF
      IF (ABS(SIGM).LT.RM) THEN
       DSDEM = E
      ELSE
       IF ( ABS(SIGE).LT.RM) THEN
        DSDEM = E
       ELSE
        DSDEM = ET
       ENDIF
      ENDIF
      END
