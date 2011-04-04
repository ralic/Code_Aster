      SUBROUTINE NM1DCI(FAMI,KPG,KSP,IMATE,EM,EP,SIGM,
     &                  DEPS,VIM,OPTION,MATERI,SIGP,VIP,DSDE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------

      IMPLICIT NONE
C ----------------------------------------------------------------------
C          PLASTICITE VON MISES CINEMATIQUE BILINEAIRE MONODIM
C          ON PEUT AVOIR T0 DIFF TREF

C IN FAMI   : FAMILLE DU POINT DE GAUSS
C IN KPG    :  NUMERO DU POINT DE GAUSS
C IN KSP    :  NUMERO DU SOUS-POINT DE GAUSS
C IN IMATE  : POINTEUR MATERIAU
C IN  EM        : MODULE D YOUNG MOINS
C IN  EP        : MODULE D YOUNG PLUS

C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION MOINS 
C                       - INCREMENT DEFORMATION THERMIQUE
C IN  VIM     : VARIABLE INTERNES MOINS
C IN  OPTION     : OPTION DE CALCUL

C OUT SIG     : CONTRAINTES PLUS
C OUT VIP     : VARIABLE INTERNES PLUS
C OUT DSDE    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8 EP,EM,SIGY
      REAL*8 SIGM,DEPS,VIM(2)
      REAL*8 SIGP,VIP(2),DSDE,SIELEQ
      CHARACTER*16 OPTION
      CHARACTER*(*) FAMI,MATERI
      INTEGER KPG,KSP,IMATE
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8 SIGE,DP,VALRES(2),ETM,ETP,XP,XM,HM,HP

      CHARACTER*2 FB2,CODRES(2)
      CHARACTER*8 NOMECL(2)

      DATA NOMECL/'D_SIGM_E','SY'/
C     ------------------------------------------------------------------
      FB2 = 'FM'
      CALL RCVALB(FAMI,KPG,KSP,'-',IMATE,MATERI,'ECRO_LINE',0,' ',0.D0,
     &            1,NOMECL,VALRES,  CODRES,FB2)
      ETM = VALRES(1)
      HM = EM*ETM/ (EM-ETM)

      CALL RCVALB(FAMI,KPG,KSP,'+',IMATE,MATERI,'ECRO_LINE',0,' ',0.D0,
     &            2,NOMECL,VALRES,CODRES,FB2)
      ETP = VALRES(1)
      HP = EP*ETP/ (EP-ETP)
      SIGY = VALRES(2)
      XM = VIM(1)
C     ------------------------------------------------------------------
      SIGE = EP* (SIGM/EM+DEPS) - HP/HM*XM

      SIELEQ = ABS(SIGE)
C     ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C     ------------------------------------------------------------------
      IF (OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN
        IF (SIELEQ.LE.SIGY) THEN
          VIP(2) = 0.D0
          DSDE = EP
          DP = 0.D0
          XP = HP/HM*XM
          SIGP = EP* (SIGM/EM+DEPS)
          VIP(1) = XP
        ELSE
          VIP(2) = 1.D0
          DP = (SIELEQ-SIGY)/ (EP+HP)
          IF (OPTION.EQ.'FULL_MECA_ELAS') THEN
            DSDE = EP
          ELSE
            DSDE = ETP
          ENDIF
          XP = HP/HM*XM + HP*DP*SIGE/SIELEQ
          SIGP = XP + SIGY*SIGE/SIELEQ
          VIP(1) = XP
        END IF
      END IF
      IF (OPTION(1:10).EQ.'RIGI_MECA_') THEN
        IF ((VIM(2).LT.0.5D0).OR.(OPTION.EQ.'RIGI_MECA_ELAS')) THEN
          DSDE = EP
        ELSE
          DSDE = ETP
        END IF
      END IF
      END
