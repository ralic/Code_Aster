      SUBROUTINE NM1DIS(IMATE,TEMPM,TEMPP,TREF,EM,EP,ALPHAM,ALPHAP,SIGM,
     &                  DEPS,VIM,OPTION,COMPOR,SIGP,VIP,DSDE)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/01/2003   AUTEUR JMBHH01 J.M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C          PLASTICITE VON MISES ISOTROPE BILINEAIRE MONODIM
C          ON PEUT AVOIR T0 DIFF TREF


C IN  T        : TEMPERATURE PLUS
C IN  TM       : TEMPERATURE MOINS
C IN  E        : MODULE D YOUNG
C IN  ET       : PENTE D ECROUISSAGE
C IN  ALPH     : COEF DILAT THERMIQUE
C IN  SY       : LIMITE D ELASTICITE INITIALE

C IN  SIGM    : CONTRAINTE AU TEMPS MOINS
C               UTILISE UNIQUEMENT POUR EVALUER DSDEM
C IN  DEPS    : DEFORMATION  TOTALE PLUS - DEFORMATION TOTALE MOINS
C IN  EPSPM   : DEFORMATION  PLASTIQUE MOINS
C IN  PM      : DEFORMATION  PLASTIQUE CUMULEE MOINS

C OUT SIG     : CONTRAINTES PLUS
C OUT EPSP    : DEFORMATION  PLASTIQUE PLUS
C OUT P       : DEFORMATION  PLASTIQUE CUMULEE PLUS
C OUT DSDE    : DSIG/DEPS
C     ------------------------------------------------------------------
C     ARGUMENTS
C     ------------------------------------------------------------------
      REAL*8 TEMPP,TEMPM,EM,EP,ET,ALPHAM,ALPHAP,SIGY,TREF
      REAL*8 SIGM,DEPS,PM,VIM(*),VIP(*),RESU
      REAL*8 SIGP,DSDE,RBID
      CHARACTER*16 OPTION,COMPOR(*)
      INTEGER IMATE
C     ------------------------------------------------------------------
C     VARIABLES LOCALES
C     ------------------------------------------------------------------
      REAL*8 RPRIM,RM,SIGE,VALPAR,VALRES(2),DEPSTH,AIRERP,DUM
      REAL*8 SIELEQ,RP,DP,NU
      INTEGER JPROLM,JVALEM,NBVALM,NBVALP,NBPAR,JPROLP,JVALEP
      CHARACTER*2 BL2,FB2,CODRES(2)
      CHARACTER*8 NOMPAR,NOMECL(2),TYPE
      DATA NOMECL/'D_SIGM_EPSI','SY'/


      BL2 = '  '
      FB2 = 'FM'

      NBPAR = 1
      NOMPAR = 'TEMP'
      PM = VIM(1)

C --- CARACTERISTIQUES ECROUISSAGE LINEAIRE

      IF ((COMPOR(1).EQ.'VMIS_ISOT_LINE') .OR.
     &   (COMPOR(1).EQ.'GRILLE_ISOT_LINE')) THEN
        VALPAR = TEMPP
        CALL RCVALA(IMATE,'ECRO_LINE',NBPAR,NOMPAR,VALPAR,1,NOMECL,
     &              VALRES,CODRES,FB2)
        CALL RCVALA(IMATE,'ECRO_LINE',NBPAR,NOMPAR,VALPAR,1,NOMECL(2),
     &              VALRES(2),CODRES(2),BL2)
        IF (CODRES(2).NE.'OK') VALRES(2) = 0.D0
        ET = VALRES(1)
        SIGY = VALRES(2)
        RPRIM = EP*ET/ (EP-ET)
        RM = RPRIM*VIM(1) + SIGY

C --- CARACTERISTIQUES ECROUISSAGE DONNE PAR COURBE DE TRACTION

      ELSE IF (COMPOR(1).EQ.'VMIS_ISOT_TRAC') THEN
        VALPAR = TEMPM
        CALL RCTYPE(IMATE,1,NOMPAR,VALPAR,RESU,TYPE)
        CALL RCTRAC(IMATE,'TRACTION','SIGM',RESU,JPROLM,JVALEM,NBVALM,
     &              EM)
        VALPAR = TEMPP
        CALL RCTYPE(IMATE,1,NOMPAR,VALPAR,RESU,TYPE)
        CALL RCTRAC(IMATE,'TRACTION','SIGM',RESU,JPROLP,JVALEP,NBVALP,
     &              EP)
        CALL RCFONC('S','TRACTION',JPROLP,JVALEP,NBVALP,SIGY,DUM,DUM,
     &              DUM,DUM,DUM,DUM,DUM,DUM)
        CALL RCFONC('V','TRACTION',JPROLP,JVALEP,NBVALP,RBID,RBID,RBID,
     &              VIM(1),RM,RPRIM,AIRERP,RBID,RBID)
        ET=RPRIM
      END IF
C     ------------------------------------------------------------------
C     ESTIMATION ELASTIQUE
C     ------------------------------------------------------------------
      DEPSTH = ALPHAP* (TEMPP-TREF) - ALPHAM* (TEMPM-TREF)
      SIGE = EP* (SIGM/EM+DEPS-DEPSTH)
      SIELEQ = ABS(SIGE)
C     ------------------------------------------------------------------
C     CALCUL EPSP, P , SIG
C     ------------------------------------------------------------------
      IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA') THEN
        IF (SIELEQ.LE.RM) THEN
          DP=0.D0
          SIGP = SIGE
          DSDE = EP
          VIP(2) = 0.D0
          VIP(1) = VIM(1) 
          SIGP = SIGE
        ELSE
          VIP(2) = 1.D0
          IF ((COMPOR(1).EQ.'VMIS_ISOT_LINE') .OR.
     &     (COMPOR(1).EQ.'GRILLE_ISOT_LINE')) THEN
            DP = ABS(SIGE) - RM
            DP = DP/ (RPRIM+EP)
            RP = SIGY + RPRIM* (PM+DP)
            DSDE = ET
          ELSE
            NU=0.5D0
            CALL RCFONC('E','TRACTION',JPROLP,JVALEP,NBVALP,RBID,EP,
     &                  NU,VIM(1),RP,RPRIM,AIRERP,ABS(SIGE),DP)
            DSDE = EP*RPRIM/ (EP+RPRIM)
          END IF
          VIP(1) = VIM(1) + DP
          SIGP = SIGE/ (1.D0+EP*DP/RP)
        END IF
      END IF
      IF (OPTION.EQ.'RIGI_MECA_TANG') THEN
        IF (VIM(2).LT.0.5D0) THEN
          DSDE = EP
        ELSE
          DSDE = ET
        END IF
      END IF
      END
