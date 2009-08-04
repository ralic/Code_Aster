      SUBROUTINE NZEDGA (FAMI,KPG,KSP,NDIM,IMAT,COMPOR,CRIT,
     &                   INSTAM,INSTAP,EPSM,DEPS,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIDEP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/08/2009   AUTEUR MEUNIER S.MEUNIER 
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
C TOLE CRP_20
C TOLE CRP_21

      IMPLICIT NONE
      INTEGER            NDIM,IMAT,IRET,KPG,KSP
      CHARACTER*16      COMPOR(*),OPTION
      CHARACTER*(*)      FAMI
      REAL*8             CRIT(3),INSTAM,INSTAP
      REAL*8             EPSM(6),DEPS(6)
      REAL*8             SIGM(6),VIM(5),SIGP(6),VIP(5),DSIDEP(6,6)

C ----------------------------------------------------------------------
C     REALISE LA LOI DE VON MISES ISOTROPE ET ELASTIQUE POUR LES
C     ELEMENTS METALLURGIQUES EN PETITES DEFORMATIONS
C     MATERIAU ZIRCALOY POUR EDGAR
C
C     AVEC OU SANS PLASTICITE DE TRANSFORMATION
C     AVEC OU SANS RESTAURATION ECROUISSAGE METALLURGIQUE
C     RELATION DE COMPORTEMENT ELASTO-PLASTIQUE OU
C                               ELASTO-VISCOPLASTIQUE
C     ECROUISSAGE ISOTROPE LINEAIRE OU NON LINEAIRE
C
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  IMAT    : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE CARREE
C     IRET    : CODE RETOUR DE LA RESOLUTION DE L'EQUATION SCALAIRE
C               (NZCALC)
C                              IRET=0 => PAS DE PROBLEME
C                              IRET=1 => ECHEC
C
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX YY ZZ XY XZ YZ
C.......................................................................

      INTEGER  JPROL,JVALE,NBVAL(3),MAXVAL,NZ
      INTEGER  NDIMSI,I,J,K,MODE,IRE2,IRET1,IRET2

      REAL*8   PHASE(3),PHASM(3),ZALPHA
      REAL*8   TEMP,DT

      REAL*8   EPSTH,E,DEUXMU,DEUMUM,TROISK
      REAL*8   FMEL,SY(3),H(3),HMOY,HPLUS(3),R(3),RMOY
      REAL*8   THETA(4),EPSTHE(2)
      REAL*8   ETA(3),N(3),UNSURN(3),C(3),M(3),CMOY,MMOY,CR
      REAL*8   DZ(2),DZ1(2),DZ2(2),VI(3),DVIN,VIMOY,DS
      REAL*8   TRANS,KPT(2),ZVARIM,ZVARIP,DELTAZ

      REAL*8   TREPSM,TRDEPS,TRSIGM,TRSIGP
      REAL*8   DVDEPS(6),DVSIGM(6),DVSIGP(6)
      REAL*8   SIGEL(6),SIG0(6),SIELEQ,SIGEPS

      REAL*8   PLASTI,DP,SEUIL

      REAL*8   COEF1,COEF2,COEF3,DV,N0(3),B

      REAL*8   RBID,PRECR,R8PREM
      REAL*8   KRON(6)
      REAL*8   VALRES(12)

      CHARACTER*1 C1
      CHARACTER*2   CODRET(12),TEST
      CHARACTER*8   NOMRES(12),NOMCLE(3),ZIRC(2)

      LOGICAL     RESI,RIGI

      DATA         KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      DATA        ZIRC /'ALPHPUR','ALPHBETA'/

C *******************
C 1 - INITIALISATION
C *******************

      RESI   = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI   = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'

      IF(NDIM.EQ.2) THEN
        NDIMSI=4
      ELSE
        NDIMSI=6
      ENDIF

      DT = INSTAP-INSTAM


C 1.1 - NOMBRE DE PHASES

      NZ=3

C 1.2 - RECUPERATION DES PHASES METALLURGIQUES

      IF (RESI) THEN

        C1='+'
        DO 5 K=1,NZ-1
          CALL RCVARC(' ',ZIRC(K),'+',FAMI,KPG,KSP,PHASE(K),IRE2)
          IF (IRE2.EQ.1) PHASE(K)=0.D0
          CALL RCVARC(' ',ZIRC(K),'-',FAMI,KPG,KSP,PHASM(K),IRE2)
          IF (IRE2.EQ.1) PHASM(K)=0.D0
 5      CONTINUE

      ELSE

        C1='-'
        DO 10 K=1,NZ-1
          CALL RCVARC(' ',ZIRC(K),'-',FAMI,KPG,KSP,PHASE(K),IRE2)
          IF (IRE2.EQ.1) PHASE(K)=0.D0
 10     CONTINUE

      ENDIF

      CALL RCVARC(' ','TEMP',C1,FAMI,KPG,KSP,TEMP,IRET1)
      CALL VERIFT(FAMI,KPG,KSP,C1,IMAT,'ELAS_META',2,EPSTHE,IRET2)

      ZALPHA=PHASE(1)+PHASE(2)
      PHASE(NZ)=1.D0-ZALPHA

C 1.3 - TEST SUR LES PHASES

      PRECR=R8PREM()
      DO 15 K=1,NZ
        IF (PHASE(K).LE.PRECR) PHASE(K)=0.D0
        IF (PHASE(K).GE.1.D0)  PHASE(K)=1.D0
 15    CONTINUE
      IF (ZALPHA.LE.PRECR) ZALPHA=0.D0
      IF (ZALPHA.GE.1.D0)  ZALPHA=1.D0

C ****************************************
C 2 - RECUPERATION DES CARACTERISTIQUES
C ****************************************

C 2.1 - ELASTIQUE ET THERMIQUE

      NOMRES(1)='E'
      NOMRES(2)='NU'
      NOMRES(3)='F_ALPHA'
      NOMRES(4)='C_ALPHA'
      NOMRES(5)='PHASE_REFE'
      NOMRES(6)='EPSF_EPSC_TREF'

      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS_META',0,' ',0.D0,
     &            6,NOMRES,VALRES,CODRET,'F ')
      DEUMUM = VALRES(1)/(1.D0+VALRES(2))

      CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','ELAS_META',0,' ',0.D0,
     &            6,NOMRES,VALRES,CODRET,'F ')
      EPSTH = PHASE(NZ)*(EPSTHE(1)-(1.D0-VALRES(5))*VALRES(6))
     &     + ZALPHA*(EPSTHE(2) + VALRES(5)*VALRES(6))
      E      = VALRES(1)
      DEUXMU = E/(1.D0+VALRES(2))
      TROISK = E/(1.D0-2.D0*VALRES(2))

      IF (COMPOR(1)(1:4) .EQ. 'META' ) THEN

        PLASTI=VIM(4)

C 2.2 - LOI DES MELANGES

        IF(COMPOR(1)(1:6).EQ.'META_P') THEN
          NOMRES(1) ='F1_SY'
          NOMRES(2) ='F2_SY'
          NOMRES(3) ='C_SY'
          NOMRES(4) ='SY_MELAN'
        ENDIF

        IF(COMPOR(1)(1:6).EQ.'META_V') THEN
          NOMRES(1) ='F1_S_VP'
          NOMRES(2) ='F2_S_VP'
          NOMRES(3) ='C_S_VP'
          NOMRES(4) ='S_VP_MEL'
        ENDIF

        CALL RCVALB(FAMI,1,1,'+',IMAT,' ','ELAS_META',
     &              1,'META',ZALPHA,1,
     &              NOMRES(4),FMEL,CODRET(4),'  ')
        IF (CODRET(4).NE.'OK') FMEL = ZALPHA

C 2.3 - LIMITE D ELASTICITE

        CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','ELAS_META',0,' ',0.D0,
     &              3,NOMRES,SY,CODRET,'F ')

        IF (RESI) THEN

C 2.4 - RESTAURATION D ECROUISSAGE

          IF(COMPOR(1)(1:12).EQ.'META_P_IL_RE'  .OR.
     &      COMPOR(1)(1:15).EQ.'META_P_IL_PT_RE' .OR.
     &      COMPOR(1)(1:12).EQ.'META_V_IL_RE'    .OR.
     &      COMPOR(1)(1:15).EQ.'META_V_IL_PT_RE' .OR.
     &      COMPOR(1)(1:13).EQ.'META_P_INL_RE'   .OR.
     &      COMPOR(1)(1:16).EQ.'META_P_INL_PT_RE'.OR.
     &      COMPOR(1)(1:13).EQ.'META_V_INL_RE'   .OR.
     &      COMPOR(1)(1:16).EQ.'META_V_INL_PT_RE') THEN

            NOMRES(1) ='C_F1_THETA'
            NOMRES(2) ='C_F2_THETA'
            NOMRES(3) ='F1_C_THETA'
            NOMRES(4) ='F2_C_THETA'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_RE',0,'  ',
     &                 0.D0,4,NOMRES,THETA,CODRET,'F ')

          ELSE

            DO 20 I=1,4
              THETA(I)=1.D0
 20         CONTINUE

          ENDIF

C 2.5 - VISCOSITE

          IF (COMPOR(1)(1:6) .EQ. 'META_V') THEN

            NOMRES(1) = 'F1_ETA'
            NOMRES(2) = 'F2_ETA'
            NOMRES(3) = 'C_ETA'

            NOMRES(4) = 'F1_N'
            NOMRES(5) = 'F2_N'
            NOMRES(6) = 'C_N'

            NOMRES(7) ='F1_C'
            NOMRES(8) ='F2_C'
            NOMRES(9) ='C_C'

            NOMRES(10) = 'F1_M'
            NOMRES(11) = 'F2_M'
            NOMRES(12) = 'C_M'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_VISC',0,
     &                 ' ',0.D0,6,NOMRES,VALRES,CODRET,'F ')

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_VISC',0,' ',
     &                 0.D0,6,NOMRES(7),VALRES(7),CODRET(7),'  ')

            DO  25 K=1,NZ
              ETA(K) = VALRES(K)
              N(K) = VALRES(NZ+K)
              UNSURN(K)=1/N(K)
              IF (CODRET(2*NZ+K) .NE. 'OK') VALRES(2*NZ+K)=0.D0
              C(K) =VALRES(2*NZ+K)
              IF (CODRET(3*NZ+K) .NE. 'OK') VALRES(3*NZ+K)=20.D0
              M(K) = VALRES(3*NZ+K)
 25         CONTINUE

          ELSE

            DO 30 K=1,NZ
              ETA(K) = 0.D0
              N(K)= 20.D0
              UNSURN(K)= 1.D0
              C(K) = 0.D0
              M(K) = 20.D0
 30         CONTINUE

          ENDIF

C 2.6 - CALCUL DE VIM+DG-DS ET DE RMOY

          DO 35 K=1,NZ-1
            DZ(K)= PHASE(K)-PHASM(K)
            IF (DZ(K).GE.0.D0) THEN
              DZ1(K)=DZ(K)
              DZ2(K)=0.D0
            ELSE
              DZ1(K)=0.D0
              DZ2(K)=-DZ(K)
            ENDIF
 35       CONTINUE

          IF(PHASE(NZ).GT.0.D0)THEN
            DVIN=0.D0
            DO 40 K=1,NZ-1
              DVIN=DVIN+DZ2(K)*(THETA(2+K)*VIM(K)-VIM(NZ))/PHASE(NZ)
 40         CONTINUE
            VI(NZ)=VIM(NZ)+DVIN
            VIMOY=PHASE(NZ)*VI(NZ)
          ELSE
            VI(NZ)   = 0.D0
            VIMOY=0.D0
          ENDIF

          DO 45 K=1,NZ-1
            IF (PHASE(K).GT.0.D0)THEN
              DVIN=DZ1(K)*(THETA(K)*VIM(NZ)-VIM(K))/PHASE(K)
              VI(K)=VIM(K)+DVIN
              VIMOY=VIMOY+PHASE(K)*VI(K)
            ELSE
              VI(K)=0.D0
            ENDIF
 45       CONTINUE

C 2.7 - RESTAURATION D ORIGINE VISQUEUSE

          CMOY=0.D0
          MMOY=0.D0
          DO 50 K=1,NZ
            CMOY=CMOY+PHASE(K)*C(K)
            MMOY=MMOY+PHASE(K)*M(K)
 50       CONTINUE

          CR=CMOY*VIMOY
          IF (CR .LE. 0.D0) THEN
            DS=0.D0
          ELSE
            DS= DT*(CR**MMOY)
          ENDIF

          DO 55 K=1,NZ
            IF (PHASE(K).GT.0.D0)THEN
              VI(K)=VI(K)-DS
              IF(VI(K).LE.0.D0) VI(K)=0.D0
            ENDIF
 55       CONTINUE

C 2.8 - PLASTICITE DE TRANSFORMATION

          TRANS = 0.D0
          IF (COMPOR(1)(1:12) .EQ. 'META_P_IL_PT'    .OR.
     &      COMPOR(1)(1:13) .EQ. 'META_P_INL_PT'   .OR.
     &      COMPOR(1)(1:15) .EQ. 'META_P_IL_PT_RE' .OR.
     &      COMPOR(1)(1:16) .EQ. 'META_P_INL_PT_RE'.OR.
     &      COMPOR(1)(1:12) .EQ. 'META_V_IL_PT'    .OR.
     &      COMPOR(1)(1:13) .EQ. 'META_V_INL_PT'   .OR.
     &      COMPOR(1)(1:15) .EQ. 'META_V_IL_PT_RE' .OR.
     &      COMPOR(1)(1:16) .EQ. 'META_V_INL_PT_RE') THEN

            NOMRES(1) = 'F1_K'
            NOMRES(2) = 'F2_K'
            NOMRES(3) = 'F1_D_F_META'
            NOMRES(4) = 'F2_D_F_META'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_PT',0,' ',
     &                 0.D0,2,NOMRES,VALRES,CODRET ,'F ')

            DO 60 K=1,NZ-1
              KPT (K) = VALRES(K)
              ZVARIM  = PHASM(K)
              ZVARIP  = PHASE(K)
              DELTAZ = (ZVARIP - ZVARIM)
              IF (DELTAZ.GT.0.D0) THEN
                J = 2+K
                CALL RCVALB(FAMI,1,1,'+',IMAT,' ','META_PT',
     &                     1,'META',ZALPHA,1,
     &                      NOMRES(J),VALRES(J),CODRET(J), 'F ')
                TRANS = TRANS + KPT(K)*VALRES(J)*(ZVARIP-ZVARIM)
              ENDIF
 60         CONTINUE

          ENDIF

        ELSE

          DO 65 K=1,NZ
            VI(K)=VIM(K)
 65       CONTINUE

        ENDIF

C 2.9 - CALCUL DE HMOY ET RMOY (ON INCLUE LE SIGY)

        IF(COMPOR(1)(1:9).EQ.'META_P_IL' .OR.
     &    COMPOR(1)(1:9).EQ.'META_V_IL' )THEN

          NOMRES(1) ='F1_D_SIGM_EPSI'
          NOMRES(2) ='F2_D_SIGM_EPSI'
          NOMRES(3) ='C_D_SIGM_EPSI'

          CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_ECRO_LINE',0,
     &               ' ',0.D0,3,NOMRES,H,CODRET,'F ')

          H(1)=H(1)*E/(E-H(1))
          H(2)=H(2)*E/(E-H(2))
          H(3)=H(3)*E/(E-H(3))

          DO 70 K=1,NZ
            R(K)=H(K)*VI(K)+SY(K)
 70       CONTINUE

        ENDIF

        IF (COMPOR(1)(1:10) .EQ. 'META_P_INL' .OR.
     &     COMPOR(1)(1:10) .EQ. 'META_V_INL') THEN

          NOMCLE(1)='SIGM_F1'
          NOMCLE(2)='SIGM_F2'
          NOMCLE(3)='SIGM_C'

          IF (IRET1.EQ.1) CALL U2MESS('F','CALCULEL_31')
          DO 75 K=1,NZ
            CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TEMP,
     &                 JPROL,JVALE,NBVAL(K),RBID)
            CALL RCFONC('V','META_TRACTION',JPROL,JVALE,NBVAL(K),
     &               RBID,RBID,RBID,VI(K),R(K),H(K),RBID,RBID,RBID)
            R(K) = R(K) + SY(K)
 75      CONTINUE

         MAXVAL = MAX(NBVAL(1),NBVAL(2),NBVAL(3))

        ENDIF

        IF (ZALPHA.GT. 0.D0) THEN
          RMOY=PHASE(1)*R(1)+PHASE(2)*R(2)
          RMOY = RMOY/ZALPHA
          HMOY=PHASE(1)*H(1)+PHASE(2)*H(2)
          HMOY = HMOY/ZALPHA
        ELSE
          RMOY = 0.D0
          HMOY = 0.D0
        ENDIF
        RMOY =(1.D0-FMEL)*R(NZ)+FMEL*RMOY
        HMOY = (1.D0-FMEL)*H(NZ)+FMEL*HMOY

      ELSE

        TRANS =0.D0

      ENDIF

C ********************************
C 3 - DEBUT DE L ALGORITHME
C ********************************

      TRDEPS = (DEPS(1)+DEPS(2)+DEPS(3))/3.D0
      TREPSM = (EPSM(1)+EPSM(2)+EPSM(3))/3.D0
      TRSIGM = (SIGM(1)+SIGM(2)+SIGM(3))/3.D0
      TRSIGP = TROISK*(TREPSM+TRDEPS)-TROISK*EPSTH

      DO 80 I=1,NDIMSI
        DVDEPS(I)   = DEPS(I) - TRDEPS * KRON(I)
        DVSIGM(I)   = SIGM(I) - TRSIGM * KRON(I)
 80   CONTINUE

      SIELEQ = 0.D0
      DO 85 I=1,NDIMSI
        SIGEL(I) = DEUXMU*DVSIGM(I)/DEUMUM + DEUXMU*DVDEPS(I)
        SIELEQ   = SIELEQ + SIGEL(I)**2
 85   CONTINUE
      SIELEQ     = SQRT(1.5D0*SIELEQ)

      IF (SIELEQ .GT. 0.D0) THEN
        DO 90 I=1,NDIMSI
          SIG0(I) = SIGEL(I)/SIELEQ
 90     CONTINUE
      ELSE
        DO 95 I=1,NDIMSI
          SIG0(I) = 0.D0
 95     CONTINUE
      ENDIF

C ************************
C 4 - RESOLUTION
C ************************

      IF (RESI) THEN

C 4.1 - COMPORTEMENT ELASTIQUE - CALCUL DE SIGMA

        IF (COMPOR(1)(1:4) .EQ. 'ELAS') THEN

          DO 100 I = 1,NDIMSI
            SIGP(I) = SIGEL(I)+TRSIGP*KRON(I)
 100      CONTINUE

C 4.2 - COMPORTEMENT PLASTIQUE
C 4.2.1 - CALCUL DE DP

        ELSE IF (COMPOR(1)(1:4) .EQ. 'META') THEN

          SEUIL= SIELEQ-(1.5D0*DEUXMU*TRANS+1.D0)*RMOY

          IF (SEUIL.LT.0.D0) THEN
            VIP(4) = 0.D0
            DP = 0.D0
          ELSE
            VIP(4) = 1.D0
            CALL NZCALC(CRIT,PHASE,NZ,FMEL,SEUIL,DT,TRANS,
     &                  HMOY,DEUXMU,ETA,UNSURN,DP,IRET)
            IF(IRET.EQ.1) GOTO 9999

C DANS LE CAS NON LINEAIRE
C VERIFICATION QU ON EST DANS LE BON INTERVALLE

            IF(COMPOR(1)(1:10) .EQ. 'META_P_INL' .OR.
     &        COMPOR(1)(1:10) .EQ. 'META_V_INL') THEN

              DO 105 J=1,MAXVAL
                TEST='OK'
                DO 110 K=1,NZ
                  IF(PHASE(K).GT.0.D0) THEN
                    VIP(K)=VI(K)+DP
                    HPLUS(K)=H(K)
                    CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TEMP,
     &                         JPROL,JVALE,NBVAL(K),RBID)
                    CALL RCFONC('V','META_TRACTION',JPROL,JVALE,
     &                          NBVAL(K),RBID,RBID,RBID,VIP(K),
     &                          R(K),H(K),RBID,RBID,RBID)
                     R(K) = R(K) + SY(K)
                    IF(ABS(H(K)-HPLUS(K)).GT.PRECR) TEST='NO'
                  ENDIF
 110            CONTINUE
                IF (TEST.EQ.'OK') GO TO 600

                HMOY=0.D0
                RMOY=0.D0
                IF(ZALPHA.GT.0.D0) THEN
                  DO 115 K=1,NZ-1
                    IF(PHASE(K).GT.0.D0) THEN
                      RMOY = RMOY + PHASE(K)*(R(K)-H(K)*DP)
                      HMOY = HMOY + PHASE(K)*H(K)
                    ENDIF
 115              CONTINUE
                  RMOY=FMEL*RMOY/ZALPHA
                  HMOY=FMEL*HMOY/ZALPHA
                ENDIF
                IF (PHASE(NZ).GT.0.D0) THEN
                  RMOY = (1.D0-FMEL)*(R(NZ)-H(NZ)*DP)+RMOY
                  HMOY = (1.D0-FMEL)*H(NZ)+HMOY
                ENDIF
                SEUIL= SIELEQ - (1.5D0*DEUXMU*TRANS + 1.D0)*RMOY
                CALL NZCALC(CRIT,PHASE,NZ,FMEL,SEUIL,DT,TRANS,
     &                      HMOY,DEUXMU,ETA,UNSURN,DP,IRET)
                IF(IRET.EQ.1) GOTO 9999
  105         CONTINUE
              CALL ASSERT((TEST.NE.'NO').OR.(J.NE.MAXVAL))
  600         CONTINUE
            ENDIF
          ENDIF

C 4.2.2 - CALCUL DE SIGMA

          PLASTI = VIP(4)

          DO 120 I = 1,NDIMSI
            DVSIGP(I) = SIGEL(I) - 1.5D0*DEUXMU*DP*SIG0(I)
            DVSIGP(I) = DVSIGP(I)/(1.5D0*DEUXMU*TRANS + 1.D0)
            SIGP(I) = DVSIGP(I) + TRSIGP*KRON(I)
  120     CONTINUE

C 4.2.3 - CALCUL DE VIP ET RMOY

          DO 125 K=1,NZ
            IF(PHASE(K).GT.0.D0) THEN
              VIP(K)=VI(K)+DP
            ELSE
              VIP(K)=0.D0
            ENDIF
 125      CONTINUE

          VIP(5)=0.D0
          IF(PHASE(NZ).GT.0.D0)THEN

            IF(COMPOR(1)(1:9).EQ.'META_P_IL'.OR.
     &        COMPOR(1)(1:9).EQ.'META_V_IL')THEN
              VIP(5)=VIP(5)+(1-FMEL)*H(NZ)*VIP(NZ)
            ENDIF

            IF(COMPOR(1)(1:10).EQ.'META_P_INL'.OR.
     &        COMPOR(1)(1:10).EQ.'META_V_INL')THEN
              VIP(5)=VIP(5)+(1-FMEL)*(R(NZ)-SY(NZ))
            ENDIF

          ENDIF

          IF (ZALPHA.GT.0.D0) THEN
            DO 130 K=1,NZ-1

              IF(COMPOR(1)(1:9).EQ.'META_P_IL'.OR.
     &          COMPOR(1)(1:9).EQ.'META_V_IL')THEN
                VIP(5)=VIP(5)+FMEL*PHASE(K)*H(K)*VIP(K)/ZALPHA
              ENDIF

              IF(COMPOR(1)(1:10).EQ.'META_P_INL'.OR.
     &          COMPOR(1)(1:10).EQ.'META_V_INL')THEN
                VIP(5)=VIP(5)+FMEL*PHASE(K)*(R(K)-SY(K))/ZALPHA
              ENDIF

 130        CONTINUE
          ENDIF
        ENDIF
      ENDIF

C *******************************
C 5 - MATRICE TANGENTE DSIGDF
C *******************************

      IF (RIGI) THEN

        MODE=2
        IF (COMPOR(1)(1:6).EQ.'META_V') MODE=1

C 5.1 - MATRICE ELASTIQUE

        CALL MATINI(6,6,0.D0,DSIDEP)

        DO 140 I=1,NDIMSI
          DSIDEP(I,I) =1.D0
 140    CONTINUE

        DO 145 I=1,3
          DO 150 J=1,3
            DSIDEP(I,J) = DSIDEP(I,J)-1.D0/3.D0
 150      CONTINUE
 145    CONTINUE

        IF (OPTION(1:9) .EQ. 'FULL_MECA') THEN
          COEF1=(1.5D0*DEUXMU*TRANS+1.D0)
        ELSE
          COEF1=1.D0
        ENDIF

        DO 155 I=1,NDIMSI
          DO 160 J=1,NDIMSI
            DSIDEP(I,J)=DSIDEP(I,J)*DEUXMU/COEF1
 160      CONTINUE
 155    CONTINUE

C 5.2 - PARTIE PLASTIQUE

        B=1.D0
        COEF2 =0.D0
        COEF3=0.D0
        IF (COMPOR(1)(1:4) .EQ. 'META') THEN

          IF (PLASTI .GE. 0.5D0) THEN

            IF (OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN

              SIGEPS = 0.D0
              DO 165 I = 1,NDIMSI
                SIGEPS = SIGEPS + DVSIGP(I)*DVDEPS(I)
 165          CONTINUE

              IF ((MODE.EQ.1) .OR. ((MODE .EQ. 2) .AND.
     &           (SIGEPS.GE.0.D0))) THEN

                B = 1.D0-(1.5D0*DEUXMU*DP/SIELEQ)
                DV=0.D0
                IF (MODE .EQ.1) THEN
                  DO 170 K=1,NZ
                    N0(K) = (1-N(K))/N(K)
 170              CONTINUE
                  DV = (1-FMEL)*PHASE(NZ)*(ETA(NZ)/N(NZ)/DT)
     &              * ((DP/DT)**N0(NZ))
                  IF (ZALPHA.GT.0.D0) THEN
                    DO 175 K=1,NZ-1
                      IF (PHASE(K).GT. 0.D0)
     &               DV = DV+FMEL*(PHASE(K)/ZALPHA)
     &                  * (ETA(K)/N(K)/DT)*((DP/DT)**N0(K))
 175                CONTINUE
                  ENDIF
                ENDIF

                COEF2 = HMOY +DV
                COEF2 = (1.5D0*DEUXMU*TRANS+1.D0)*COEF2
                COEF2 = (1.5D0*DEUXMU)+COEF2
                COEF2 = 1/COEF2 - DP/SIELEQ
                COEF2 =((1.5D0*DEUXMU)**2)*COEF2

              ENDIF

            ENDIF

            IF (OPTION(1:14) .EQ. 'RIGI_MECA_TANG') THEN
              IF (MODE .EQ. 2)
     &           COEF2 = ((1.5D0*DEUXMU)**2)/(1.5D0*DEUXMU+HMOY)
            ENDIF

            COEF3 = COEF2/COEF1

          ENDIF

        ENDIF

        DO 180 I=1,NDIMSI
          DO 185 J=1,NDIMSI
            DSIDEP(I,J) = DSIDEP(I,J)*B
 185      CONTINUE
 180    CONTINUE

        DO 190 I = 1,3
          DO 195 J =1,3
            DSIDEP(I,J) = DSIDEP(I,J)+TROISK/3.D0
 195      CONTINUE
 190    CONTINUE

        DO 200 I=1,NDIMSI
          DO 205 J=1,NDIMSI
            DSIDEP(I,J) = DSIDEP(I,J)- COEF3*SIG0(I)*SIG0(J)
 205      CONTINUE
 200    CONTINUE

      ENDIF

 9999 CONTINUE

      END
