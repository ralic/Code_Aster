      SUBROUTINE LCGDPM (FAMI,KPG,KSP,NDIM,IMAT,COMPOR,CRIT,
     &                   INSTAM,INSTAP,FM,DF,SIGM,VIM,
     &                   OPTION,SIGP,VIP,DSIGDF,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
      CHARACTER*16      COMPOR(3),OPTION
      CHARACTER*(*)      FAMI
      REAL*8             CRIT(3),INSTAM,INSTAP
      REAL*8             DF(3,3),FM(3,3)
      REAL*8             VIM(8),VIP(8)
      REAL*8             SIGM(*),SIGP(*),DSIGDF(6,3,3)

C.......................................................................
C       INTEGRATION DE LA LOI DE COMPORTEMENT PLASTIQUE ISOTROPE
C              EN GRANDES DEFORMATIONS DE TYPE SIMO-MIEHE
C                       POUR ACIER
C.......................................................................
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DF      : INCREMENT DU GRADIENT DE LA TRANSFORMATION
C IN  FM      : GRADIENT DE LA TRANSFORMATION A L INSTANT PRECEDENT
C IN  SIGM    : CONTRAINTES DE CAUCHY A L INSTANT PTECEDENT
C IN  VIM     : VARIABLES INTERNES A L INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES DE CAUCHY A L'INSTANT ACTUEL
C OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIGDF  : DERIVEE DE SIGMA PAR RAPPORT A F
C
C          ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C          L'ORDRE :  XX,YY,ZZ,XY,XZ,YZ
C.......................................................................

      INTEGER  JPROL,JVALE,NBVAL(5),MAXVAL,NZ
      INTEGER  I,J,K,L,MODE,IRE2,IRET1,IRET2
      INTEGER  IND(3,3),NBR

      REAL*8   PHASE(5),PHASM(5),ZALPHA
      REAL*8   TEMP,DT,EPSTHE(2)

      REAL*8   TTRG,EPSTH,E,NU,MU,MUM,TROISK
      REAL*8   FMEL,SY(5),H(5),HMOY,HPLUS(5),R(5),RMOY
      REAL*8   THETA(8)
      REAL*8   ETA(5),N(5),UNSURN(5),C(5),M(5),CMOY,MMOY,CR
      REAL*8   DZ(4),DZ1(4),DZ2(4),VI(5),DVIN,VIMOY,DS
      REAL*8   TRANS,KPT(4),ZVARIM,ZVARIP,DELTAZ

      REAL*8   JM,JP,DJ,DFB(3,3)
      REAL*8   TAUM(6),DVTAUM(6),TRTAUM,EQTAUM
      REAL*8   TAUP(6),DVTAUP(6),TRTAUP
      REAL*8   TAU(6),DVTAU(6),TRTAU,EQTAU
      REAL*8   BEM(6),BEL(6),DVBEL(6),TRBEL
      REAL*8   DVTEL(6),EQTEL

      REAL*8   PLASTI,DP,SEUIL,MUTILD

      REAL*8   JE2,JE3,XM,XP,SOL(3)

      REAL*8   COEFF1,COEFF2,COEFF3,COEFF4,COEFF5,COEFF6,COEFF7
      REAL*8   COEFF8,COEFF9,DV,RB,N0(5)
      REAL*8   MAT0(3,3),MAT1(3,3),MAT2(6,3,3),MAT3(3,3)

      REAL*8   RBID,PRECR,R8PREM
      REAL*8   KR(6),PDTSCA(6)
      REAL*8   VALRES(20)

      CHARACTER*1 C1
      CHARACTER*2 CODRET(20),TEST
      CHARACTER*8 NOMRES(20),NOMCLE(5),ACIER(4)

      LOGICAL     RESI,RIGI

      DATA        KR/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA        PDTSCA/1.D0,1.D0,1.D0,2.D0,2.D0,2.D0/
      DATA        IND/1,4,5,
     &                4,2,6,
     &                5,6,3/

      DATA ACIER /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/

C SIGNIFICATION DES VARIABLES LOCALES
C
C  FB(I,J)=(J**(-1/3))*F(I,J)
C  GPM(I,J) : DEFORMATION PLASTIQUE LAGRANGIENNE A L'INSTANT PRECEDENT
C  B(I,J)=FB(I,K)*GPM(K,L)*FB(J,L)
C  BETR(1)=B(1,1), BETR(2)=B(2,2),...,BETR(6)=B(2,3)=B(3,2)
C
C TENSEUR DE DEFORMATION ELASTIQUE EULERIEN : BE
C TENSEUR DE DEFORMATION PLASTIQUE LAGRANGIEN : GP
C TAU : TENSEUR DE KIRSHHOFF

C *******************
C 1 - INITIALISATION
C *******************

      RESI   = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI   = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'

      DT=INSTAP-INSTAM


C 1.1 - NOMBRE DE PHASES

      NZ=5

C 1.2 - RECUPERATION DES PHASES

      IF (RESI) THEN

        C1='+'
        DO 5 K=1,NZ-1
          CALL RCVARC(' ',ACIER(K),'+',FAMI,KPG,KSP,PHASE(K),IRE2)
          IF (IRE2.EQ.1) PHASE(K)=0.D0
          CALL RCVARC(' ',ACIER(K),'-',FAMI,KPG,KSP,PHASM(K),IRE2)
          IF (IRE2.EQ.1) PHASM(K)=0.D0
 5      CONTINUE

      ELSE

        C1='-'
        DO 10 K=1,NZ-1
          CALL RCVARC(' ',ACIER(K),'-',FAMI,KPG,KSP,PHASE(K),IRE2)
          IF (IRE2.EQ.1) PHASE(K)=0.D0
 10     CONTINUE

      ENDIF

      CALL VERIFT(FAMI,KPG,KSP,C1,IMAT,'ELAS_META',2,EPSTHE,IRET1)
      CALL RCVARC(' ','TEMP',C1,FAMI,KPG,KSP,TEMP,IRET2)
      ZALPHA=PHASE(1)+PHASE(2)+PHASE(3)+PHASE(4)
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
      MUM=VALRES(1)/(2.D0*(1.D0+VALRES(2)))

      CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','ELAS_META',0,' ',0.D0,
     &            6,NOMRES,VALRES,CODRET,'F ')
      EPSTH = PHASE(NZ)*(EPSTHE(1)-(1.D0-VALRES(5))*VALRES(6))
     &     + ZALPHA*(EPSTHE(2)+VALRES(5)*VALRES(6))
      E=VALRES(1)
      NU=VALRES(2)
      MU=E/(2.D0*(1.D0+NU))
      TROISK = E/(1.D0-2.D0*NU)

      IF(COMPOR(1)(1:4).EQ.'META') THEN

        PLASTI=VIM(6)

C 2.2 - LOI DES MELANGES

        IF(COMPOR(1)(1:6).EQ.'META_P') THEN
          NOMRES(1) ='F1_SY'
          NOMRES(2) ='F2_SY'
          NOMRES(3) ='F3_SY'
          NOMRES(4) ='F4_SY'
          NOMRES(5) ='C_SY'
          NOMRES(6) ='SY_MELANGE'
        ENDIF

        IF(COMPOR(1)(1:6).EQ.'META_V') THEN
          NOMRES(1) ='F1_S_VP'
          NOMRES(2) ='F2_S_VP'
          NOMRES(3) ='F3_S_VP'
          NOMRES(4) ='F4_S_VP'
          NOMRES(5) ='C_S_VP'
          NOMRES(6) ='S_VP_MEL'
        ENDIF

        CALL RCVALB(FAMI,1,1,'+',IMAT,' ','ELAS_META',
     &              1,'META',ZALPHA,1,
     &              NOMRES(6),FMEL,CODRET(6),'  ')
        IF (CODRET(6).NE.'OK') FMEL = ZALPHA

C 2.3 - LIMITE D ELASTICITE

        CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','ELAS_META',0,' ',0.D0,
     &              5,NOMRES,SY,CODRET,'F ')

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
            NOMRES(3) ='C_F3_THETA'
            NOMRES(4) ='C_F4_THETA'
            NOMRES(5) ='F1_C_THETA'
            NOMRES(6) ='F2_C_THETA'
            NOMRES(7) ='F3_C_THETA'
            NOMRES(8) ='F4_C_THETA'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_RE',0,'  ',
     &                 0.D0,8,NOMRES,THETA,CODRET,'F ')
          ELSE

            DO 20 I=1,8
              THETA(I)=1.D0
 20         CONTINUE

          ENDIF

C 2.5 - VISCOSITE

          IF (COMPOR(1)(1:6) .EQ. 'META_V') THEN

            NOMRES(1) = 'F1_ETA'
            NOMRES(2) = 'F2_ETA'
            NOMRES(3) = 'F3_ETA'
            NOMRES(4) = 'F4_ETA'
            NOMRES(5) = 'C_ETA'

            NOMRES(6) = 'F1_N'
            NOMRES(7) = 'F2_N'
            NOMRES(8) = 'F3_N'
            NOMRES(9) = 'F4_N'
            NOMRES(10) = 'C_N'

            NOMRES(11) ='F1_C'
            NOMRES(12) ='F2_C'
            NOMRES(13) ='F3_C'
            NOMRES(14) ='F4_C'
            NOMRES(15) ='C_C'

            NOMRES(16) = 'F1_M'
            NOMRES(17) = 'F2_M'
            NOMRES(18) = 'F3_M'
            NOMRES(19) = 'F4_M'
            NOMRES(20) = 'C_M'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_VISC',0,
     &                 ' ',0.D0,10,NOMRES,VALRES,CODRET,'F ')

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_VISC',0,' ',
     &                 0.D0,10,NOMRES(11),VALRES(11),CODRET(11),'  ')

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

C 2.6 - CALCUL DE VIM+DG-DS

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
              DVIN=DVIN+DZ2(K)*(THETA(4+K)*VIM(K)-VIM(NZ))/PHASE(NZ)
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
            NOMRES(3) = 'F3_K'
            NOMRES(4) = 'F4_K'
            NOMRES(5) = 'F1_D_F_META'
            NOMRES(6) = 'F2_D_F_META'
            NOMRES(7) = 'F3_D_F_META'
            NOMRES(8) = 'F4_D_F_META'

            CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_PT',0,' ',
     &                 0.D0,4,NOMRES,VALRES,CODRET ,'F ')

            DO 60 K=1,NZ-1
              KPT (K) = VALRES(K)
              ZVARIM  = PHASM(K)
              ZVARIP  = PHASE(K)
              DELTAZ = (ZVARIP - ZVARIM)
              IF (DELTAZ.GT.0.D0) THEN
                J = 4+K
                CALL RCVALB(FAMI,1,1,'+',IMAT,' ','META_PT',
     &                      1,'META',ZALPHA,1,
     &                      NOMRES(J),VALRES(J),CODRET(J), 'F ')
                TRANS = TRANS + KPT(K)*VALRES(J)*(ZVARIP-ZVARIM)
              ENDIF
 60         CONTINUE

          ENDIF

        ELSE

          TRANS=0.D0
          DO 65 K=1,NZ
            VI(K)=VIM(K)
 65       CONTINUE

        ENDIF

C 2.9 - CALCUL DE HMOY ET RMOY (ON INCLUE LE SIGY)

        IF(COMPOR(1)(1:9).EQ.'META_P_IL' .OR.
     &    COMPOR(1)(1:9).EQ.'META_V_IL' )THEN

          NOMRES(1) ='F1_D_SIGM_EPSI'
          NOMRES(2) ='F2_D_SIGM_EPSI'
          NOMRES(3) ='F3_D_SIGM_EPSI'
          NOMRES(4) ='F4_D_SIGM_EPSI'
          NOMRES(5) ='C_D_SIGM_EPSI'

          CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_ECRO_LINE',0,
     &               ' ',0.D0,5,NOMRES,H,CODRET,'F ')

          H(1)=H(1)*E/(E-H(1))
          H(2)=H(2)*E/(E-H(2))
          H(3)=H(3)*E/(E-H(3))
          H(4)=H(4)*E/(E-H(4))
          H(5)=H(5)*E/(E-H(5))

          DO 70 K=1,NZ
            R(K) = H(K)*VI(K)+SY(K)
 70       CONTINUE

        ENDIF

        IF (COMPOR(1)(1:10) .EQ. 'META_P_INL' .OR.
     &     COMPOR(1)(1:10) .EQ. 'META_V_INL') THEN

          NOMCLE(1)='SIGM_F1'
          NOMCLE(2)='SIGM_F2'
          NOMCLE(3)='SIGM_F3'
          NOMCLE(4)='SIGM_F4'
          NOMCLE(5)='SIGM_C'

          IF (IRET2.EQ.1) CALL U2MESS('F','CALCULEL_31')
          DO 75 K=1,NZ
            CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TEMP,
     &                 JPROL,JVALE,NBVAL(K),RBID)
            CALL RCFONC('V','META_TRACTION',JPROL,JVALE,NBVAL(K),
     &               RBID,RBID,RBID,VI(K),R(K),H(K),RBID,RBID,RBID)
            R(K) = R(K) + SY(K)
 75      CONTINUE

         MAXVAL = MAX(NBVAL(1),NBVAL(2),NBVAL(3),NBVAL(4),NBVAL(5))

        ENDIF

        IF (ZALPHA.GT. 0.D0) THEN
          RMOY=PHASE(1)*R(1)+PHASE(2)*R(2)+PHASE(3)*R(3)+PHASE(4)*R(4)
          RMOY = RMOY/ZALPHA
          HMOY=PHASE(1)*H(1)+PHASE(2)*H(2)+PHASE(3)*H(3)+PHASE(4)*H(4)
          HMOY = HMOY/ZALPHA
        ELSE
          RMOY = 0.D0
          HMOY = 0.D0
        ENDIF
        RMOY =(1.D0-FMEL)*R(NZ)+FMEL*RMOY
        HMOY = (1.D0-FMEL)*H(NZ)+FMEL*HMOY

      ELSE

        TRANS=0.D0
        PLASTI=0.D0

      ENDIF

C ********************************
C 3 - DEBUT DE L ALGORITHME
C ********************************

C 3.1 - JM=DET(FM),DJ=DET(DF),J=JM*DJ ET DFB

      JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &  -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &  +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

      DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &  -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &  +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))

      JP=JM*DJ

      DO 100 I=1,3
        DO 110 J=1,3
          IF (DJ.LE.0.D0) THEN
            IRET = 1
            GOTO 9999
          ELSE
            DFB(I,J)=(DJ**(-1.D0/3.D0))*DF(I,J)
          ENDIF
 110    CONTINUE
 100  CONTINUE

C 3.2 - CONTRAINTES DE KIRSHHOFF A L INSTANT PRECEDENT

      TAUM(5)=0.D0
      TAUM(6)=0.D0
      DO 120 I=1,2*NDIM
        TAUM(I)=JM*SIGM(I)
 120  CONTINUE

      TRTAUM=TAUM(1)+TAUM(2)+TAUM(3)
      EQTAUM=0.D0
      DO 130 I=1,6
        DVTAUM(I)=TAUM(I)-KR(I)*TRTAUM/3.D0
        EQTAUM=EQTAUM+PDTSCA(I)*(DVTAUM(I)**2.D0)
 130  CONTINUE
      EQTAUM=SQRT(1.5D0*EQTAUM)

C 3.3 - DEFORMATIONS ELASTIQUES A L INSTANT PRECEDENT :
C BEM=DVTAUM/MUM+KR*TRBEM/3.D0

      IF (COMPOR(1)(1:4).EQ.'ELAS') THEN
        XM = (JM**(-2.D0/3.D0))*(1.D0-2.D0*VIM(1)/3.D0)
      ELSE
        XM = (JM**(-2.D0/3.D0))*(1.D0-2.D0*VIM(8)/3.D0)
      ENDIF
      DO 140 I=1,6
        BEM(I)=DVTAUM(I)/MUM+KR(I)*XM
 140  CONTINUE

C 3.4 - BEL(I,J)=DFB(I,K)*BEM(K,L)*DFB(J,L)

      DO 150 I=1,3
        DO 160 J=1,3
          BEL(IND(I,J))=0.D0
          DO 170 K=1,3
            DO 180 L=1,3
              BEL(IND(I,J))=BEL(IND(I,J))
     &       +DFB(I,K)*BEM(IND(K,L))*DFB(J,L)
 180        CONTINUE
 170      CONTINUE
 160    CONTINUE
 150  CONTINUE

C 3.5 - TRACE ET PARTIE DEVIATORIQUE DU TENSEUR BEL

      TRBEL=BEL(1)+BEL(2)+BEL(3)

      DO 190 I=1,6
        DVBEL(I)=BEL(I)-KR(I)*TRBEL/3.D0
 190  CONTINUE

C 3.6 - CONTRAINTE ELASTIQUE (PARTIE DEVIATORIQUE)

      DO 200 I=1,6
        DVTEL(I)=MU*DVBEL(I)
 200  CONTINUE

C 3.7 - CONTRAINTE EQUIVALENTE ELASTIQUE ET SEUIL

      EQTEL=0.D0
      DO 210 I=1,6
        EQTEL=EQTEL+PDTSCA(I)*DVTEL(I)*DVTEL(I)
 210  CONTINUE
      EQTEL=SQRT(1.5D0*EQTEL)

C 3.8 - TRACE DU TENSEUR DE KIRSHHOFF (CONNUE CAR NE DEPEND QUE DE J)

      TRTAUP=(TROISK*((JP*JP)-1.D0)/6.D0)
     &     -(TROISK*EPSTH*(JP+(1.D0/JP))/2.D0)

      DP=0.D0

C ************************
C 4 - RESOLUTION
C ************************

      IF (RESI) THEN

C 4.1 - COMPORTEMENT ELASTIQUE - CALCUL DE SIGMA

        IF(COMPOR(1)(1:4).EQ.'ELAS') THEN

          DO 220 I=1,6
            TAUP(I)=DVTEL(I)+KR(I)*TRTAUP
 220      CONTINUE

          DO 230 I=1,2*NDIM
            SIGP(I)=TAUP(I)/JP
 230      CONTINUE

C 4.2 - COMPORTEMENT PLASTIQUE
C 4.2.1 - CALCUL DE DP

        ELSE IF (COMPOR(1)(1:4).EQ.'META') THEN

          SEUIL=EQTEL-(1.D0+MU*TRANS*TRBEL)*RMOY

          IF (SEUIL.LT.0.D0) THEN
            VIP(6)=0.D0
            DP=0.D0
          ELSE
            VIP(6)=1.D0
            MUTILD=2.D0*MU*TRBEL/3.D0
            CALL NZCALC(CRIT,PHASE,NZ,FMEL,SEUIL,DT,TRANS,
     &                  HMOY,MUTILD,ETA,UNSURN,DP,IRET)
            IF(IRET.EQ.1) GOTO 9999

C DANS LE CAS NON LINEAIRE
C VERIFICATION QU ON EST DANS LE BON INTERVALLE

            IF(COMPOR(1)(1:10).EQ.'META_P_INL' .OR.
     &        COMPOR(1)(1:10).EQ.'META_V_INL' )THEN

              DO 240 J=1,MAXVAL
                TEST='OK'
                DO 241 K=1,NZ
                  IF(PHASE(K).GT.0.D0) THEN
                    VIP(K)=VI(K)+DP
                    HPLUS(K)=H(K)
                    IF (IRET2.EQ.1) CALL U2MESS('F','CALCULEL_31')
                    CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TEMP,
     &                         JPROL,JVALE,NBVAL(K),RBID)
                    CALL RCFONC('V','META_TRACTION',JPROL,JVALE,
     &                          NBVAL(K),RBID,RBID,RBID,VIP(K),
     &                          R(K),H(K),RBID,RBID,RBID)
                     R(K)=R(K)+SY(K)
                    IF(ABS(H(K)-HPLUS(K)).GT.PRECR) TEST='NO'
                  ENDIF
 241            CONTINUE
                IF (TEST.EQ.'OK') GO TO 600

                HMOY=0.D0
                RMOY=0.D0
                IF(ZALPHA.GT.0.D0) THEN
                  DO 242 K=1,NZ-1
                    IF(PHASE(K).GT.0.D0) THEN
                      RMOY = RMOY + PHASE(K)*(R(K)-H(K)*DP)
                      HMOY = HMOY + PHASE(K)*H(K)
                    ENDIF
 242              CONTINUE
                  RMOY=FMEL*RMOY/ZALPHA
                  HMOY=FMEL*HMOY/ZALPHA
                ENDIF
                IF (PHASE(NZ).GT.0.D0) THEN
                  RMOY = (1.D0-FMEL)*(R(NZ)-H(NZ)*DP)+RMOY
                  HMOY = (1.D0-FMEL)*H(NZ)+HMOY
                ENDIF
                SEUIL=EQTEL-(1.D0+MU*TRANS*TRBEL)*RMOY
                CALL NZCALC(CRIT,PHASE,NZ,FMEL,SEUIL,DT,TRANS,
     &                      HMOY,MUTILD,ETA,UNSURN,DP,IRET)
                IF(IRET.EQ.1) GOTO 9999
  240         CONTINUE
              IF((TEST.EQ.'NO').AND.(J.EQ.MAXVAL))THEN
                CALL U2MESS('F','ALGORITH_94')
              ENDIF
  600         CONTINUE
            ENDIF
          ENDIF

C 4.2.2 - CALCUL DE SIGMA

          PLASTI=VIP(6)

          DO 250 I=1,6
            IF(EQTEL.GT.0.D0)THEN
              DVTAUP(I)=DVTEL(I)-MU*DP*TRBEL*DVTEL(I)/EQTEL
              DVTAUP(I)=DVTAUP(I)/(1.D0+MU*TRANS*TRBEL)
            ELSE
              DVTAUP(I)=DVTEL(I)/(1.D0+MU*TRANS*TRBEL)
            ENDIF
            TAUP(I)=DVTAUP(I)+KR(I)*TRTAUP
 250      CONTINUE

          DO 260 I=1,2*NDIM
            SIGP(I)=TAUP(I)/JP
 260      CONTINUE

C 4.2.3 - CALCUL DE VIP ET RMOY

          DO 270 K=1,NZ
            IF(PHASE(K).GT.0.D0) THEN
              VIP(K)=VI(K)+DP
            ELSE
              VIP(K)=0.D0
            ENDIF
 270      CONTINUE

          VIP(7)=0.D0
          IF(PHASE(NZ).GT.0.D0)THEN

            IF(COMPOR(1)(1:9).EQ.'META_P_IL'.OR.
     &        COMPOR(1)(1:9).EQ.'META_V_IL')THEN
              VIP(7)=VIP(7)+(1-FMEL)*H(NZ)*VIP(NZ)
            ENDIF

            IF(COMPOR(1)(1:10).EQ.'META_P_INL'.OR.
     &        COMPOR(1)(1:10).EQ.'META_V_INL')THEN
              VIP(7)=VIP(7)+(1-FMEL)*(R(NZ)-SY(NZ))
            ENDIF

          ENDIF

          IF (ZALPHA.GT.0.D0) THEN
            DO 280 K=1,NZ-1

              IF(COMPOR(1)(1:9).EQ.'META_P_IL'.OR.
     &          COMPOR(1)(1:9).EQ.'META_V_IL')THEN
                VIP(7)=VIP(7)+FMEL*PHASE(K)*H(K)*VIP(K)/ZALPHA
              ENDIF

              IF(COMPOR(1)(1:10).EQ.'META_P_INL'.OR.
     &          COMPOR(1)(1:10).EQ.'META_V_INL')THEN
                VIP(7)=VIP(7)+FMEL*PHASE(K)*(R(K)-SY(K))/ZALPHA
              ENDIF

 280        CONTINUE
          ENDIF
        ENDIF
      ENDIF

C *******************************
C 5 - MATRICE TANGENTE DSIGDF
C *******************************

      IF (RIGI) THEN

        IF(OPTION(1:14).EQ.'RIGI_MECA_TANG') THEN
          DO 300 I=1,6
            TAU(I)=TAUM(I)
 300      CONTINUE
        ELSE
          DO 310 I=1,6
            TAU(I)=TAUP(I)
 310      CONTINUE
        ENDIF

        MAT0(1,1)=DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2)
        MAT0(2,2)=DF(1,1)*DF(3,3)-DF(1,3)*DF(3,1)
        MAT0(3,3)=DF(1,1)*DF(2,2)-DF(1,2)*DF(2,1)
        MAT0(1,2)=DF(3,1)*DF(2,3)-DF(2,1)*DF(3,3)
        MAT0(2,1)=DF(1,3)*DF(3,2)-DF(1,2)*DF(3,3)
        MAT0(1,3)=DF(2,1)*DF(3,2)-DF(3,1)*DF(2,2)
        MAT0(3,1)=DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2)
        MAT0(2,3)=DF(3,1)*DF(1,2)-DF(1,1)*DF(3,2)
        MAT0(3,2)=DF(2,1)*DF(1,3)-DF(1,1)*DF(2,3)

        DO 330 I=1,3
          DO 340 J=1,3
            MAT1(I,J)=0.D0
            DO 350 K=1,3
              MAT1(I,J)=MAT1(I,J)+DFB(I,K)*BEM(IND(K,J))
 350        CONTINUE
 340      CONTINUE
 330    CONTINUE

        DO 360 I=1,3
          DO 370 J=1,3
            DO 380 K=1,3
              DO 390 L=1,3
                MAT2(IND(I,J),K,L)=KR(IND(I,K))*MAT1(J,L)
     &                            +KR(IND(J,K))*MAT1(I,L)
 390          CONTINUE
 380        CONTINUE
 370      CONTINUE
 360    CONTINUE

        EQTAU=0.D0
        TRTAU=(TAU(1)+TAU(2)+TAU(3))/3.D0
        DO 391 I=1,6
          DVTAU(I)=TAU(I)-TRTAU*KR(I)
          EQTAU=EQTAU+PDTSCA(I)*(DVTAU(I)**2.D0)
 391    CONTINUE
        EQTAU=SQRT(1.5D0*EQTAU)
        IF(EQTAU.GT.0.D0)THEN
          COEFF1=1.D0+MU*TRANS*TRBEL+MU*DP*TRBEL/EQTAU
        ELSE
          COEFF1=1.D0+MU*TRANS*TRBEL
        ENDIF
        COEFF2=(DJ**(-1.D0/3.D0))*MU/(COEFF1*JP)
        COEFF3=-2.D0*MU/(3.D0*COEFF1*JP*DJ)
        COEFF4=(TROISK*JP/3.D0)-TROISK*EPSTH
     &       *(1.D0-(JP**(-2.D0)))/2.D0
        COEFF4=COEFF4/DJ

        DO 400 I=1,6
          DO 410 J=1,3
            DO 420 K=1,3
              DSIGDF(I,J,K)=MAT2(I,J,K)-2.D0*KR(I)*MAT1(J,K)/3.D0
              DSIGDF(I,J,K)= COEFF2*DSIGDF(I,J,K)
     &                    + COEFF3*DVBEL(I)*MAT0(J,K)
     &                    + COEFF4*KR(I)*MAT0(J,K)
     &                    - TAU(I)*MAT0(J,K)/(JP*DJ)
 420        CONTINUE
 410      CONTINUE
 400    CONTINUE

        IF(PLASTI.EQ.0.D0)THEN
          COEFF5=-2.D0*TRANS*COEFF2
          COEFF6=2.D0*TRANS*MU*TRBEL/(3.D0*JP*DJ*COEFF1)
          DO 430 I=1,6
            DO 440 J=1,3
              DO 450 K=1,3
                DSIGDF(I,J,K) = DSIGDF(I,J,K)
     &                       + COEFF5*DVTAU(I)*MAT1(J,K)
     &                       + COEFF6*DVTAU(I)*MAT0(J,K)
 450          CONTINUE
 440        CONTINUE
 430      CONTINUE
        ELSE
          MODE=2
          IF (COMPOR(1)(1:6) .EQ. 'META_V') MODE=1
          IF (MODE .EQ. 1) THEN
            IF (DP .GT. 0.D0) THEN
              DO 178 I=1,NZ
                N0(I) = (1-N(I))/N(I)
 178          CONTINUE
              DV = (1-FMEL)*PHASE(NZ)*(ETA(NZ)/N(NZ)/DT)
     &          * ((DP/DT)**N0(NZ))
              IF (ZALPHA.GT.0.D0) THEN
                DO 179 I=1,NZ-1
                  IF (PHASE(I) .GT. 0.D0)
     &           DV = DV+FMEL*(PHASE(I)/ZALPHA)*(ETA(I)/N(I)/DT)*
     &               ((DP/DT)**N0(I))
 179            CONTINUE
              ENDIF
            ELSE
              DV =0.D0
            ENDIF
            IF(OPTION(1:9).EQ.'FULL_MECA')THEN
              RB=HMOY+DV
            ELSE
              RB=0.D0
            ENDIF
          ELSE
            RB=HMOY
          ENDIF

          IF((OPTION(1:9).EQ.'FULL_MECA').OR.
     &     ((OPTION(1:14).EQ.'RIGI_MECA_TANG').AND.
     &      (MODE.EQ.2))) THEN

            COEFF5=MU*TRBEL+RB*(1.D0+MU*TRANS*TRBEL)
            COEFF6=-3.D0*MU*TRBEL*(EQTAU-RB*DP)/((EQTAU**3.D0)*COEFF5)
            COEFF6=COEFF6*COEFF2
            COEFF7=-2.D0*COEFF1*RB*(EQTAU*TRANS+DP)/(EQTAU*COEFF5)
            COEFF7=COEFF7*COEFF2
            COEFF8=0.D0
            DO 451 I=1,6
              COEFF8=COEFF8+PDTSCA(I)*DVTAU(I)*DVBEL(I)
  451       CONTINUE
            COEFF9=COEFF1*RB*(EQTAU*TRANS+DP)/EQTAU
     &           +3.D0*MU*COEFF8*(EQTAU-RB*DP)/(2.D0*(EQTAU**3.D0))
            COEFF9=-COEFF9*COEFF3*TRBEL/COEFF5

            DO 455 I=1,3
              DO 456 J=1,3
                MAT3(I,J)=0.D0
                DO 457 K=1,3
                  MAT3(I,J)=MAT3(I,J)+DVTAU(IND(I,K))*MAT1(K,J)
  457           CONTINUE
  456         CONTINUE
  455       CONTINUE
            DO 460 I=1,6
              DO 470 J=1,3
                DO 480 K=1,3
                  DSIGDF(I,J,K) = DSIGDF(I,J,K)
     &                         + COEFF6*DVTAU(I)*MAT3(J,K)
     &                         + COEFF7*DVTAU(I)*MAT1(J,K)
     &                         + COEFF9*DVTAU(I)*MAT0(J,K)
 480            CONTINUE
 470          CONTINUE
 460        CONTINUE
          ENDIF
        ENDIF
      ENDIF

C *******************************
C 6 - CORRECTION SUR TRBE
C *******************************

      IF (RESI) THEN

        TRTAU=TAUP(1)+TAUP(2)+TAUP(3)
        EQTAU=0.D0
        DO 500 I=1,6
          DVTAU(I)=TAUP(I)-KR(I)*TRTAU/3.D0
          EQTAU=EQTAU+PDTSCA(I)*(DVTAU(I)**2.D0)
 500    CONTINUE
        EQTAU=SQRT(1.5D0*EQTAU)
        JE2=(EQTAU**2.D0)/(3.D0*(MU**2.D0))
        JE3=DVTAU(1)*(DVTAU(2)*DVTAU(3)-DVTAU(6)*DVTAU(6))
     &    -DVTAU(4)*(DVTAU(4)*DVTAU(3)-DVTAU(5)*DVTAU(6))
     &    +DVTAU(5)*(DVTAU(4)*DVTAU(6)-DVTAU(5)*DVTAU(2))
        JE3=JE3/(MU**3.D0)

        CALL ZEROP3(0.D0,-JE2,JE3-1.D0,SOL,NBR)
        IF (NBR.LE.1) THEN
          XP=SOL(1)
        ELSE
          XP=SOL(1)
          DO 510 I=2,NBR
            IF ((ABS(SOL(I)-XM)).LT.(ABS(SOL(I-1)-XM))) XP=SOL(I)
 510      CONTINUE
        ENDIF
        IF (COMPOR(1)(1:4).EQ.'ELAS') THEN
          VIP(1) = 3.D0*(1.D0-(JP**(2.D0/3.D0))*XP)/2.D0
        ELSE
          VIP(8) = 3.D0*(1.D0-(JP**(2.D0/3.D0))*XP)/2.D0
        ENDIF

      ENDIF

 9999 CONTINUE

      END
