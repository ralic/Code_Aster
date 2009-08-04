      SUBROUTINE NZCIFW (FAMI,KPG,KSP,NDIM,IMAT,COMPOR,CRIT,
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
      REAL*8             SIGM(6),VIM(37),SIGP(6),VIP(37),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     REALISE LA LOI DE VON MISES CINEMATIQUE POUR LES
C     ELEMENTS METALLURGIQUES EN PETITES DEFORMATIONS
C
C     AVEC OU SANS PLASTICITE DE TRANSFORMATION
C     AVEC OU SANS RESTAURATION ECROUISSAGE METALLURGIQUE
C     RELATION DE COMPORTEMENT ELASTO-PLASTIQUE OU
C                               ELASTO-VISCOPLASTIQUE
C     ECROUISSAGE CINEMATIQUE LINEAIRE
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

      INTEGER  NZ
      INTEGER  NDIMSI,I,J,K,L,MODE,IRE2

      REAL*8   PHASE(5),PHASM(5),ZALPHA
      REAL*8   DT

      REAL*8   EPSTH,E,DEUXMU,DEUMUM,TROISK
      REAL*8   FMEL,SY(5),SYMOY,H(5),HMOY,RPRIM
      REAL*8   THETA(8)
      REAL*8   ETA(5),N(5),UNSURN(5),C(5),M(5),CMOY,MMOY,CR
      REAL*8   DZ(4),DZ1(4),DZ2(4),VI(30),DVIN,VIMT(30)
      REAL*8   XMOY(6),DS(6),XMOYEQ
      REAL*8   TRANS,KPT(4),ZVARIM,ZVARIP,DELTAZ

      REAL*8   TREPSM,TRDEPS,TRSIGM,TRSIGP
      REAL*8   DVDEPS(6),DVSIGM(6),DVSIGP(6)
      REAL*8   SIGEL(6),SIGEL2(6),SIG0(6),SIELEQ,SIGEPS

      REAL*8   PLASTI,DP,SEUIL

      REAL*8   COEF1,COEF2,COEF3,DV,N0(5),B

      REAL*8   PRECR,R8PREM,RAC2
      REAL*8   KRON(6)
      REAL*8   VALRES(20),EPSTHE(2)

      CHARACTER*1 C1
      CHARACTER*2   CODRET(20)
      CHARACTER*8   NOMRES(20),ACIER(4)

      LOGICAL     RESI,RIGI

      DATA         KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      DATA ACIER /'PFERRITE','PPERLITE','PBAINITE','PMARTENS'/

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

      NZ=5

C 1.2 - RECUPERATION DES PHASES METALLURGIQUES

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

      CALL VERIFT(FAMI,KPG,KSP,C1,IMAT,'ELAS_META',2,EPSTHE,IRET)

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

C 1.4 - MISE AU FORMAT DES CONTRAINTES DE RAPPEL

      IF (COMPOR(1)(1:4) .EQ. 'META') THEN

        RAC2 = SQRT(2.D0)
        DO 20 I=4,NDIMSI
          DO 25 K = 1,NZ
            L=I+(K-1)*6
            VIM(L)=VIM(L)*RAC2
 25       CONTINUE
 20     CONTINUE

      ENDIF

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

        PLASTI=VIM(37)

C 2.2 - LOI DES MELANGES

        IF(COMPOR(1)(1:6).EQ.'META_P') THEN
          NOMRES(1) ='F1_SY'
          NOMRES(2) ='F2_SY'
          NOMRES(3) ='F3_SY'
          NOMRES(4) ='F4_SY'
          NOMRES(5) ='C_SY'
          NOMRES(6) ='SY_MELAN'
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
     &             1,'META',ZALPHA,1,
     &              NOMRES(6),FMEL,CODRET(6),'  ')
        IF (CODRET(6).NE.'OK') FMEL = ZALPHA

C 2.3 - LIMITE D ELASTICITE

        CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','ELAS_META',0,' ',0.D0,
     &              5,NOMRES,SY,CODRET,'F ')

C 2.4 - PENTE D ECROUISSAGE

        NOMRES(1) ='F1_D_SIGM_EPSI'
        NOMRES(2) ='F2_D_SIGM_EPSI'
        NOMRES(3) ='F3_D_SIGM_EPSI'
        NOMRES(4) ='F4_D_SIGM_EPSI'
        NOMRES(5) ='C_D_SIGM_EPSI'

        CALL RCVALB(FAMI,KPG,KSP,C1,IMAT,' ','META_ECRO_LINE',0,
     &               ' ',0.D0,5,NOMRES,H,CODRET,'F ')

        H(1)=(2.D0/3.D0)*H(1)*E/(E-H(1))
        H(2)=(2.D0/3.D0)*H(2)*E/(E-H(2))
        H(3)=(2.D0/3.D0)*H(3)*E/(E-H(3))
        H(4)=(2.D0/3.D0)*H(4)*E/(E-H(4))
        H(5)=(2.D0/3.D0)*H(5)*E/(E-H(5))

        HMOY=0.D0
        DO 30 K=1,NZ
          HMOY=HMOY+PHASE(K)*H(K)
 30     CONTINUE

        IF (RESI) THEN

C 2.5 - RESTAURATION D ECROUISSAGE

          IF (COMPOR(1)(1:15) .EQ. 'META_P_CL_PT_RE' .OR.
     &       COMPOR(1)(1:12) .EQ. 'META_P_CL_RE'    .OR.
     &       COMPOR(1)(1:12) .EQ. 'META_V_CL_RE'    .OR.
     &       COMPOR(1)(1:15) .EQ. 'META_V_CL_PT_RE' ) THEN

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

            DO 35 I=1,8
              THETA(I)=1.D0
 35         CONTINUE

          ENDIF

C 2.6 - VISCOSITE

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

            DO  40 K=1,NZ
              ETA(K) = VALRES(K)
              N(K) = VALRES(NZ+K)
              UNSURN(K)=1/N(K)
              IF (CODRET(2*NZ+K) .NE. 'OK') VALRES(2*NZ+K)=0.D0
              C(K) =VALRES(2*NZ+K)
              IF (CODRET(3*NZ+K) .NE. 'OK') VALRES(3*NZ+K)=20.D0
              M(K) = VALRES(3*NZ+K)
 40         CONTINUE

          ELSE

            DO 45 K=1,NZ
              ETA(K) = 0.D0
              N(K)= 20.D0
              UNSURN(K)= 1.D0
              C(K) = 0.D0
              M(K) = 20.D0
 45         CONTINUE

          ENDIF

C 2.7 - CALCUL DE VIM+DG

          DO 50 K=1,NZ-1
            DZ(K)= PHASE(K)-PHASM(K)
            IF (DZ(K).GE.0.D0) THEN
              DZ1(K)=DZ(K)
              DZ2(K)=0.D0
            ELSE
              DZ1(K)=0.D0
              DZ2(K)=-DZ(K)
            ENDIF
 50       CONTINUE

          IF (PHASE(NZ).GT.0.D0)THEN
            DO 55 I=1,NDIMSI
              DVIN=0.D0
              DO 60 K=1,NZ-1
                L=I+(K-1)*6
                DVIN = DVIN
     &         + DZ2(K)*(THETA(4+K)*VIM(L)-VIM(24+I))/PHASE(NZ)
 60           CONTINUE
              VI(24+I)  = VIM(24+I)+DVIN
              IF((VI(24+I)*VIM(24+I)).LT.0.D0) VI(24+I)=0.D0
 55         CONTINUE
          ELSE
            DO 65 I=1,NDIMSI
              VI(24+I)=0.D0
 65         CONTINUE
          ENDIF

          DO 70  K=1,NZ-1
            DO  75 I=1,NDIMSI
              L=I+(K-1)*6
              IF (PHASE(K).GT.0.D0)THEN
                DVIN = DZ1(K)*(THETA(K)*VIM(24+I)-VIM(L))/PHASE(K)
                VI(L)  = VIM(L)+DVIN
                IF((VI(L)*VIM(L)) .LT. 0.D0) VI(L)=0.D0
              ELSE
                VI(L)  = 0.D0
              ENDIF
 75         CONTINUE
 70       CONTINUE

C 2.8 - RESTAURATION D ORIGINE VISQUEUSE

          DO 80 I=1,NDIMSI
            XMOY(I)=0.D0
            DO 85 K=1,NZ
              L=I+(K-1)*6
              XMOY(I)=XMOY(I)+PHASE(K)*H(K)*VI(L)
 85         CONTINUE
 80       CONTINUE

          XMOYEQ = 0.D0
          DO 90 I=1,NDIMSI
            XMOYEQ=XMOYEQ+XMOY(I)**2.D0
 90       CONTINUE
          XMOYEQ= SQRT(1.5D0*XMOYEQ)

          CMOY=0.D0
          MMOY=0.D0
          DO 100 K=1,NZ
            CMOY=CMOY+PHASE(K)*C(K)
            MMOY=MMOY+PHASE(K)*M(K)
 100      CONTINUE

          CR=CMOY*XMOYEQ
          IF (XMOYEQ.GT.0.D0) THEN
            DO 105 I=1,NDIMSI
              DS(I)= 3.D0*DT*(CR**MMOY)*XMOY(I)/(2.D0*XMOYEQ)
 105        CONTINUE
          ELSE
            DO 110 I=1,NDIMSI
              DS(I)= 0.D0
 110        CONTINUE
          ENDIF

          DO 115 K=1,NZ
            DO 120 I=1,NDIMSI
              L=I+(K-1)*6
              IF (PHASE(K).GT.0.D0)THEN
                VIMT(L)=VI(L)
                VI(L)=VI(L)-DS(I)
                IF((VI(L)*VIMT(L)).LT.0.D0) VI(L)=0.D0
              ENDIF
 120        CONTINUE
 115      CONTINUE

C 2.9 - PLASTICITE DE TRANSFORMATION

          TRANS = 0.D0
          IF (COMPOR(1)(1:12) .EQ. 'META_P_CL_PT'    .OR.
     &       COMPOR(1)(1:15) .EQ. 'META_P_CL_PT_RE' .OR.
     &       COMPOR(1)(1:12) .EQ. 'META_V_CL_PT'    .OR.
     &       COMPOR(1)(1:15) .EQ. 'META_V_CL_PT_RE' ) THEN

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

            DO 125 K=1,NZ-1
              KPT (K) = VALRES(K)
              ZVARIM  = PHASM(K)
              ZVARIP  = PHASE(K)
              DELTAZ = (ZVARIP - ZVARIM)
              IF (DELTAZ.GT.0.D0) THEN
                J = 4+K
                CALL RCVALB(FAMI,1,1,'+',IMAT,' ','META_PT',
     &                     1,'META',ZALPHA,1,
     &                      NOMRES(J),VALRES(J),CODRET(J), 'F ')
                TRANS = TRANS + KPT(K)*VALRES(J)*(ZVARIP-ZVARIM)
              ENDIF
 125        CONTINUE

          ENDIF

        ELSE

          DO 130 K=1,NZ
            DO 135 I=1,NDIMSI
              L=I+(K-1)*6
              VI(L)=VIM(L)
 135        CONTINUE
 130      CONTINUE

C         INITIALISATION DE TRANS
          TRANS=0.D0

          DO 140 I=1,NDIMSI
            XMOY(I)=0.D0
            DO 145 K=1,NZ
              L=I+(K-1)*6
              XMOY(I)=XMOY(I)+PHASE(K)*H(K)*VI(L)
 145        CONTINUE
 140      CONTINUE

        ENDIF

C 2.10 - CALCUL DE SYMOY

        IF (ZALPHA.GT.0.D0) THEN
          SYMOY = PHASE(1)*SY(1)+PHASE(2)*SY(2)+PHASE(3)*SY(3)
     &         + PHASE(4)*SY(4)
          SYMOY = SYMOY/ZALPHA
        ELSE
          SYMOY = 0.D0
        ENDIF
        SYMOY =(1.D0-FMEL)*SY(NZ)+FMEL*SYMOY

      ELSE

        TRANS=0.D0
        DO 150 I=1,NDIMSI
          XMOY(I)=0.D0
 150    CONTINUE

      ENDIF

C ********************************
C 3 - DEBUT DE L ALGORITHME
C ********************************

      TRDEPS = (DEPS(1)+DEPS(2)+DEPS(3))/3.D0
      TREPSM = (EPSM(1)+EPSM(2)+EPSM(3))/3.D0
      TRSIGM = (SIGM(1)+SIGM(2)+SIGM(3))/3.D0
      TRSIGP = TROISK*(TREPSM+TRDEPS)-TROISK*EPSTH

      DO 155 I=1,NDIMSI
        DVDEPS(I)   = DEPS(I) - TRDEPS * KRON(I)
        DVSIGM(I)   = SIGM(I) - TRSIGM * KRON(I)
 155  CONTINUE

      SIELEQ = 0.D0
      DO 160 I = 1,NDIMSI
        SIGEL(I) = DEUXMU*DVSIGM(I)/DEUMUM + DEUXMU*DVDEPS(I)
        SIGEL2(I)= SIGEL(I)-(1.5D0*DEUXMU*TRANS+1.D0)*XMOY(I)
        SIELEQ   = SIELEQ + SIGEL2(I)**2
 160  CONTINUE
      SIELEQ     = SQRT(1.5D0*SIELEQ)

      IF (SIELEQ .GT. 0.D0) THEN
        DO 165 I = 1,NDIMSI
          SIG0(I) = SIGEL2(I)/SIELEQ
 165    CONTINUE
      ELSE
        DO 170 I = 1,NDIMSI
          SIG0(I) = 0.D0
 170    CONTINUE
      ENDIF

C ************************
C 4 - RESOLUTION
C ************************

      IF (RESI) THEN

C 4.1 - COMPORTEMENT ELASTIQUE - CALCUL DE SIGMA

        IF (COMPOR(1)(1:4) .EQ. 'ELAS') THEN

          DO 175 I = 1,NDIMSI
            SIGP(I) = SIGEL(I)+TRSIGP*KRON(I)
 175      CONTINUE

C 4.2 - COMPORTEMENT PLASTIQUE
C 4.2.1 - CALCUL DE DP

        ELSE IF (COMPOR(1)(1:4) .EQ. 'META') THEN

          SEUIL= SIELEQ-(1.5D0*DEUXMU*TRANS+1.D0)*SYMOY

          IF (SEUIL.LT.0.D0) THEN
            VIP(37) = 0.D0
            DP = 0.D0
          ELSE
            VIP(37) = 1.D0
            RPRIM=3.D0*HMOY/2.D0
            IF (COMPOR(1)(1:6) .EQ. 'META_P') THEN
              DP=SEUIL/(1.5D0*DEUXMU+(1.5D0*DEUXMU*TRANS+1.D0)*RPRIM)
            ELSE
              CALL NZCALC(CRIT,PHASE,NZ,FMEL,SEUIL,DT,TRANS,
     &                    RPRIM,DEUXMU,ETA,UNSURN,DP,IRET)
              IF(IRET.EQ.1) GOTO 9999
            ENDIF
          ENDIF

C 4.2.2 - CALCUL DE SIGMA

          PLASTI=VIP(37)

          DO 180 I = 1,NDIMSI
            DVSIGP(I) = SIGEL(I) - 1.5D0*DEUXMU*DP*SIG0(I)
            DVSIGP(I) = DVSIGP(I)/(1.5D0*DEUXMU*TRANS + 1.D0)
            SIGP(I) = DVSIGP(I) + TRSIGP*KRON(I)
  180     CONTINUE

C 4.2.3 - CALCUL DE VIP ET XMOY

          DO 185 K=1,NZ
            DO 190 I=1,NDIMSI
              L=I+(K-1)*6
              IF (PHASE(K).GT.0.D0)THEN
                VIP(L)  = VI(L)+3.D0*DP*SIG0(I)/2.D0
              ELSE
                VIP(L)  = 0.D0
              ENDIF
 190        CONTINUE
 185      CONTINUE

          DO 195 I=1,NDIMSI
            VIP(30+I)= XMOY(I)+3.D0*HMOY*DP*SIG0(I)/2.D0
 195      CONTINUE

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

        DO 210 I=1,NDIMSI
          DSIDEP(I,I) = 1.D0
 210    CONTINUE

        DO 215 I=1,3
          DO 220 J=1,3
            DSIDEP(I,J) = DSIDEP(I,J)-1.D0/3.D0
 220      CONTINUE
 215    CONTINUE

        IF (OPTION(1:9) .EQ. 'FULL_MECA') THEN
          COEF1 = (1.5D0*DEUXMU*TRANS+1.D0)
        ELSE
          COEF1 = 1.D0
        ENDIF

        DO 225 I=1,NDIMSI
          DO 230 J=1,NDIMSI
            DSIDEP(I,J) = DSIDEP(I,J)*DEUXMU/COEF1
 230      CONTINUE
 225    CONTINUE

C 5.2 - PARTIE PLASTIQUE

        B=1.D0
        COEF2 =0.D0
        COEF3=0.D0
        IF (COMPOR(1)(1:4) .EQ. 'META') THEN

          IF (PLASTI .GE. 0.5D0) THEN

            IF (OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN

              SIGEPS = 0.D0
              DO 235 I = 1,NDIMSI
                DVSIGP(I)=DVSIGP(I)-XMOY(I)
                SIGEPS = SIGEPS + DVSIGP(I)*DVDEPS(I)
 235          CONTINUE

              IF ((MODE .EQ.1) .OR. ((MODE .EQ. 2) .AND.
     &            (SIGEPS.GE.0.D0))) THEN

                B = 1.D0-(1.5D0*DEUXMU*DP/SIELEQ)
                DV = 0.D0
                IF (MODE .EQ.1) THEN
                  DO 240 K=1,NZ
                    N0(K) = (1-N(K))/N(K)
 240              CONTINUE
                  DV = (1-FMEL)*PHASE(NZ)*(ETA(NZ)/N(NZ)/DT)
     &              * ((DP/DT)**N0(NZ))
                  IF (ZALPHA.GT.0.D0) THEN
                    DO 245 K=1,NZ-1
                      IF (PHASE(K).GT. 0.D0)
     &               DV = DV+FMEL*(PHASE(K)/ZALPHA)
     &                  * (ETA(K)/N(K)/DT)*((DP/DT)**N0(K))
 245                CONTINUE
                  ENDIF
                ENDIF

                COEF2 = 3.D0*HMOY/2.D0 + DV
                COEF2 = (1.5D0*DEUXMU*TRANS+1.D0)*COEF2
                COEF2 = (1.5D0*DEUXMU)+COEF2
                COEF2 = 1/COEF2 - DP/SIELEQ
                COEF2 =((1.5D0*DEUXMU)**2)*COEF2

              ENDIF

            ENDIF

            IF (OPTION(1:14) .EQ. 'RIGI_MECA_TANG') THEN
              IF (MODE .EQ. 2)
     &           COEF2 = ((1.5D0*DEUXMU)**2)/(1.5D0*DEUXMU+1.5D0*HMOY)
            ENDIF

            COEF3 = COEF2/COEF1

          ENDIF

        ENDIF

        DO 250 I=1,NDIMSI
          DO 255 J=1,NDIMSI
            DSIDEP(I,J) = DSIDEP(I,J)*B
 255      CONTINUE
 250    CONTINUE

        DO 260 I = 1,3
          DO 265 J =1,3
            DSIDEP(I,J) = DSIDEP(I,J)+TROISK/3.D0
 265      CONTINUE
 260    CONTINUE

        DO 270 I=1,NDIMSI
          DO 275 J=1,NDIMSI
            DSIDEP(I,J) = DSIDEP(I,J)- COEF3*SIG0(I)*SIG0(J)
 275      CONTINUE
 270    CONTINUE

      ENDIF

C 6 - MISE AU FORMAT CONTRAINTE DE RAPPEL

      IF (COMPOR(1)(1:4) .EQ. 'META') THEN

        DO 280 I=4,NDIMSI
          DO 285 K = 1,NZ
            L=I+(K-1)*6
            VIM(L)=VIM(L)/RAC2
            IF (RESI) VIP(L)=VIP(L)/RAC2
 285      CONTINUE
          IF (RESI) VIP(30+I)= VIP(30+I)/RAC2
 280    CONTINUE

      ENDIF

 9999 CONTINUE

      END
