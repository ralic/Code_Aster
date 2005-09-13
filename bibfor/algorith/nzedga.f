      SUBROUTINE NZEDGA (NDIM,IMAT,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                   PHASM,PHASP,OPTION,SIGP,VIP,DSIDEP,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/09/2005   AUTEUR LEBOUVIE F.LEBOUVIER 
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
      IMPLICIT NONE
      INTEGER            NDIM,IMAT,IRET
      CHARACTER*16       COMPOR(*),OPTION
      REAL*8             CRIT(3),INSTAM,INSTAP,TM,TP,TREF
      REAL*8             EPSM(6),DEPS(6),PHASM(3),PHASP(3)
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
C IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
C IN  EPSM    : DEFORMATIONS A L'INSTANT DU CALCUL PRECEDENT
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  PHASM   : VARIABLES METALLURGIQUES A L'INSTANT PRECEDENT
C IN  PHASP   : VARIABLES METALLURGIQUES A L'INSTANT DU CALCUL
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
C
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      REAL*8             DEUXMU,E,NU,TROISK,KRON(6)
      REAL*8             EM,NUM,TROIKM,DEUMUM,FMEL
      REAL*8             VALRES(14)
      REAL*8             DEPSTH(6),DEPSDV(6),SIGDV(6),SIGEL(6)
      REAL*8             EPSMO,SIGMO
      REAL*8             SEUIL,DP,PLASTI,B,RPRIM,RP,R(3),RM,DS
      REAL*8             ZALPHM,ZALPHP,ZBETAM,ZBETAP
      REAL*8             TTRGM,COEF1M,COEF2M,EPSTHM
      REAL*8             TTRGP,COEF1P,COEF2P,EPSTHP
      REAL*8             DZ(2),DZ1(2),DZ2(2),DVIN(3),A,THETA(4)
      REAL*8             SIGMP(6),SIG0(6),SIMOEQ,SIELEQ,SIGEPS
      REAL*8             DT,N(3),N0(3),C(3),M(3),UNSURN(3),CR,ETA(3)
      REAL*8             KPT(3),TRANS,ZVARIM,ZVARIP,DELTAZ
      REAL*8             RBID,HALPHA,RALPHA,CM,MM
      REAL*8             HPLUS(3),H(3),RPLUS(3),SY(3),ECRO(3)
      REAL*8             DH(3)
      REAL*8             COEF1,COEF2,COEF3,DV,ECROAL,EPS
      INTEGER            NDIMSI,MODE,JPROL,JVALE,NBVALE(3),NBMAX,NZ
      INTEGER            I,J,K,L
      CHARACTER*2        BL2, FB2, CODRET(14), TEST
      CHARACTER*8        NOMRES(14),NOMCLE(5)
      DATA               KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C 1 INITIALISATIONS
      EPS = 1.D-9
      FB2 = 'F '

      DO 100 K=1,6
      DO 100 L=1,6
        DSIDEP(K,L) = 0.D0
 100  CONTINUE
      IF (NDIM.EQ.2) THEN
        NDIMSI=4
      ELSE
        NDIMSI=6
      ENDIF
C   NOMBRE DE PHASE
      NZ=3

C 2 RECUPERATION DES CARACTERISTIQUES (ELAS ET DILATATION)

      IF (COMPOR(1)(1:4) .EQ. 'ELAS'.OR.
     &    COMPOR(1)(1:4) .EQ. 'META' ) THEN
       NOMRES(1)='E'
       NOMRES(2)='NU'
       NOMRES(3)='F_ALPHA'
       NOMRES(4)='C_ALPHA'
       NOMRES(5)='PHASE_REFE'
       NOMRES(6)='EPSF_EPSC_TREF'
       CALL RCVALA(IMAT,' ','ELAS_META',1,'TEMP',TM,6,NOMRES,VALRES,
     &             CODRET, FB2 )
       ZALPHM  = PHASM(1) + PHASM(2)
       ZBETAM = 1.D0 - ZALPHM
       PHASM(3)=ZBETAM
       TTRGM  = TM-TREF
       COEF1M = ZBETAM*(VALRES(4)*TTRGM-(1.D0-VALRES(5))*VALRES(6))
       COEF2M = ZALPHM*(VALRES(3)*TTRGM + VALRES(5)*VALRES(6))
       EPSTHM = COEF1M + COEF2M
       EM     = VALRES(1)
       NUM    = VALRES(2)
       DEUMUM = EM/(1.D0+NUM)
       TROIKM = EM/(1.D0-2.D0*NUM)
       CALL RCVALA(IMAT,' ','ELAS_META',1,'TEMP',TP,6,NOMRES,
     &             VALRES,CODRET, FB2 )
       ZALPHP  = PHASP(1) + PHASP(2)
       ZBETAP = 1.D0 - ZALPHP
       PHASP(3)=ZBETAP
       TTRGP  = TP -TREF
       COEF1P = ZBETAP*(VALRES(4)*TTRGP-(1.D0-VALRES(5))*VALRES(6))
       COEF2P = ZALPHP*(VALRES(3)*TTRGP + VALRES(5)*VALRES(6))
       EPSTHP = COEF1P + COEF2P
       E      = VALRES(1)
       NU     = VALRES(2)
       DEUXMU = E/(1.D0+NU)
       TROISK = E/(1.D0-2.D0*NU)
       DT = INSTAP-INSTAM

C 3 CALCUL DE DEPSDV
       EPSMO = 0.D0
       DO 110 K=1,3
        DEPSTH(K)   = DEPS(K) -(EPSTHP-EPSTHM)
        DEPSTH(K+3) = DEPS(K+3)
        EPSMO = EPSMO + DEPSTH(K)
 110   CONTINUE

       EPSMO = EPSMO/3.D0
       DO 115 K=1,NDIMSI
        DEPSDV(K)   = DEPSTH(K) - EPSMO * KRON(K)
 115   CONTINUE
      ENDIF

C 4 RECUPERATION CARACTERISTIQUES (PLASTIQUE)

      IF (COMPOR(1)(1:4) .EQ. 'META' ) THEN
       PLASTI=VIM(4)
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
       CALL RCVALA(IMAT,' ','ELAS_META',1,'META',ZALPHP,1,
     &             NOMRES(4),FMEL,CODRET,'  ')
       IF (CODRET(4).NE.'OK') FMEL = ZALPHP
       CALL RCVALA(IMAT,' ','ELAS_META',1,'TEMP',TP,3,
     &             NOMRES,VALRES,CODRET,'F ')
       SY(1)=VALRES(1)
       SY(2)=VALRES(2)
       SY(3)=VALRES(3)

       IF(COMPOR(1)(1:9).EQ.'META_P_IL' .OR.
     &    COMPOR(1)(1:9).EQ.'META_V_IL' )THEN
        NOMRES(5) ='F1_D_SIGM_EPSI'
        NOMRES(6) ='F2_D_SIGM_EPSI'
        NOMRES(7) ='C_D_SIGM_EPSI'

        CALL RCVALA(IMAT,' ','META_ECRO_LINE',1,'TEMP',TP,3,
     &              NOMRES(5),VALRES(5),CODRET(5),'F ')

        H(1)=VALRES(5)*E/(E-VALRES(5))
        H(2)=VALRES(6)*E/(E-VALRES(6))
        H(3)=VALRES(7)*E/(E-VALRES(7))

       ENDIF

C 5 RECUPERATION CARACTERISTIQUES (RESTAURATION ECROU DE METALLO)

       DO 200 K=1,2
        DZ(K)   = PHASP(K)-PHASM(K)
        IF (DZ(K).GE.0.D0) THEN
         DZ1(K)=DZ(K)
         DZ2(K)=0.D0
        ELSE
         DZ1(K)=0.D0
         DZ2(K)=-DZ(K)
        ENDIF
 200   CONTINUE
       IF (COMPOR(1)(1:12) .EQ. 'META_P_IL_RE'  .OR.
     &     COMPOR(1)(1:15) .EQ. 'META_P_IL_PT_RE' .OR.
     &     COMPOR(1)(1:13) .EQ. 'META_P_INL_RE'  .OR.
     &     COMPOR(1)(1:16) .EQ. 'META_P_INL_PT_RE' .OR.
     &     COMPOR(1)(1:12) .EQ. 'META_V_IL_RE'  .OR.
     &     COMPOR(1)(1:15) .EQ. 'META_V_IL_PT_RE' .OR.
     &     COMPOR(1)(1:13) .EQ. 'META_V_INL_RE'  .OR.
     &     COMPOR(1)(1:16) .EQ. 'META_V_INL_PT_RE'  ) THEN

        NOMRES(1) ='C_F1_THETA'
        NOMRES(2) ='C_F2_THETA'
        NOMRES(3) ='F1_C_THETA'
        NOMRES(4) ='F2_C_THETA'

        CALL RCVALA(IMAT,' ','META_RE',1,'TEMP ',TP,4,
     &              NOMRES,VALRES,CODRET, FB2)
        DO 23 I=1,4
         THETA(I) = VALRES(I)
 23     CONTINUE
       ELSE
        DO 19 I=1,4
         THETA(I) = 1.D0
 19     CONTINUE
       ENDIF

C 6 CARACTERISTIQUES MATERIAUX (VISCO)

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
        CALL RCVALA(IMAT,' ','META_VISC',1,'TEMP',TP,12,NOMRES,
     &              VALRES,CODRET,FB2)

        DO  29 I=1,3
           ETA(I) = VALRES(I)
           N(I) = VALRES(3+I)
           UNSURN(I)=1/N(I)
           IF (CODRET(6+I) .NE. 'OK') VALRES(6+I)=0.D0
           C(I) =VALRES(6+I)
           IF (CODRET(9+I) .NE. 'OK') VALRES(9+I)=20.D0
           M(I) = VALRES(9+I)
 29     CONTINUE
       ELSE
        DO 39 I=1,3
           ETA(I) = 0.D0
           N(I)= 20.D0
           UNSURN(I)= 1.D0
           C(I) = 0.D0
           M(I) = 20.D0
 39     CONTINUE
       ENDIF

C 7 CALCUL DE A,RP
C      RESTAURATION ECROUISSAGE METALLO
       DO 420 K=1,2
        IF (PHASP(K).GT. EPS)THEN
         DVIN(K) = DZ1(K)*THETA(K)*VIM(3)/PHASP(K)
     &           - DZ1(K)*VIM(K)/PHASP(K)
         VIM(K)  = VIM(K)+DVIN(K)
         IF(VIM(K) .LT. 0.D0) VIM(K)=0.D0
        ELSE
         DVIN(K) = 0.D0
         VIM(K)  = 0.D0
        ENDIF
 420   CONTINUE
       IF (ZBETAP .GT. EPS)THEN
        DO 430 K=1,2
         DVIN(3) = DZ2(K)*THETA(2+K)*VIM(K)/ZBETAP
     &           - DZ2(K)*VIM(3)/ZBETAP
 430    CONTINUE
        VIM(3)  = VIM(3)+DVIN(3)
        IF(VIM(3) .LT. 0.D0) VIM(3)=0.D0
       ELSE
        DVIN(3)  = 0.D0
        VIM(3)   = 0.D0
       ENDIF
       RM = ZBETAP*VIM(3)
       DO 440 K=1,2
        RM = RM+PHASP(K)*VIM(K)
 440   CONTINUE

C      RESTAURATION ECROUISSAGE VISQUEUSE
       CM=ZBETAP*C(3)
       DO 49 I=1,2
          CM=CM+PHASP(I)*C(I)
 49    CONTINUE

       MM=ZBETAP*M(3)
       DO 59 I=1,2
          MM=MM+PHASP(I)*M(I)
 59    CONTINUE
       CR=CM*RM
       IF (CR .LE. 0.D0) THEN
        DS=0.D0
       ELSE
        DS= DT*((CM*RM)**MM)
       ENDIF

       DO 421 K=1,2
        IF (PHASP(K).GT. EPS)THEN
         DVIN(K) = -(DS)
         VIM(K)  = VIM(K)+DVIN(K)
         IF(VIM(K) .LT. 0.D0) VIM(K)=0.D0
        ELSE
         DVIN(K) = 0.D0
         VIM(K)  = 0.D0
        ENDIF
 421   CONTINUE
       IF (ZBETAP .GT. EPS)THEN
         DVIN(3)  =   -(DS)
        VIM(3)  = VIM(3)+DVIN(3)
        IF(VIM(3) .LT. 0.D0) VIM(3)=0.D0
       ELSE
        DVIN(3)  = 0.D0
        VIM(3)   = 0.D0
       ENDIF

C  CALCUL DES RK

       IF ((COMPOR(1)(1:9) .EQ. 'META_P_IL') .OR.
     &     (COMPOR(1)(1:9) .EQ. 'META_V_IL')) THEN
        DO 422 K=1,3
         R(K)=H(K)*VIM(K)+SY(K)
 422    CONTINUE
       ELSEIF ((COMPOR(1)(1:10) .EQ. 'META_P_INL') .OR.
     &         (COMPOR(1)(1:10) .EQ. 'META_V_INL')) THEN
        NOMCLE(1)(1:7)='SIGM_F1'
        NOMCLE(2)(1:7)='SIGM_F2'
        NOMCLE(3)(1:6)='SIGM_C'

        DO 423 K=1,3
         CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TP,
     &               JPROL,JVALE,NBVALE(K),RBID)
         CALL RCFONC('V','META_TRACTION',JPROL,JVALE,NBVALE(K),
     &               RBID,RBID,RBID,VIM(K),R(K),H(K),RBID,RBID,RBID)
         R(K) = R(K) + SY(K)
 423    CONTINUE
       ENDIF

       IF ( ZALPHP .GT. EPS) THEN
        RALPHA = PHASP(1)*R(1)+PHASP(2)*R(2)
        RALPHA = RALPHA/ZALPHP

        HALPHA = PHASP(1)*H(1)+PHASP(2)*H(2)
        HALPHA = HALPHA/ZALPHP
       ELSE
        RALPHA = 0.D0
        HALPHA = 0.D0
       ENDIF
       A  =(1.D0-FMEL)*R(3)+FMEL*RALPHA
       RP=A
       RPRIM = (1.D0-FMEL)*H(3)+FMEL*HALPHA
      ELSE
       RP =0.D0
       RPRIM=0.D0
      ENDIF

C 8 RECUPERATION CARACTERISTIQUES (PLAS DE TRANSFORMATION)

      TRANS = 0.D0
      IF (COMPOR(1)(1:12) .EQ. 'META_P_IL_PT'    .OR.
     &    COMPOR(1)(1:13) .EQ. 'META_P_INL_PT'   .OR.
     &    COMPOR(1)(1:15) .EQ. 'META_P_IL_PT_RE' .OR.
     &    COMPOR(1)(1:16) .EQ. 'META_P_INL_PT_RE'.OR.
     &    COMPOR(1)(1:12) .EQ. 'META_V_IL_PT'    .OR.
     &    COMPOR(1)(1:13) .EQ. 'META_V_INL_PT'   .OR.
     &    COMPOR(1)(1:15) .EQ. 'META_V_IL_PT_RE' .OR.
     &    COMPOR(1)(1:16) .EQ. 'META_V_INL_PT_RE') THEN

       NOMRES(1)  = 'F1_K'
       NOMRES(2) = 'F2_K'

       NOMRES(3) = 'F1_D_F_META'
       NOMRES(4) = 'F2_D_F_META'

       CALL RCVALA(IMAT,' ','META_PT',1,'TEMP',TP,2,
     &             NOMRES,VALRES,CODRET(9), FB2 )
       DO 20 I=1,2
        KPT (I) = VALRES(I)

        ZVARIM  = PHASM(I)
        ZVARIP  = PHASP(I)
        DELTAZ = (ZVARIP - ZVARIM)
        IF (DELTAZ.GT.0) THEN
        J=I+2
          CALL RCVALA(IMAT,' ','META_PT',1,'META',ZALPHP,1,
     &               NOMRES(J),VALRES(J),CODRET(J), FB2 )
          TRANS  = TRANS + KPT(I)*VALRES(J)*(ZVARIP-ZVARIM)
        ENDIF
  20   CONTINUE

      ENDIF

C 9 CALCUL SIGDV,SEUIL

        SIGMO = 0.D0
        DO 113 K =1,3
          SIGMO = SIGMO + SIGM(K)
 113    CONTINUE
        SIGMO = SIGMO /3.D0
        DO 114 K=1,NDIMSI
          SIGMP(K)=DEUXMU/DEUMUM*(SIGM(K)-SIGMO*KRON(K)) +
     &             TROISK/TROIKM*SIGMO*KRON(K)
 114    CONTINUE
        SIGMO = 0.D0
        DO 119 K =1,3
          SIGMO = SIGMO + SIGMP(K)
 119    CONTINUE
        SIGMO = SIGMO /3.D0
        SIELEQ = 0.D0
        DO 117 K = 1,NDIMSI
          SIGDV(K) = SIGMP(K)- SIGMO * KRON(K)
          SIGEL(K) = SIGDV(K) + DEUXMU * DEPSDV(K)
          SIELEQ     = SIELEQ + SIGEL(K)**2
 117    CONTINUE
        SIELEQ     = SQRT(1.5D0*SIELEQ)
        IF (SIELEQ .GT. 0.D0) THEN
         DO 118 K = 1,NDIMSI
           SIG0(K) = SIGEL(K)/SIELEQ
 118     CONTINUE
        ELSE
        DO 129 K = 1,NDIMSI
           SIG0(K) = 0.D0
 129     CONTINUE
        ENDIF
        DP = 0.D0
        SEUIL      = SIELEQ - (1.5D0*DEUXMU*TRANS + 1.D0) * RP

C
C  10-- CALCUL DE DP,SIGDV(+),VIP
C       --------------------------
       IF (OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA' ) THEN
        IF (COMPOR(1)(1:4) .EQ. 'ELAS') THEN
         DO 145 K = 1,NDIMSI
          SIGP(K) = SIGMP(K)+DEUXMU*DEPSDV(K)+TROISK*EPSMO*KRON(K)
 145     CONTINUE
        ELSE IF (COMPOR(1)(1:4) .EQ. 'META') THEN
         IF (SEUIL.LT.0.D0) THEN
          VIP(4) = 0.D0
          DP = 0.D0
         ELSE
          VIP(4) = 1.D0
          CALL NZCALC(CRIT,PHASP,NZ,FMEL,SEUIL,DT,TRANS,
     &                RPRIM,DEUXMU,ETA,UNSURN,DP,IRET)
          IF(IRET.EQ.1) GOTO 9999

C TEST SI PENTE COURBE TRACTION BIEN ESTIME

          IF((COMPOR(1)(1:10) .EQ. 'META_P_INL') .OR.
     &       (COMPOR(1)(1:10) .EQ. 'META_V_INL')) THEN
           DO 221 K=1,2
            IF (PHASP(K).GT. EPS)THEN
             DVIN(K) = DP
             VIP(K)  = VIM(K)+DVIN(K)
             IF(VIP(K) .LT. 0.D0) VIP(K)=0.D0
            ELSE
             DVIN(K) = 0.D0
             VIP(K)  = 0.D0
            ENDIF
 221       CONTINUE
           IF (ZBETAP .GT. EPS)THEN
            DVIN(3)  = DP
            VIP(3)  = VIM(3)+DVIN(3)
            IF(VIP(3) .LT. 0.D0) VIP(3)=0.D0
           ELSE
            DVIN(3)  = 0.D0
            VIP(3)   = 0.D0
           ENDIF
           NBMAX = MAX(NBVALE(1),NBVALE(2),NBVALE(3))
           DO 223 J=1,NBMAX
            TEST='OK'
            DO 222 K=1,3
             CALL RCTRAC(IMAT,'META_TRACTION',NOMCLE(K),TP,
     &                   JPROL,JVALE,NBVALE(K),RBID)
             CALL RCFONC('V','META_TRACTION',JPROL,JVALE,NBVALE(K),
     &         RBID,RBID,RBID,VIP(K),RPLUS(K),HPLUS(K),RBID,RBID,RBID)
             RPLUS(K) = RPLUS(K) + SY(K)
 222        CONTINUE
            DO 224 I=1,3
             IF (H(I).GE.HPLUS(I)) THEN
              DH(I)=H(I)-HPLUS(I)
             ELSE
              DH(I)=HPLUS(I)-H(I)
             ENDIF
             IF (DH(I) .GT. 1.D-05) THEN
              H(I)=HPLUS(I)
              R(I)=RPLUS(I)
              TEST='NO'
             ENDIF
  224       CONTINUE
            IF (TEST .EQ. 'OK') THEN
             GO TO 500
            ELSE
             IF ( ZALPHP .GT. EPS) THEN
              RALPHA = PHASP(1)*R(1)+PHASP(2)*R(2)

              RALPHA = RALPHA/ZALPHP
              HALPHA = PHASP(1)*H(1)+ PHASP(2)*H(2)

              HALPHA = HALPHA/ZALPHP

             ELSE
              RALPHA = 0.D0
              HALPHA = 0.D0
             ENDIF
             RP= (1.D0-FMEL)*R(3)+FMEL*RALPHA
             RPRIM= (1.D0-FMEL)*H(3)+FMEL*HALPHA
             RP=RP-RPRIM*DP
             SEUIL= SIELEQ - (1.5D0*DEUXMU*TRANS + 1.D0) * RP
             CALL NZCALC(CRIT,PHASP,NZ,FMEL,SEUIL,DT,TRANS,
     &                  RPRIM,DEUXMU,ETA,UNSURN,DP,IRET)
             IF(IRET.EQ.1) GOTO 9999
            ENDIF
  223      CONTINUE
           IF ((J .EQ. NBMAX).AND. (TEST .EQ. 'NO'))
     &       CALL UTMESS ('F','NZISFW',
     &       'PROBLEME AVEC COURBE DE TRACTION')
           ENDIF
          ENDIF
  500     CONTINUE

          PLASTI = VIP(4)

          DO 160 K = 1,NDIMSI
            SIGDV(K) = SIGEL(K) - (1.5D0*DEUXMU*DP*SIG0(K))
            SIGDV(K) = SIGDV(K)/(1.5D0*DEUXMU*TRANS + 1.D0)
            SIGP(K) = SIGDV(K) + (SIGMO + TROISK*EPSMO)*KRON(K)
  160     CONTINUE
C
          DO 220 K=1,2
          IF (PHASP(K).GT. EPS)THEN
             DVIN(K) = DP
             VIP(K)  = VIM(K)+DVIN(K)
             IF(VIP(K) .LT. 0.D0) VIP(K)=0.D0
          ELSE
             DVIN(K) = 0.D0
             VIP(K)  = 0.D0
          ENDIF
 220      CONTINUE
          IF (ZBETAP .GT. EPS)THEN
             DVIN(3)  = DP
             VIP(3)  = VIM(3)+DVIN(3)
             IF(VIP(3) .LT. 0.D0) VIP(3)=0.D0
          ELSE
             DVIN(3)  = 0.D0
             VIP(3)   = 0.D0
          ENDIF



          DO 240 K=1,3
           ECRO(K)=R(K)-SY(K)
 240      CONTINUE
          IF ( ZALPHP .GT. EPS) THEN
             ECROAL = PHASP(1)*ECRO(1)+PHASP(2)*ECRO(2)

             ECROAL= ECROAL/ZALPHP
          ELSE
             ECROAL= 0.D0
          ENDIF
          VIP(5)= (1.D0-FMEL)*ECRO(3)+FMEL*ECROAL

          ENDIF
        ENDIF
C
C       --7 CALCUL OPERATEUR  TANGENT
C       ----------------------------------------
C
       IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG' .OR.
     &       OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN

           MODE=2
           DO 121 I=1,NZ
             IF (ETA(I) .GT. 0.D0) MODE=1
 121       CONTINUE

C       --MATRICE ELASTIQUE

           DO 120 K=1,6
              DSIDEP(K,K) =1.D0
 120       CONTINUE
           DO 130 K=1,3
            DO 131 L=1,3
              DSIDEP(K,L) = DSIDEP(K,L) -1.D0/3.D0
 131        CONTINUE
 130      CONTINUE
          IF ( OPTION(1:9) .EQ. 'FULL_MECA')
     &      COEF1=(1.5D0*DEUXMU*TRANS+1.D0)
          IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG')
     &      COEF1=1.D0
          DO 190 K=1,NDIMSI
           DO 191 L=1,NDIMSI
            DSIDEP(K,L) =DSIDEP(K,L)*DEUXMU/COEF1
 191       CONTINUE
 190      CONTINUE
          B=1.D0
          COEF2 =0.D0
          COEF3=0.D0
          IF (COMPOR(1)(1:4) .EQ. 'META') THEN
            IF (PLASTI .GE. 0.5D0) THEN            
              SIGEPS = 0.D0
              DO 170 K = 1,NDIMSI
                SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 170          CONTINUE
              IF (OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN
               
               IF ((MODE .EQ.1) .OR. ((MODE .EQ. 2) .AND.
     &             (SIGEPS.GE.0.D0))) THEN
                 B = (1.5D0*DEUXMU*DP/SIELEQ)
                 B=1-B
                 IF (MODE .EQ.1) THEN
                    IF (DP .EQ. 0.D0) THEN
                      DV=0.D0
                    ELSE
                     DO 178 I=1,3
                       N0(I) = (1-N(I))/N(I)
 178                 CONTINUE
                 DV=(1-FMEL)*ZBETAP*(ETA(3)/N(3)/DT)*((DP/DT)**N0(3))
                     DO 179 I=1,2
                     IF (PHASP(I) .GT. 0.D0)
     &                DV= DV+FMEL*(PHASP(I)/ZALPHP)*(ETA(I)/N(I)/DT)*
     &                    ((DP/DT)**N0(I))
 179                 CONTINUE
                   ENDIF
                 ELSE
                    DV=0.D0
                 ENDIF     
                 COEF2 = RPRIM +DV
                 COEF2 = (1.5D0*DEUXMU*TRANS+1.D0)*COEF2
                 COEF2 = (1.5D0*DEUXMU)+COEF2
                 COEF2 = 1/COEF2 - DP/SIELEQ
                 COEF2 =((1.5D0*DEUXMU)**2)*COEF2
               ENDIF
              ENDIF   
              IF (OPTION(1:14) .EQ. 'RIGI_MECA_TANG') THEN
               IF ((MODE .EQ. 2) .AND.(SIGEPS.GE.0.D0))  THEN
               COEF2 =  ( 1.5D0 * DEUXMU)**2
               COEF2 = COEF2/(1.5D0*DEUXMU+RPRIM)
               ENDIF
              ENDIF
              COEF3 = COEF2/COEF1
             ENDIF
            ENDIF
            DO 172 K=1,NDIMSI
                DO 173 L=1,NDIMSI
                   DSIDEP(K,L) = DSIDEP(K,L)*B
 173            CONTINUE
 172        CONTINUE

           DO 192 K = 1,3
            DO 193 L =1,3
             DSIDEP(K,L) = DSIDEP(K,L)+TROISK/3.D0
 193        CONTINUE
 192       CONTINUE
           DO 180 K=1,NDIMSI
               DO 181 L=1,NDIMSI
                  DSIDEP(K,L) = DSIDEP(K,L)- COEF3 *SIG0(K)*SIG0(L)
 181           CONTINUE
 180       CONTINUE
          
      ENDIF
      
 9999 CONTINUE

C FIN ------------------------------------------------------------------
      END
