      SUBROUTINE NZCIFW (NDIM,IMAT,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,EPSM,DEPS,SIGM,VIM,
     &                   PHASM,PHASP,OPTION,SIGP,VIP,DSIDEP)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/06/2001   AUTEUR CIBHHLV L.VIVAN 
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
      INTEGER            NDIM,IMAT
      CHARACTER*16       COMPOR(*),OPTION
      REAL*8             CRIT(3),INSTAM,INSTAP,TM,TP,TREF
      REAL*8             EPSM(6),DEPS(6),PHASM(7),PHASP(7)
      REAL*8             SIGM(6),VIM(37),SIGP(6),VIP(37),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     REALISE LA LOI DE VON MISES CINEMATIQUE POUR LES
C     ELEMENTS METALLURGIQUES EN PETITES DEFORMATIONS
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
      REAL*8             VALRES(20)
      REAL*8             DEPSTH(6),DEPSDV(6),SIGDV(6),SIGEL(6)
      REAL*8             EPSMO,SIGMO
      REAL*8             SEUIL,DP,PLASTI,B,RPRIM,RP,R(5),RM
      REAL*8             ZALPHM,ZALPHP,ZAUSTM,ZAUSTP
      REAL*8             TTRGM,COEF1M,COEF2M,EPSTHM
      REAL*8             TTRGP,COEF1P,COEF2P,EPSTHP
      REAL*8             DZ(4),DZ1(4),DZ2(4),DVIN(37),A,THETA(8)
      REAL*8             SIGMP(6),SIG0(6),SIMOEQ,SIELEQ,SIGEPS
      REAL*8             SIGEL2(6),DEPSP(6),VIMT(37)
      REAL*8             DT,N(5),N0(5),C(5),M(5),UNSURN(5),CR,ETA(5)
      REAL*8             KPT(3),TRANS,ZVARIM,ZVARIP,DELTAZ
      REAL*8             RBID,HALPHA,SYALPH,CM,MM
      REAL*8             HPLUS(5),H(5),RPLUS(5),SY(5),SYK,ECRO(5)
      REAL*8             DH(5)
      REAL*8             XK(6),X(35), ALM(6),ALMEQ, AR, DS(6)
      REAL*8             COEF1,COEF2,COEF3,DV,ECROAL,RAC2
      INTEGER            NDIMSI,MODE,JPROL,JVALE,NBVALE(5),NBMAX,NZ
      INTEGER            I,J,K,L
      CHARACTER*2        BL2, FB2, CODRET(20), TEST
      CHARACTER*8        NOMRES(20)
      CHARACTER*8        NOMCLE(5)
      DATA               KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

C 1 INITIALISATIONS

      FB2 = 'F '

      DO 100 K=1,6
      DO 100 L=1,6
        DSIDEP(K,L) = 0.D0
 100  CONTINUE
      IF(NDIM.EQ.2) THEN
        NDIMSI=4
      ELSE
        NDIMSI=6
      ENDIF

C   NOMBRE DE PHASE
      NZ=5
      
C MISE AU FORMAT DES CONTRAINTES DE RAPPEL
      RAC2 = SQRT(2.D0)
      
      DO 17 J=4,NDIMSI
         DO 177 K = 1,NZ
             L=J+(K-1)*6
             VIM(L)=VIM(L)*RAC2
 177     CONTINUE  
  17  CONTINUE    
  
      PLASTI = VIM(37)  
      
C 2 RECUPERATION DES CARACTERISTIQUES (ELAS ET DILATATION)

      IF (COMPOR(1)(1:4) .EQ. 'ELAS'.OR.
     &    COMPOR(1)(1:4) .EQ. 'META' ) THEN
       NOMRES(1)='E'
       NOMRES(2)='NU'
       NOMRES(3)='F_ALPHA'
       NOMRES(4)='C_ALPHA'
       NOMRES(5)='PHASE_REFE'
       NOMRES(6)='EPSF_EPSC_TREF'
       CALL RCVALA(IMAT,'ELAS_META',1,'TEMP',TM,6,NOMRES,VALRES,
     &             CODRET, FB2 )
       ZALPHM  = PHASM(1) + PHASM(2) + PHASM(3) + PHASM(4)
       ZAUSTM = 1.D0 - ZALPHM
       PHASM(5)=ZAUSTM
       TTRGM  = TM-TREF
       COEF1M = ZAUSTM*(VALRES(4)*TTRGM-(1.D0-VALRES(5))*VALRES(6))
       COEF2M = ZALPHM*(VALRES(3)*TTRGM + VALRES(5)*VALRES(6))
       EPSTHM = COEF1M + COEF2M
       EM     = VALRES(1)
       NUM    = VALRES(2)
       DEUMUM = EM/(1.D0+NUM)
       TROIKM = EM/(1.D0-2.D0*NUM)
       CALL RCVALA(IMAT,'ELAS_META',1,'TEMP',TP,6,NOMRES,
     &             VALRES,CODRET, FB2 )
       ZALPHP  = PHASP(1) + PHASP(2) + PHASP(3) + PHASP(4)
       ZAUSTP = 1.D0 - ZALPHP
       PHASP(5)=ZAUSTP
       TTRGP  = TP -TREF
       COEF1P = ZAUSTP*(VALRES(4)*TTRGP-(1.D0-VALRES(5))*VALRES(6))
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
       CALL RCVALA(IMAT,'ELAS_META',1,'META',ZALPHP,1,
     &             NOMRES(6),FMEL,CODRET(6),'  ')
       IF (CODRET(6).NE.'OK') FMEL = ZALPHP
       CALL RCVALA(IMAT,'ELAS_META',1,'TEMP',TP,5,
     &             NOMRES,VALRES,CODRET,'F ')
       SY(1)=VALRES(1)
       SY(2)=VALRES(2)
       SY(3)=VALRES(3)
       SY(4)=VALRES(4)
       SY(5)=VALRES(5)

        IF(COMPOR(1)(1:9).EQ.'META_P_CL' .OR.
     &    COMPOR(1)(1:9).EQ.'META_V_CL' )THEN
        NOMRES(7) ='F1_D_SIGM_EPSI'
        NOMRES(8) ='F2_D_SIGM_EPSI'
        NOMRES(9) ='F3_D_SIGM_EPSI'
        NOMRES(10) ='F4_D_SIGM_EPSI'
        NOMRES(11) ='C_D_SIGM_EPSI'

        CALL RCVALA(IMAT,'META_ECRO_LINE',1,'TEMP',TP,5,
     &              NOMRES(7),VALRES(7),CODRET(7),'F ')

        H(1)=(2.D0/3.D0)*VALRES(7)*E/(E-VALRES(7))
        H(2)=(2.D0/3.D0)*VALRES(8)*E/(E-VALRES(8))
        H(3)=(2.D0/3.D0)*VALRES(9)*E/(E-VALRES(9))
        H(4)=(2.D0/3.D0)*VALRES(10)*E/(E-VALRES(10))
        H(5)=(2.D0/3.D0)*VALRES(11)*E/(E-VALRES(11))

       ENDIF


C 5 RECUPERATION CARACTERISTIQUES (RESTAURATION ECROU DE METALLO)

       DO 200 K=1,4
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
     &     COMPOR(1)(1:16) .EQ. 'META_V_INL_PT_RE' .OR.
     &     COMPOR(1)(1:16) .EQ. 'META_P_CL_PT_RE' .OR.
     &     COMPOR(1)(1:12) .EQ. 'META_P_CL_RE' .OR.
     &     COMPOR(1)(1:12) .EQ. 'META_V_CL_RE' .OR.
     &     COMPOR(1)(1:15) .EQ. 'META_V_CL_PT_RE'
     &        ) THEN

        NOMRES(1) ='C_F1_THETA'
        NOMRES(2) ='C_F2_THETA'
        NOMRES(3) ='C_F3_THETA'
        NOMRES(4) ='C_F4_THETA'
        NOMRES(5) ='F1_C_THETA'
        NOMRES(6) ='F2_C_THETA'
        NOMRES(7) ='F3_C_THETA'
        NOMRES(8) ='F4_C_THETA'
        CALL RCVALA(IMAT,'META_RE',1,'TEMP ',TP,8,
     &              NOMRES,VALRES,CODRET, FB2)
        DO 23 I=1,8
         THETA(I) = VALRES(I)
 23     CONTINUE
       ELSE
        DO 19 I=1,8
         THETA(I) = 1.D0
 19     CONTINUE
       ENDIF

C 6 CARACTERISTIQUES MATERIAUX (VISCO)

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

        CALL RCVALA(IMAT,'META_VISC',1,'TEMP',TP,20,NOMRES,
     &              VALRES,CODRET,FB2)

        DO  29 I=1,NZ

           ETA(I) = VALRES(I)
           N(I) = VALRES(NZ+I)
           UNSURN(I)=1/N(I)
           C(I) =VALRES(2*NZ+I)
           M(I) = VALRES(3*NZ+I)
 29     CONTINUE
       ELSE
        DO 39 I=1,NZ
           ETA(I) = 0.D0
           N(I)= 20.D0
           UNSURN(I)= 1.D0
           C(I) = 0.D0
           M(I) = 20.D0
 39     CONTINUE
       ENDIF


C 7 CALCUL DE A (TERME ECROUISSAGE POUR DP=0),SYK

       DO 420  K= 1, NZ-1
          DO  4200 J= 1, NDIMSI
              L=J+(K-1)*6
              IF (PHASP(K).GT.0.D0)THEN
                  VIMT(L)=VIM(L)
                  DVIN(L) = DZ1(K)*THETA(K)*VIM(24+J)/PHASP(K)
     &                   - DZ1(K)*VIM(L)/PHASP(K)
                  VIM(L)  = VIM(L)+DVIN(L)
                 IF((VIM(L)*VIMT(L)) .LT. 0.D0) VIM(L)=0.D0
              ELSE
                  DVIN(L) = 0.D0
                  VIM(L)  = 0.D0
              ENDIF
 4200     CONTINUE
 420    CONTINUE
        IF (ZAUSTP .GT. 0.D0)THEN
           DO 432 J=1, NDIMSI
              DO 431 K= 1, NZ-1
                  VIMT(24+J)=VIM(24+J)
                  DVIN(24+J) = DZ2(K)*THETA(4+K)*VIM(J*K)/ZAUSTP
     &                      - DZ2(K)*VIM(24+J)/ZAUSTP
431           CONTINUE
               VIM(24+J)  = VIM(24+J)+DVIN(24+J)
               IF((VIM(24+J)* VIMT(24+J)) .LT. 0.D0) VIM(24+J)=0.D0
432        CONTINUE
         ELSE
            DVIN(24+J)  = 0.D0
            VIM(24+J)   = 0.D0
         ENDIF


C   CALCUL DE VARIABLE ECROUISSAGE ALPHA MOYEN
          DO 441 J=1,NDIMSI
             ALM(J) = 0.D0
             DO 440 K=1,NZ
                L=J+(K-1)*6
                ALM(J) = ALM(J)+PHASP(K)*VIM(L)
 440         CONTINUE
 441       CONTINUE

C      RESTAURATION ECROUISSAGE VISQUEUSE
       CM=ZAUSTP*C(5)
       DO 49 I=1,4
          CM=CM+PHASP(I)*C(I)
 49    CONTINUE

       MM=ZAUSTP*M(5)
       DO 59 I=1,4
          MM=MM+PHASP(I)*M(I)
 59    CONTINUE
       ALMEQ = 0.D0
       DO 590 J=1,NDIMSI
           ALMEQ=ALMEQ+ALM(J)**2.D0
 590   CONTINUE
       ALMEQ = SQRT(1.5D0*ALMEQ)
       AR=ALMEQ*CM
       DO 591 J=1,NDIMSI
          IF (AR .LE. 0.D0) THEN
             DS(J)=0.D0
          ELSE
             DS(J)= DT*((AR)**MM)*ALM(J)/ALMEQ
          ENDIF
 591   CONTINUE
       DO 421 K=1,5
         DO 4210 J=1,NDIMSI
           L=J+(K-1)*6
           IF (PHASP(K).GT.0.D0)THEN

              DVIN(L) = -(DS(J))
              VIMT(L)=VIM(L)
              VIM(L)  = VIM(L)+DVIN(L)
              VIM(L)  = VIM(L)+DVIN(L)
             IF((VIM(L)*VIMT(L)) .LT. 0.D0) VIM(L)=0.D0

           ELSE
              DVIN(L) = 0.D0
              VIM(L)  = 0.D0
           ENDIF
 4210  CONTINUE
 421   CONTINUE

C  CALCUL DES XK (TERME ECROUISSAGE MOYEN), SYK (MOYEN)



       IF ( ZALPHP .GT. 0.D0) THEN
        SYALPH = PHASP(1)*SY(1)+PHASP(2)*SY(2)+PHASP(3)*SY(3)
     &         + PHASP(4)*SY(4)
        SYALPH = SYALPH/ZALPHP
       ELSE
        SYALPH = 0.D0
       ENDIF
       SYK  =(1.D0-FMEL)*SY(5)+FMEL*SYALPH
       DO 88 K=1,NZ
          DO 881 J=1,NDIMSI
             L=J+(K-1)*6
             X(L)=H(K)*VIM(L)
881       CONTINUE
88     CONTINUE
       DO 882 J=1,NDIMSI
          XK(J)=0.D0
          DO 883 K=1,NZ
             L=J+(K-1)*6
             XK(J)= XK(J)+PHASP(K)*X(L)
883       CONTINUE
882    CONTINUE
       RPRIM =0.D0
       DO 884 K=1,NZ
          RPRIM = RPRIM+PHASP(K)*H(K)
884    CONTINUE
       RPRIM = 3.D0/2.D0*RPRIM

C 8 RECUPERATION CARACTERISTIQUES (PLAS DE TRANSFORMATION)

      TRANS = 0.D0
      IF (COMPOR(1)(1:12) .EQ. 'META_P_IL_PT'    .OR.
     &    COMPOR(1)(1:13) .EQ. 'META_P_INL_PT'   .OR.
     &    COMPOR(1)(1:15) .EQ. 'META_P_IL_PT_RE' .OR.
     &    COMPOR(1)(1:16) .EQ. 'META_P_INL_PT_RE'.OR.
     &    COMPOR(1)(1:12) .EQ. 'META_V_IL_PT'    .OR.
     &    COMPOR(1)(1:13) .EQ. 'META_V_INL_PT'   .OR.
     &    COMPOR(1)(1:15) .EQ. 'META_V_IL_PT_RE' .OR.
     &    COMPOR(1)(1:16) .EQ. 'META_V_INL_PT_RE' .OR.
     &     COMPOR(1)(1:15) .EQ. 'META_P_CL_PT_RE' .OR.
     &     COMPOR(1)(1:12) .EQ. 'META_P_CL_PT' .OR.
     &     COMPOR(1)(1:12) .EQ. 'META_V_CL_PT' .OR.
     &     COMPOR(1)(1:15) .EQ. 'META_V_CL_PT_RE'   ) THEN

       NOMRES(1)  = 'F1_K'
       NOMRES(2) = 'F2_K'
       NOMRES(3) = 'F3_K'
       NOMRES(4) = 'F4_K'
       NOMRES(5) = 'F1_D_F_META'
       NOMRES(6) = 'F2_D_F_META'
       NOMRES(7) = 'F3_D_F_META'
       NOMRES(8) = 'F4_D_F_META'

       CALL RCVALA(IMAT,'META_PT',1,'TEMP',TP,4,
     &             NOMRES,VALRES,CODRET, FB2 )
       DO 20 I=1,4
        KPT (I) = VALRES(I)
 20    CONTINUE
       DO 21 I=1,4
        ZVARIM  = PHASM(I )
        ZVARIP  = PHASP(I )
        DELTAZ = (ZVARIP - ZVARIM)
        IF (DELTAZ.GT.0) THEN
         J = 4+I
         CALL RCVALA(IMAT,'META_PT',1,'META',ZALPHP,1,
     &               NOMRES(J),VALRES(J),CODRET(J), FB2 )
          TRANS = TRANS + KPT(I)*VALRES(J)*(ZVARIP-ZVARIM)
        ENDIF
 21    CONTINUE
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
          SIGEL2(K)= SIGEL(K)-(1.5D0*DEUXMU*TRANS + 1.D0)*XK(K)
          
          SIELEQ     = SIELEQ + SIGEL2(K)**2
 117    CONTINUE
        SIELEQ     = SQRT(1.5D0*SIELEQ)
        IF (SIELEQ .GT. 0.D0) THEN
         DO 118 K = 1,NDIMSI
           SIG0(K) = SIGEL2(K)/SIELEQ

 118     CONTINUE

        ELSE
        DO 129 K = 1,NDIMSI
           SIG0(K) = 0.D0
 129     CONTINUE
        ENDIF
        DP = 0.D0
        SEUIL      = SIELEQ - (1.5D0*DEUXMU*TRANS + 1.D0) * SYK

C
C  10-- CALCUL DE DP,SIGDV(+),VIP
C       --------------------------
C       VIP(37) = 0.D0

       
       IF (OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA' ) THEN
        IF (COMPOR(1)(1:4) .EQ. 'ELAS') THEN
         DO 145 K = 1,NDIMSI
          SIGP(K) = SIGMP(K)+DEUXMU*DEPSDV(K)+TROISK*EPSMO*KRON(K)
 145     CONTINUE
        ELSE IF (COMPOR(1)(1:4) .EQ. 'META') THEN
          IF (SEUIL.LT.0.D0) THEN
            VIP(37) = 0.D0
            DP = 0.D0
          ELSE
            VIP(37) = 1.D0
            CALL NZCALC(NDIM,IMAT,CRIT,PHASP,NZ,FMEL,SEUIL,DT,TRANS,
     &                RPRIM,DEUXMU,ETA,UNSURN,DP)
          ENDIF
          PLASTI=VIP(37)
  
          DO 160 K = 1,NDIMSI
              DEPSP(K)=1.5D0*DP*SIG0(K)
              SIGDV(K) = SIGEL(K) - DEUXMU*DEPSP(K)
              SIGDV(K) = SIGDV(K)/(1.5D0*DEUXMU*TRANS + 1.D0)
              SIGP(K) = SIGDV(K) + (SIGMO + TROISK*EPSMO)*KRON(K)
  160     CONTINUE
C
          DO 220 K=1,5
              DO 2200 J=1,NDIMSI
              L=J+(K-1)*6
              IF (PHASP(K).GT.0.D0)THEN

                  DVIN(L) = DEPSP(J)
                  VIP(L)  = VIM(L)+DVIN(L)
             ELSE
                  DVIN(L) = 0.D0
                  VIP(L)  = 0.D0
              ENDIF
 2200         CONTINUE
 220       CONTINUE

            DO 242 K=1,NZ
               DO 243 J=1,NDIMSI
                 L=J+(K-1)*6
                 X(L)=H(K)*VIP(L)
243            CONTINUE
242         CONTINUE

            DO 240 J=1,NDIMSI
                XK(J)=0.D0
                DO 241 K=1,NZ
                   L=J+(K-1)*6
                   XK(J)= XK(J)+PHASP(K)*X(L)
241             CONTINUE
               VIP(30+J)=XK(J)

240        CONTINUE

        ENDIF
      ENDIF


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
              
              IF (OPTION(1:9)  .EQ. 'FULL_MECA' ) THEN
               SIGEPS = 0.D0
               DO 170 K = 1,NDIMSI
                SIGDV(K)=SIGDV(K)-XK(K)
                SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 170           CONTINUE
               IF ((MODE .EQ.1) .OR. ((MODE .EQ. 2) .AND.
     &             (SIGEPS.GE.0.D0))) THEN
                 B = (1.5D0*DEUXMU*DP/SIELEQ)
                 B=1-B
                 IF (MODE .EQ.1) THEN
                    IF (DP .EQ. 0.D0) THEN
                      DV=0.D0
                    ELSE
                     DO 178 I=1,5
                       N0(I) = (1-N(I))/N(I)
 178                 CONTINUE
                 DV=(1-FMEL)*ZAUSTP*(ETA(5)/N(5)/DT)*((DP/DT)**N0(5))
                     DO 179 I=1,4
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
                SIGEPS = 0.D0
               DO 1722 K = 1,NDIMSI
                SIGDV(K)=SIGDV(K)-XK(K)
                SIGEPS = SIGEPS + SIGDV(K)*DEPSDV(K)
 1722           CONTINUE
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
 173              CONTINUE
 172            CONTINUE

          DO 192 K = 1,3
           DO 193 L =1,3
            DSIDEP(K,L) = DSIDEP(K,L)+TROISK/3.D0
 193       CONTINUE
 192      CONTINUE
           DO 180 K=1,NDIMSI
                DO 181 L=1,NDIMSI
                  DSIDEP(K,L) = DSIDEP(K,L)- COEF3 *SIG0(K)*SIG0(L)
 181            CONTINUE
 180           CONTINUE
          
      ENDIF
      
C    MISE AU FORMAT CONTRAINTE DE RAPPEL
           RAC2 = SQRT(2.D0)
           DO 245 J=4,NDIMSI
               DO 246 K = 1,NZ
               L=J+(K-1)*6
               VIM(L)=VIM(L)/RAC2
 246           CONTINUE    
 245       CONTINUE
           IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA')     THEN
              DO 2451 J=4,NDIMSI
               DO  2461 K = 1,NZ
               L=J+(K-1)*6
               VIP(L)=VIP(L)/RAC2
 2461         CONTINUE             
 2451          CONTINUE
           ENDIF
         

 9999 CONTINUE

C FIN ------------------------------------------------------------------
      END
