      SUBROUTINE NMCCAM (NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &                   INSTAM,INSTAP,TM,TP,TREF,DEPS,SIGM,PCRM,
     &                   OPTION,SIGP,PCRP,DSIDEP,RETCOM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C  TOLE CRP_20
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      INTEGER            NDIM,IMATE,RETCOM
      CHARACTER*8        TYPMOD(*)
      CHARACTER*16       COMPOR(*),OPTION
      REAL*8             CRIT(3),INSTAM,INSTAP,TM,TP,TREF
      REAL*8             DEPS(6),DEUXMU
      REAL*8             SIGM(6),PCRM(7),SIGP(6),PCRP(7),DSIDEP(6,6)
C ----------------------------------------------------------------------
C     REALISE LA LOI DE CAM CLAY ELASTOPLASTIQUE POUR LES
C     ELEMENTS ISOPARAMETRIQUES EN PETITES DEFORMATIONS
C
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  IMATE   : ADRESSE DU MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
C IN  INSTAP  : INSTANT DU CALCUL
C IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPS    : INCREMENT DE DEFORMATION
C IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
C IN  PCRM    : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
C IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
C OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
C OUT PCRP    : VARIABLES INTERNES A L'INSTANT ACTUEL
C OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
C
C               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
C               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
C
C
C
C
      LOGICAL     CPLAN
      INTEGER     IADZI,IAZK24,IRET, IISNAN
      REAL*8      EPXMAX
      PARAMETER   (EPXMAX = 5.D0)
      CHARACTER*8 NOMAIL
      REAL*8      VALRES(10),VALPAM(3)
      REAL*8      MU, LAMBDA,KAPA,PORO,PRESCR,M, KCAM, PTRAC
      REAL*8      COEF,PORO1,PORO2,YOUNG,NU,E0,XK0,XK,FONC
      REAL*8      DEPSMO,DEPPMO,DEPSEQ
      REAL*8      SIGMMO,SIGPMO,DELTAP,SIEQM,SIEQP,SIELEQ,SIMOEL,SPARDS
      REAL*8      KRON(6),DEPSDV(6),DEPSTH(6),SIGMDV(6),SIGPDV(6)
      REAL*8      DELTAS(6),SIGEL(6),TPLUS(6)
      REAL*8      A(6),AA(6),FV(6)
      REAL*8      FFI(6,6),EE(6,6),C(6,6),CC(6,6)
      REAL*8      V(6,6),S(6,6),T(6,6),VV(6,6)
      REAL*8      HH(6,6),SES(6,6),GG(6,6),SPS(6,6),HHM(6,6)
      REAL*8      D1G(6,6),D1GHHM(6,6),ID2(6,6),DEVHYD(6,6),DEVHYM(6,6)
      REAL*8      F1,F2,F3,F4,F,FP
      REAL*8      F1P, F2P, F3P, F4P
      REAL*8      FXI1,FXI2,FXI3,FXI4,FXI
      REAL*8      H,HP,XC,XD,XLAM,XA,XU,XG,XH,XE,XF,XV,XI,RAP
      REAL*8      CT,V0,SEUIL
      REAL*8      XINF,XSUP,RBID
      REAL*8      DIFF,DIFF2
      REAL*8      ZERO,UN,DEUX,TROIS,SIX,UNSDE,TOL,PTIT,R8MIEM
      REAL*8      VALM,VALP
      INTEGER     NDIMSI,SIGNF,SIGNFI
      INTEGER     I,K,L,ITER, MATR
      INTEGER ICODRE(9)
      CHARACTER*8 NOMRES(10),NOMPAR(10)
C ======================================================================
      REAL*8       VALRM(5)
      INTEGER      VALIM
      CHARACTER*16 VALKM(5)
C ======================================================================
      PARAMETER   ( ZERO   = 0.D0   )
      PARAMETER   ( UN     = 1.D0   )
      PARAMETER   ( DEUX   = 2.D0   )
      PARAMETER   ( TROIS  = 3.D0   )
      PARAMETER   ( SIX    = 6.D0   )
      PARAMETER   ( UNSDE  = 0.5D0  )


      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA        TOL/1.D-6/
C DEB ------------------------------------------------------------------
C
C     -- 1 INITIALISATIONS :
C     ----------------------
      CPLAN  =  TYPMOD(1) .EQ. 'C_PLAN'
      NDIMSI = 2*NDIM
      RETCOM = 0
C
      PTIT = R8MIEM()
C
C     -- 2 RECUPERATION DES CARACTERISTIQUES MATERIAUX
C     -------------------------------------------------
      NOMRES(1)='ALPHA'
      NOMRES(2)='MU'
      NOMRES(3)='PORO'
      NOMRES(4)='KAPA'
      NOMRES(5)='LAMBDA'
      NOMRES(6)='M'
      NOMRES(7)='PRES_CRIT'
      NOMRES(8)='KCAM'
      NOMRES(9)='PTRAC'
C
      NOMPAR(1) = 'TEMP'
      VALPAM(1) = TM
C

      IF (COMPOR(1)(1:9) .EQ. 'CAM_CLAY ' ) THEN

         CALL RCVALA(IMATE,' ','ELAS',1,NOMPAR,VALPAM,1,
     &                 NOMRES(1),VALRES(1),ICODRE(1), 0)

         IF ((IISNAN(TP).EQ.0).AND.(IISNAN(TM).GT.0)) THEN
           IF ((IISNAN(TREF).GT.0).OR.(ICODRE(1) .NE.0)) THEN
             CALL U2MESS('F','CALCULEL_31')
           ELSE
             COEF = VALRES(1)*(TP-TREF) - VALRES(1)*(TM-TREF)
           ENDIF
         ELSE
             VALRES(1) = 0.D0
             COEF = 0.D0
         ENDIF
         CALL RCVALA(IMATE,' ','CAM_CLAY ',1,NOMPAR,VALPAM,8,
     &                 NOMRES(2),VALRES(2),ICODRE(2), 2)
         MU     = VALRES(2)
         PORO   = VALRES(3)
         KAPA   = VALRES(4)
         LAMBDA = VALRES(5)
         M      = VALRES(6)
         PRESCR = VALRES(7)
         KCAM   = VALRES(8)
         PTRAC  = VALRES(9)

      ENDIF
      IF (((COMPOR(1)(1:6) .EQ. 'KIT_HM') .OR.
     &     (COMPOR(1)(1:7) .EQ. 'KIT_HHM') .OR.
     &     (COMPOR(1)(1:7) .EQ. 'KIT_THM') .OR.
     &     (COMPOR(1)(1:8) .EQ. 'KIT_THHM')).AND.
     &     (COMPOR(11)(1:9) .EQ. 'CAM_CLAY ')) THEN

         CALL RCVALA(IMATE,' ','ELAS',1,NOMPAR,VALPAM,1,
     &                 NOMRES(1),VALRES(1),ICODRE(1), 0)
         IF ( ICODRE(1) .NE.0    ) VALRES(1) = 0.D0
         COEF = VALRES(1)*(TP-TREF) - VALRES(1)*(TM-TREF)

         CALL RCVALA(IMATE,' ','CAM_CLAY ',1,NOMPAR,VALPAM,8,
     &                 NOMRES(2),VALRES(2),ICODRE(2), 2)
         MU     = VALRES(2)
         PORO   = VALRES(3)
         PORO1  = PORO
         KAPA   = VALRES(4)
         LAMBDA = VALRES(5)
         M      = VALRES(6)
         PRESCR = VALRES(7)
         KCAM   = VALRES(8)
         PTRAC  = VALRES(9)

         CALL RCVALA(IMATE,' ','THM_INIT',1,NOMPAR,VALPAM,1,
     &                 NOMRES(3),VALRES(3),ICODRE(3), 2)
         PORO = VALRES(3)
         PORO2 = PORO
         DIFF = PORO1-PORO2
         IF (ABS(DIFF) .GT. TOL) THEN
           CALL U2MESS('F','ALGORITH6_60')
         ELSE
           PORO=PORO1
         ENDIF
      ENDIF
         DEUXMU = DEUX*MU
         E0     = PORO/(1.D0-PORO)
         XK0    = (1.D0+E0)/KAPA
         XK     = (1.D0+E0)/(LAMBDA-KAPA)
         IF ((KCAM.NE.ZERO).AND.(KCAM.LE.(-XK0*PTRAC))) THEN
             CALL U2MESS('F','COMPOR1_42')
         ENDIF
C
C     -- 3 CALCUL DE DEPSMO ET DEPSDV :
C     --------------------------------
      IF (CPLAN)THEN
           CALL U2MESS('F','ALGORITH6_63')
      ENDIF
      DEPSMO = 0.D0
      DO 110 K=1,NDIMSI
        DEPSTH(K) = DEPS(K)
 110  CONTINUE
      DO 111 K=1,3
        DEPSTH(K) = DEPSTH(K) - COEF
        DEPSMO = DEPSMO + DEPSTH(K)
 111  CONTINUE
      DEPSMO = -DEPSMO
      DO 115 K=1,NDIMSI
        DEPSDV(K)   = DEPSTH(K) + DEPSMO/3.D0 * KRON(K)
 115  CONTINUE
C
C     -- 4 CALCUL DE SIGMMO, SIGMDV, SIGEL,SIMOEL,SIELEQ, SIEQM :
C     -------------------------------------------------------------
      SIGMMO = 0.D0
      DO 116 K =1,3
        SIGMMO = SIGMMO + SIGM(K)
 116  CONTINUE
      SIGMMO = -SIGMMO /3.D0
      IF (SIGMMO.LT.PTRAC) THEN
           CALL U2MESS('F','ALGORITH6_64')
      ENDIF
      SIELEQ = 0.D0
      SIEQM = 0.D0
      DO 117 K = 1,NDIMSI
        SIGMDV(K) = SIGM(K) + SIGMMO * KRON(K)
        SIEQM = SIEQM + SIGMDV(K)**2
        SIGEL(K)  = SIGMDV(K) + DEUXMU * DEPSDV(K)
        SIELEQ    = SIELEQ + SIGEL(K)**2
 117  CONTINUE
      SIELEQ   = SQRT(1.5D0*SIELEQ)
      SIEQM    = SQRT(1.5D0*SIEQM)

         IF (  ((XK0*DEPSMO) .GT. EPXMAX) ) THEN

           CALL TECAEL(IADZI,IAZK24)
           VALKM(1) = ZK24(IAZK24-1+3) (1:8)
           VALRM(1) =  EPXMAX
           VALKM(2) ='EXP(XK0*DEPSMO)'
           VALRM(2) = (XK0*DEPSMO)
           VALKM(3) ='EXP(XK*DEPSMO)'
           VALRM(3) = (XK*DEPSMO)
           CALL U2MESG('A','COMPOR1_41',3,VALKM,0,VALIM,3,VALRM)
           RETCOM = 1
           GO TO 30
         ENDIF
      SIMOEL = SIGMMO*EXP(XK0*DEPSMO)+KCAM/XK0*(EXP(XK0*DEPSMO)-UN)
C ---- INITIALISATION A T=0
      IF (PCRM(1).EQ. 0.D0)  THEN

        PCRM(1) = PRESCR
        PCRM(3) = SIMOEL
        PCRM(4) = SIELEQ
        PCRM(5) = 0.D0
        PCRM(6) = 0.D0
        PCRM(7) = E0

C ---- ON VERIFIE LA COHERENCE DES DONNEES MECA DE DEPART
        NU = (TROIS*((UN+E0)*SIGMMO+KAPA*KCAM)-DEUXMU*KAPA)/
     &         (SIX*((UN+E0)*SIGMMO+KAPA*KCAM)+DEUXMU*KAPA)

        YOUNG = DEUXMU*(UN+NU)

        IF ((YOUNG.LE.ZERO).OR.
     &      (NU.LE.ZERO).OR.
     &      (NU.GT.UNSDE)) THEN
          CALL TECAEL(IADZI,IAZK24)
          NOMAIL = ZK24(IAZK24-1+3) (1:8)
          CALL U2MESK('F','COMPOR1_3',1,NOMAIL)
        ENDIF

      ENDIF

C
C     -- 5 CALCUL DU CRITERE :
C     ----------------------
        FONC = SIELEQ**2+M*M*(SIMOEL-PTRAC)**2-
     &              2.D0*M*M*(SIMOEL-PTRAC)*PCRM(1)

C     -- 6  TEST DE PLASTIFICATION ET CALCUL DE PCRP SIGP, SIGPDV :
C     ------------------------------------------------------------
      IF ( OPTION(1:9) .EQ. 'RAPH_MECA' .OR.
     &     OPTION(1:9) .EQ. 'FULL_MECA'     ) THEN
        IF (FONC.LE.0.D0) THEN
C      -- TRAITEMENT DE L'ELASTICITE
           PCRP(1) = PCRM(1)
           PCRP(2) = 0.D0
            DO 118 K=1,NDIMSI
              SIGPDV(K) = SIGEL(K)
              SIGP(K)   = SIGEL(K)-SIMOEL*KRON(K)
 118  CONTINUE

           PCRP(3) = SIMOEL
           PCRP(4) = SIELEQ
           PCRP(5) = 0.D0
           PCRP(6) = 0.D0

           IF ((PCRM(3).NE.ZERO).AND.((PCRP(3)/PCRM(3)).GT.ZERO)) THEN

             PCRP(7) = PCRM(7) - KAPA*LOG(PCRP(3)/PCRM(3))

           ELSE

             PCRP(7) = PCRM(7)

           ENDIF

        ELSE
C     -- PLASTIFICATION : CALCUL DE LA DEFORMATION
C     -- VOLUMIQUE PLASTIQUE : DEPPMO
         PCRP(2) = 1.D0
         SEUIL = M**2*(PCRM(1)-PTRAC)**2

         XINF = 0.D0
C     -- RECHERCHE DE LA BORNE SUP

         IF (ABS((SIMOEL - PCRM(1) - PTRAC)/SEUIL).LT. PTIT) THEN
           V0=0.D0
           GOTO 100
         ENDIF

         IF (ABS(XK0*SIMOEL + KCAM + XK*PCRM(1)).LT. PTIT) THEN

           IF ((-DEUX*(SIMOEL-PCRM(1)-PTRAC)/
     &           (XK0*SIMOEL+KCAM-XK*PTRAC)).LT.ZERO) THEN

              XSUP = 1.D0/(XK+XK0)*LOG(ABS(SIMOEL-PTRAC)/PCRM(1))

           ELSE
C       RESULTAT D UN DEVELOPPEMENT LIMITE D ORDRE 2

             IF ((SIMOEL-PTRAC).GT.PCRM(1))THEN

              XSUP = SQRT((-DEUX*(SIMOEL-PCRM(1)-PTRAC)/
     &           (XK0*SIMOEL+KCAM-XK*PTRAC)))

             ELSE

              XSUP = -SQRT((-DEUX*(SIMOEL-PCRM(1)-PTRAC)/
     &           (XK0*SIMOEL+KCAM-XK*PTRAC)))
             ENDIF

          ENDIF
        ELSE
C       RESULTAT D UN DEVELOPPEMENT LIMITE D ORDRE 1
              XSUP = (SIMOEL - PCRM(1) - PTRAC)/
     &          (XK0*SIMOEL + KCAM + XK*PCRM(1))

        ENDIF

C     --RESOLUTION AVEC LA METHODE DE NEWTON ENTRE LES BORNES
         V0 = XINF


         IF (  ((-XK0*V0) .GT. EPXMAX) .OR.
     &         ((XK*V0)   .GT. EPXMAX) ) THEN

           CALL TECAEL(IADZI,IAZK24)
           VALKM(1) = ZK24(IAZK24-1+3) (1:8)
           VALRM(1) =  EPXMAX
           VALKM(2) ='EXP(-XK0*V0)'
           VALRM(2) = (-XK0*V0)
           VALKM(3) ='EXP(XK*V0)'
           VALRM(3) = (XK*V0)
           CALL U2MESG('A','COMPOR1_41',3,VALKM,0,VALIM,3,VALRM)
           RETCOM = 1
           GO TO 30
         ENDIF

           F1 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
           F2 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
     &           -2.D0*PCRM(1)*EXP(XK*V0)
           F3 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
     &           -PCRM(1)*EXP(XK*V0)
           F4 = (1.D0+3.D0*DEUXMU*V0/2.D0/M/M/F3)

           F =SIELEQ**2+M**2*F4**2*F1*F2


           F1P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
           F2P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
     &                 -2.D0*XK*PCRM(1)*EXP(XK*V0)
           F3P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
     &                      -XK*PCRM(1)*EXP(XK*V0)
           F4P = 3.D0*DEUXMU/2.D0/(M**2)*(F3-V0*F3P)/F3/F3

           FP = M**2*F4**2*(F1P*F2 + F1*F2P) +
     &          2.D0*M**2*F4*F4P*F1*F2

C
           DO 200 ITER = 1, NINT(CRIT(1))

C     --CRITERE DE CONVERGENCE
           IF ((ABS(F)/SEUIL) . LE. CRIT(3))   GOTO 100

C     --CONSTRUCTION DU NOUVEL ITERE
           V0 = V0-F/FP
           IF (XSUP.GT.0.D0) THEN
             IF (V0.LE.XINF .OR. V0.GE.XSUP)  V0 = (XINF+XSUP)/2
           ELSE
             IF (V0.LE.XSUP .OR. V0.GE.XINF)  V0 = (XINF+XSUP)/2
           ENDIF

C     --CALCUL DE LA FONCTION EN V0 ET DE SA DERIVEE

           IF (  ((-XK0*V0) .GT. EPXMAX) .OR.
     &           ((XK*V0)   .GT. EPXMAX)  ) THEN

             CALL TECAEL(IADZI,IAZK24)
             VALKM(1) = ZK24(IAZK24-1+3) (1:8)
             VALRM(1) =  EPXMAX
             VALKM(2) ='EXP(-XK0*V0)'
             VALRM(2) = (-XK0*V0)
             VALKM(3) ='EXP(XK*V0)'
             VALRM(3) = (XK*V0)
             CALL U2MESG('A','COMPOR1_41',3,VALKM,0,VALIM,3,VALRM)
             RETCOM = 1
             GO TO 30
           ENDIF
           F1 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
           F2 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
     &           -2.D0*PCRM(1)*EXP(XK*V0)
           F3 = (SIMOEL+KCAM/XK0)*EXP(-XK0*V0)-KCAM/XK0-PTRAC
     &           -PCRM(1)*EXP(XK*V0)
           F4 = (1.D0+3.D0*DEUXMU*V0/2.D0/M/M/F3)

           F =SIELEQ**2+M**2*F4**2*F1*F2

           IF (F.GT.ZERO) SIGNF =  1
           IF (F.LT.ZERO) SIGNF = -1

           F1P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
           F2P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
     &                 -2.D0*XK*PCRM(1)*EXP(XK*V0)
           F3P =-(XK0*SIMOEL+KCAM)*EXP(-XK0*V0)
     &                      -XK*PCRM(1)*EXP(XK*V0)
           F4P = 3.D0*DEUXMU/2.D0/(M**2)*(F3-V0*F3P)/F3/F3

           FP = M**2*F4**2*(F1P*F2 + F1*F2P) +
     &          2.D0*M**2*F4*F4P*F1*F2


         IF (  ((-XK0*XINF) .GT. EPXMAX) .OR.
     &         ((XK*XINF)   .GT. EPXMAX)  ) THEN

            CALL TECAEL(IADZI,IAZK24)
            VALKM(1) = ZK24(IAZK24-1+3) (1:8)
            VALRM(1) =  EPXMAX
            VALKM(2) ='EXP(-XK0*XINF)'
            VALRM(2) = (-XK0*XINF)
            VALKM(3) ='EXP(XK*XINF)'
            VALRM(3) = (XK*XINF)
            CALL U2MESG('A','COMPOR1_41',3,VALKM,0,VALIM,3,VALRM)
            RETCOM = 1
            GO TO 30
         ENDIF

          FXI1 = (SIMOEL+KCAM/XK0)*EXP(-XK0*XINF)-KCAM/XK0-PTRAC
          FXI2 = (SIMOEL+KCAM/XK0)*EXP(-XK0*XINF)-KCAM/XK0-PTRAC
     &             -2.D0*PCRM(1)*EXP(XK*XINF)
          FXI3 = (SIMOEL+KCAM/XK0)*EXP(-XK0*XINF)-KCAM/XK0-PTRAC
     &            -PCRM(1)*EXP(XK*XINF)
          FXI4 = (1.D0+3.D0*DEUXMU*XINF/2.D0/M/M/FXI3)

          FXI=SIELEQ**2+M**2*FXI4**2*FXI1*FXI2

          IF (FXI.GT.ZERO) SIGNFI =  1
          IF (FXI.LT.ZERO) SIGNFI = -1

          IF ((SIGNF*SIGNFI).LT.ZERO) XSUP = V0
          IF ((SIGNF*SIGNFI).GT.ZERO) XINF = V0

 200  CONTINUE
         RETCOM = 1
         GO TO 30
 100  CONTINUE
          DEPPMO=V0
C
C     -- REACTUALISATION DE LA VARIABLE INTERNE
         IF (  ((XK*DEPPMO) .GT. EPXMAX) .OR.
     &          (XK0*(DEPSMO-DEPPMO)   .GT. EPXMAX)  ) THEN

            CALL TECAEL(IADZI,IAZK24)
            VALKM(1) = ZK24(IAZK24-1+3) (1:8)
            VALRM(1) =  EPXMAX
            VALKM(2) ='EXP(XK*DEPPMO)'
            VALRM(2) = (XK*DEPPMO)
            VALKM(3) ='EXP(XK0*(DEPSMO-DEPPMO))'
            VALRM(3) = (XK0*(DEPSMO-DEPPMO))
            CALL U2MESG('A','COMPOR1_41',3,VALKM,0,VALIM,3,VALRM)
            RETCOM = 1
            GO TO 30
         ENDIF


          PCRP(1) = PCRM(1)*EXP(XK*DEPPMO)
C     -- REACTUALISATION DES CONTRAINTES
          SIGPMO = (SIGMMO+KCAM/XK0)*EXP(XK0*(DEPSMO-DEPPMO))-KCAM/XK0
          CALL R8INIR(6,0.D0,SIGPDV,1)
           DO 119 K=1,NDIMSI
             SIGPDV(K) = SIGEL(K)/(1.D0+(3.D0*DEUXMU/2.D0*DEPPMO)/
     &                (M*M*(SIGPMO-PCRP(1)-PTRAC)))
             SIGP(K) = SIGPDV(K)-SIGPMO*KRON(K)
 119  CONTINUE


C ---- V(3) CONTRAINTE VOLUMIQUE
          PCRP(3) = SIGPMO

C ---- V(4) CONTRAINTE EQUIVALENTE
          SIEQP = 0.0D0
          DO 440 K = 1, NDIMSI
           SIEQP = SIEQP + SIGPDV(K)**2.D0
 440    CONTINUE
          PCRP(4) = SQRT(1.5D0*SIEQP)

C ---- V(5) DEFORMATION PLASTIQUE VOLUMIQUE
          PCRP(5) = PCRM(5) + DEPPMO

C ---- V(6) DEFORMATION PLASTIQUE EQUIVALENTE
          DEPSEQ = 0.0D0
          DO 450 K = 1, NDIMSI
            DEPSEQ = DEPSEQ + DEPSDV(K)*DEPSDV(K)
 450    CONTINUE
          DEPSEQ = SQRT(2.D0/3.D0*DEPSEQ)
          PCRP(6) = PCRM(6) + DEPSEQ

C ---- V(7) :: INDICE DES VIDES
          IF ((PCRM(3).NE.ZERO).AND.
     &        ((PCRP(3)/PCRM(3)).GT.ZERO).AND.
     &        ((PCRP(1)/PCRM(1)).GT.ZERO).AND.
     &        (PCRM(1).NE.ZERO)) THEN

           PCRP(7) = PCRM(7) - KAPA * LOG(PCRP(3)/PCRM(3))
     &  - (LAMBDA-KAPA) * LOG(PCRP(1)/PCRM(1))

           ELSE

           PCRP(7) = PCRM(7)

          ENDIF

        ENDIF

      ENDIF

C
C     -- 7 CALCUL DE L'OPERATEUR TANGENT :
C     --------------------------------
      IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG'.OR.
     &     OPTION(1:9)  .EQ. 'FULL_MECA'         ) THEN
C
       IF ( OPTION(1:14) .EQ. 'RIGI_MECA_TANG' ) THEN
           IF (PCRM(2) .EQ. 0.D0) THEN
            MATR = 0
           ELSE
            MATR = 1
           END IF
       END IF
       IF ( OPTION(1:9) .EQ. 'FULL_MECA' ) THEN
           IF (PCRP(2) .EQ. 1.D0) THEN
            MATR = 2
           ELSE
            MATR = 0
           END IF
       END IF
C      INITIALISATION DE L'OPERATEUR TANGENT
C     ---------------------------------------
         CALL MATINI(6,6,0.D0,DSIDEP)
C
C     -- 7.1 CALCUL DE DSIDEP(6,6)-ELASTIQUE:
C     ---------------------------------------
        IF (MATR .EQ. 0) THEN
          DO 127 K=1,3
            DO 128 L=1,3
              DSIDEP(K,L) = XK0*SIMOEL+KCAM-DEUXMU/3.D0
 128  CONTINUE
 127  CONTINUE
            DO 129 K=1,NDIMSI
               DSIDEP(K,K) = DSIDEP(K,K)+DEUXMU
 129  CONTINUE
        END IF
C
C     -- 7.2 CALCUL DE DSIDEP(6,6)-EN VITESSE :
C     ---------------------------------------
      IF ( MATR .EQ. 1 ) THEN
C
      CALL R8INIR(6*6,0.D0,DSIDEP,1)
C     -- 7.2.1 CALCUL DU MODULE ELASTOPLASTIQUE H

        VALM = 0.D0
        DO 158 I = 1, NDIMSI
        VALM = VALM + SIGMDV(I)**2
158   CONTINUE

        H = 4.D0*M**4*(SIGMMO-PTRAC)*(SIGMMO-PTRAC-PCRM(1))*
     &  (XK0*(SIGMMO-PTRAC-PCRM(1))+XK*PCRM(1))+DEUXMU*9.D0*VALM


C     -- 7.2.2 CALCUL D'UN TERME INTERMEDIAIRE
          DO 160 K=1,3
             A(K) = 0.D0
 160  CONTINUE
          DO 130 K=1,3
             A(K)=-DEUX*XK0*M*M*(SIGMMO-PTRAC)*(SIGMMO-PTRAC-PCRM(1))
     &                                          *KRON(K)+
     &                3.D0*DEUXMU*SIGMDV(K)
 130  CONTINUE
       CALL R8INIR(3,0.D0,AA,1)
          DO 131 K=4,NDIMSI
             AA(K) = 3.D0*DEUXMU*SIGMDV(K)
 131  CONTINUE
C
C     -- 7.2.3 CALCUL DES TERMES DE DSIDEP
       CALL R8INIR(NDIMSI*NDIMSI,0.D0,DSIDEP,1)
          DO 132 K=1,3
           DO 133 L=1,3
             DSIDEP(K,L)=XK0*(SIGMMO-PTRAC)-DEUXMU/3.D0-A(K)*A(L)/H
 133  CONTINUE
 132  CONTINUE
          DO 134 K=1,3
          DO 135 L=4,NDIMSI
             DSIDEP(K,L) = -A(K)*AA(L)
             DSIDEP(K,L) = DSIDEP(K,L)/H
             DSIDEP(L,K) = DSIDEP(K,L)
 135  CONTINUE
 134  CONTINUE
          DO 136 K=4,NDIMSI
          DO 137 L=4,NDIMSI
             DSIDEP(K,L) = -AA(K)*AA(L)
             DSIDEP(K,L) = DSIDEP(K,L)/H
 137  CONTINUE
 136  CONTINUE
           DO 138 K=1,NDIMSI
           DSIDEP(K,K) = DEUXMU + DSIDEP(K,K)
 138  CONTINUE

        ENDIF
C
      IF ( MATR .EQ. 2 ) THEN
      CALL R8INIR(6*6,0.D0,DSIDEP,1)

C     -- 7.2.1 CALCUL DU MODULE ELASTOPLASTIQUE H

        VALP = 0.D0
        DO 159 I = 1, NDIMSI
        VALP = VALP + SIGPDV(I)**2
159   CONTINUE

        H = 4.D0*M**4*(SIGPMO-PTRAC)*(SIGPMO-PTRAC-PCRP(1))*
     &  (XK0*(SIGPMO-PTRAC-PCRP(1))+XK*PCRP(1))+DEUXMU*9.D0*VALP

C     -- 7.2.2 CALCUL D'UN TERME INTERMEDIAIRE
       CALL R8INIR(3,0.D0,A,1)
       CALL R8INIR(3,0.D0,AA,1)

          DO 4130 K=1,3
             A(K) = -DEUX*XK0*M*M*(SIGPMO-PTRAC)*(SIGPMO-PTRAC-PCRP(1))
     &                                          *KRON(K)+
     &                3.D0*DEUXMU*SIGPDV(K)
4130  CONTINUE

          DO 4131 K=4,NDIMSI
             AA(K) = 3.D0*DEUXMU*SIGPDV(K)
4131  CONTINUE

C     -- 7.2.3 CALCUL DES TERMES DE DSIDEP
       CALL R8INIR(NDIMSI*NDIMSI,0.D0,DSIDEP,1)
          DO 4132 K=1,3
           DO 4133 L=1,3
             DSIDEP(K,L)=XK0*(SIGPMO-PTRAC)-DEUXMU/3.D0-A(K)*A(L)/H
4133  CONTINUE
4132  CONTINUE
          DO 4134 K=1,3
          DO 4135 L=4,NDIMSI
             DSIDEP(K,L) = -A(K)*AA(L)
             DSIDEP(K,L) = DSIDEP(K,L)/H
             DSIDEP(L,K) = DSIDEP(K,L)
4135  CONTINUE
4134  CONTINUE
          DO 4136 K=4,NDIMSI
          DO 4137 L=4,NDIMSI
             DSIDEP(K,L) = -AA(K)*AA(L)
             DSIDEP(K,L) = DSIDEP(K,L)/H
4137  CONTINUE
4136  CONTINUE
           DO 4138 K=1,NDIMSI
           DSIDEP(K,K) = DEUXMU + DSIDEP(K,K)
4138  CONTINUE

        ENDIF
C     -- 7.3 CALCUL DE DSIDEP(6,6)-MATRICE COHERENTE :
C     ----------------------------------------------
        IF ( MATR .EQ. 3 ) THEN
      SIEQP = 0.0D0
      DO 300 K=1,NDIMSI
           SIEQP = SIEQP + SIGPDV(K)**2
 300  CONTINUE
       SIEQP = SQRT(1.5D0*SIEQP)
       DIFF2 = ABS((PCRP(1)-SIGPMO)/PCRP(1))
      IF (DIFF2.LT.CRIT(3)) THEN
C
C     -- 7.3.1 OPERATEUR TANGENT COHERENT AU POINT CRITIQUE
C     -- TRAITEMENT DE LA PARTIE DEVIATORIQUE
C     -- CALCUL DE Q+
C     -- CALCUL DU TENSEUR HH QUI MULTIMPLIE LA DEFORMATION
       CALL R8INIR(6*6,0.D0,SES,1)
       DO 1000 K=1,NDIMSI
         DO 1001 L = 1,NDIMSI
          SES(K,L) = 1.D0/2.D0*(SIGPDV(K)*SIGEL(L)+SIGEL(K)*SIGPDV(L))
 1001 CONTINUE
 1000 CONTINUE
       CALL R8INIR(6*6,0.D0,HH,1)
       DO 301 K=1,NDIMSI
       DO 302 L=1,NDIMSI
            HH(K,L) = -DEUXMU*3.D0*SES(K,L)/2.D0/SIELEQ/SIEQP
 302  CONTINUE
 301  CONTINUE
       DO 303 K=1,NDIMSI
            HH(K,K) = DEUXMU+HH(K,K)
 303  CONTINUE
         IF (NDIM.EQ.2) THEN
           HH(5,5) = 1.D0
           HH(6,6) = 1.D0
         ENDIF
C     -- INVERSE DE HH
        CALL R8INIR(6*6,0.D0,HHM,1)
           DO 304 K=1,6
               HHM(K,K)=1.D0
 304  CONTINUE
        CALL MGAUSS('NFWP',HH,HHM,6,6,6,RBID,IRET)

C     -- CALCUL DU TENSEUR GG QUI MULTIMPLIE LA CONTRAINTE
       CALL R8INIR(6*6,0.D0,GG,1)
       CALL R8INIR(6*6,0.D0,SPS,1)
       DO 1002 K=1,NDIMSI
         DO 1003 L = 1,NDIMSI
          SPS(K,L) = SIGPDV(K)*SIGPDV(L)
 1003 CONTINUE
 1002 CONTINUE
       DO 305 K=1,NDIMSI
       DO 306 L=1,NDIMSI
            GG(K,L) = -3.D0*SIELEQ*SPS(K,L)/2.D0/SIEQP**3
 306  CONTINUE
 305  CONTINUE
       DO 307 K=1,NDIMSI
            GG(K,K) = SIELEQ/SIEQP + GG(K,K)
 307  CONTINUE
C     --  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
C     -- DEVIATORIQUES
       CALL R8INIR(6*6,0.D0,V,1)
       DO 315 K = 1,3
       DO 316 L = 1,3
         V(K,L) = -1.D0/3.D0
         V(L,K) = V(K,L)
 316  CONTINUE
 315  CONTINUE
      DO 317 K= 1,NDIMSI
         V(K,K) = V(K,K) + 1.D0
 317  CONTINUE
C     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
C     --  DES CONTRAINTES DEVIATORIQUES PAR GG
       CALL R8INIR(6*6,0.D0,D1G,1)
       CALL PROMAT(V,6,NDIMSI,NDIMSI,GG,6,NDIMSI,NDIMSI,D1G)
C     -- PRODUIT DU RESULTAT PAR L'INVERSE DE HH
       CALL R8INIR(6*6,0.D0,D1GHHM,1)
       CALL PROMAT(D1G,6,NDIMSI,NDIMSI,HHM,6,NDIMSI,NDIMSI,D1GHHM)
C
C     -- 7.3.2 TRAITEMENT DE LA PARTIE HYDROSTATIQUE
C     --  PRODUIT DE LA MATRICE DE PROJECTION SUR L'ESPACE
C     --  DES CONTRAINTES DEVIATORIQUES PAR LA MATRICE IDENTITE
C     --  D'ORDRE 2
       CALL R8INIR(6*6,0.D0,ID2,1)
       DO 308 K=1,3
       DO 309 L=1,3
            ID2(K,L) = -1.D0/3.D0/XK0/SIGPMO
 309  CONTINUE
 308  CONTINUE
C     -- SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
       CALL R8INIR(6*6,0.D0,DEVHYD,1)
       DO 310 K=1,NDIMSI
       DO 311 L=1,NDIMSI
            DEVHYD(K,L) = D1GHHM(K,L)/DEUXMU + ID2(K,L)
 311  CONTINUE
 310  CONTINUE
         IF (NDIM.EQ.2) THEN
           DEVHYD(5,5) = 1.D0
           DEVHYD(6,6) = 1.D0
         ENDIF
C     -- INVERSE DE LA SOMME DES TERMES DEVIATORIQUE ET HYDROSTATIQUE
        CALL R8INIR(6*6,0.D0,DEVHYM,1)
           DO 312 K=1,6
               DEVHYM(K,K)=1.D0
 312  CONTINUE
        CALL MGAUSS('NFWP',DEVHYD,DEVHYM,6,6,6,RBID,IRET)
C     -- TERMES DE L'OPERATEUR TANGENT
       CALL R8INIR(6*6,0.D0,DSIDEP,1)
       DO 313 K=1,6
       DO 314 L=1,6
            DSIDEP(K,L) = DEVHYM(K,L)
 314  CONTINUE
 313  CONTINUE
      ELSE
C
C      ---7.4 OPERATEUR TANGENT COHERENT CAS GENERAL
C      -- CALCUL DES INCREMENTS DE P ET DE S
        DELTAP = SIGPMO - SIGMMO
       CALL R8INIR(6,0.D0,DELTAS,1)
           DO 140 K=1,NDIMSI
              DELTAS(K)=SIGPDV(K)-SIGMDV(K)
 140  CONTINUE
C
C     --  CALCUL DE VECTEURS INTERMEDIAIRES
        SPARDS = 0.D0
           DO 141 K = 1,NDIMSI
               SPARDS = SPARDS+DELTAS(K)*SIGPDV(K)
 141  CONTINUE
       CALL R8INIR(6,0.D0,TPLUS,1)
           DO 142 K = 1, NDIMSI
               TPLUS(K) = SIGPDV(K) + DELTAS(K)
 142  CONTINUE
C
C      -- 7.4.1 TERMES NECESSAIRES A LA PARTIE DEVIATORIQUE
        HP = 4.D0*M**4*XK*SIGPMO*PCRP(1)*(SIGPMO-PCRP(1))
C
        XC = 9.D0*SPARDS/HP
        XD = 6.D0*M*M*(SIGPMO-PCRP(1))*DELTAP/HP
        XV = 3.D0*SPARDS + 2.D0*M**2*(SIGPMO-PCRP(1))*DELTAP
        XLAM = XV/HP
        XA = (4.D0*XLAM*XK*M**4*SIGPMO*(SIGPMO-2.D0*PCRP(1))+
     &       2.D0*M**2*DELTAP)*M**2*(SIGPMO-PCRP(1))/(M**2*XLAM+
     &       (1.D0/2.D0/XK/PCRP(1)))
        XI = 2.D0*M**2*(SIGPMO-PCRP(1))-2.D0*M**4*XLAM*
     &      (SIGPMO-PCRP(1))/((1.D0/2.D0/XK/PCRP(1))+M**2*XLAM)
        RAP = XI/(HP+XA)
C
C     -- CALCUL DE LA MATRICE CC-SYMETRISATION DE TPLUS.I
C
       CALL R8INIR(6*6,0.D0,CC,1)
          DO 172 K=1,NDIMSI
          DO 173 L=1,NDIMSI
              CC(K,L)=(TPLUS(K)*KRON(L)+KRON(K)*TPLUS(L))/2.D0
 173  CONTINUE
 172  CONTINUE
C          DO 172 K=1,3
C          DO 173 L=1,3
C              CC(K,L)=(TPLUS(K)+TPLUS(L))/2.D0
C 173  CONTINUE
C 172  CONTINUE
C          DO 174 K=1,3
C          DO 175 L=4,NDIMSI
C              CC(K,L)=TPLUS(L)/2.D0
C              CC(L,K)=CC(K,L)
C 175  CONTINUE
C 174  CONTINUE

C     -- CALCUL DES TERMES D'UNE MATRICE INTERMEDIAIRE C
C
       CALL R8INIR(6*6,0.D0,C,1)
          DO 170 K=1,NDIMSI
          DO 171 L=1,NDIMSI
             C(K,L) = 9.D0/2.D0/(HP+XA)*(SIGPDV(K)*TPLUS(L)+
     &                                  TPLUS(K)*SIGPDV(L))
 171  CONTINUE
 170  CONTINUE
           DO 149 K=1,NDIMSI
               C(K,K) = C(K,K)+1.D0/DEUXMU+XC+XD
 149  CONTINUE
C
C     -- ASSEMBLAGE DES TERMES POUR LA PARTIE DEVIATORIQUE
       CALL R8INIR(6*6,0.D0,EE,1)
           DO 180 K=1,NDIMSI
           DO 181 L=1,NDIMSI
               EE(K,L) = C(K,L) - RAP*CC(K,L)
  181   CONTINUE
  180   CONTINUE
C
C      -- TERMES NECESSAIRES A LA PARTIE HYDROSTATIQUE
        XU = 2.D0*M**2*XK*PCRP(1)
        XG = XLAM*XU/(1.D0+XLAM*XU)
        XH = XU*(SIGPMO-PCRP(1))/(1.D0+XLAM*XU)
        XE = 1.D0+XH*2.D0*M**2*DELTAP/HP+XH*4.D0*XK*M**4*
     &      SIGPMO*(SIGPMO-2.D0*PCRP(1))*XV/HP/HP
        XF = (2.D0*M**2*(SIGPMO-PCRP(1))+2.D0*M**2*DELTAP-
     &      XG*2.D0*M**2*DELTAP)/HP-4.D0*XK*M**4*XV/HP/HP*
     &     ((2.D0*SIGPMO-PCRP(1))*PCRP(1)+XG*SIGPMO*
     &       (SIGPMO-2.D0*PCRP(1)))
        CT = (1.D0+2.D0*M**2*XK0*SIGPMO*(XLAM-XG*XLAM-
     &       XLAM*XF*XH/XE+XF/XE*(SIGPMO-PCRP(1))))/(XK0*SIGPMO)

C     --  VECTEUR INTERMEDIAIRE
       CALL R8INIR(6,0.D0,FV,1)
       DO 190 K=1,NDIMSI
           FV(K)=3.D0*XF/XE*SIGPDV(K)-CT*KRON(K)/3.D0
 190  CONTINUE
C     -- SYMMETRISATION DEFV ET SA PROJECTION SUR L'ESPACE
C     -- DES CONTRAINTES HYDROSTATIQUES
       CALL R8INIR(6*6,0.D0,FFI,1)
        DO 195 K=1,3
        DO 196 L=1,3
             FFI(K,L) = -(1.D0/3.D0)*(FV(K)+FV(L))/2.D0
 196  CONTINUE
 195  CONTINUE
        DO 197 K=1,3
        DO 198 L=4,NDIMSI
             FFI(K,L) = -(1.D0/3.D0)*FV(L)/2.D0
             FFI(L,K) = FFI(K,L)
 198  CONTINUE
 197  CONTINUE
C     --  MATRICE DE PROJECTION SUR L'ESPACE DES CONTRAINTES
C     -- DEVIATORIQUES
       CALL R8INIR(6*6,0.D0,V,1)
       DO 185 K = 1,3
       DO 186 L = 1,3
         V(K,L) = -1.D0/3.D0
         V(L,K) = V(K,L)
 186  CONTINUE
 185  CONTINUE
      DO 187 K= 1,NDIMSI
         V(K,K) = V(K,K) + 1.D0
 187  CONTINUE
C     -- PROJECTION DE EE SUR L'ESPACE DES CONTRAINTES
C     -- DEVIATORIQUES
       CALL R8INIR(6*6,0.D0,S,1)
       CALL PROMAT(EE,6,NDIMSI,NDIMSI,V,6,NDIMSI,NDIMSI,S)

CC
C     -- COMBINAISON DES DEUX PARTIES DEVIATORIQUE ET
C     -- HYDROSTATIQUE
       CALL R8INIR(6*6,0.D0,T,1)
        DO 204 K = 1,NDIMSI
        DO 205 L = 1,NDIMSI
           T(K,L) =  S(K,L)+ FFI(K,L)
 205  CONTINUE
 204  CONTINUE
         IF (NDIM.EQ.2) THEN
           T(5,5) = 1.D0
           T(6,6) = 1.D0
         ENDIF
C     -- INVERSE DE LA MATRICE T
       CALL R8INIR(6*6,0.D0,VV,1)
           DO 108 K=1,6
               VV(K,K)=1.D0
 108  CONTINUE
       CALL MGAUSS('NFWP',T,VV,6,6,6,RBID,IRET)
C     --  7.3.3 CALCUL DES TERMES DSIDEP L'OPERATEUR TANGENT
       CALL R8INIR(6*6,0.D0,DSIDEP,1)
        DO 106 K = 1,6
        DO 107 L = 1,6
        DSIDEP(K,L) = VV(K,L)
 107  CONTINUE
 106  CONTINUE
C
      ENDIF
      ENDIF
      ENDIF
C ======================================================================
 30   CONTINUE
C =====================================================================
      END
