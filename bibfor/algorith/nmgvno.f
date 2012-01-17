      SUBROUTINE NMGVNO(FAMI,NDIM,NNO1,NNO2,NPG,IW,VFF1,VFF2,
     &  IDFDE1,IDFDE2,
     &  GEOM,TYPMOD,OPTION,MAT,COMPOR,LGPG,CRIT,INSTAM,INSTAP,
     &  DDLM,DDLD,ANGMAS,SIGM,VIM,SIGP,VIP,MATR,VECT,CODRET)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/01/2012   AUTEUR PELLET J.PELLET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C TOLE CRP_21
C
       IMPLICIT NONE
C
       CHARACTER*8   TYPMOD(*)
       CHARACTER*(*) FAMI
       CHARACTER*16  OPTION, COMPOR(*)
       INTEGER NDIM,NNO1,NNO2,NPG,IDFDE1,IDFDE2,IW,MAT,LGPG,CODRET
       REAL*8  VFF1(NNO1,NPG),VFF2(NNO2,NPG)
       REAL*8  GEOM(NDIM,NNO1)
       REAL*8  CRIT(*),INSTAM,INSTAP
       REAL*8  DDLM(*),DDLD(*),SIGM(2*NDIM+1,NPG),SIGP(2*NDIM+1,NPG)
       REAL*8  VIM(LGPG,NPG),VIP(LGPG,NPG),MATR(*),VECT(*)
       REAL*8  ANGMAS(3)
C
C ---------------------------------------------------------------------
C
C     RAPH_MECA, RIGI_MECA_* ET FULL_MECA_* POUR GRAD_VARI (2D ET 3D)
C
C IN  NDIM    : DIMENSION DES ELEMENTS
C IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
C IN  VFF1    : VALEUR DES FONCTIONS DE FORME (FAMILLE U)
C IN  IDFDE1  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE U)
C IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE E)
C IN  VFF2    : VALEUR DES FONCTIONS DE FORME (FAMILLE E)
C IN  IDFDE2  : DERIVEES DES FONCTIONS DE FORME DE REFERENCE (FAMILLE E)
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : POIDS DES POINTS DE GAUSS DE REFERENCE (INDICE)
C IN  GEOM    : COORDONNEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODEELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  MAT     : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT PRECEDENT
C IN  INSTAP  : INSTANT DE CALCUL
C IN  TEMPM   : TEMPERATURE AUX NOEUDS A L'INSTANT PRECEDENT
C IN  TEMPP   : TEMPERATURE AUX NOEUDS A L'INSTANT DE CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DDLM    : DDL A L'INSTANT PRECEDENT
C IN  DDLD    : INCREMENT DES DDL
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  LGPG    : LONGUEUR DU TABLEAU DES VARIABLES INTERNES
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA   ET FULL_MECA_*)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA   ET FULL_MECA_*)
C OUT MATR    : MATRICE DE RIGIDITE   (RIGI_MECA_* ET FULL_MECA_*)
C OUT VECT    : FORCES INTERIEURES    (RAPH_MECA   ET FULL_MECA_*)
C OUT CODRET  : CODE RETOUR
C MEM DFDI2   :
C ---------------------------------------------------------------------
C
      INTEGER K2(1)
      CHARACTER*8 NOM(1),FAMIL,POUM

      LOGICAL RESI,RIGI,GRAND,AXI,ELAS,FULL
      INTEGER NDDL,NDIMSI,G,COD(27),N,I,M,J,KL,PQ,OS,OSA,KK
      INTEGER IU(3*27),IA(8),KPG,SPT
      REAL*8  RAC2,C,VAL(1)
      REAL*8  DEPLM(3*27),DEPLD(3*27),DFDI1(27,3)
      REAL*8  AVM,AVD,AVP,AGM(3),AGD(3),AGP(3),BP
      REAL*8  R,WG,EPSM(6),EPSD(6),F(3,3),B(6,3,27)
      REAL*8  NONLOC(2),SIGMAM(6),SIGMA(6),DSIDEP(6,6,4),T1,T2
      REAL*8  DI,CHAR,R8BID
      REAL*8  DFDI2(8*3)
      REAL*8  CRITD(20)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)

      DATA  NOM /'C_GRAD_V'/
C
C ---------------------------------------------------------------------
C
C - INITIALISATION

      RESI = OPTION(1:9).EQ.'FULL_MECA' .OR. OPTION(1:9).EQ.'RAPH_MECA'
      RIGI = OPTION.EQ.'RIGI_MECA_TANG'
      FULL = OPTION(1:9).EQ.'FULL_MECA'
      ELAS = OPTION.EQ.'FULL_MECA_ELAS' .OR. OPTION.EQ.'RIGI_MECA_ELAS'
C
      IF (ELAS) THEN
        FULL = .FALSE.
      ENDIF
C
      RAC2   = SQRT(2.D0)
      GRAND  = .FALSE.
      AXI    = TYPMOD(1) .EQ. 'AXIS'
      NDDL   = NNO1*NDIM + NNO2
      NDIMSI = 2*NDIM
C
      CALL R8INIR(2,0.D0,NONLOC,1)
C
C      NOM(1) = 'C_GRAD_VARI'
C
      FAMIL='FPG1'
      KPG=1
      SPT=1
      POUM='+'
      CALL RCVALB(FAMIL,KPG,SPT,POUM,MAT,' ','NON_LOCAL',0,' ',0.D0,1,
     &            NOM,VAL,K2,2)
      CALL COEFDG(COMPOR(1), MAT, DI)
C
      C = VAL(1)
C
      DO 5 G=1,NPG
        COD(G)=0
 5    CONTINUE
C
      IF (RIGI) CALL R8INIR((NDDL*(NDDL+1))/2,0.D0,MATR,1)
      IF (FULL) CALL R8INIR((NDDL*(NDDL+1))/2,0.D0,MATR,1)
      IF (ELAS) CALL R8INIR((NDDL*(NDDL+1))/2,0.D0,MATR,1)
      IF (RESI) CALL R8INIR(NDDL,0.D0,VECT,1)
C
      CALL NMGVDN(NDIM,NNO1,NNO2,IU,IA)
C
C    EXTRACTION DES DEPLACEMENTS
C
      DO 10 N = 1,NNO1
        DO 20 I = 1,NDIM
          DEPLM(I+(N-1)*NDIM) = DDLM(IU(NNO1*(I-1)+N))
          IF (RIGI) THEN
            DEPLD(I+(N-1)*NDIM) = 0.D0
          ELSE
            DEPLD(I+(N-1)*NDIM) = DDLD(IU(NNO1*(I-1)+N))
          ENDIF
  20    CONTINUE
  10  CONTINUE
C
C - CREATION D'UN VECTEUR VALANT 0 POUR ABSENCE DE DEPLACEMENT
C
      DO 30 N = 1,NNO2
        CRITD(N) = 0.D0
        DO 40 I = 1,NDIM
          CRITD(N) = CRITD(N) + ABS(DDLD(IU(NNO1*(I-1)+N)))
  40    CONTINUE
  30  CONTINUE
C
C - CALCUL POUR CHAQUE POINT DE GAUSS
C
      DO 1000 G=1,NPG
C
C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR A
C
        CALL DFDMIP(NDIM,NNO2,AXI,GEOM,G,IW,VFF2(1,G),IDFDE2,R,WG,DFDI2)
        AVM = 0
        AVD = 0
        DO 50 N = 1,NNO2
          AVM = AVM + VFF2(N,G)*DDLM(IA(N))
          AVD = AVD + VFF2(N,G)*DDLD(IA(N))
          IF (RIGI) THEN
            AVD = 0.D0
          ENDIF
  50    CONTINUE
        AVP = AVM + AVD
C
        IF (AVP.GT.1.D0) THEN
          AVP = 1.D0
        ENDIF
C
        DO 60 I = 1,NDIM
          AGM(I) = 0
          AGD(I) = 0
          DO 70 N = 1,NNO2
            AGM(I) = AGM(I) + DFDI2(NNO2*(I-1)+N)*DDLM(IA(N))
            AGD(I) = AGD(I) + DFDI2(NNO2*(I-1)+N)*DDLD(IA(N))
            IF (RIGI) THEN
              AGD(I) = 0.D0
            ENDIF
  70      CONTINUE
          AGP(I) = AGM(I) + AGD(I)
  60    CONTINUE
C
C      CALCUL DES ELEMENTS GEOMETRIQUES DE L'EF POUR U
C
        CALL DFDMIP(NDIM,NNO1,AXI,GEOM,G,IW,VFF1(1,G),IDFDE1,R,WG,DFDI1)
        CALL R8INIR(6,0.D0,EPSM,1)
        CALL R8INIR(6,0.D0,EPSD,1)
C
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLM,F,EPSM)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFDI1,DEPLD,F,EPSD)
        CALL NMMABU(NDIM,NNO1,.FALSE.,GRAND,DFDI1,B)
        IF (AXI) THEN
          DO 80 N=1,NNO1
            B(3,1,N) = VFF1(N,G)/R
  80      CONTINUE
        ENDIF
C
        DO 90 KL = 1,3
          SIGMAM(KL) = SIGM(KL,G)
  90    CONTINUE
        DO 100 KL = 4,NDIMSI
          SIGMAM(KL) = SIGM(KL,G)*RAC2
 100    CONTINUE
C
        NONLOC(1)= AVP
        NONLOC(2)= C
C
        CALL NMCOMP(FAMI,G,1,NDIM,TYPMOD,MAT,COMPOR,CRIT,
     &              INSTAM,INSTAP,
     &              6,EPSM,EPSD,
     &              6,SIGMAM,VIM(1,G),
     &              OPTION,
     &              ANGMAS,
     &              2,NONLOC,
     &              SIGMA,VIP(1,G),6*6*4,DSIDEP,1,R8BID,COD(G))
C
        IF(COD(G).EQ.1) GOTO 9000
C
C      FORCE INTERIEURE ET CONTRAINTES DE CAUCHY
C
        IF (RESI) THEN
C
C        CONTRAINTES
C
          DO 110 KL = 1,3
            SIGP(KL,G) = SIGMA(KL)
 110      CONTINUE
          DO 120 KL = 4,NDIMSI
            SIGP(KL,G) = SIGMA(KL)/RAC2
 120      CONTINUE
C
          SIGP(NDIMSI+1,G) = DSIDEP(1,1,4)
          BP = SIGP(NDIMSI+1,G)
C
C        VECTEUR FINT:U
C
          DO 130 N=1,NNO1
            DO 140 I=1,NDIM
              KK = IU(NNO1*(I-1)+N)
              T1 = 0
              DO 150 KL = 1,NDIMSI
                T1 = T1 + SIGMA(KL)*B(KL,I,N)
 150          CONTINUE
              VECT(KK) = VECT(KK) + WG*T1
 140        CONTINUE
 130      CONTINUE
C
C        VECTEUR FINT:A
C
          DO 160 N=1,NNO2
            T1 = VFF2(N,G)*BP
            T2 = 0
            DO 170 I = 1,NDIM
              T2 = T2 + C*DFDI2(NNO2*(I-1)+N)*AGP(I)
 170        CONTINUE
            KK = IA(N)
            VECT(KK) = VECT(KK) + WG*(T2+T1)
 160      CONTINUE

        ENDIF
C
C   CALCUL DE LA MATRICE DE RIGIDITE
C   STOCKAGE TRIANGLE INFERIEUR LIGNE DE DFI/DUJ
C
        IF (ELAS .OR. RIGI .OR. FULL) THEN
C
C        MATRICE K:U(I,N),U(J,M)
C
          DO 180 N = 1,NNO1
            DO 190 I = 1,NDIM
              OS = ((IU(NNO1*(I-1)+N)-1)*IU(NNO1*(I-1)+N))/2
              DO 200 M = 1,NNO1
                DO 210 J = 1,NDIM
                  IF (IU(NNO1*(J-1)+M).GT.IU(NNO1*(I-1)+N)) GOTO 821
                  KK = OS+IU(NNO1*(J-1)+M)
                  T1 = 0
                  DO 220 KL = 1,NDIMSI
                    DO 230 PQ = 1,NDIMSI
                      T1 = T1+DSIDEP(KL,PQ,1)*B(PQ,J,M)*B(KL,I,N)
 230                CONTINUE
 220              CONTINUE
                  MATR(KK) = MATR(KK) + WG*T1
 210            CONTINUE
 200          CONTINUE
 821          CONTINUE
 190        CONTINUE
 180      CONTINUE
C
C        MATRICES K:A(N),A(M) SI ENDO NON-NUL
C
          DO 240 N = 1,NNO2
            OSA = ((IA(N)-1)*IA(N))/2
            DO 250 M = 1,NNO2
              T1 = VFF2(N,G)*VFF2(M,G)*DSIDEP(1,1,3)
              T2 = 0
              DO 260 I = 1,NDIM
                T2 = T2 + DFDI2(NNO2*(I-1)+N)*DFDI2(NNO2*(I-1)+M)
 260          CONTINUE
              T2 = C*T2
              IF (IA(M).LE.IA(N)) THEN
                KK = OSA+IA(M)
                MATR(KK) = MATR(KK) + WG*(T2+T1)
              ENDIF
 250        CONTINUE
 240      CONTINUE
C
        ENDIF
C
        IF (RIGI .OR. FULL) THEN
C
C        MATRICES K:A(N),U(J,M)
C
          DO 270 N = 1,NNO2
            DO 280 M = 1,NNO1
              DO 290 J = 1,NDIM
                T1 = 0
                DO 300 KL = 1,NDIMSI
                  T1 = T1 + DSIDEP(KL,1,2)*B(KL,J,M)
 300            CONTINUE
                T1 = VFF2(N,G)*T1
                IF (IA(N).GE.IU(NNO1*(J-1)+M)) THEN
                  KK = ((IA(N)-1)*IA(N))/2 + IU(NNO1*(J-1)+M)
                ELSE
                  KK = ((IU(NNO1*(J-1)+M)-1)*IU(NNO1*(J-1)+M))/2 +IA(N)
                END IF
                MATR(KK) = MATR(KK) + WG*T1
 290          CONTINUE
 280        CONTINUE
 270      CONTINUE
C
        ENDIF
C
        IF (ELAS .OR. RIGI .OR. FULL) THEN
C
          DO 310 N = 1,NNO2
            OSA = ((IA(N)-1)*IA(N))/2
            DO 320 M = 1,NNO2
              IF (IA(M).LE.IA(N)) THEN
                KK=OSA+IA(M)
C
                CHAR = DDLD(IA(M))
C
                IF (CHAR.EQ.0.D0 .AND. CRITD(M).NE.0.D0) THEN
                  IF (IA(M).EQ.IA(N)) THEN
                    MATR(KK) = DI
                  ELSE
                    MATR(KK) = 0.D0
                  ENDIF
                ENDIF
C
                CHAR = DDLD(IA(N))
C
                IF (CHAR.EQ.0.D0 .AND. CRITD(N).NE.0.D0) THEN
                  IF (IA(M).EQ.IA(N)) THEN
                    MATR(KK) = DI
                  ELSE
                    MATR(KK) = 0.D0
                  ENDIF
                ENDIF
C
                IF (IA(M).EQ.IA(N)) THEN
                  IF (CRITD(N).EQ.0.D0 .AND. AVP.EQ.0.D0) THEN
                    MATR(KK) = DI
                  ENDIF
                ENDIF
              ENDIF
 320        CONTINUE
 310      CONTINUE
C
        ENDIF
C
        IF (RIGI .OR. FULL) THEN
C
          DO 330 N = 1,NNO2
C
            CHAR = DDLD(IA(N))
C
            IF (CHAR.EQ.0.D0 .AND. CRITD(N).NE.0.D0) THEN
              DO 340 M = 1,NNO1
                DO 350 J = 1,NDIM
                  IF (IA(N).GE.IU(NNO1*(J-1)+M)) THEN
                    KK=((IA(N)-1)*IA(N))/2 + IU(NNO1*(J-1)+M)
                  ELSE
                    KK=((IU(NNO1*(J-1)+M)-1)*IU(NNO1*(J-1)+M))/2+IA(N)
                  ENDIF
                  MATR(KK) = 0.D0
 350            CONTINUE
 340          CONTINUE
            ENDIF
C
 330      CONTINUE
C
        ENDIF
C
1000  CONTINUE
C
9000  CONTINUE
C
      CALL CODERE(COD,NPG,CODRET)
C
      END
