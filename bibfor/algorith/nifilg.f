      SUBROUTINE NIFILG(NDIM,NNO1,NNO2,NNO3,NPG,IW,VFF1,VFF2,VFF3,IDFF1,
     &                  DFF1,VU,VG,VP,GEOMI,TYPMOD,OPTION,MATE,COMPOR,
     &                  LGPG,CRIT,INSTM,INSTP,DDLM,DDLD,ANGMAS,SIGM,VIM,
     &                  SIGP,VIP,RESI,RIGI,VECT,MATR,MATSYM,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/01/2013   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21
C TOLE CRP_20
C TOLE CRS_1404
C RESPONSABLE SFAYOLLE S.FAYOLLE
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      LOGICAL RESI,RIGI
      CHARACTER*8  TYPMOD(*)
      CHARACTER*16 COMPOR(*),OPTION
      INTEGER MATE,NDIM,NPG,NNO1,NNO2,NNO3,IW,IDFF1,CODRET,LGPG
      INTEGER VU(3,27),VG(27),VP(27)
      REAL*8 VFF1(NNO1,NPG),VFF2(NNO2,NPG),VFF3(NNO3,NPG),CRIT(*)
      REAL*8 INSTM,INSTP,ANGMAS(*)
      REAL*8 GEOMI(NDIM,NNO1),DDLM(*),DDLD(*),SIGM(2*NDIM,NPG)
      REAL*8 SIGP(2*NDIM,NPG),VIM(LGPG,NPG),VIP(LGPG,NPG)
      REAL*8 DFF1(NNO1,4)
      REAL*8 VECT(*),MATR(*)
      REAL*8 FTP(3,3),EPSML(6)
      REAL*8 TN(6),DEPS(6),GN(3,3),LAMB(3),LOGL(3)
C-----------------------------------------------------------------------
C          CALCUL DES OPTIONS DE MECANIQUE NON LINEAIRE
C          GRANDES DEFORMATIONS QUASI-INCOMPRESSIBLES
C          3D/D_PLAN/AXIS
C          ROUTINE APPELEE PAR TE0590
C-----------------------------------------------------------------------
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AU GONFLEMENT
C IN  NNO3    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IW      : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES AU GONFLEMENT
C IN  VFF3    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  IDFF2   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C OUT DFF1    : DERIVEE FONCTION DE FORME (PT DE GAUSS COURANT) A T+
C OUT DFF2    : DERIVEE FONCTION DE FORME (PT DE GAUSS COURANT)
C IN  VU
C IN  VG
C IN  VP
C IN  GEOMI   : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  MATE    : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTM   : INSTANT PRECEDENT
C IN  INSTP   : INSTANT DE CALCUL
C IN  DDLM    : DEGRES DE LIBERTE A L'INSTANT PRECEDENT
C IN  DDLD    : INCREMENT DES DEGRES DE LIBERTE
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT VECT    : FORCES INTERNES
C OUT MATR    : MATRICE DE RIGIDITE (RIGI_MECA_TANG ET FULL_MECA)
C OUT CODRET  : CODE RETOUR
C-----------------------------------------------------------------------

      LOGICAL AXI,GRAND,MATSYM
      INTEGER NDDL,NDU,VIJ(3,3),OS,KK
      INTEGER IA,NA,RA,SA,IB,NB,RB,SB,JA,JB
      INTEGER LIJ(3,3),I,J
      INTEGER G,KL,COD(27)
      REAL*8 GEOMM(3*27),GEOMP(3*27),DEPLM(3*27),DEPLP(3*27)
      REAL*8 GM,GD,GP,PM,PD,PP,R,W,WP,JM,JP,FTM(3,3)
      REAL*8 FM(3,3),EPSM(6),EPSP(6),CORM
      REAL*8 TAUHY,TAUDV(6),FP(3,3),TAUP(6)
      REAL*8 DSIDEP(6,6),D(6,6),FTR(3,3)
      REAL*8 PRESM(27),PRESD(27),GONFM(27),GONFD(27)
      REAL*8 DDOT,TAMPON(10),T1,T2,ID(3,3),KR(6),RBID
      REAL*8 CORP
      REAL*8 DTDE(6,6),TP(6)
      REAL*8 DDEV(6,6),DEVD(6,6)
      REAL*8 IDEV(6,6)
      REAL*8 TAULDC(6)
      REAL*8 IDDID,DEVDI(6),IDDEV(6),DDDEV(6,6)
      REAL*8 PK2(6), PK2M(6)
      INTEGER VIAJA,VIBJB,VUIANA,VGRA,VPSA

      PARAMETER (GRAND = .TRUE.)
      DATA    KR   /1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA    ID   /1.D0, 0.D0, 0.D0,
     &              0.D0, 1.D0, 0.D0,
     &              0.D0, 0.D0, 1.D0/
      DATA    VIJ  / 1, 4, 5,
     &               4, 2, 6,
     &               5, 6, 3 /
      DATA    IDEV   / 2.D0, -1.D0, -1.D0,0.D0,0.D0,0.D0,
     &                -1.D0,  2.D0, -1.D0,0.D0,0.D0,0.D0,
     &                -1.D0, -1.D0,  2.D0,0.D0,0.D0,0.D0,
     &                 0.D0,  0.D0,  0.D0,3.D0,0.D0,0.D0,
     &                 0.D0,  0.D0,  0.D0,0.D0,3.D0,0.D0,
     &                 0.D0,  0.D0,  0.D0,0.D0,0.D0,3.D0/


C-----------------------------------------------------------------------

C - INITIALISATION
      AXI  = TYPMOD(1).EQ.'AXIS'
      NDDL = NNO1*NDIM + NNO2 + NNO3
      NDU  = NDIM
      IF (AXI) NDU = 3

C - REACTUALISATION DE LA GEOMETRIE ET EXTRACTION DES CHAMPS
      DO 10 NA = 1,NNO1
        DO 11 IA = 1,NDIM
          GEOMM(IA+NDIM*(NA-1)) = GEOMI(IA,NA) + DDLM(VU(IA,NA))
          GEOMP(IA+NDIM*(NA-1)) = GEOMM(IA+NDIM*(NA-1))+DDLD(VU(IA,NA))
          DEPLM(IA+NDIM*(NA-1)) = DDLM(VU(IA,NA))
          DEPLP(IA+NDIM*(NA-1)) = DDLM(VU(IA,NA))+DDLD(VU(IA,NA))
 11     CONTINUE
 10   CONTINUE

      DO 20 RA = 1,NNO2
        GONFM(RA) = DDLM(VG(RA))
        GONFD(RA) = DDLD(VG(RA))
 20   CONTINUE
      DO 30 SA = 1,NNO3
        PRESM(SA) = DDLM(VP(SA))
        PRESD(SA) = DDLD(VP(SA))
 30   CONTINUE

      IF (RESI) CALL R8INIR(NDDL,0.D0,VECT,1)
      IF (RIGI) THEN
        IF (MATSYM) THEN
          CALL R8INIR(NDDL*(NDDL+1)/2,0.D0,MATR,1)
        ELSE
          CALL R8INIR(NDDL*NDDL,0.D0,MATR,1)
        ENDIF
      ENDIF

      CALL R8INIR(36,0.D0,DSIDEP,1)

C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 1000 G = 1,NPG

C - CALCUL DES DEFORMATIONS
        CALL DFDMIP(NDIM,NNO1,AXI,GEOMI,G,IW,VFF1(1,G),IDFF1,R,W,DFF1)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFF1,DEPLM,FM,EPSM)
        CALL NMEPSI(NDIM,NNO1,AXI,GRAND,VFF1(1,G),R,DFF1,DEPLP,FP,EPSP)
        CALL DFDMIP(NDIM,NNO1,AXI,GEOMP,G,IW,VFF1(1,G),IDFF1,R,WP,DFF1)

        CALL NMMALU(NNO1,AXI,R,VFF1(1,G),DFF1,LIJ)

        JM = FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &     - FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &     + FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))
        JP = FP(1,1)*(FP(2,2)*FP(3,3)-FP(2,3)*FP(3,2))
     &     - FP(2,1)*(FP(1,2)*FP(3,3)-FP(1,3)*FP(3,2))
     &     + FP(3,1)*(FP(1,2)*FP(2,3)-FP(1,3)*FP(2,2))

        IF (JP.LE.0.D0) THEN
          CODRET = 1
          GOTO 9999
        ENDIF

C - CALCUL DE LA PRESSION ET DU GONFLEMENT AU POINT DE GAUSS
        GM = DDOT(NNO2,VFF2(1,G),1,GONFM,1)
        GD = DDOT(NNO2,VFF2(1,G),1,GONFD,1)
        GP = GM+GD

        PM = DDOT(NNO3,VFF3(1,G),1,PRESM,1)
        PD = DDOT(NNO3,VFF3(1,G),1,PRESD,1)
        PP = PM+PD

C - CALCUL DES DEFORMATIONS ENRICHIES
        CORM = (EXP(GM)/JM)**(1.D0/3.D0)
        CALL DCOPY(9,FM,1,FTM,1)
        CALL DSCAL(9,CORM,FTM,1)

        CORP = (EXP(GP)/JP)**(1.D0/3.D0)
        CALL DCOPY(9,FP,1,FTP,1)
        CALL DSCAL(9,CORP,FTP,1)

C -   APPEL A LA LOI DE COMPORTEMENT
        COD(G) = 0
        CALL R8INIR(36,0.D0,DTDE,1)
        CALL R8INIR(6,0.D0,TP,1)
        CALL R8INIR(6,0.D0,TAUP,1)

        CALL PRELOG(NDIM,LGPG,VIM(1,G),GN,LAMB,LOGL,FTM,FTP,EPSML,
     &              DEPS,TN,RESI,COD(G))

        CALL NMCOMP('RIGI',G,1,NDIM,TYPMOD,MATE,COMPOR,CRIT,INSTM,INSTP,
     &              6,EPSML,DEPS,6,TN,VIM(1,G),OPTION,ANGMAS,10,TAMPON,
     &              TP,VIP(1,G),36,DTDE,1,RBID,COD(G))

C DSIDEP = 2dS/dC = dS/dE_GL
        CALL POSLOG(RESI,RIGI,TN,TP,FTM,LGPG,VIP(1,G),NDIM,FTP,G,DTDE,
     &              SIGM(1,G),.FALSE.,'RIGI',MATE,INSTP,ANGMAS,GN,LAMB,
     &              LOGL,SIGP(1,G),DSIDEP,PK2M,PK2,COD(G))

        IF (COD(G).EQ.1) THEN
          CODRET = 1
          IF (.NOT. RESI) CALL U2MESS('F','ALGORITH14_75')
          GOTO 9999
        ENDIF

        IF (RESI) THEN
          CALL DSCAL(2*NDIM,EXP(GP),SIGP(1,G),1)
          CALL DCOPY(2*NDIM,SIGP(1,G),1,TAUP,1)
          CALL DSCAL(2*NDIM,1.D0/JP,SIGP(1,G),1)

C - CONTRAINTE HYDROSTATIQUE ET DEVIATEUR
          TAUHY = (TAUP(1)+TAUP(2)+TAUP(3))/3.D0
          DO 100 KL = 1,6
            TAUDV(KL) = TAUP(KL) - TAUHY*KR(KL)
 100      CONTINUE

C - VECTEUR FINT:U
          DO 200 NA=1,NNO1
            DO 210 IA=1,NDU
              KK = VU(IA,NA)
              T1 = 0.D0
              DO 220 JA = 1,NDU
                T2 = TAUDV(VIJ(IA,JA)) + PP*ID(IA,JA)
                T1 = T1 + T2*DFF1(NA,LIJ(IA,JA))
 220          CONTINUE
              VECT(KK) = VECT(KK) + W*T1
 210        CONTINUE
 200      CONTINUE

C - VECTEUR FINT:G
          T2 = TAUHY - PP
          DO 230 RA=1,NNO2
            KK = VG(RA)
            T1 = VFF2(RA,G)*T2
            VECT(KK) = VECT(KK) + W*T1
 230      CONTINUE

C - VECTEUR FINT:P
          T2 = LOG(JP) - GP
          DO 240 SA=1,NNO3
            KK = VP(SA)
            T1 = VFF3(SA,G)*T2
            VECT(KK) = VECT(KK) + W*T1
 240      CONTINUE
        END IF

C - MATRICE TANGENTE
        IF (RIGI) THEN
          IF (RESI) THEN
            CALL DCOPY(9,FTP,1,FTR,1)
          ELSE
            CALL DCOPY(2*NDIM,SIGM(1,G),1,TAUP,1)
            CALL DSCAL(2*NDIM,JM,TAUP,1)
            CALL DCOPY(9,FTM,1,FTR,1)
          ENDIF

C - CALCUL DE L'OPERATEUR TANGENT SYMÉTRISÉ D
          CALL DSDE2D(3,FTR,DSIDEP,D)

          CALL PMAT(6,IDEV/3.D0,D,DEVD)
          CALL PMAT(6,D,IDEV/3.D0,DDEV)
          CALL PMAT(6,DEVD,IDEV/3.D0,DDDEV)

C - CALCUL DE D^DEV:ID ET ID:D^DEV
C - CALCUL DU TENSEUR DE CONTRAINTE : TRACE ET PARTIE DEVIATORIQUE
          IDDID=0.D0
          TAUHY = (TAUP(1)+TAUP(2)+TAUP(3))/3.D0
          DO 380 I = 1,6
            DEVDI(I)  = DEVD(I,1)+DEVD(I,2)+DEVD(I,3)
            IDDEV(I)  = DDEV(1,I)+DDEV(2,I)+DDEV(3,I)
            TAUDV(I)  = TAUP(I) - TAUHY*KR(I)
            TAULDC(I) = TAUP(I) + (PP-TAUHY)*KR(I)
            DO 390 J = 1,6
              IDDID=IDDID+KR(I)*D(I,J)*KR(J)
 390        CONTINUE
 380      CONTINUE

          IF(MATSYM)THEN
C - MATRICE SYMETRIQUE
C - TERME K:UX
          DO 400 NA = 1,NNO1
            DO 410 IA = 1,NDU
              VUIANA = VU(IA,NA)
              OS = (VUIANA-1)*VUIANA/2

C - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
              DO 420 NB = 1,NNO1
                DO 430 IB = 1,NDU
                  IF(VU(IB,NB).LE.VUIANA)THEN
                  KK = OS+VU(IB,NB)
                  T1 = 0.D0
C - RIGIDITE DE COMPORTEMENT
                  DO 440 JA = 1,NDU
                    VIAJA=VIJ(IA,JA)
                    DO 450 JB = 1,NDU
                      VIBJB=VIJ(IB,JB)
                      T2 = DDDEV(VIAJA,VIBJB)
                      T2 = T2 + TAUP(VIJ(IA,JB))*KR(VIJ(IB,JA))
                      T2 = T2 + TAUP(VIJ(JB,JA))*KR(VIJ(IA,IB))
                      T2 = T2 - 2.D0/3.D0*(TAUP(VIAJA)*KR(VIBJB)
     &                                    +TAUP(VIBJB)*KR(VIAJA))
                      T2 = T2 + 2.D0/3.D0*TAUHY*KR(VIAJA)*KR(VIBJB)
                      T1 = T1+DFF1(NA,LIJ(IA,JA))*T2*DFF1(NB,LIJ(IB,JB))
 450                CONTINUE
 440              CONTINUE

C - RIGIDITE GEOMETRIQUE
                  DO 460 JB = 1,NDU
                    T1 = T1 - DFF1(NA,LIJ(IA,IB))*DFF1(NB,LIJ(IB,JB))
     &                      *TAULDC(VIJ(IA,JB))
 460              CONTINUE
                  MATR(KK) = MATR(KK) + W*T1
                  ENDIF
 430            CONTINUE
 420          CONTINUE

C - TERME K:UG      KUG(NDIM,NNO1,NNO2)
              DO 470 RB = 1,NNO2
                IF(VG(RB).LT.VUIANA)THEN
                KK = OS + VG(RB)
                T1 = 0.D0
                DO 480 JA = 1,NDU
                  VIAJA=VIJ(IA,JA)
                  T2 = (DEVDI(VIAJA)+2.D0*TAUDV(VIAJA))/3.D0
                  T1 = T1 + DFF1(NA,LIJ(IA,JA))*T2*VFF2(RB,G)
 480            CONTINUE
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 470          CONTINUE

C - TERME K:UP      KUP(NDIM,NNO1,NNO3)
              DO 490 SB = 1,NNO3
                IF(VP(SB).LT.VUIANA)THEN
                KK = OS + VP(SB)
                T1 = DFF1(NA,LIJ(IA,IA))*VFF3(SB,G)
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 490          CONTINUE
 410        CONTINUE
 400      CONTINUE

C - TERME K:GX
          DO 500 RA = 1,NNO2
            VGRA = VG(RA)
            OS = (VGRA-1)*VGRA/2

C - TERME K:GU      KGU(NDIM,NNO2,NNO1)
            DO 510 NB = 1,NNO1
              DO 520 IB = 1,NDU
                IF(VU(IB,NB).LT.VGRA)THEN
                KK = OS + VU(IB,NB)
                T1 = 0.D0
                DO 530 JB = 1,NDU
                  VIBJB=VIJ(IB,JB)
                  T2 = (IDDEV(VIBJB)+2.D0*TAUDV(VIBJB))/3.D0
                  T1 = T1 + VFF2(RA,G)*T2*DFF1(NB,LIJ(IB,JB))
 530            CONTINUE
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 520          CONTINUE
 510        CONTINUE

C - TERME K:GG      KGG(NNO2,NNO2)
            DO 540 RB = 1,NNO2
              IF(VG(RB).LE.VGRA)THEN
              KK = OS + VG(RB)
              T2 = IDDID/9.D0+2.D0*TAUHY/3.D0
              T1 = VFF2(RA,G)*T2*VFF2(RB,G)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 540        CONTINUE

C - TERME K:GP      KGP(NNO2,NNO3)
            DO 550 SB = 1,NNO3
              IF(VP(SB).LT.VGRA)THEN
              KK = OS + VP(SB)
              T1 = - VFF2(RA,G)*VFF3(SB,G)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 550        CONTINUE
 500      CONTINUE

C - TERME K:PX
          DO 600 SA = 1,NNO3
            VPSA = VP(SA)
            OS = (VPSA-1)*VPSA/2

C - TERME K:PU      KPU(NDIM,NNO3,NNO1)
            DO 610 NB = 1,NNO1
              DO 620 IB = 1,NDU
                IF(VU(IB,NB).LT.VPSA)THEN
                KK = OS + VU(IB,NB)
                T1 = VFF3(SA,G)*DFF1(NB,LIJ(IB,IB))
                MATR(KK) = MATR(KK) + W*T1
                ENDIF
 620          CONTINUE
 610        CONTINUE

C - TERME K:PG      KPG(NNO3,NNO2)
            DO 630 RB = 1,NNO2
              IF(VG(RB).LT.VPSA)THEN
              KK = OS + VG(RB)
              T1 = - VFF3(SA,G)*VFF2(RB,G)
              MATR(KK) = MATR(KK) + W*T1
              ENDIF
 630        CONTINUE

C - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
 600      CONTINUE

          ELSE
C - MATRICE NON SYMETRIQUE
C - TERME K:UX
          DO 401 NA = 1,NNO1
            DO 411 IA = 1,NDU
              OS = (VU(IA,NA)-1)*NDDL

C - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
              DO 421 NB = 1,NNO1
                DO 431 IB = 1,NDU
                  KK = OS+VU(IB,NB)
                  T1 = 0.D0
C - RIGIDITE DE COMPORTEMENT
                  DO 441 JA = 1,NDU
                    VIAJA=VIJ(IA,JA)
                    DO 451 JB = 1,NDU
                      VIBJB=VIJ(IB,JB)
                      T2 = DDDEV(VIAJA,VIBJB)
                      T2 = T2 + TAUP(VIJ(IA,JB))*KR(VIJ(IB,JA))
                      T2 = T2 + TAUP(VIJ(JB,JA))*KR(VIJ(IA,IB))
                      T2 = T2 - 2.D0/3.D0*(TAUP(VIAJA)*KR(VIBJB)
     &                                   +KR(VIAJA)*TAUP(VIBJB))
                      T2 = T2 + 2.D0*KR(VIAJA)*KR(VIBJB)*TAUHY/3.D0
                      T1 = T1+DFF1(NA,LIJ(IA,JA))*T2*DFF1(NB,LIJ(IB,JB))
 451                CONTINUE
 441              CONTINUE

C - RIGIDITE GEOMETRIQUE
                  DO 461 JB = 1,NDU
                    T1 = T1 - DFF1(NA,LIJ(IA,IB))*DFF1(NB,LIJ(IB,JB))
     &                      *TAULDC(VIJ(IA,JB))
 461              CONTINUE
                  MATR(KK) = MATR(KK) + W*T1
 431            CONTINUE
 421          CONTINUE

C - TERME K:UG      KUG(NDIM,NNO1,NNO2)
              DO 471 RB = 1,NNO2
                KK = OS + VG(RB)
                T1 = 0.D0
                DO 481 JA = 1,NDU
                  VIAJA=VIJ(IA,JA)
                  T2 = (DEVDI(VIAJA)+2.D0*TAUDV(VIAJA))/3.D0
                  T1 = T1 + DFF1(NA,LIJ(IA,JA))*T2*VFF2(RB,G)
 481            CONTINUE
                MATR(KK) = MATR(KK) + W*T1
 471          CONTINUE

C - TERME K:UP      KUP(NDIM,NNO1,NNO3)
              DO 491 SB = 1,NNO3
                KK = OS + VP(SB)
                T1 = DFF1(NA,LIJ(IA,IA))*VFF3(SB,G)
                MATR(KK) = MATR(KK) + W*T1
 491          CONTINUE
 411        CONTINUE
 401      CONTINUE

C - TERME K:GX
          DO 501 RA = 1,NNO2
            OS = (VG(RA)-1)*NDDL

C - TERME K:GU      KGU(NDIM,NNO2,NNO1)
            DO 511 NB = 1,NNO1
              DO 521 IB = 1,NDU
                KK = OS + VU(IB,NB)
                T1 = 0.D0
                DO 531 JB = 1,NDU
                  VIBJB=VIJ(IB,JB)
                  T2 = (IDDEV(VIBJB)+2.D0*TAUDV(VIBJB))/3.D0
                  T1 = T1 + VFF2(RA,G)*T2*DFF1(NB,LIJ(IB,JB))
 531            CONTINUE
                MATR(KK) = MATR(KK) + W*T1
 521          CONTINUE
 511        CONTINUE

C - TERME K:GG      KGG(NNO2,NNO2)
            DO 541 RB = 1,NNO2
              KK = OS + VG(RB)
              T2 = IDDID/9.D0+2.D0*TAUHY/3.D0
              T1 = VFF2(RA,G)*T2*VFF2(RB,G)
              MATR(KK) = MATR(KK) + W*T1
 541        CONTINUE

C - TERME K:GP      KGP(NNO2,NNO3)
            DO 551 SB = 1,NNO3
              KK = OS + VP(SB)
              T1 = - VFF2(RA,G)*VFF3(SB,G)
              MATR(KK) = MATR(KK) + W*T1
 551        CONTINUE
 501      CONTINUE

C - TERME K:PX
          DO 601 SA = 1,NNO3
            OS = (VP(SA)-1)*NDDL

C - TERME K:PU      KPU(NDIM,NNO3,NNO1)
            DO 611 NB = 1,NNO1
              DO 621 IB = 1,NDU
                KK = OS + VU(IB,NB)
                T1 = VFF3(SA,G)*DFF1(NB,LIJ(IB,IB))
                MATR(KK) = MATR(KK) + W*T1
 621          CONTINUE
 611        CONTINUE

C - TERME K:PG      KPG(NNO3,NNO2)
            DO 631 RB = 1,NNO2
              KK = OS + VG(RB)
              T1 = - VFF3(SA,G)*VFF2(RB,G)
              MATR(KK) = MATR(KK) + W*T1
 631        CONTINUE

C - TERME K:PP = 0.D0      KPP(NNO3,NNO3)

 601      CONTINUE
          ENDIF
        END IF
 1000 CONTINUE

C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG,CODRET)

 9999 CONTINUE
      END
