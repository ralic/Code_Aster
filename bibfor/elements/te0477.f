      SUBROUTINE TE0477(OPTION,NOMTE)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/09/2011   AUTEUR TARDIEU N.TARDIEU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
C          ELEMENT SHB8PS
      PARAMETER (NBRES=2)
      CHARACTER*8 TYPMOD
      CHARACTER*4 FAMI
      INTEGER ICODRE(NBRES)
      CHARACTER*8 NOMRES(NBRES)
      CHARACTER*16 NOMTE,OPTION,NOMSHB
      REAL*8 RE(24,24),SIGMA(120),D(36)
      REAL*8 DUSDX(180),FSTAB(12),SIGMM(120),SIGMP(120)
      REAL*8 VALRES(NBRES),SIGM(6),SIMP(6)
      REAL*8 R8VIDE,LC,G,ANGMAS(3),INSTM,INSTP,DSIDEP(6,6)
      REAL*8 DEPSLO(120),EPS2D(6),DEPS2D(6),EPSLOC(120)
      INTEGER IINSTM,IINSTP,IRET
      REAL*8 NU,E,PARA(11)
      REAL*8 XIDEPP(60),RE6(18,18),RE15(45,45),RE20(60,60)
      REAL*8 DUDDD(180)
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C------------FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      FAMI = 'RIGI'
      CALL ELREF4(' ',FAMI,NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)

C --- INITIALISATIONS :
C     -----------------
      CALL IDSSHB(NDIM,NNO,NPG,NOMSHB)
      DO 10 I = 1,11
           PARA(I) = 0.D0
   10 CONTINUE
C  ###############################################################
C  -- ELASTOPLASTICITE
C  ###############################################################
      IF (OPTION.EQ.'FULL_MECA' .OR. OPTION.EQ.'RAPH_MECA' .OR.
     &    OPTION.EQ.'RIGI_MECA_TANG') THEN
C - PARAMETRES EN ENTREE
        CALL JEVECH('PGEOMER','L',IGEOM)
        CALL JEVECH('PMATERC','L',IMATE)
        CALL JEVECH('PCONTMR','L',ICONTM)
        CALL JEVECH('PVARIMR','L',IVARIM)
        CALL JEVECH('PDEPLMR','L',IDEPLM)
        CALL JEVECH('PDEPLPR','L',IDEPLP)
        CALL JEVECH('PCOMPOR','L',ICOMPO)
        CALL JEVECH('PCARCRI','L',ICARCR)
        CALL JEVECH('PINSTMR','L',IINSTM)
        CALL JEVECH('PINSTPR','L',IINSTP)
        INSTM = ZR(IINSTM)
        INSTP = ZR(IINSTP)
C - PARAMETRES EN SORTIE
        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PMATUUR','E',IMATUU)
        END IF
C
        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
          CALL JEVECH('PVECTUR','E',IVECTU)
          CALL JEVECH('PCONTPR','E',ICONTP)
          CALL JEVECH('PVARIPR','E',IVARIP)
        END IF
C - PARAMETRES MATERIAU

        NOMRES(1) = 'E'
        NOMRES(2) = 'NU'
        NBV = 2
C ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
C ----  ET DU TEMPS
C -----------
        CALL MOYTEM(FAMI,NPG,1,'+',TEMPM,IRET)
        CALL RCVALB(FAMI,1,1,'+',ZI(IMATE),' ','ELAS',
     &             1,'TEMP',TEMPM,NBV,NOMRES,
     &             VALRES, ICODRE,1)
        E = VALRES(1)
        NU = VALRES(2)
C ----   PARAMETRES MATERIAU
        YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
        LAG = 0
        PARA(1) = E
        PARA(2) = NU
        PARA(3) = YGOT
        PARA(4) = 0
        PARA(5) = 1
        PARA(6) = LAG
C  =============================================
C  -  ACTUALISATION : GEOM ORIG + DEPL DEBUT PAS
C  =============================================
        IF (ZK16(ICOMPO+2).NE.'PETIT') THEN
           IF (ZK16(ICOMPO+2).NE.'GROT_GDEP') THEN
              CALL U2MESG('F','COMPOR1_69',1,ZK16(ICOMPO+2),0,0,0,0.D0)
           ENDIF 
        ENDIF
        IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
          IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &        OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &        OPTION(1:9).EQ.'FULL_MECA') THEN
            DO 20 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLM+I-1)
  20        CONTINUE
          END IF
        END IF
C  =============================================
C  -  CALCUL DES CONTRAINTES
C  =============================================
        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN

C ----   PARAMETRES MATERIAU
          D(1) = E
          D(2) = NU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
C*          WORK(150) = 1
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
C*          WORK(3) = LAG
          PARA(1) = E
          PARA(2) = NU
          PARA(3) = YGOT
          PARA(4) = 0
          PARA(5) = 1
          PARA(6) = LAG
          DO 30 I = 1,120
            SIGMM(I) = 0.D0
            SIGMA(I) = 0.D0
            EPSLOC(I) = 0.D0
            DEPSLO(I) = 0.D0
   30     CONTINUE
C
          DO 40 I = 1,180
            DUSDX(I) = 0.D0
            DUDDD(I) = 0.D0
   40     CONTINUE
C
          DO 60 I=1,NPG
            DO 50 J=1,6
             SIGMM(6*(I-1)+J)=ZR(ICONTM+18*(I-1)+J-1)
   50       CONTINUE
   60     CONTINUE
C
          IF (NOMSHB.EQ.'SHB8') THEN
            DO 70 I=1,12
              FSTAB(I) = ZR(ICONTM+I-1+6)
   70       CONTINUE
            CALL SH8EPS(ZR(IGEOM),ZR(IDEPLM),EPSLOC,DUDDD)
            CALL SH8EPS(ZR(IGEOM),ZR(IDEPLP),DEPSLO,DUSDX)
          ELSE IF (NOMSHB.EQ.'SHB6') THEN
            CALL SH6EPS(ZR(IGEOM),ZR(IDEPLM),EPSLOC,DUDDD)
            CALL SH6EPS(ZR(IGEOM),ZR(IDEPLP),DEPSLO,DUSDX)
C
          ELSE IF (NOMSHB.EQ.'SHB15') THEN
            CALL SH1EPS(ZR(IGEOM),ZR(IDEPLM),EPSLOC,DUDDD)
            CALL SH1EPS(ZR(IGEOM),ZR(IDEPLP),DEPSLO,DUSDX)
C
          ELSE IF (NOMSHB.EQ.'SHB20') THEN
            CALL SH2EPS(ZR(IGEOM),ZR(IDEPLM),EPSLOC,DUDDD)
            CALL SH2EPS(ZR(IGEOM),ZR(IDEPLP),DEPSLO,DUSDX)
          END IF
C
          DO 80 IPG=1,NPG
             EPS2D(1)  = EPSLOC(6*(IPG-1)+1)
             EPS2D(2)  = EPSLOC(6*(IPG-1)+2)
             EPS2D(3)  = EPSLOC(6*(IPG-1)+3)
             EPS2D(4)  = EPSLOC(6*(IPG-1)+4)/SQRT(2.D0)
             EPS2D(5)  = EPSLOC(6*(IPG-1)+5)
             EPS2D(6)  = EPSLOC(6*(IPG-1)+6)
C
             DEPS2D(1) = DEPSLO(6*(IPG-1)+1)
             DEPS2D(2) = DEPSLO(6*(IPG-1)+2)
             DEPS2D(3) = DEPSLO(6*(IPG-1)+3)
             DEPS2D(4) = DEPSLO(6*(IPG-1)+4)/SQRT(2.D0)
             DEPS2D(5) = DEPSLO(6*(IPG-1)+5)
             DEPS2D(6) = DEPSLO(6*(IPG-1)+6)
C
C  recuperer SIG2D *SQRT(2.D0) /SQRT(2.D0)
C
             SIGM(1)  = SIGMM(6*(IPG-1)+1)
             SIGM(2)  = SIGMM(6*(IPG-1)+2)
             SIGM(3)  = SIGMM(6*(IPG-1)+3)
             SIGM(4)  = SIGMM(6*(IPG-1)+4)*SQRT(2.D0)
             SIGM(5)  = SIGMM(6*(IPG-1)+5)
             SIGM(6)  = SIGMM(6*(IPG-1)+6)
C
             TYPMOD='C_PLAN'
             CALL R8INIR(3, R8VIDE(), ANGMAS ,1)
             CALL R8INIR(36,0.D0,DSIDEP,1)
             CALL NMCOMP('RIGI',IPG,1,2,TYPMOD,ZI(IMATE),
     &            ZK16(ICOMPO),ZR(ICARCR),INSTM,INSTP,
     &              EPS2D,DEPS2D,
     &              SIGM,ZR(IVARIM+2*(IPG-1)),
     &              OPTION,
     &              ANGMAS,
     &              LC,
     &              SIMP,
     &              ZR(IVARIP+2*(IPG-1)),DSIDEP,IRET)
             G = D(1)/(2.D0*(1.D0+D(2)))
             SIGMA(6*(IPG-1)+1) = SIMP(1)
             SIGMA(6*(IPG-1)+2) = SIMP(2)
             SIGMA(6*(IPG-1)+3) = SIGM(3)+
     &             D(1)*DEPSLO(6*(IPG-1)+3)
             SIGMA(6*(IPG-1)+4) = SIMP(4)/SQRT(2.D0)
             SIGMA(6*(IPG-1)+5) = SIGM(5)
     &              +G*DEPSLO(6*(IPG-1)+5)
             SIGMA(6*(IPG-1)+6) = SIGM(6)
     &              +G*DEPSLO(6*(IPG-1)+6)
 80        CONTINUE
          DO 90 I = 1,120
            SIGMP(I) = 0.D0
 90       CONTINUE
          CALL SHBPKC(SIGMA,SIGMP,DUSDX,NPG)
        END IF
C  ===========================================
C  -  MATRICE DE RIGIDITE TANGENTE
C  ===========================================
        IF (OPTION(1:16).EQ.'RIGI_MECA_TANG' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
C ----   PARAMETRES MATERIAU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
          PARA(1) = E
          PARA(2) = NU
          PARA(3) = YGOT
          PARA(4) = 0
          PARA(5) = 1
          PARA(6) = LAG
          DO 100 I=1,120
             SIGMA(I)=0.D0
  100     CONTINUE
          IF (NOMSHB .EQ. 'SHB8') THEN
            DO 120 I = 1,24
              DO 110 J = 1,24
                RE(I,J) = 0.D0
  110         CONTINUE
  120       CONTINUE
            CALL SH8RIG(ZR(IGEOM),PARA,RE)
            IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
C ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
              IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
                DO 130 I=1,12
                  FSTAB(I) = ZR(ICONTM+I-1+6)
  130           CONTINUE
                DO 150 I=1,5
                  DO 140 J=1,6
                   SIGMA(6*(I-1)+J)=ZR(ICONTM+18*(I-1)+J-1)
  140             CONTINUE
  150           CONTINUE
                CALL SH8MEK(ZR(IGEOM),SIGMA,RE)
              ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
                CALL SH8MEK(ZR(IGEOM),SIGMA,RE)
              END IF
            END IF
C ----   RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ----   DEMI-MATRICE DE RIGIDITE
            CALL JEVECH('PMATUUR','E',IMATUU)
            K = 0
            DO 170 I = 1,24
              DO 160 J = 1,I
                K = K + 1
                ZR(IMATUU+K-1) = RE(I,J)
  160         CONTINUE
  170       CONTINUE
C
          ELSE IF (NOMSHB .EQ. 'SHB6') THEN
            DO 190 I = 1,18
              DO 180 J = 1,18
                RE6(I,J) = 0.D0
  180         CONTINUE
  190       CONTINUE
            CALL SH6RIG(ZR(IGEOM),PARA,RE6)
            IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
C ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
              IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
                DO 210 I=1,5
                  DO 200 J=1,6
                   SIGMA(6*(I-1)+J)=ZR(ICONTM+18*(I-1)+J-1)
  200             CONTINUE
  210           CONTINUE
                CALL SH6MEK(ZR(IGEOM),SIGMA,RE6)
              ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
                CALL SH6MEK(ZR(IGEOM),SIGMA,RE6)
              END IF
            END IF
            CALL JEVECH('PMATUUR','E',IMATUU)
            K = 0
            DO 230 I = 1,18
              DO 220 J = 1,I
                K = K + 1
                ZR(IMATUU+K-1) = RE6(I,J)
  220         CONTINUE
  230       CONTINUE
C
          ELSE IF (NOMSHB.EQ.'SHB15') THEN
            DO 250 I = 1,45
              DO 240 J = 1,45
                RE15(I,J) = 0.D0
  240         CONTINUE
  250       CONTINUE
            CALL SH1RIG(ZR(IGEOM),PARA,RE15)
            IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
C ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
              IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
                DO 270 I=1,15
                  DO 260 J=1,6
                   SIGMA(6*(I-1)+J)=ZR(ICONTM+18*(I-1)+J-1)
  260             CONTINUE
  270           CONTINUE
                CALL SH1MEK(ZR(IGEOM),SIGMA,RE15)
              ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
                CALL SH1MEK(ZR(IGEOM),SIGMA,RE15)
              END IF
            END IF
            CALL JEVECH('PMATUUR','E',IMATUU)
            K = 0
            DO 290 I = 1,45
              DO 280 J = 1,I
                K = K + 1
                ZR(IMATUU+K-1) = RE15(I,J)
  280         CONTINUE
  290       CONTINUE
C
          ELSE IF (NOMSHB.EQ.'SHB20') THEN
            DO 310 I = 1,60
              DO 300 J = 1,60
                RE20(I,J) = 0.D0
  300         CONTINUE
  310       CONTINUE
            CALL SH2RIG(ZR(IGEOM),PARA,RE20)
            IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
C ----      RIGIDITE ELASTIQUE + RIGIDITE GEOMETRIQUE
              IF (OPTION(1:16).EQ.'RIGI_MECA_TANG') THEN
                DO 330 I=1,20
                  DO 320 J=1,6
                   SIGMA(6*(I-1)+J)=ZR(ICONTM+18*(I-1)+J-1)
  320             CONTINUE
  330           CONTINUE
                CALL SH2MEK(ZR(IGEOM),SIGMA,RE20)
              ELSE IF (OPTION(1:9).EQ.'FULL_MECA') THEN
                CALL SH2MEK(ZR(IGEOM),SIGMA,RE20)
              END IF
              CALL JEVECH('PMATUUR','E',IMATUU)
              K = 0
              DO 350 I = 1,60
                DO 340 J = 1,I
                 K = K + 1
                 ZR(IMATUU+K-1) = RE20(I,J)
  340           CONTINUE
  350         CONTINUE
            END IF
          END IF
C ----   RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
C ----   DEMI-MATRICE DE RIGIDITE
        END IF
C  ===============================================================
C  -  CALCUL DES FORCES INTERNES BT.SIGMA
C  ===============================================================
        IF (OPTION(1:9).EQ.'RAPH_MECA' .OR.
     &      OPTION(1:9).EQ.'FULL_MECA') THEN
C  -  ACTUALISATION : GEOM DEBUT PAS + INCR ITER

          IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
             DO 360 I = 1,3*NNO
              ZR(IGEOM+I-1) = ZR(IGEOM+I-1) + ZR(IDEPLP+I-1)
  360       CONTINUE
          END IF
C ----   PARAMETRES MATERIAU
          YGOT = E
C ----       PARAMETRES MATERIAUX POUR LE CALCUL DE LA
C ----       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
C ----       WORK(150) : TYPE DE LOI DE COMPORTEMENT
C ----       1: SHB8PS PLEXUS
C ----       2: CONTRAINTES PLANES
C ----       3: 3D COMPLETE
C ----       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
C ----       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
          LAG = 0
C           ON PASSE EN PARAMETRES
C           ZR(IGEOM) : GEOMETRIE + DEPL DEBUT PAS + INCR DEPL
C           WORK : PARAMETRES MATERIAU
C                  WORK(1)=E  WORK(2)=NU  WORK(3)=LAG
C           ZR(IDEPLP) : INCR DEPLACEMENT
C                        (PAS UTILISE CAR LAGRANGIEN ACTUALISE)
C           ZR(ICONTP) : CONTRAINTE DE CAUCHY FIN DE PAS
C           ZR(IVARIM) (DE 2 A 14) : CONTRAINTES DE STABILISATION
C           ON RECUPERE :
C ----      ZR(IVECTU) : FORCES INTERNES FIN DE PAS
          PARA(1) = E
          PARA(2) = NU
          PARA(3) = YGOT
          PARA(4) = 0
          PARA(5) = 1
          PARA(6) = LAG
          IF (ZK16(ICOMPO+2).EQ.'GROT_GDEP') THEN
            DO 370 I = 1,3*NNO
              XIDEPP(I) = ZR(IDEPLP+I-1)
  370       CONTINUE
          ELSE
            DO 380 I = 1,3*NNO
              XIDEPP(I) = ZR(IDEPLP+I-1)
  380       CONTINUE
          END IF
          IF (NOMSHB .EQ. 'SHB8') THEN
            DO 390 I=1,12
               FSTAB(I) = ZR(ICONTM+I-1+6)
  390       CONTINUE
            CALL SH8FOR(ZR(IGEOM),PARA,XIDEPP,
     &            SIGMP,FSTAB,ZR(IVECTU))
            DO 400 I=1,12
              ZR(ICONTP+I-1+6)=FSTAB(I)
  400       CONTINUE
            DO 420 I=1,NPG
              DO 410 J=1,6
               ZR(ICONTP+18*(I-1)+J-1)=SIGMP(6*(I-1)+J)
  410         CONTINUE
  420       CONTINUE
          ELSE IF (NOMSHB.EQ.'SHB6') THEN
            CALL SH6FOR(ZR(IGEOM),PARA,XIDEPP,
     &            SIGMP,ZR(IVECTU))
            DO 440 I=1,NPG
              DO 430 J=1,6
               ZR(ICONTP+18*(I-1)+J-1)=SIGMP(6*(I-1)+J)
  430         CONTINUE
  440       CONTINUE
          ELSE IF (NOMSHB.EQ.'SHB15') THEN
            CALL SH1FOR(ZR(IGEOM),PARA,XIDEPP,
     &            SIGMP,ZR(IVECTU))
            DO 460 I=1,NPG
              DO 450 J=1,6
               ZR(ICONTP+18*(I-1)+J-1)=SIGMP(6*(I-1)+J)
  450         CONTINUE
  460       CONTINUE
          ELSE IF (NOMSHB.EQ.'SHB20') THEN
            CALL SH2FOR(ZR(IGEOM),PARA,XIDEPP,
     &            SIGMP,ZR(IVECTU))
            DO 480 I=1,NPG
              DO 470 J=1,6
               ZR(ICONTP+18*(I-1)+J-1)=SIGMP(6*(I-1)+J)
  470         CONTINUE
  480       CONTINUE
          END IF
        END IF
      END IF
      END
