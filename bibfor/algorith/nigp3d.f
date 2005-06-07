       SUBROUTINE  NIGP3D ( NNO1, NNO2, NPG1, IPOIDS, IVF1, IVF2,
     &                     IDFDE1, DFDIM, DFDIP, GEOM,
     &                     TYPMOD, OPTION, IMATE, COMPOR, LGPG, CRIT,
     &                     INSTAM, INSTAP, 
     &                     TM, TP, TREF,
     &                     DEPLM, DDEPL,
     &                     ANGMAS,
     &                     GONFLM, DGONFL,
     &                     SIGM, VIM, SIGP, VIP,
     &                     FINTU, FINTA, KUU, KUA, KAA, CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/12/2004   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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

       IMPLICIT NONE

       INTEGER       NNO1, NNO2, NPG1, IMATE, LGPG, CODRET
       INTEGER      IPOIDS, IVF1, IVF2,IDFDE1
       CHARACTER*8  TYPMOD(*)
       CHARACTER*16 COMPOR(*),OPTION


       REAL*8        DFDIM(NNO1,3)
       REAL*8        DFDIP(NNO1,3), GEOM(3,NNO1), CRIT(6)
       REAL*8        INSTAM, INSTAP, TM(NNO1),TP(NNO1), TREF
       REAL*8        DEPLM(3,20), DDEPL(3,20), GONFLM(2,8)
       REAL*8        DGONFL(2,8), SIGM(7,NPG1), SIGP(7,NPG1)
       REAL*8        VIM(LGPG,NPG1),VIP(LGPG,NPG1)
       REAL*8        KUU(3,20,3,20),KUA(3,20,2,8), KAA(2,8,2,8)
       REAL*8        FINTU(3,20), FINTA(2,8)
       REAL*8        ANGMAS(3)
C......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C                               EN 3D GRANDES TRANSFORMATIONS
C......................................................................
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION ET
C                               AU GONFLEMENT
C IN  NPG1    : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C                                                 ET AU GONFLEMENT
C IN  DFDE1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  DFDN1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  DFDK1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  INSTAM  : INSTANT PRECEDENT
C IN  INSTAP  : INSTANT DE CALCUL
C IN  TM      : TEMPERATURE AUX NOEUDS A L'INSTANT PRECEDENT
C IN  TP      : TEMPERATURE AUX NOEUDS A L'INSTANT DE CALCUL
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
C IN  DDEPL   : INCREMENT DE DEPLACEMENT
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  GONFLM  : P ET G  A L'INSTANT PRECEDENT
C IN  DGONFL  : INCREMENT POUR P ET G
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT

C OUT DFDIM    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C                    EN INSTAM
C OUT DFDIP    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C                    EN INSTAP
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT FINTU   : FORCES INTERNES
C OUT FINTA   : FORCES INTERNES LIEES AUX MULTIPLICATEURS
C OUT KUU     : MATRICE DE RIGIDITE (RIGI_MECA_TANG ET FULL_MECA)
C OUT KUA     : MATRICE DE RIGIDITE TERMES CROISES  U - (PG)
C                                   (RIGI_MECA_TANG ET FULL_MECA)
C OUT KAA     : MATRICE DE RIGIDITE TERME PG (RIGI_MECA_TANG,FULL_MECA)
C......................................................................
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      INTEGER  ZI
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
C --------- FIN  DECLARATIONS  NORMALISEES  JEVEUX ---------------------

      LOGICAL   GRAND, RESI, RIGI
      INTEGER   KPG, I, J, K, L, M, N, KL, IND(3,3), COD(27)

      REAL*8    TIERS, KRON(6)
      REAL*8    TEMPM,TEMPP,PM,GM,DP,DG,GP,JM,DJ,JP,TETJM,TETJD,TETJP
      REAL*8    GEOMM(3,20), GEOMP(3,20), POIDS0,POIDS
      REAL*8    FM(3,3),DF(3,3),FBID(3,3),EPS(6),FMLDC(3,3),DFLDC(3,3)
      REAL*8    SIGMC(6),SIGPC(6),SIGC(6),DVSIGC(6), TRSIGC
      REAL*8    DSIDEP(6,3,3),SIGEQ(6),SIGDF(3,20)
      REAL*8    DSIDF(3,3,3,20),SIGDF2(3,20),PROJ(6,3,3)
      REAL*8    TINT(3,3), TL(3,3), TLDF
      REAL*8    RBID, TMP, R8VIDE

      DATA         KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA         IND / 1, 4, 5,
     &                   4, 2, 6,
     &                   5, 6, 3 /
C       voir s'il faut les passer en parametres ou non
C       REAL*8        DFDIM(NNO1,3),DFDIP(NNO1,3)

C-----------------------------------------------------------------------
C - INITIALISATION

      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      GRAND = .TRUE.
      TIERS = 1.D0/3.D0
      IF (RESI) THEN
      CALL R8INIR(60 ,  0.D0, FINTU,1)
      CALL R8INIR(16 ,  0.D0, FINTA,1)
      ENDIF
      IF (RIGI) THEN
      CALL R8INIR(3600, 0.D0, KUU,1)
      CALL R8INIR(960 , 0.D0, KUA,1)
      CALL R8INIR( 256, 0.D0, KAA,1)
      ENDIF
C - INITIALISATION CODES RETOURS
      DO 1955 KPG=1,NPG1
         COD(KPG)=0
1955  CONTINUE


C    REACTUALISATION DE LA GEOMETRIE
      DO 5 N = 1,NNO1
        DO 6 I = 1,3
          GEOMM(I,N) = GEOM(I,N) + DEPLM(I,N)
          GEOMP(I,N) = GEOM(I,N) + DEPLM(I,N) + DDEPL(I,N)
 6      CONTINUE
 5    CONTINUE

C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 800 KPG=1,NPG1

C - CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
        TEMPM = 0.D0
        TEMPP = 0.D0
        DO 10 N=1,NNO1
          TEMPM = TEMPM + TM(N)*ZR(IVF1+N+(KPG-1)*NNO1-1)
          TEMPP = TEMPP + TP(N)*ZR(IVF1+N+(KPG-1)*NNO1-1)
 10     CONTINUE
C      CALCUL DE LA PRESSION ET DU GONFLEMENT
        GM = 0.D0
        DG = 0.D0
        PM = 0.D0
        DP = 0.D0
        DO 20 N = 1, NNO2
          PM = PM + ZR(IVF2+N+(KPG-1)*NNO2-1)*GONFLM(1,N)
          GM = GM + ZR(IVF2+N+(KPG-1)*NNO2-1)*GONFLM(2,N)
          DP = DP + ZR(IVF2+N+(KPG-1)*NNO2-1)*DGONFL(1,N)
          DG = DG + ZR(IVF2+N+(KPG-1)*NNO2-1)*DGONFL(2,N)
 20     CONTINUE
        GP = GM + DG
        GP = MAX(GP,-0.999D0)

C - CALCUL DES ELEMENTS GEOMETRIQUES
C     CALCUL DE F EN T-  =>  FM
        CALL NMGEOM(3,NNO1,.FALSE.,GRAND,GEOM,KPG,IPOIDS,
     &              IVF1,IDFDE1,DEPLM,POIDS0,DFDIM,
     &              FM,EPS,RBID)
C     CALCUL DE F POUR DT  =>  DF ET DFDIM
        CALL NMGEOM(3,NNO1,.FALSE.,GRAND,GEOMM,KPG,IPOIDS,
     &              IVF1,IDFDE1,DDEPL,POIDS,DFDIM,
     &              DF,EPS,RBID)
C     CALCUL DE DFDIP ET POIDS EN T+
        CALL NMGEOM(3,NNO1,.FALSE.,GRAND,GEOMP,KPG,IPOIDS,
     &              IVF1,IDFDE1,DEPLM,POIDS,DFDIP,
     &              FBID,EPS,RBID)

C   CALCUL DU GRADIENT DE LA TRANSFORMATION POUR LA LOI DE COMPORTEMENT
        JM= FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
     &    -FM(2,1)*(FM(1,2)*FM(3,3)-FM(1,3)*FM(3,2))
     &    +FM(3,1)*(FM(1,2)*FM(2,3)-FM(1,3)*FM(2,2))

        DJ=DF(1,1)*(DF(2,2)*DF(3,3)-DF(2,3)*DF(3,2))
     &    -DF(2,1)*(DF(1,2)*DF(3,3)-DF(1,3)*DF(3,2))
     &    +DF(3,1)*(DF(1,2)*DF(2,3)-DF(1,3)*DF(2,2))
        DJ = ABS(DJ)
        JP=JM*DJ

C     CALCUL DE F ET DF POUR LA LOI DE COMPORTEMENT
        TETJM = ((1.D0+GM)/JM)**TIERS
        TETJD =  ((1.D0 + GP)/(1.D0+GM) /DJ )**TIERS
        TETJP =  ((1.D0 + GP)/ JP )**TIERS
        CALL DCOPY(9,FM,1,FMLDC,1)
        CALL DSCAL(9,TETJM,FMLDC,1)
        CALL DCOPY(9,DF,1,DFLDC,1)
        CALL DSCAL(9,TETJD,DFLDC,1)

C      CONTRAINTE EN T- POUR LA LOI DE COMPORTEMENT
        DO 62 I=1,3
          SIGMC(I) = ( SIGM(I,KPG) + PM +
     &    ( SIGM(7,KPG) + PM )* TETJP**3 ) /TETJD
 62     CONTINUE
        DO 65 I=4,6
          SIGMC(I) = SIGM(I,KPG) / TETJD
 65     CONTINUE
C -    APPEL A LA LOI DE COMPORTEMENT
        CALL NMCOMP(3,TYPMOD,IMATE,COMPOR,CRIT,
     &              INSTAM,INSTAP,
     &              TEMPM,TEMPP,TREF,
     &              0.D0, 0.D0,
     &              0.D0, 0.D0, 0.D0,
     &              -1.D0,-1.D0,
     &              FMLDC,DFLDC,
     &              SIGMC,VIM(1,KPG),
     &              OPTION,
     &              RBID,RBID,
     &              0,RBID,RBID,
     &              R8VIDE(),R8VIDE(),
     &              ANGMAS,
     &              RBID,
     &              SIGPC,VIP(1,KPG),DSIDEP,COD(KPG))

       IF(COD(KPG).EQ.1) THEN
         GOTO 800
       ENDIF

C      CALCUL DE LA TRACE ET DU DEVIATEUR DE SIGC
        IF (RESI) THEN
          CALL DCOPY(6,SIGPC,1,SIGC,1)
        ELSE
          CALL DCOPY(6,SIGMC,1,SIGC,1)
        END IF
        TRSIGC  = SIGC(1) + SIGC(2) + SIGC(3)
        DO 70 I = 1,6
          DVSIGC(I) = SIGC(I) - TRSIGC/3.D0*KRON(I)
 70     CONTINUE

C      CALCUL DE LA CONTRAINTE D EQUILIBRE DG/DJ(SIG'+p I )
C        ET SIGDF
       IF (RESI) THEN
          DO 68 I = 1,6
            SIGEQ(I) = ( SIGC(I) - TRSIGC/3.D0*KRON(I) ) * TETJD +
     &                 (PM + DP)*KRON(I)
 68       CONTINUE
          DO 500 N = 1,NNO1
            DO 510 I = 1,3
              SIGDF(I,N) =  SIGEQ(IND(I,1))*DFDIP(N,1)
     &                   + SIGEQ(IND(I,2))*DFDIP(N,2)
     &                   + SIGEQ(IND(I,3))*DFDIP(N,3)
 510        CONTINUE
 500      CONTINUE
        ELSE
          DO 502 N = 1,NNO1
            DO 512 I = 1,3
              SIGDF(I,N) = SIGM(IND(I,1),KPG)*DFDIP(N,1)
     &                  + SIGM(IND(I,2),KPG)*DFDIP(N,2)
     &                  + SIGM(IND(I,3),KPG)*DFDIP(N,3)
 512        CONTINUE
 502      CONTINUE
        END IF

C     CALCUL DE LA MATRICE TANGENTE
C  ------------------------------------------------------------
        IF ( OPTION(1:9) .EQ. 'RIGI_MECA'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN

C       CALCULS INTERMEDIAIRES
C       TL = ID : DSIDEP
C       TLDF = TL : DF
         TLDF = 0.D0
          DO 550 I=1,3
            DO 560 J=1,3
            TL(I,J) = DSIDEP(1,I,J)+ DSIDEP(2,I,J) + DSIDEP(3,I,J)
            TLDF = TLDF + TL (I,J) * DF(J,I)
 560        CONTINUE
 550      CONTINUE

C       PROJ = DSIDEP - 1/3*  1* TL
          DO 570  I=1,3
            DO 580  J=1,3
              DO 590 K=1,3
                DO 595 L=1,3
                  PROJ(IND(I,J),K,L) = DSIDEP(IND(I,J),K,L) -
     &               KRON(IND(I,J))* TL(K,L) * TIERS
 595            CONTINUE
 590          CONTINUE
 580        CONTINUE
 570      CONTINUE

          DO 600 I = 1,3
            DO 610 N = 1,NNO1
              DO 620 J = 1,3
                DO 630 K= 1,3
                  DSIDF(I,J,K,N) =  PROJ(IND(I,1),J,K)*DFDIP(N,1)
     &                           + PROJ(IND(I,2),J,K)*DFDIP(N,2)
     &                           + PROJ(IND(I,3),J,K)*DFDIP(N,3)
 630            CONTINUE
 620          CONTINUE
 610        CONTINUE
 600      CONTINUE

C       TINT = H : DF - 1/3 L:DF Id + (DJ/DTETA)**1/3 SIG^D
          CALL R8INIR(9, 0.D0, TINT,1)
          DO 700 I=1,3
            DO 710 J=1,3
              DO 720 K=1,3
                DO 730 L=1,3
                  TINT(I,J) = TINT(I,J) + PROJ(IND(I,J),K,L)*DF(L,K)
 730            CONTINUE
 720          CONTINUE
              TINT(I,J)  = TINT(I,J) + DVSIGC(IND(I,J)) /TETJD
 710        CONTINUE
 700      CONTINUE

          DO 501 N = 1,NNO1
            DO 511 I = 1,3
              SIGDF2(I,N) = TINT(I,1)*DFDIP(N,1)
     &                   + TINT(I,2)*DFDIP(N,2)
     &                   + TINT(I,3)*DFDIP(N,3)
 511        CONTINUE
 501      CONTINUE

C        CALCUL DES DIFFERENTS TERMES DE LA MATRICE TANGENTE
C-------------------------------------------------------------
C        TERME K_UU
          DO 150 N = 1,NNO1
            DO 151 M = 1,NNO1
              DO 152 I = 1,3
                DO 153 J = 1,3
                  KUU(I,N,J,M) = KUU(I,N,J,M) + POIDS * (
     &                 DFDIP(N,I) * SIGDF(J,M) - DFDIP(M,I) * SIGDF(J,N)
     &               + TETJD * (
     &            DFDIM(N,1) * DSIDF(J,I,1,M) +
     &            DFDIM(N,2) * DSIDF(J,I,2,M) +
     &            DFDIM(N,3) * DSIDF(J,I,3,M) )
     &              - TETJD**2*TIERS * DFDIP(N,I) * SIGDF2(J,M)
     &           )
 153            CONTINUE
 152          CONTINUE
 151        CONTINUE
 150      CONTINUE

C        TERME K_UP
          DO 112 N = 1, NNO1
            DO 111 I = 1,3
              DO 110 M = 1, NNO2
                KUA(I,N,1,M) = KUA(I,N,1,M) +
     &                   POIDS * DFDIP(N,I) * ZR(IVF2+M+(KPG-1)*NNO2-1)
 110          CONTINUE
 111        CONTINUE
 112      CONTINUE

C        TERME K_UG
          DO 130 N = 1, NNO1
            DO 126 M = 1, NNO2
              DO 129 I = 1,3
C   expression obtenue a partir de Fu
                KUA(I,N,2,M) = KUA(I,N,2,M) +
     &           TETJD**2 *TIERS / (1.D0+GP) * POIDS *
     &           SIGDF2(I,N)  *ZR(IVF2+M+(KPG-1)*NNO2-1)
 129          CONTINUE
 126        CONTINUE
 130      CONTINUE

C        TERME K_PP = 0

C        TERME K_PG
          DO 145 N = 1,NNO2
            DO 140 M = 1,NNO2
              TMP=ZR(IVF2+N+(KPG-1)*NNO2-1) * ZR(IVF2+M+(KPG-1)*NNO2-1)
              KAA(1,N,2,M) = KAA(1,N,2,M) - POIDS0*TMP
              KAA(2,M,1,N) = KAA(2,M,1,N) - POIDS0*TMP
 140        CONTINUE
 145      CONTINUE

C        TERME K_GG
          DO 170 N = 1,NNO2
            DO 169 M = 1,NNO2
              KAA(2,N,2,M) = KAA(2,N,2,M) +
     &             POIDS0 /9.D0 / TETJD**2 / TETJM**3 /(1.D0+GP) *
     &            ( -2.D0 * TRSIGC + TLDF * TETJD ) *
     &         ZR(IVF2+N+(KPG-1)*NNO2-1)*ZR(IVF2+M+(KPG-1)*NNO2-1)
 169        CONTINUE
 170      CONTINUE

        END IF

C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C-----------------------------------------------------------------
        IF (OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN

C        STOCKAGE DES CONTRAINTES
          DO 190 KL=1,6
            SIGP(KL,KPG) = SIGEQ(KL)
 190      CONTINUE
          SIGP(7,KPG) = TRSIGC *TIERS / TETJD**2 *JM/(1.D0 + GM)
     &                     - PM - DP

C       CALCUL DE FINT_U
          DO 240 N=1,NNO1
            DO 239 I=1,3
              FINTU(I,N) = FINTU(I,N) + POIDS * SIGDF(I,N)
 239        CONTINUE
 240      CONTINUE

C        CALCUL DE FINT_P ET FINT_G
          DO 291 N = 1, NNO2
            TMP = (JP - 1.D0 - GP)*ZR(IVF2+N+(KPG-1)*NNO2-1)
            FINTA(1,N) = FINTA(1,N) + TMP*POIDS0
            FINTA(2,N) = FINTA(2,N) + POIDS0 * SIGP(7,KPG) *
     &                  ZR(IVF2+N+(KPG-1)*NNO2-1)
 291      CONTINUE

        END IF

800   CONTINUE
C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG1,CODRET)
      END
