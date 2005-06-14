       SUBROUTINE  NIGP2D ( NNO1, NNO2, NPG1, IPOIDS, IVF1 , IVF2,
     &                      IDFDE1, DFDIM, DFDIP, GEOM,
     &                      TYPMOD, OPTION, IMATE, COMPOR, LGPG, CRIT,
     &                      INSTAM, INSTAP,
     &                      TM, TP, TREF,
     &                      DEPLM, DDEPL,
     &                      ANGMAS,
     &                      GONFLM, DGONFL, 
     &                      SIGM, VIM, SIGP, VIP,
     &                      FINTU, FINTA, KUU, KUA, KAA, CODRET)
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

       INTEGER        NNO1, NNO2, NPG1, IMATE, LGPG, CODRET
       INTEGER       IPOIDS, IVF1 , IVF2, IDFDE1
       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  COMPOR(*), OPTION


       REAL*8         DFDIM(NNO1,2), DFDIP(NNO1,2)
       REAL*8         GEOM(2,NNO1), CRIT(6)
       REAL*8         INSTAM, INSTAP, TM(NNO1), TP(NNO1), TREF
       REAL*8         DEPLM(2,9), DDEPL(2,9), GONFLM(2,4)
       REAL*8         DGONFL(2,4), SIGM(5,NPG1), SIGP(5,NPG1)
       REAL*8         VIM(LGPG,NPG1), VIP(LGPG,NPG1)
       REAL*8         KUU(2,9,2,9), KUA(2,9,2,4), KAA(2,4,2,4)
       REAL*8         FINTU(2,9), FINTA(2,4)
       REAL*8         ANGMAS(3)

C......................................................................
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPO-ELASTICITE
C......................................................................
C IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
C IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION ET
C                               AU GONFLEMENT
C IN  NPG1    : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
C IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
C                  ET AU GONFLEMENT
C IN  DFDE1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
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
C OUT KUA     : MATRICE DE RIGIDITE TERMES CROISES U - PG
C                                       (RIGI_MECA_TANG ET FULL_MECA)
C OUT KAA     : MATRICE DE RIGIDITE TERME PG (RIGI_MECA_TANG, FULL_MECA)
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

      LOGICAL   GRAND, AXI, RESI, RIGI
      INTEGER   KPG, I, J, K, L, M, N, IND(3,3), COD(9)

      REAL*8    TIERS, KRON(6)
      REAL*8    TEMPM,TEMPP,PM,GM,DP,DG,GP,JM,DJ,JP,TETJM,TETJD,TETJP
      REAL*8    GEOMM(2,9), GEOMP(2,9),POIDS0,POIDS, RM, RP
      REAL*8    FM(3,3),DF(3,3),FBID(3,3),EPS(6),FMLDC(3,3),DFLDC(3,3)
      REAL*8    SIGMC(4),SIGPC(4),SIGC(4),DVSIGC(4),TRSIGC
      REAL*8    DSIDEP(6,3,3),SIGEQ(4),SIGDF(2,9)
      REAL*8    DSIDF(2,2,2,9),SIGDF2(2,9),PROJ(6,3,3)
      REAL*8    TINT(3,3),TL(3,3), TLDF, VFF2, VFFN, VFFM
      REAL*8    RBID, TMP, R8VIDE

      DATA        KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/
      DATA    IND / 1, 4, 5,
     &              4, 2, 6,
     &              5, 6, 3 /
C-----------------------------------------------------------------------
C - INITIALISATION

      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
      AXI   = TYPMOD(1) .EQ. 'AXIS'
      GRAND = .TRUE.
      TIERS = 1.D0/3.D0
      IF (RESI) THEN
        CALL R8INIR(18, 0.D0, FINTU,1)
        CALL R8INIR( 8, 0.D0, FINTA,1)
      ENDIF
      IF (RIGI) THEN
        CALL R8INIR(324, 0.D0, KUU,1)
        CALL R8INIR(144, 0.D0, KUA,1)
        CALL R8INIR( 64, 0.D0, KAA,1)
      ENDIF

C - INITIALISATION CODES RETOURS
      DO 7 KPG=1,NPG1
         COD(KPG)=0
 7    CONTINUE

C    REACTUALISATION DE LA GEOMETRIE
      DO 5 N = 1,NNO1
        DO 6 I = 1,2
          GEOMM(I,N) = GEOM(I,N) + DEPLM(I,N)
          GEOMP(I,N) = GEOM(I,N) + DEPLM(I,N) + DDEPL(I,N)
 6      CONTINUE
 5    CONTINUE

C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 800 KPG = 1,NPG1

C - CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
        TEMPM = 0.D0
        TEMPP = 0.D0
        DO 10 N=1,NNO1
          TEMPM = TEMPM + TM(N)*ZR(IVF1-1+N+(KPG-1)*NNO1)
          TEMPP = TEMPP + TP(N)*ZR(IVF1-1+N+(KPG-1)*NNO1)
 10     CONTINUE
C       CALCUL DE LA PRESSION ET DU GONFLEMENT
        GM = 0.D0
        DG = 0.D0
        PM = 0.D0
        DP = 0.D0
        DO 20 N = 1, NNO2
          VFF2 = ZR(IVF2-1+N+(KPG-1)*NNO2)
          PM = PM + VFF2*GONFLM(1,N)
          GM = GM + VFF2*GONFLM(2,N)
          DP = DP + VFF2*DGONFL(1,N)
          DG = DG + VFF2*DGONFL(2,N)
 20     CONTINUE
        GP = GM + DG
        GP = MAX(GP,-0.999D0)

C - CALCUL DES ELEMENTS GEOMETRIQUES
C     CALCUL DE F EN T-  =>  FM
        CALL NMGEOM(2,NNO1,AXI,GRAND,GEOM,KPG,IPOIDS,IVF1,IDFDE1,
     &              DEPLM,POIDS0,DFDIM,
     &              FM,EPS,RM)

C     CALCUL DE F POUR DT  =>  DF, RM  ET DFDIM
        CALL NMGEOM(2,NNO1,AXI,GRAND,GEOMM,KPG,IPOIDS,IVF1,IDFDE1,
     &              DDEPL,POIDS,DFDIM,
     &              DF,EPS,RM)

C     CALCUL DE DFDIP, RP ET POIDS EN T+
        CALL NMGEOM(2,NNO1,AXI,GRAND,GEOMP,KPG,IPOIDS,IVF1,IDFDE1,
     &              DEPLM,POIDS,DFDIP,
     &              FBID,EPS,RP)

C   CALCUL DU GRADIENT DE LA TRANSFORMATION POUR LA LOI DE COMPORTEMENT
        JM=FM(1,1)*(FM(2,2)*FM(3,3)-FM(2,3)*FM(3,2))
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
          SIGMC(I) =( SIGM(I,KPG) - PM +
     &    ( SIGM(5,KPG) + PM )* TETJP**3 ) /TETJD
 62     CONTINUE
        SIGMC(4) = SIGM(4,KPG) / TETJD


       CALL NMCOMP(2,TYPMOD,IMATE,COMPOR,CRIT,
     &             INSTAM,INSTAP,
     &             TEMPM,TEMPP,TREF,
     &             0.D0, 0.D0,
     &             0.D0, 0.D0,0.D0,
     &             -1.D0,-1.D0,
     &             FMLDC,DFLDC,
     &             SIGMC,VIM(1,KPG),
     &             OPTION,
     &             RBID,RBID,
     &             0,RBID,RBID,
     &             R8VIDE(),R8VIDE(),
     &             ANGMAS,
     &             RBID,
     &             SIGPC,VIP(1,KPG),DSIDEP,COD(KPG))
C       IF (.NOT.AXI)  TYPMOD(1) = 'D_PLAN  '

      IF(COD(KPG).EQ.1) THEN
         GOTO 800
       ENDIF

C      CALCUL DE LA TRACE ET DU DEVIATEUR DE SIGC
        IF (RESI) THEN
          CALL DCOPY(4,SIGPC,1,SIGC,1)
        ELSE
          CALL DCOPY(4,SIGMC,1,SIGC,1)
        END IF
        TRSIGC  = SIGC(1) + SIGC(2) + SIGC(3)
        DO 70 I = 1,4
          DVSIGC(I) = SIGC(I) - TRSIGC/3.D0*KRON(I)
 70     CONTINUE

C      CALCUL DE LA CONTRAINTE D EQUILIBRE DG/DJ(SIG'+p I )
C        ET SIGDF
        IF (RESI) THEN
          DO 68 I = 1,4
            SIGEQ(I) = ( SIGC(I) - TRSIGC/3.D0*KRON(I) ) * TETJD +
     &                 (PM + DP)*KRON(I)
 68       CONTINUE
        ELSE
          DO 69 I = 1,4
            SIGEQ(I) = SIGM(I,KPG)
 69       CONTINUE
        END IF
          DO 500 N = 1,NNO1
            DO 510 I = 1,2
              SIGDF(I,N) =  SIGEQ(IND(I,1))*DFDIP(N,1)
     &                   + SIGEQ(IND(I,2))*DFDIP(N,2)
 510        CONTINUE
 500      CONTINUE

C     CALCUL DE LA MATRICE TANGENTE
C  ------------------------------------------------------------
        IF ( OPTION(1:16) .EQ. 'RIGI_MECA_TANG'
     &    .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN

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

          DO 600 I = 1,2
            DO 610 N = 1,NNO1
              DO 620 J = 1,2
                DO 630 K= 1,2
                  DSIDF(I,J,K,N) = PROJ(IND(I,1),J,K)*DFDIP(N,1)
     &                           + PROJ(IND(I,2),J,K)*DFDIP(N,2)
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
            DO 511 I = 1,2
              SIGDF2(I,N) = TINT(I,1)*DFDIP(N,1)
     &                   + TINT(I,2)*DFDIP(N,2)
 511        CONTINUE
 501      CONTINUE

C        CALCUL DES DIFFERENTS TERMES DE LA MATRICE TANGENTE
C-------------------------------------------------------------
C        TERME K_UU
          DO 150 N = 1,NNO1
            DO 151 M = 1,NNO1
              DO 152 I = 1,2
                DO 153 J = 1,2
                  KUU(I,N,J,M) = KUU(I,N,J,M) + POIDS * (
     &                 DFDIP(N,I) * SIGDF(J,M) - DFDIP(M,I) * SIGDF(J,N)
     &               - TETJD**2*TIERS * DFDIP(N,I) * SIGDF2(J,M)
     &               + TETJD * (
     &            DFDIM(N,1) * DSIDF(J,I,1,M) +
     &            DFDIM(N,2) * DSIDF(J,I,2,M) )
     &           )
 153            CONTINUE
 152          CONTINUE
 151        CONTINUE
 150      CONTINUE

C        TERMES COMPLEMENTAIRES EN AXISYMETRIQUE
          IF (AXI) THEN
            DO 155 N = 1,NNO1
              VFFN = ZR(IVF1-1+N+(KPG-1)*NNO1)
              DO 156 M = 1,NNO1
                VFFM = ZR(IVF1-1+M+(KPG-1)*NNO1)
                KUU(1,N,1,M) = KUU(1,N,1,M) + POIDS * (
     &            -TIERS*(TETJD)**2 * VFFN * VFFM*
     &                                  TINT(3,3) /RP**2
     &            + TETJD**2*VFFN/RM*PROJ(3,3,3)*VFFM/RP
     &            )
                DO 157 I = 1,2
                  KUU(1,N,I,M) =  KUU(1,N,I,M) + POIDS * (
     &             VFFN /RP * SIGDF(I,M)
     &             -TIERS*(TETJD)**2 * VFFN /RP * SIGDF2(I,M)
     &             +TETJD**2 * VFFN/RM *
     &                  ( PROJ(IND(I,1),3,3)*DFDIP(M,1) +
     &                    PROJ(IND(I,2),3,3)*DFDIP(M,2))
     &                  )
                  KUU(I,N,1,M) =  KUU(I,N,1,M) + POIDS * (
     &              DFDIP(N,I) * SIGEQ(3) * VFFM/RP
     &              -TIERS*(TETJD)**2 * DFDIP(N,I)*
     &                             TINT(3,3) * VFFM/RP
     &              + TETJD**2 * (DFDIM(N,1) * PROJ(3,I,1) +
     &                             DFDIM(N,2)*PROJ(3,I,2) )*
     &                             VFFM/RP
     &                    )

 157          CONTINUE
 156          CONTINUE
 155        CONTINUE
          END IF

C        TERME K_UP
          DO 112 N = 1, NNO1
            VFFN = ZR(IVF1-1+N+(KPG-1)*NNO1)
            DO 111 M = 1, NNO2
              VFFM = ZR(IVF2-1+M+(KPG-1)*NNO2)
              DO 110 I = 1,2
                KUA(I,N,1,M) = KUA(I,N,1,M) +
     &                   POIDS * DFDIP(N,I) * VFFM
 110          CONTINUE
C        TERMES COMPLEMENTAIRES EN AXISYMETRIQUE
              IF (AXI) THEN
                KUA(1,N,1,M) = KUA(1,N,1,M) +
     &                   POIDS * VFFN / RP * VFFM
              END IF
 111        CONTINUE
 112      CONTINUE

C        TERME K_UG
          DO 130 N = 1, NNO1
            DO 126 M = 1, NNO2
              DO 129 I = 1,2
                KUA(I,N,2,M) = KUA(I,N,2,M) +
     &                TETJD**2 * TIERS / (1.D0+GP) * POIDS *
     &                SIGDF2(I,N)  * VFFM
 129          CONTINUE
C        TERMES COMPLEMENTAIRES EN AXISYMETRIQUE
              IF (AXI) THEN
                KUA(1,N,2,M) = KUA(1,N,2,M) + POIDS*TETJD**2 *TIERS/
     &            (1.D0+GP)*TINT(3,3)*VFFN/RP * VFFM
              END IF
 126        CONTINUE
 130      CONTINUE

C        TERME K_PP = 0

C        TERME K_PG
          DO 145 N = 1,NNO2
            VFFN = ZR(IVF2-1+N+(KPG-1)*NNO2)
            DO 140 M = 1,NNO2
              VFFM = ZR(IVF2-1+M+(KPG-1)*NNO2)
              TMP = VFFN * VFFM
              KAA(1,N,2,M) = KAA(1,N,2,M) - POIDS0*TMP
              KAA(2,M,1,N) = KAA(2,M,1,N) - POIDS0*TMP
 140        CONTINUE
 145      CONTINUE

C        TERME K_GG
          DO 170 N = 1,NNO2
            VFFN = ZR(IVF2-1+N+(KPG-1)*NNO2)
            DO 169 M = 1,NNO2
              VFFM = ZR(IVF2-1+M+(KPG-1)*NNO2)
              KAA(2,N,2,M) = KAA(2,N,2,M) +
     &           POIDS /9.D0 / TETJD**2  /(1.D0+GP)**2 *
     &           ( -2.D0 * TRSIGC /TETJD + TLDF ) *
     &            VFFN * VFFM
C               KAA(2,N,2,M) = KAA(2,N,2,M) +
C      &           POIDS0 /9.D0 / TETJD**2 / TETJM**3 /(1.D0+GP) *
C      &           ( -2.D0 * TRSIGC + TLDF * TETJD ) *
C      &            VFFN * VFFM
 169        CONTINUE
 170      CONTINUE

        ENDIF

C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C-----------------------------------------------------------------

        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN

C        STOCKAGE DES CONTRAINTES
C------------------------------------------
          DO 290 K=1,4
            SIGP(K,KPG) = SIGEQ(K)
 290      CONTINUE
            SIGP(5,KPG) = TRSIGC *TIERS / TETJD**2 *JM/(1.D0 + GM)
     &                     - PM - DP
C        CALCUL DE FINT_U
          DO 190 N=1,NNO1
            VFFN = ZR(IVF1-1+N+(KPG-1)*NNO1)
            DO 189 I=1,2
              FINTU(I,N) = FINTU(I,N) + POIDS * SIGDF(I,N)
 189        CONTINUE
            IF (AXI) THEN
              FINTU(1,N) = FINTU(1,N) + POIDS *
     &                       SIGEQ(3) * VFFN/RP
            END IF
 190      CONTINUE

C        CALCUL DE FINT_P ET FINT_G
          DO 191 N = 1, NNO2
            VFF2 = ZR(IVF2-1+N+(KPG-1)*NNO2)
            TMP = (JP - 1.D0 - GP)*VFF2
            FINTA(1,N) = FINTA(1,N) + TMP*POIDS0
            FINTA(2,N) = FINTA(2,N) + POIDS0 * SIGP(5,KPG) *VFF2
 191      CONTINUE

        ENDIF

 800  CONTINUE
C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG1,CODRET)


      END
