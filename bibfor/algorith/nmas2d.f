      SUBROUTINE NMAS2D(NNO,NPG,IPOIDS,IVF,IDFDE,GEOM,TYPMOD,OPTION,
     &                  IMATE,COMPOR,LGPG,CRIT,
     &                  INSTAM,INSTAP,
     &                  TM,TP,TREF,
     &                  HYDRM,HYDRP,
     &                  SECHM,SECHP,SREF,
     &                  NZ,PHASM,PHASP,
     &                  DEPLM,DEPLP,
     &                  ANGMAS,
     &                  EPAM,EPAP,DEFANE,
     &                  SIGM,VIM,
     &                  DFDI,DEF,SIGP,VIP,MATUU,VECTU,CODRET)


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/06/2005   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_20
C TOLE CRP_21
      IMPLICIT NONE
      INTEGER NNO,NPG,IMATE,LGPG,NZ,CODRET,COD(9),NPGS
      INTEGER IPOIDS,IVF,IDFDE
      CHARACTER*8 TYPMOD(*)
      CHARACTER*16 OPTION,COMPOR(4)
      REAL*8 INSTAM,INSTAP
      REAL*8 GEOM(2,NNO),CRIT(3),TM(NNO),TP(NNO)
      REAL*8 HYDRM(NNO),HYDRP(NNO),SECHM(NNO),SECHP(NNO),SREF
      REAL*8 PHASM(NZ,NPG),PHASP(NZ,NPG),TREF
      REAL*8 DEPLM(1:2,1:NNO),DEPLP(1:2,1:NNO),DFDI(NNO,2)
      REAL*8 DEF(4,NNO,2),EPAM(*),EPAP(*)
      REAL*8 SIGM(10,NPG),SIGP(10,NPG)
      REAL*8 VIM(LGPG,NPG),VIP(LGPG,NPG)
      REAL*8 MATUU(*),VECTU(2,NNO),ANGMAS(3)

      LOGICAL DEFANE
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPO-ELASTICITE EN 2D POUR LE QUAD4 SOUS INTEGRE
C           STABILITE PAR ASSUMED STRAIN
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODEELISATION
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
C IN  HYDRM   : HYDRATATION AUX POINTS DE GAUSS EN T-
C IN  HYDRP   : HYDRATATION AUX POINTS DE GAUSS EN T+
C IN  SECHM   : SECHAGE AUX NOEUDS EN T-
C IN  SECHP   : SECHAGE AUX NOEUDS EN T+
C IN  SREF    : SECHAGE DE REFERENCE
C IN  PHASM   : PHASE METALLURGIQUE A L'INSTANT PRECEDENT
C IN  PHASP   : PHASE METALLURGIQUE A L'INSTANT DE CALCUL
C IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
C IN  DEPLP   : INCREMENT DE DEPLACEMENT
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  EPAM    : DEFORMATIONS ANELASTIQUES A L'INSTANT PRECEDENT
C IN  EPAP    : DEFORMATIONS ANELASTIQUES A L'INSTANT DU CALCUL
C IN  DEFANE  : VRAI SI LES DEFORMATIONS ANELASTIQUES SONT PRESENTES
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C.......................................................................
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


      LOGICAL GRAND,AXI
      INTEGER KPG,KK,KKD,N,I,M,J,J1,KL,KPGS,PROJ
      REAL*8 DSIDEP(6,6),F(3,3),EPS(6),DEPS(6),R,SIGMA(6),SIGN(6)
      REAL*8 POIDS,TEMPM,TEMPP,TMP,EPSANP(6),EPSANM(6),SIG(6)
      REAL*8 HYDRGM,HYDRGP,SECHGM,SECHGP,ELGEOM(10,9)
      REAL*8 RAC2,R8VIDE

C     AJ. VARIABLES
      REAL*8 JAC,SIGAS(4,4),PQX,PQY,DEFC(4,4,2)
      REAL*8 DH(8),GAMMA(8),COOPG(8)
      REAL*8 SDKDX(4),SDKDY(4),SDEDX(4),SDEDY(4),POI2SG(4)
      REAL*8 SDFDY(4,4),SDFDX(4,4),SDFDE(4,4),SDFDK(4,4)
      REAL*8 QPLUS(6),QMOINS(6),DQ(6),DEFN(4,4,2),KRON(3,3)
      CHARACTER*16 OPTIOS
      DATA KRON/1.D0,0.D0,0.D0,0.D0,1.D0,0.D0,0.D0,0.D0,1.D0/


C - INITIALISATION
C   ==============

C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 OPTIMAL BENDING
C           2 INCOMPRESSIBLE
      PROJ = 2
      RAC2 = SQRT(2.D0)
      GRAND = .FALSE.
      AXI = TYPMOD(1) .EQ. 'AXIS'

      DO 20 I = 1,3
        DO 10 J = 1,3
          F(I,J) = KRON(I,J)
   10   CONTINUE
   20 CONTINUE


C - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT
      CALL LCEGEO(NNO,NPG,IPOIDS,IVF,IDFDE,GEOM,TYPMOD,OPTION,
     &            IMATE,COMPOR,LGPG,ELGEOM)

C - INITIALISATION CODES RETOURS
      DO 30 KPG = 1,NPG
        COD(KPG) = 0
   30 CONTINUE

C - INITIALISATION QUAS4
      CALL INIQS4(NNO,SDFDE,SDFDK,POI2SG,COOPG)

C - CALCUL DU VECTEUR GAMMA
      GAMMA(1) = (GEOM(1,4)* (GEOM(2,2)-GEOM(2,3))+
     &           GEOM(1,2)* (GEOM(2,3)-GEOM(2,4))+
     &           GEOM(1,3)* (GEOM(2,4)-GEOM(2,2)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(2) = (GEOM(1,4)* (GEOM(2,3)-GEOM(2,1))+
     &           GEOM(1,3)* (GEOM(2,1)-GEOM(2,4))+
     &           GEOM(1,1)* (GEOM(2,4)-GEOM(2,3)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(3) = (GEOM(1,4)* (GEOM(2,1)-GEOM(2,2))+
     &           GEOM(1,1)* (GEOM(2,2)-GEOM(2,4))+
     &           GEOM(1,2)* (GEOM(2,4)-GEOM(2,1)))/
     &           (2* (((GEOM(1,4)-GEOM(1,2))* (GEOM(2,1)-GEOM(2,
     &           3)))+ (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))

      GAMMA(4) = (GEOM(1,3)* (GEOM(2,1)-GEOM(2,2))+
     &           GEOM(1,1)* (GEOM(2,2)-GEOM(2,3))+
     &           GEOM(1,2)* (GEOM(2,3)-GEOM(2,1)))/
     &           (2* (((GEOM(1,2)-GEOM(1,4))* (GEOM(2,1)-GEOM(2,
     &           3)))- (GEOM(1,1)-GEOM(1,3))* (GEOM(2,2)-GEOM(2,4))))




C - CALCUL POUR LE POINT DE GAUSS CENTRAL
      KPG = 1

C - CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
C - DE L HYDRATATION ET DU SECHAGE AU POINT DE GAUSS
C - ET DES DEFORMATIONS ANELASTIQUES AU POINT DE GAUSS

      TEMPM = 0.D0
      TEMPP = 0.D0
      HYDRGM = HYDRM(KPG)
      HYDRGP = HYDRP(KPG)
      SECHGM = 0.D0
      SECHGP = 0.D0
      DO 40 J = 1,6
        EPSANM(J) = 0.D0
        EPSANP(J) = 0.D0
   40 CONTINUE

      DO 60 N = 1,NNO
        IF (DEFANE) THEN
          DO 50 J = 1,4
            EPSANM(J) = EPSANM(J) + EPAM(4* (N-1)+J)*
     &                  ZR(IVF+N+(KPG-1)*NNO-1)
            EPSANP(J) = EPSANP(J) + EPAP(4* (N-1)+J)*
     &                  ZR(IVF+N+(KPG-1)*NNO-1)
   50     CONTINUE
        END IF
        TEMPM = TEMPM + TM(N)*ZR(IVF+N+(KPG-1)*NNO-1)
        TEMPP = TEMPP + TP(N)*ZR(IVF+N+(KPG-1)*NNO-1)
        SECHGM = SECHGM + SECHM(N)*ZR(IVF+N+(KPG-1)*NNO-1)
        SECHGP = SECHGP + SECHP(N)*ZR(IVF+N+(KPG-1)*NNO-1)

   60 CONTINUE


C - CALCUL DES ELEMENTS GEOMETRIQUES

C     CALCUL DE DFDI,F,EPS,DEPS,R(EN AXI) ET POIDS

      DO 70 J = 1,6
        EPS(J) = 0.D0
        DEPS(J) = 0.D0
   70 CONTINUE
      CALL NMGEOM(2,NNO,AXI,GRAND,GEOM,KPG,
     &              IPOIDS,IVF,IDFDE,DEPLM,POIDS,DFDI,F,EPS,R)

C     CALCUL DE DEPS
      CALL NMGEOM(2,NNO,AXI,GRAND,GEOM,KPG,
     &              IPOIDS,IVF,IDFDE,DEPLP,POIDS,DFDI,F,DEPS,R)

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
      DO 90 N = 1,NNO
        DO 80 I = 1,2
          DEF(1,N,I) = F(I,1)*DFDI(N,1)
          DEF(2,N,I) = F(I,2)*DFDI(N,2)
          DEF(3,N,I) = 0.D0
          DEF(4,N,I) = (F(I,1)*DFDI(N,2)+F(I,2)*DFDI(N,1))/RAC2
   80   CONTINUE
   90 CONTINUE


      DO 100 I = 1,3
        SIGN(I) = SIGM(I,KPG)
  100 CONTINUE
      SIGN(4) = SIGM(4,KPG)*RAC2


C - LOI DE COMPORTEMENT
      IF (OPTION(1:9).EQ.'RAPH_MECA') THEN
        OPTIOS = 'FULL_MECA'
      ELSE
        OPTIOS = OPTION
      END IF

      CALL NMCOMP(KPG,2,TYPMOD,IMATE,COMPOR,CRIT,
     &            INSTAM,INSTAP,
     &            TEMPM,TEMPP,TREF,
     &            HYDRGM,HYDRGP,
     &            SECHGM,SECHGP,SREF,
     &            EPS,DEPS,
     &            SIGN,VIM(1,KPG),
     &            OPTIOS,
     &            EPSANM,EPSANP,
     &            NZ,PHASM(1,KPG),PHASP(1,KPG),
     &            ANGMAS,
     &            ELGEOM(1,KPG),
     &            SIGMA,VIP(1,KPG),DSIDEP,COD(KPG))

C - ERREUR D'INTEGRATION
      IF (COD(KPG).EQ.1) THEN
        GO TO 320
      END IF



      IF (OPTION(1:10).EQ.'RIGI_MECA_' .OR.
     &    OPTION(1:9).EQ.'FULL_MECA') THEN


C     CALCUL DE KC (MATRICE DE RIGIDITE AU CENTRE)
C     --------------------------------------------
        DO 150 N = 1,NNO
          DO 140 I = 1,2
            DO 110,KL = 1,4
              SIG(KL) = 0.D0
              SIG(KL) = SIG(KL) + DEF(1,N,I)*DSIDEP(1,KL)
              SIG(KL) = SIG(KL) + DEF(2,N,I)*DSIDEP(2,KL)
              SIG(KL) = SIG(KL) + DEF(3,N,I)*DSIDEP(3,KL)
              SIG(KL) = SIG(KL) + DEF(4,N,I)*DSIDEP(4,KL)
  110       CONTINUE
            DO 130 J = 1,2
              DO 120 M = 1,N
                IF (M.EQ.N) THEN
                  J1 = I
                ELSE
                  J1 = 2
                END IF
C               RIGIDITE ELASTIQUE
                TMP = 0.D0
                TMP = TMP + SIG(1)*DEF(1,M,J)
                TMP = TMP + SIG(2)*DEF(2,M,J)
                TMP = TMP + SIG(3)*DEF(3,M,J)
                TMP = TMP + SIG(4)*DEF(4,M,J)
C               STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                IF (J.LE.J1) THEN
                  KKD = (2* (N-1)+I-1)* (2* (N-1)+I)/2
                  KK = KKD + 2* (M-1) + J
                  MATUU(KK) = MATUU(KK) + TMP*POIDS
                END IF
  120         CONTINUE
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE




C           CORRECTION DE LA MATRICE DE RIGIDITE
C                 CALCUL DE KSTAB
C     --------------------------------------------
        NPGS = 4

C        CALCUL DES TERMES EVALUES AUX 4 POINTS DE GAUSS
        DO 160 KPGS = 1,NPGS

          CALL DFDA2D(KPGS,NNO,POI2SG(KPGS),SDFDE,SDFDK,SDEDX,SDEDY,
     &                SDKDX,SDKDY,SDFDX,SDFDY,GEOM,JAC)

          DH(2*KPGS-1) = COOPG(2*KPGS-1)*SDKDX(KPGS) +
     &                   COOPG(2*KPGS)*SDEDX(KPGS)
          DH(2*KPGS) = COOPG(2*KPGS-1)*SDKDY(KPGS) +
     &                 COOPG(2*KPGS)*SDEDY(KPGS)


          CALL CALSTA(PROJ,GAMMA,DH,DEF,NNO,KPGS,SIG,TMP,KK,
     &                KKD,MATUU,DSIDEP,JAC)

  160   CONTINUE
      END IF





C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY

      IF (OPTION(1:9).EQ.'FULL_MECA' .OR.
     &    OPTION(1:9).EQ.'RAPH_MECA') THEN

C     INITIALISATION
        NPGS = 4
        PQX = 0.D0
        PQY = 0.D0


C     DEPLACEMENTS GENERALISES
        DO 170 KL = 1,NNO
          PQX = PQX + GAMMA(KL)*DEPLP(1,KL)
          PQY = PQY + GAMMA(KL)*DEPLP(2,KL)
  170   CONTINUE


C      INCREMENT DES CONTRAINTES GENERALISEES
        DO 180 I = 1,6
          QMOINS(I) = SIGM(I+4,KPG)

C         QUAS4 SANS PROJECTION
C         ---------------------
          IF (PROJ.EQ.0) THEN
            DQ(1) = DSIDEP(1,1)*PQX
            DQ(2) = DSIDEP(2,1)*PQY
            DQ(3) = DSIDEP(1,2)*PQX
            DQ(4) = DSIDEP(2,2)*PQY
            DQ(5) = DSIDEP(4,4)*PQX
            DQ(6) = DSIDEP(4,4)*PQY

C         INCOMPRESSIBLE
C         --------------
          ELSE IF (PROJ.EQ.2) THEN
            DQ(1) = (DSIDEP(1,1)-DSIDEP(2,1))*PQX
            DQ(2) = (DSIDEP(2,1)-DSIDEP(1,1))*PQY
            DQ(3) = (DSIDEP(1,2)-DSIDEP(2,2))*PQX
            DQ(4) = (DSIDEP(2,2)-DSIDEP(1,2))*PQY
            DQ(5) = 0.D0
            DQ(6) = 0.D0

          END IF

          QPLUS(I) = QMOINS(I) + DQ(I)
  180   CONTINUE


C      OPERATEUR DE GRADIENT AU CENTRE
        DO 200 N = 1,NNO
          DO 190 I = 1,2
            DEFC(1,N,I) = DEF(1,N,I)
            DEFC(2,N,I) = DEF(2,N,I)
            DEFC(3,N,I) = DEF(3,N,I)
            DEFC(4,N,I) = DEF(4,N,I)
  190     CONTINUE
  200   CONTINUE


C      OPERATEUR DE STABILISATION DU GRADIENT AU 4 POINTS DE GAUSS
        DO 290 KPGS = 1,NPGS


          CALL DFDA2D(KPGS,NNO,POI2SG(KPGS),SDFDE,SDFDK,SDEDX,SDEDY,
     &                SDKDX,SDKDY,SDFDX,SDFDY,GEOM,JAC)

          DH(2*KPGS-1) = COOPG(2*KPGS-1)*SDKDX(KPGS) +
     &                   COOPG(2*KPGS)*SDEDX(KPGS)
          DH(2*KPGS) = COOPG(2*KPGS-1)*SDKDY(KPGS) +
     &                 COOPG(2*KPGS)*SDEDY(KPGS)


          DO 220 N = 1,NNO
            DO 210 I = 1,2

C         QUAS4 SANS PROJECTION
C         ---------------------
              IF (PROJ.EQ.0) THEN
                DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)
                DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)
                DEFN(3,N,I) = 0.D0
                DEFN(4,N,I) = (F(I,1)*GAMMA(N)*DH(2*KPGS)+
     &                        F(I,2)*GAMMA(N)*DH(2*KPGS-1))

C         OPTIMAL BENDING
C         ---------------
              ELSE IF (PROJ.EQ.1) THEN
                DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)
                DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)
                DEFN(3,N,I) = 0.D0
                DEFN(4,N,I) = 0.D0

C         INCOMPRESSIBLE
C         --------------
              ELSE IF (PROJ.EQ.2) THEN
                DEFN(1,N,I) = F(I,1)*GAMMA(N)*DH(2*KPGS-1)* (0.5D0) +
     &                        F(I,2)*GAMMA(N)*DH(2*KPGS)* (-0.5D0)
                DEFN(2,N,I) = F(I,2)*GAMMA(N)*DH(2*KPGS)*0.5D0 +
     &                        F(I,1)*GAMMA(N)*DH(2*KPGS-1)* (-0.5D0)
                DEFN(3,N,I) = 0.D0
                DEFN(4,N,I) = 0.D0

              END IF

  210       CONTINUE
  220     CONTINUE


C    CONTRAINTES DE HOURGLASS

C         QUAS4 SANS PROJECTION
C         ---------------------
          IF (PROJ.EQ.0) THEN
            SIGAS(1,KPGS) = QPLUS(1)*DH(2*KPGS-1) + QPLUS(2)*DH(2*KPGS)
            SIGAS(2,KPGS) = QPLUS(3)*DH(2*KPGS-1) + QPLUS(4)*DH(2*KPGS)
            SIGAS(3,KPGS) = 0.D0
            SIGAS(4,KPGS) = (QPLUS(5)*DH(2*KPGS)+QPLUS(6)*DH(2*KPGS-1))/
     &                      2

C         OPTIMAL BENDING
C         ---------------
          ELSE IF (PROJ.EQ.1) THEN
            SIGAS(1,KPGS) = QPLUS(1)*DH(2*KPGS-1) + QPLUS(2)*DH(2*KPGS)
            SIGAS(2,KPGS) = QPLUS(3)*DH(2*KPGS-1) + QPLUS(4)*DH(2*KPGS)
            SIGAS(3,KPGS) = 0.D0
            SIGAS(4,KPGS) = 0.D0

C         INCOMPRESSIBLE
C         --------------
          ELSE IF (PROJ.EQ.2) THEN
            SIGAS(1,KPGS) = (QPLUS(1)*DH(2*KPGS-1)+QPLUS(2)*DH(2*KPGS))
            SIGAS(2,KPGS) = (QPLUS(3)*DH(2*KPGS-1)+QPLUS(4)*DH(2*KPGS))
            SIGAS(3,KPGS) = 0.D0
            SIGAS(4,KPGS) = 0.D0
          END IF




C     CALCUL DES FORCES INTERNES

          DO 250 N = 1,NNO
            DO 240 I = 1,2
              DO 230 KL = 1,3
                VECTU(I,N) = VECTU(I,N) + DEFC(KL,N,I)*SIGAS(KL,KPGS)*
     &                       JAC + DEFN(KL,N,I)*SIGAS(KL,KPGS)*JAC
  230         CONTINUE
              VECTU(I,N) = VECTU(I,N) + DEFC(4,N,I)*SIGAS(4,KPGS)*JAC*
     &                     RAC2 + DEFN(4,N,I)*SIGAS(4,KPGS)*JAC
  240       CONTINUE
  250     CONTINUE

          DO 280 N = 1,NNO
            DO 270 I = 1,2
              DO 260 KL = 1,3
                VECTU(I,N) = VECTU(I,N) + DEFC(KL,N,I)*SIGMA(KL)*JAC +
     &                       DEFN(KL,N,I)*SIGMA(KL)*JAC
  260         CONTINUE
              VECTU(I,N) = VECTU(I,N) + DEFC(4,N,I)*SIGMA(4)*JAC +
     &                     DEFN(4,N,I)*SIGMA(4)*JAC/RAC2
  270       CONTINUE
  280     CONTINUE
  290   CONTINUE


        DO 300 KL = 1,3
          SIGP(KL,KPG) = SIGMA(KL)
  300   CONTINUE
        SIGP(4,KPG) = SIGMA(4)/RAC2


        DO 310 I = 1,6
          SIGP(I+4,KPG) = QPLUS(I)
  310   CONTINUE


      END IF


  320 CONTINUE
C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG,CODRET)

      END
