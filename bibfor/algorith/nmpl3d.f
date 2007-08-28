       SUBROUTINE  NMPL3D(FAMI,NNO,NPG,IPOIDS,IVF,IDFDE,GEOM,TYPMOD,
     &                    OPTION,IMATE,COMPOR,LGPG,CRIT,
     &                    INSTAM,INSTAP,
     &                    DEPLM,DEPLP,
     &                    ANGMAS,
     &                    SIGM,VIM,
     &                    MATSYM,DFDI,DEF,SIGP,VIP,MATUU,VECTU,CODRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2007   AUTEUR PROIX J-M.PROIX 
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
C RESPONSABLE MABBAS M.ABBAS
C TOLE CRP_21

       IMPLICIT NONE
C
       INTEGER       NNO, NPG, IMATE, LGPG, CODRET
C
       CHARACTER*(*) FAMI
       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  OPTION, COMPOR(16)
C
       REAL*8        INSTAM,INSTAP,ANGMAS(*)
       REAL*8        GEOM(3,NNO), CRIT(*)
       REAL*8        DEPLM(1:3,1:NNO),DEPLP(1:3,1:NNO),DFDI(NNO,3)
       REAL*8        DEF(6,NNO,3)
       REAL*8        SIGM(6,NPG),SIGP(6,NPG)
       REAL*8        VIM(LGPG,NPG),VIP(LGPG,NPG)
       REAL*8        MATUU(*),VECTU(3,NNO)
C
       LOGICAL       MATSYM
      COMMON / NMPALE / UNSURK,UNSURM,VALDEN
      REAL*8            UNSURK,UNSURM,VALDEN
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
C
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPO-ELASTICITE EN 3D
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  IPOIDS  : POIDS DES POINTS DE GAUSS
C IN  IVF     : VALEUR  DES FONCTIONS DE FORME
C IN  IDFDE   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
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
C IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
C IN  DEPLP   : INCREMENT DE DEPLACEMENT
C IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
C IN  VARDEP  : VARIABLE DELOCALISEE EN T+
C IN  DELOCA  : VRAI SI VARIABLES DELOCALISEES PRESENTES
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
C OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C......................................................................

      LOGICAL GRAND

      INTEGER KPG,KK,N,I,M,J,J1,KL, KKD,COD(27),IPOIDS,IVF,IDFDE,IRET

      REAL*8 DSIDEP(6,6),F(3,3),EPS(6),DEPS(6),R,SIGMA(6),SIGN(6),SIG(6)
      REAL*8 POIDS,TMP,RAC2
      REAL*8 R8VIDE,ELGEOM(10,27)
C - INITIALISATION

      RAC2   = SQRT(2.D0)
      GRAND  = .FALSE.

C - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT

      CALL LCEGEO(NNO,NPG,IPOIDS,IVF,IDFDE,GEOM,TYPMOD,OPTION,
     &            IMATE,COMPOR,LGPG,ELGEOM)


C - INITIALISATION CODES RETOURS
      DO 1955 KPG=1,NPG
         COD(KPG)=0
1955  CONTINUE

C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 800 KPG=1,NPG


C - CALCUL DES ELEMENTS GEOMETRIQUES
C
C     CALCUL DE DFDI,F,EPS,DEPS,R(EN AXI) ET POIDS
C
        DO 20 J = 1,6
          EPS (J)=0.D0
          DEPS(J)=0.D0
20      CONTINUE
C
        CALL NMGEOM(3,NNO,.FALSE.,GRAND,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              DEPLM,POIDS,DFDI,F,EPS,R)
C
C     CALCUL DE DEPS
C
        CALL NMGEOM(3,NNO,.FALSE.,GRAND,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              DEPLP,POIDS,DFDI,F,DEPS,R)
C
C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 40 N=1,NNO
          DO 30 I=1,3
            DEF(1,N,I) =  F(I,1)*DFDI(N,1)
            DEF(2,N,I) =  F(I,2)*DFDI(N,2)
            DEF(3,N,I) =  F(I,3)*DFDI(N,3)
            DEF(4,N,I) = (F(I,1)*DFDI(N,2) + F(I,2)*DFDI(N,1))/RAC2
            DEF(5,N,I) = (F(I,1)*DFDI(N,3) + F(I,3)*DFDI(N,1))/RAC2
            DEF(6,N,I) = (F(I,2)*DFDI(N,3) + F(I,3)*DFDI(N,2))/RAC2
 30       CONTINUE
 40     CONTINUE
C
        DO 60 I=1,3
          SIGN(I) = SIGM(I,KPG)
 60     CONTINUE
        DO 65 I=4,6
          SIGN(I) = SIGM(I,KPG)*RAC2
 65     CONTINUE

C
C - LOI DE COMPORTEMENT
C
C -    APPEL A LA LOI DE COMPORTEMENT
        CALL NMCOMP(FAMI,KPG,1,3,TYPMOD,IMATE,COMPOR,CRIT,
     &              INSTAM,INSTAP,
     &              EPS,DEPS,
     &              SIGN,VIM(1,KPG),
     &              OPTION,
     &              ANGMAS,
     &              ELGEOM(1,KPG),
     &              SIGMA,VIP(1,KPG),DSIDEP,COD(KPG))
        IF(COD(KPG).EQ.1) THEN
         GOTO 1956
       ENDIF
C
C
C - CALCUL DE LA MATRICE DE RIGIDITE
C
        IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN
C
          IF (MATSYM) THEN
            DO 160 N=1,NNO
              DO 150 I=1,3
                KKD = (3*(N-1)+I-1) * (3*(N-1)+I) /2
                DO 151,KL=1,6
                  SIG(KL)=0.D0
                  SIG(KL)=SIG(KL)+DEF(1,N,I)*DSIDEP(1,KL)
                  SIG(KL)=SIG(KL)+DEF(2,N,I)*DSIDEP(2,KL)
                  SIG(KL)=SIG(KL)+DEF(3,N,I)*DSIDEP(3,KL)
                  SIG(KL)=SIG(KL)+DEF(4,N,I)*DSIDEP(4,KL)
                  SIG(KL)=SIG(KL)+DEF(5,N,I)*DSIDEP(5,KL)
                  SIG(KL)=SIG(KL)+DEF(6,N,I)*DSIDEP(6,KL)
151             CONTINUE
                DO 140 J=1,3
                  DO 130 M=1,N
                    IF (M.EQ.N) THEN
                      J1 = I
                    ELSE
                      J1 = 3
                    ENDIF
C
C                   RIGIDITE ELASTIQUE
                    TMP=0.D0
                    TMP=TMP+SIG(1)*DEF(1,M,J)
                    TMP=TMP+SIG(2)*DEF(2,M,J)
                    TMP=TMP+SIG(3)*DEF(3,M,J)
                    TMP=TMP+SIG(4)*DEF(4,M,J)
                    TMP=TMP+SIG(5)*DEF(5,M,J)
                    TMP=TMP+SIG(6)*DEF(6,M,J)
C
C                   STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                    IF (J.LE.J1) THEN
                       KK = KKD + 3*(M-1)+J
                       MATUU(KK) = MATUU(KK) + TMP*POIDS
                    END IF
C
 130              CONTINUE
 140            CONTINUE
 150          CONTINUE
 160        CONTINUE
          ELSE
            DO 560 N=1,NNO
              DO 550 I=1,3
                DO 551,KL=1,6
                  SIG(KL)=0.D0
                  SIG(KL)=SIG(KL)+DEF(1,N,I)*DSIDEP(1,KL)
                  SIG(KL)=SIG(KL)+DEF(2,N,I)*DSIDEP(2,KL)
                  SIG(KL)=SIG(KL)+DEF(3,N,I)*DSIDEP(3,KL)
                  SIG(KL)=SIG(KL)+DEF(4,N,I)*DSIDEP(4,KL)
                  SIG(KL)=SIG(KL)+DEF(5,N,I)*DSIDEP(5,KL)
                  SIG(KL)=SIG(KL)+DEF(6,N,I)*DSIDEP(6,KL)
551             CONTINUE
                DO 540 J=1,3
                  DO 530 M=1,NNO
C
C                   RIGIDITE ELASTIQUE
                    TMP=0.D0
                    TMP=TMP+SIG(1)*DEF(1,M,J)
                    TMP=TMP+SIG(2)*DEF(2,M,J)
                    TMP=TMP+SIG(3)*DEF(3,M,J)
                    TMP=TMP+SIG(4)*DEF(4,M,J)
                    TMP=TMP+SIG(5)*DEF(5,M,J)
                    TMP=TMP+SIG(6)*DEF(6,M,J)
C
C                   STOCKAGE SANS SYMETRIE
                    KK = 3*NNO*(3*(N-1)+I-1) + 3*(M-1)+J
                    MATUU(KK) = MATUU(KK) + TMP*POIDS
C
 530              CONTINUE
 540            CONTINUE
 550          CONTINUE
 560        CONTINUE
          ENDIF
        ENDIF
C
C
C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C
        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN
C
          DO 230 N=1,NNO
            DO 220 I=1,3
              DO 210 KL=1,6
                VECTU(I,N)=VECTU(I,N)+DEF(KL,N,I)*SIGMA(KL)*POIDS
 210          CONTINUE
 220        CONTINUE
 230      CONTINUE
C
          DO 310 KL=1,3
            SIGP(KL,KPG) = SIGMA(KL)
 310      CONTINUE
          DO 320 KL=4,6
            SIGP(KL,KPG) = SIGMA(KL)/RAC2
 320      CONTINUE
C
        ENDIF
800   CONTINUE
1956  CONTINUE
C - SYNTHESE DES CODES RETOURS
      CALL CODERE(COD,NPG,CODRET)
      END
