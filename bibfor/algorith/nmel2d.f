       SUBROUTINE  NMEL2D(FAMI,NNO, NPG, IPOIDS, IVF, IDFDE,
     &             GEOM, TYPMOD, OPTION, IMATE, COMPOR, LGPG, CRIT, T,
     &             TREF, DEPL, ANGMAS,DFDI, PFF, DEF, SIG, VI,
     &             MATUU, VECTU, CODRET )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 28/08/2006   AUTEUR CIBHHPD L.SALMONA 
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
C TOLE CRP_21
       IMPLICIT NONE

       INTEGER       NNO,NPG,IMATE,LGPG,CODRET,IPOIDS,IVF,IDFDE
       CHARACTER*8   TYPMOD(*)
       CHARACTER*16  OPTION, COMPOR(4)
       CHARACTER*4   FAMI

       REAL*8        GEOM(2,NNO), CRIT(3), T(NNO), TREF
       REAL*8        ANGMAS(3)
       REAL*8        DEPL(1:2,1:NNO), DFDI(NNO,2)
       REAL*8        PFF(4,NNO,NNO),  DEF(4,NNO,2)
       REAL*8        SIG(4,NPG), VI(LGPG,NPG)
       REAL*8        MATUU(*), VECTU(2,NNO)
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPER-ELASTICITE
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C              CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  T       : TEMPERATURE AUX NOEUDS
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPL    : DEPLACEMENT A PARTIR DE LA CONF DE REF
C OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
C OUT PFF     : PRODUIT DES FCT. DE FORME       AU DERNIER PT DE GAUSS
C OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
C OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
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

      INTEGER  KPG,KK,N,I,M,J,J1,KL,PQ,KKD
      LOGICAL  GRDEPL, GRROTA, AXI, CPLAN
      REAL*8   DSIDEP(6,6),F(3,3),EPS(6),R,SIGMA(6),FTF,DETF
      REAL*8   POIDS,TEMP,TMP1,TMP2,SIGP(6)

      INTEGER INDI(4),INDJ(4)
      REAL*8  RIND(4),RAC2
      DATA    INDI / 1 , 2 , 3 , 1 /
      DATA    INDJ / 1 , 2 , 3 , 2 /
      DATA    RIND / 0.5D0 , 0.5D0 , 0.5D0 , 0.70710678118655D0 /
      DATA    RAC2 / 1.4142135623731D0 /




C - INITIALISATION

      GRDEPL = COMPOR(3)(1:8) .EQ. 'GREEN   '
      GRROTA = COMPOR(3)(1:8) .EQ. 'GREEN_GR'
      AXI    = TYPMOD(1) .EQ. 'AXIS'
      CPLAN  = TYPMOD(1) .EQ. 'C_PLAN'

      IF ( GRROTA ) THEN
        CALL UTMESS ('F','NMEL2D','ELEMENTS ISOPARAMETRIQUES 2D NON'
     &              //' DISPONIBLES EN GRANDES ROTATIONS')
      ENDIF


C - CALCUL POUR CHAQUE POINT DE GAUSS

      DO 10 KPG=1,NPG

C - CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
C - 

        TEMP = 0.D0
        DO 15 N=1,NNO
          TEMP = TEMP + T(N)*ZR(IVF+N+(KPG-1)*NNO-1)
 15     CONTINUE


C - CALCUL DES ELEMENTS GEOMETRIQUES

C      CALCUL DE DFDI, F, EPS, R (EN AXI) ET POIDS
        CALL NMGEOM(2,NNO,AXI,GRDEPL,GEOM,KPG,IPOIDS,IVF,IDFDE,
     &              DEPL,POIDS,DFDI,F,EPS,R)

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 120 N=1,NNO
          DO 122 I=1,2
            DEF(1,N,I) =  F(I,1)*DFDI(N,1)
            DEF(2,N,I) =  F(I,2)*DFDI(N,2)
            DEF(3,N,I) =  0.D0
            DEF(4,N,I) = (F(I,1)*DFDI(N,2) + F(I,2)*DFDI(N,1))/RAC2
 122      CONTINUE
 120    CONTINUE

C      TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        IF (AXI) THEN
          DO 124 N=1,NNO
            DEF(3,N,1) = F(3,3)*ZR(IVF+N+(KPG-1)*NNO-1)/R
 124      CONTINUE
        ENDIF

C      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        IF ( (OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR.  OPTION(1: 9) .EQ. 'FULL_MECA'    ) .AND. GRDEPL) THEN
          DO 125 N=1,NNO
            DO 126 M=1,N
              PFF(1,N,M) =  DFDI(N,1)*DFDI(M,1)
              PFF(2,N,M) =  DFDI(N,2)*DFDI(M,2)
              PFF(3,N,M) = 0.D0
              PFF(4,N,M) =(DFDI(N,1)*DFDI(M,2)+DFDI(N,2)*DFDI(M,1))/RAC2
 126        CONTINUE
 125      CONTINUE
        ENDIF


C - LOI DE COMPORTEMENT : S(E) ET DS/DE

        CALL NMCPEL(FAMI,KPG,1,2,TYPMOD,ANGMAS,IMATE,COMPOR,CRIT,TEMP,
     &              TREF,OPTION,EPS,SIGMA,VI(1,KPG),DSIDEP,CODRET)


C - CALCUL DE LA MATRICE DE RIGIDITE

        IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN

          DO 130 N=1,NNO
            DO 131 I=1,2
              KKD = (2*(N-1)+I-1) * (2*(N-1)+I) /2
              DO 151,KL=1,4
                SIGP(KL)=0.D0
                SIGP(KL)=SIGP(KL)+DEF(1,N,I)*DSIDEP(1,KL)
                SIGP(KL)=SIGP(KL)+DEF(2,N,I)*DSIDEP(2,KL)
                SIGP(KL)=SIGP(KL)+DEF(3,N,I)*DSIDEP(3,KL)
                SIGP(KL)=SIGP(KL)+DEF(4,N,I)*DSIDEP(4,KL)
151           CONTINUE
              DO 140 J=1,2
                DO 141 M=1,N
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = 2
                  ENDIF

C                 RIGIDITE GEOMETRIQUE
                  TMP1 = 0.D0
                  IF (GRDEPL .AND. I.EQ.J) THEN
                    TMP1 = PFF(1,N,M)*SIGMA(1)
     &                   + PFF(2,N,M)*SIGMA(2)
     &                   + PFF(3,N,M)*SIGMA(3)
     &                   + PFF(4,N,M)*SIGMA(4)

C                  TERME DE CORRECTION AXISYMETRIQUE
                    IF (AXI .AND. I.EQ.1) THEN
                      TMP1=TMP1+ZR(IVF+N+(KPG-1)*NNO-1)*
     &                     ZR(IVF+M+(KPG-1)*NNO-1)/(R*R)*SIGMA(3)
                    END IF
                  ENDIF

C                RIGIDITE ELASTIQUE
                  TMP2 = SIGP(1)*DEF(1,M,J) + SIGP(2)*DEF(2,M,J)
     &                 + SIGP(3)*DEF(3,M,J) + SIGP(4)*DEF(4,M,J)

C                STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (J.LE.J1) THEN
                    KK = KKD + 2*(M-1)+J
                    MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
                  END IF

 141            CONTINUE
 140          CONTINUE
 131        CONTINUE
 130      CONTINUE
        ENDIF


C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY

        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN

          DO 185 N=1,NNO
            DO 186 I=1,2
              DO 187 KL=1,4
                VECTU(I,N)=VECTU(I,N)+DEF(KL,N,I)*SIGMA(KL)*POIDS
 187          CONTINUE
 186        CONTINUE
 185      CONTINUE


          IF ( GRDEPL ) THEN
C          CONVERSION LAGRANGE -> CAUCHY
            IF (CPLAN) F(3,3) = SQRT(ABS(2.D0*EPS(3)+1.D0))
            DETF = F(3,3)*(F(1,1)*F(2,2)-F(1,2)*F(2,1))
            DO 190 PQ = 1,4
              SIG(PQ,KPG) = 0.D0
              DO 200 KL = 1,4
                FTF = (F(INDI(PQ),INDI(KL))*F(INDJ(PQ),INDJ(KL)) +
     &          F(INDI(PQ),INDJ(KL))*F(INDJ(PQ),INDI(KL)))*RIND(KL)
                SIG(PQ,KPG) =  SIG(PQ,KPG)+ FTF*SIGMA(KL)
 200          CONTINUE
              SIG(PQ,KPG) = SIG(PQ,KPG)/DETF
 190        CONTINUE
          ELSE
            DO 210 KL=1,3
              SIG(KL,KPG) = SIGMA(KL)
 210        CONTINUE
            SIG(4,KPG) = SIGMA(4)/RAC2
          ENDIF
        ENDIF
 10   CONTINUE
      END
