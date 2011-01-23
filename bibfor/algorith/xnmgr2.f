      SUBROUTINE  XNMGR2(ELREFP,ELRESE,NDIM,COORSE,IGEOM,HE,NFH,
     &                   DDLC,DDLM,NFE,INSTAM,INSTAP,IDEPLP,SIGM,VIP,
     &                   BASLOC,NNOP,NPG,TYPMOD,OPTION,IMATE,COMPOR,
     &                   LGPG,CRIT,IDEPLM,LSN,LST,NFISS,FISNO,
     &                   SIGP,VI,MATUU,IVECTU,CODRET)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 25/01/2011   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE MASSIN P.MASSIN
C TOLE CRP_21 CRS_1404
C
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN GRANDE ROTATION ET PETITE DEFORMATION AVEC X-FEM EN 2D
C
C     TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
C.......................................................................
C
      IMPLICIT NONE
      INTEGER       NDIM,IGEOM,IMATE,LGPG,CODRET,NNOP,NPG
      INTEGER       NFISS,FISNO(NNOP,NFISS)
      INTEGER       NFH,DDLC,DDLM,NFE,IDEPLM,IVECTU,IDEPLP
      CHARACTER*8   ELREFP,TYPMOD(*),ELRESE
      CHARACTER*16  OPTION,COMPOR(4)
      CHARACTER*24  COORSE
      REAL*8        BASLOC(6*NNOP),CRIT(3),HE(NFISS)
      REAL*8        LSN(NNOP),LST(NNOP)
      REAL*8        VI(LGPG,NPG),VIP(LGPG,NPG),SIGP(4,NPG),MATUU(*)
      REAL*8        INSTAM,INSTAP,SIGM(4,NPG),SIGN(6)

C IN  ELREFP  : ELEMENT DE REFERENCE PARENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  COORSE  : COORDONNEES DES SOMMETS DU SOUS-ELEMENT
C IN  IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT PARENT
C IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ELT
C IN  NFH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
C IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
C IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
C IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
C IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ELEMENT
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  T       : TEMPERATURE AUX NOEUDS
C IN  TREF    : TEMPERATURE DE REFERENCE
C IN  DEPLM   : DEPLACEMENT A PARTIR DE LA CONF DE REF
C IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
C IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS

C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
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

      INTEGER  KPG,KK,N,I,M,J,J1,KL,PQ,KKD,INO,IG,IRET,IJ
      INTEGER  NNO,NNOS,NPGBIS,DDLS,DDLD,DDLDN,CPT,NDIMB
      INTEGER  JCOOPG,JDFD2,JGANO,JCOORS,IDFDE,IVF,IPOIDS
      LOGICAL  GRDEPL,AXI,CPLAN,RESI,RIGI
      REAL*8   F(3,3),FM(3,3),FR(3,3),EPSM(6),EPSP(6),DEPS(6)
      REAL*8   DSIDEP(6,6),SIGMA(6),FTF,DETF
      REAL*8   TMP1,TMP2,SIG(6),FE(4),BASLOG(6)
      REAL*8   XG(NDIM),XE(NDIM),FF(NNOP),JAC,LSNG,LSTG
      REAL*8   RBID1(10),RBID2(10), RBID
      REAL*8   DFDI(NNOP,NDIM),PFF(6,NNOP,NDIM),DGDGL(4,3)
      REAL*8   DEF(6,NNOP,NDIM*(1+NFH+NFE)),GRAD(3,3),ANGMAS(3)
      REAL*8   ELGEOM(10,27),DFDIB(27,3)
      REAL*8   FMM(3,3),RIND1(4),DEPLB1(3,27),DEPLB2(3,27)
      INTEGER  NNOPS,IBID,NN,MN

      INTEGER INDI(4),INDJ(4)
      REAL*8  RIND(4),RAC2
      DATA    INDI / 1 , 2 , 3 , 1 /
      DATA    INDJ / 1 , 2 , 3 , 2 /
      DATA    RIND / 0.5D0 , 0.5D0 , 0.5D0 , 0.70710678118655D0 /
      DATA    RAC2 / 1.4142135623731D0 /
      DATA    ANGMAS /0.D0, 0.D0, 0.D0/
      DATA    RIND1 / 0.5D0 , 0.5D0 , 0.5D0 , 1.D0 /
C--------------------------------------------------------------------

C     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
      DDLD=NDIM*(1+NFH+NFE)
      DDLDN = DDLD/NDIM
C
C     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
      DDLS=DDLD+DDLC
C
C     RECUPERATION DU NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT PARENT
      CALL ELREF4(' ','RIGI',IBID,IBID,NNOPS,IBID,
     &                                   IBID,IBID,IBID,IBID)
C
C - INITIALISATION
      GRDEPL  = COMPOR(3) .EQ. 'GROT_GDEP'
      AXI    = TYPMOD(1) .EQ. 'AXIS'
      CPLAN  = TYPMOD(1) .EQ. 'C_PLAN'
      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
C
      IF (AXI) THEN
        CALL U2MESS('F','XFEM2_2')
      ENDIF
C
C     ADRESSE DES COORD DU SOUS ELT EN QUESTION
      CALL JEVEUO(COORSE,'L',JCOORS)
C
      CALL ELREF5(ELRESE,'XINT',NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,
     &                  IDFDE,JDFD2,JGANO)
      CALL ASSERT(NPG.EQ.NPGBIS.AND.NDIM.EQ.NDIMB)

C - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
C - LES ARGUMENTS DFDIB, DEPLB1, DEPLB2 NE SERVENT PAS DANS CE CAS
      CALL LCEGEO(NNO      ,NPG   ,IPOIDS,IVF   ,IDFDE ,
     &            ZR(IGEOM),TYPMOD,COMPOR,NDIM  ,DFDIB ,
     &            DEPLB1   ,DEPLB2,ELGEOM)
C
C-----------------------------------------------------------------------
C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 10 KPG=1,NPG
C
C       COORDONNEES DU PT DE GAUSS DANS LE REPERE REEL : XG
        CALL VECINI(NDIM,0.D0,XG)
        DO 110 I=1,NDIM
          DO 111 N=1,NNO
          XG(I)=XG(I)+ZR(IVF-1+NNO*(KPG-1)+N)*ZR(JCOORS-1+NDIM*(N-1)+I)
 111      CONTINUE
 110    CONTINUE
C
        IF (NFE.GT.0) THEN
C         JUSTE POUR CALCULER LES FF
        CALL REEREF(ELREFP,AXI, NNOP,NNOPS,IGEOM,XG,IDEPLM,GRDEPL,
     &              NDIM,HE,RBID, RBID,
     &              FISNO,NFISS,NFH,NFE,DDLS,DDLM,FE,DGDGL,'NON',
     &              XE,FF,DFDI,FM,EPSM,GRAD)
C
C         BASE LOCALE AU POINT DE GAUSS
          CALL VECINI(6,0.D0,BASLOG)
          LSNG = 0.D0
          LSTG = 0.D0
          DO 113 INO=1,NNOP
            LSNG = LSNG + LSN(INO) * FF(INO)
            LSTG = LSTG + LST(INO) * FF(INO)
            DO 114 I=1,6
              BASLOG(I) = BASLOG(I) + BASLOC(6*(INO-1)+I) * FF(INO)
 114        CONTINUE
 113      CONTINUE
C
C         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DERIVEES
          CALL XCALF2(XG,HE,LSNG,LSTG,BASLOG,FE,DGDGL,IRET)
C         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
C         CAR ON SE TROUVE SUR LE FOND DE FISSURE
          CALL ASSERT(IRET.NE.0)
        ENDIF
C
C       COORDONNÉES DU POINT DE GAUSS DANS L'ELEMENT DE REF PARENT : XE
C       ET CALCUL DE FF, DFDI, EPSM ET EPSP
C       CALCUL EN T-
        CALL REEREF(ELREFP,AXI, NNOP,NNOPS,IGEOM,XG,IDEPLM,GRDEPL,
     &              NDIM,HE,RBID, RBID,
     &              FISNO,NFISS,NFH,NFE,DDLS,DDLM,FE,DGDGL,'OUI',
     &              XE,FF,DFDI,FM,EPSM,GRAD)
C
C       CALCUL EN T+
        CALL REEREF(ELREFP,AXI, NNOP,NNOPS,IGEOM,XG,IDEPLP,GRDEPL,
     &              NDIM,HE,RBID, RBID,
     &              FISNO,NFISS,NFH,NFE,DDLS,DDLM,FE,DGDGL,'OUI',
     &              XE,FF,DFDI,F,EPSP,GRAD)
C
C - CALCUL DE DEPS POUR LDC
        DO 25 I = 1,6
          DEPS(I)=EPSP(I)-EPSM(I)
 25     CONTINUE

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        IF (RESI) THEN
          DO 26 I=1,3
            DO 27 J=1,3
              FR(I,J) = F(I,J)
 27         CONTINUE
 26       CONTINUE
        ELSE
          DO 28 I=1,3
            DO 29 J=1,3
              FR(I,J) = FM(I,J)
 29         CONTINUE
 28       CONTINUE
        ENDIF

        DO 120 N=1,NNOP
C         FONCTIONS DE FORME CLASSIQUES
          DO 121 I=1,2
            DEF(1,N,I) =  FR(I,1)*DFDI(N,1)
            DEF(2,N,I) =  FR(I,2)*DFDI(N,2)
            DEF(3,N,I) =  0.D0
            DEF(4,N,I) = (FR(I,1)*DFDI(N,2) + FR(I,2)*DFDI(N,1))/RAC2
 121      CONTINUE
C         ENRICHISSEMENT PAR HEAVYSIDE
          DO 122 IG=1,NFH
            DO 123 I=1,2
              CPT=NDIM*(1+IG-1)+I
              DEF(1,N,CPT) =  DEF(1,N,I) * HE(FISNO(N,IG))
              DEF(2,N,CPT) =  DEF(2,N,I) * HE(FISNO(N,IG))
              DEF(3,N,CPT) =  0.D0
              DEF(4,N,CPT) =  DEF(4,N,I) * HE(FISNO(N,IG))
 123        CONTINUE
 122      CONTINUE
C         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIERES
          DO 124 IG=1,NFE
            DO 125 I=1,2
              CPT = NDIM*(1+NFH+IG-1)+I
              DEF(1,N,CPT) =  FR(I,1) *
     &             (DFDI(N,1) * FE(IG) + FF(N)*DGDGL(IG,1))

              DEF(2,N,CPT) =  FR(I,2) *
     &             (DFDI(N,2) * FE(IG) + FF(N)*DGDGL(IG,2))

              DEF(3,N,CPT) =  0.D0

              DEF(4,N,CPT) =
     &         ( FR(I,1)* (DFDI(N,2)*FE(IG)+FF(N)*DGDGL(IG,2))
     &         + FR(I,2)* (DFDI(N,1)*FE(IG)+FF(N)*DGDGL(IG,1)) )/RAC2
 125         CONTINUE
 124       CONTINUE
        CALL ASSERT(CPT.EQ.DDLD)
 120    CONTINUE
C
C       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
C       ON ENVOIE DFDM2D AVEC LES COORD DU SS-ELT
        CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,ZR(JCOORS),
     &                                   RBID1,RBID2,JAC)
C
C      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        IF (RIGI) THEN
          DO 127 I=1,NDIM
            DO 128 N=1,NNOP
              CPT = 1
              PFF(CPT,N,I) =  DFDI(N,I)
              DO 129 IG = 1,NFH
                CPT = CPT+1
                PFF(CPT,N,I) = DFDI(N,I)*HE(FISNO(N,IG))
 129          CONTINUE
              DO 132 IG = 1,NFE
                CPT = CPT+1
                PFF(CPT,N,I) = DFDI(N,I)*FE(IG)+FF(N)*DGDGL(IG,I)
 132          CONTINUE
              CALL ASSERT(CPT.EQ.DDLDN)
 128        CONTINUE
 127      CONTINUE
        ENDIF
C
C       LOI DE COMPORTEMENT
C       CONTRAINTE CAUCHY -> CONTRAINTE LAGRANGE POUR LDC EN T-
        IF (CPLAN) FM(3,3) = SQRT(ABS(2.D0*EPSM(3)+1.D0))
        CALL MATINV('S',3,FM,FMM,DETF)
        CALL VECINI(6,0.D0,SIGN)
        DO 130 PQ = 1,4
          DO 131 KL = 1,4
            FTF = ( FMM(INDI(PQ),INDI(KL)) * FMM(INDJ(PQ),INDJ(KL))
     &          +   FMM(INDI(PQ),INDJ(KL)) * FMM(INDJ(PQ),INDI(KL)))
     &           *   RIND1(KL)
            SIGN(PQ) =  SIGN(PQ)+ FTF*SIGM(KL,KPG)
 131      CONTINUE
          SIGN(PQ) = SIGN(PQ)*DETF
 130    CONTINUE
        SIGN(4) = SIGN(4)*RAC2
C
C       INTEGRATION
C
        CALL R8INIR(6,0.0D0,SIGMA,1)
        CALL NMCOMP('RIGI',KPG,1,2,TYPMOD,IMATE,COMPOR,CRIT,
     &              INSTAM,INSTAP,
     &              EPSM,DEPS,
     &              SIGN,VI(1,KPG),
     &              OPTION,
     &              ANGMAS,
     &              ELGEOM(1,KPG),
     &              SIGMA,VIP(1,KPG),DSIDEP,CODRET)

C - CALCUL DE LA MATRICE DE RIGIDITE
        IF (RIGI) THEN
C
C          RIGIDITÉ GEOMETRIQUE
          DO 240 N=1,NNOP
            CALL INDENT(N,DDLS,DDLM,NNOPS,NN)

            DO 241 M=1,N
              CALL INDENT(M,DDLS,DDLM,NNOPS,MN)
C
              DO 242 I=1,DDLDN
                DO 243 J=1,DDLDN
                  TMP1 = 0.D0
                  IF (OPTION(1:4).EQ.'RIGI') THEN
                    TMP1 = SIGN(1)*PFF(I,N,1)*PFF(J,M,1)
     &                    + SIGN(2)*PFF(I,N,2)*PFF(J,M,2)
     &                    + SIGN(4)*(PFF(I,N,1)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,1))/RAC2
                  ELSE
                    TMP1 = SIGMA(1)*PFF(I,N,1)*PFF(J,M,1)
     &                    + SIGMA(2)*PFF(I,N,2)*PFF(J,M,2)
     &                    + SIGMA(4)*(PFF(I,N,1)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,1))/RAC2
                  ENDIF
C                STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = DDLDN
                  ENDIF
                  IF (J.LE.J1) THEN
                    DO 244 IJ = 1,NDIM
                      KKD = (NN+(I-1)*NDIM+IJ-1)
     &                     * (NN+(I-1)*NDIM+IJ) /2

                      KK = KKD + MN+(J-1)*NDIM+IJ

                      MATUU(KK) = MATUU(KK) + TMP1*JAC
 244                CONTINUE
                  ENDIF
 243            CONTINUE
 242          CONTINUE
 241        CONTINUE
 240      CONTINUE
C          RIGIDITE ELASTIQUE
          DO 140 N=1,NNOP
            CALL INDENT(N,DDLS,DDLM,NNOPS,NN)

            DO 141 I=1,DDLD
              DO 142,KL=1,4
                SIG(KL) = DEF(1,N,I)*DSIDEP(1,KL)
     &                   + DEF(2,N,I)*DSIDEP(2,KL)
     &                   + DEF(3,N,I)*DSIDEP(3,KL)
     &                   + DEF(4,N,I)*DSIDEP(4,KL)
 142          CONTINUE
              DO 143 J=1,DDLD
                DO 144 M=1,N
                  CALL INDENT(M,DDLS,DDLM,NNOPS,MN)
                  TMP2 = SIG(1)*DEF(1,M,J)
     &                  + SIG(2)*DEF(2,M,J)
     &                  + SIG(3)*DEF(3,M,J)
     &                  + SIG(4)*DEF(4,M,J)

C                STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = DDLD
                  ENDIF
                  IF (J.LE.J1) THEN
                    KKD = (NN+I-1) * (NN+I) /2
                    KK = KKD + MN+J
                    MATUU(KK) = MATUU(KK) + TMP2*JAC
                  END IF
 144            CONTINUE
 143          CONTINUE
 141        CONTINUE
 140      CONTINUE
        ENDIF
C
C - CALCUL DE LA FORCE INTERIEURE

        IF (RESI) THEN
          DO 185 N=1,NNOP
            CALL INDENT(N,DDLS,DDLM,NNOPS,NN)
            DO 186 I=1,DDLD
              DO 187 KL=1,4
              ZR(IVECTU-1+NN+I)=
     &        ZR(IVECTU-1+NN+I)+DEF(KL,N,I)*SIGMA(KL)*JAC
 187          CONTINUE
 186        CONTINUE
 185      CONTINUE
C
C    CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
C
          IF (CPLAN) F(3,3) = SQRT(ABS(2.D0*EPSP(3)+1.D0))
          DETF = F(3,3)*(F(1,1)*F(2,2)-F(1,2)*F(2,1))
          DO 190 PQ = 1,4
            SIGP(PQ,KPG) = 0.D0
            DO 200 KL = 1,4
              FTF = (F(INDI(PQ),INDI(KL))*F(INDJ(PQ),INDJ(KL)) +
     &        F(INDI(PQ),INDJ(KL))*F(INDJ(PQ),INDI(KL)))*RIND(KL)
              SIGP(PQ,KPG) =  SIGP(PQ,KPG)+ FTF*SIGMA(KL)
 200        CONTINUE
            SIGP(PQ,KPG) = SIGP(PQ,KPG)/DETF
 190      CONTINUE
        ENDIF

 10   CONTINUE
      END
