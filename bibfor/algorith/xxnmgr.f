      SUBROUTINE  XXNMGR(ELREFP,ELRESE,NDIM,COORSE,IGEOM,HE,NFH,
     &                   DDLC,DDLM,NFE,INSTAM,INSTAP,IDEPLP,SIGM,VIP,
     &                   BASLOC,NNOP,NPG,TYPMOD,OPTION,IMATE,COMPOR,
     &                   LGPG,IDECPG,CRIT,IDEPL,LSN,LST,NFISS,FISNO,
     &                   SIGP,VI,MATUU,IVECTU,CODRET)

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER       NDIM,IGEOM,IMATE,LGPG,CODRET,NNOP,NPG
      INTEGER       NFISS,FISNO(NNOP,NFISS),IDECPG
      INTEGER       NFH,DDLC,DDLM,NFE
      CHARACTER*8   ELREFP,TYPMOD(*),ELRESE
      CHARACTER*16  OPTION,COMPOR(4)
      REAL*8        BASLOC(3*NDIM*NNOP),CRIT(3),HE(NFISS)
      INTEGER       IDEPL,IDEPLP,IVECTU
      REAL*8        LSN(NNOP),LST(NNOP),COORSE(*)
      REAL*8        VI(LGPG,NPG),VIP(LGPG,NPG),SIGP(2*NDIM,NPG),MATUU(*)
      REAL*8        INSTAM,INSTAP,SIGM(2*NDIM,NPG),SIGN(6)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/01/2013   AUTEUR FERTE G.FERTE 
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
C IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
C IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
C IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
C IN  NFH     : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
C IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
C IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU (EN 2D)
C IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
C IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
C IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
C IN  IDEPL   : ADRESSE DU DEPLACEMENT A PARTIR DE LA CONF DE REF
C IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
C IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS

C OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C......................................................................
C
      INTEGER  I,IG,IRET,J,J1,K,KK,KKD,KPG,L,M,MN,N,NN
      INTEGER  DDLS,DDLD,DDLDN,CPT,DEC(NNOP)
      INTEGER  IBID,IDFDE,IPOIDS,IVF,JCOOPG,JDFD2,JGANO
      INTEGER  NDIMB,NNO,NNOPS,NNOS,NPGBIS
      REAL*8   F(3,3),FM(3,3),FR(3,3),EPSM(6),EPSP(6),DEPS(6)
      REAL*8   DSIDEP(6,6),SIGMA(6),FTF,DETF
      REAL*8   TMP1,TMP2,SIG(6),FE(4),BASLOG(3*NDIM)
      REAL*8   XG(NDIM),XE(NDIM),FF(NNOP),JAC,LSNG,LSTG
      REAL*8   RBID,RBID10(10),RBID4(4),RBID33(3,3)
      REAL*8   DFDI(NNOP,NDIM),PFF(6,NNOP,NDIM),DGDGL(4,3)
      REAL*8   DEF(6,NNOP,NDIM*(1+NFH+NFE))
      REAL*8   ELGEOM(10,27),DFDIB(27,3)
      REAL*8   FMM(3,3),DEPLB1(3,27),DEPLB2(3,27)
      LOGICAL  GRDEPL,AXI,CPLAN,RESI,RIGI
C
      INTEGER INDI(6),INDJ(6)
      REAL*8  RIND(6),RIND1(6),RAC2,ANGMAS(3)
      DATA    INDI / 1 , 2 , 3 , 1 , 1 , 2 /
      DATA    INDJ / 1 , 2 , 3 , 2 , 3 , 3 /
      DATA    RIND / 0.5D0,0.5D0,0.5D0,0.70710678118655D0,
     &               0.70710678118655D0,0.70710678118655D0 /
      DATA    RAC2 / 1.4142135623731D0 /
      DATA    ANGMAS /0.D0, 0.D0, 0.D0/
      DATA    RIND1 / 0.5D0 , 0.5D0 , 0.5D0 , 1.D0, 1.D0, 1.D0 /
C--------------------------------------------------------------------

C     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
      DDLD = NDIM*(1+NFH+NFE)
      DDLDN = DDLD/NDIM
C
C     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
      DDLS = DDLD+DDLC
C
C     RECUPERATION DU NOMBRE DE NOEUDS SOMMETS DE L'ELEMENT PARENT
      CALL ELREF4(' ','RIGI',IBID,IBID,NNOPS,IBID,IBID,IBID,IBID,IBID)
C
C - INITIALISATION
      GRDEPL = COMPOR(3) .EQ. 'GROT_GDEP'
      AXI    = TYPMOD(1) .EQ. 'AXIS'
      CPLAN  = TYPMOD(1) .EQ. 'C_PLAN'
      RESI = OPTION(1:4).EQ.'RAPH' .OR. OPTION(1:4).EQ.'FULL'
      RIGI = OPTION(1:4).EQ.'RIGI' .OR. OPTION(1:4).EQ.'FULL'
C
      IF (AXI) THEN
        CALL U2MESS('F','XFEM2_5')
      ENDIF
C
      CALL ELREF5(ELRESE,'XINT',NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,
     &            IDFDE,JDFD2,JGANO)

      CALL ASSERT(NPG.EQ.NPGBIS.AND.NDIM.EQ.NDIMB)

C - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES LOIS DE COMPORTEMENT
C - LES ARGUMENTS DFDIB, DEPLB1, DEPLB2 NE SERVENT PAS DANS CE CAS
      CALL LCEGEO(NNO      ,NPG   ,IPOIDS,IVF   ,IDFDE ,
     &            ZR(IGEOM),TYPMOD,COMPOR,NDIM  ,DFDIB ,
     &            DEPLB1   ,DEPLB2,ELGEOM)
C
      DO 178 N=1,NNOP
         CALL INDENT(N,DDLS,DDLM,NNOPS,DEC(N))
178   CONTINUE
C
C-----------------------------------------------------------------------
C - CALCUL POUR CHAQUE POINT DE GAUSS DU SOUS-ELEMENT
      DO 1000 KPG=1,NPG
C
C       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        CALL VECINI(NDIM,0.D0,XG)
        DO 100 I=1,NDIM
          DO 101 N=1,NNO
            XG(I) = XG(I) + ZR(IVF-1+NNO*(KPG-1)+N)*COORSE(NDIM*(N-1)+I)
 101      CONTINUE
 100    CONTINUE
C
        IF (NFE.GT.0) THEN
C         JUSTE POUR CALCULER LES FF
          CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPL,GRDEPL,
     &                NDIM,HE,RBID,RBID,FISNO,NFISS,NFH,NFE,DDLS,
     &                DDLM,FE,DGDGL,'NON',XE,FF,DFDI,FM,EPSM,RBID33)
C
C         BASE LOCALE  ET LEVEL SETS AU POINT DE GAUSS
          CALL VECINI(3*NDIM,0.D0,BASLOG)
          LSNG = 0.D0
          LSTG = 0.D0
          DO 110 N=1,NNOP
            LSNG = LSNG + LSN(N) * FF(N)
            LSTG = LSTG + LST(N) * FF(N)
            DO 111 I=1,3*NDIM
              BASLOG(I) = BASLOG(I) + BASLOC(3*NDIM*(N-1)+I) * FF(N)
 111        CONTINUE
 110      CONTINUE
C
C         FONCTION D'ENRICHISSEMENT AU POINT DE GAUSS ET LEURS DÉRIVÉES
          IF (NDIM.EQ.2) THEN
            CALL XCALF2(HE,LSNG,LSTG,BASLOG,FE,DGDGL,IRET)
          ELSEIF (NDIM.EQ.3) THEN
            CALL XCALFE(HE,LSNG,LSTG,BASLOG,FE,DGDGL,IRET)
          ENDIF
C         ON A PAS PU CALCULER LES DERIVEES DES FONCTIONS SINGULIERES
C         CAR ON SE TROUVE SUR LE FOND DE FISSURE
          CALL ASSERT(IRET.NE.0)
        ENDIF
C
C       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
C       ET CALCUL DE FF, DFDI, EPSM ET EPSP
C       CALCUL EN T-
        CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPL,GRDEPL,
     &              NDIM,HE,RBID,RBID,FISNO,NFISS,NFH,NFE,DDLS,DDLM,
     &              FE,DGDGL,'OUI',XE,FF,DFDI,FM,EPSM,RBID33)
C
C       CALCUL EN T+
        CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPLP,GRDEPL,
     &              NDIM,HE,RBID,RBID,FISNO,NFISS,NFH,NFE,DDLS,DDLM,
     &              FE,DGDGL,'OUI',XE,FF,DFDI,F,EPSP,RBID33)
C
C       CALCUL DE DEPS POUR LDC
        DO 120 I=1,6
          DEPS(I) = EPSP(I)-EPSM(I)
 120    CONTINUE

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        IF (RESI) THEN
          DO 130 I=1,3
            DO 131 J=1,3
              FR(I,J) = F(I,J)
 131        CONTINUE
 130     CONTINUE
        ELSE
          DO 140 I=1,3
            DO 141 J=1,3
              FR(I,J) = FM(I,J)
 141        CONTINUE
 140      CONTINUE
        ENDIF

C
C       CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 150 N=1,NNOP
C         FONCTIONS DE FORME CLASSIQUES
          DO 151 I=1,NDIM
            DEF(1,N,I) = FR(I,1)*DFDI(N,1)
            DEF(2,N,I) = FR(I,2)*DFDI(N,2)
            DEF(3,N,I) = 0.D0
            DEF(4,N,I) = (FR(I,1)*DFDI(N,2) + FR(I,2)*DFDI(N,1))/RAC2
            IF (NDIM.EQ.3) THEN
              DEF(3,N,I) =  FR(I,3)*DFDI(N,3)
              DEF(5,N,I) = (FR(I,1)*DFDI(N,3) + FR(I,3)*DFDI(N,1))/RAC2
              DEF(6,N,I) = (FR(I,2)*DFDI(N,3) + FR(I,3)*DFDI(N,2))/RAC2
            ENDIF
 151      CONTINUE
C         ENRICHISSEMENT PAR HEAVYSIDE
          DO 152 IG=1,NFH
            DO 153 I=1,NDIM
              CPT = NDIM*(1+IG-1)+I
              DO 154 M=1,2*NDIM
                DEF(M,N,CPT) = DEF(M,N,I) * HE(FISNO(N,IG))
 154          CONTINUE
              IF (NDIM.EQ.2) DEF(3,N,CPT) = 0.D0
 153        CONTINUE
 152      CONTINUE
C         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
          DO 155 IG=1,NFE
            DO 156 I=1,NDIM
              CPT = NDIM*(1+NFH+IG-1)+I
              DEF(1,N,CPT) =
     &           FR(I,1)* (DFDI(N,1) * FE(IG) + FF(N)*DGDGL(IG,1))

              DEF(2,N,CPT) =
     &           FR(I,2)* (DFDI(N,2) * FE(IG) + FF(N)*DGDGL(IG,2))

              DEF(3,N,CPT) =  0.D0

              DEF(4,N,CPT) =
     &         ( FR(I,1)* (DFDI(N,2)*FE(IG)+FF(N)*DGDGL(IG,2))
     &         + FR(I,2)* (DFDI(N,1)*FE(IG)+FF(N)*DGDGL(IG,1)) )/RAC2
              IF (NDIM.EQ.3) THEN
                DEF(3,N,CPT) =
     &             FR(I,3)* (DFDI(N,3) * FE(IG) + FF(N)*DGDGL(IG,3))
                DEF(5,N,CPT) =
     &           ( FR(I,1)* (DFDI(N,3)*FE(IG)+FF(N)*DGDGL(IG,3))
     &           + FR(I,3)* (DFDI(N,1)*FE(IG)+FF(N)*DGDGL(IG,1)) )/RAC2
                DEF(6,N,CPT) =
     &           ( FR(I,3)* (DFDI(N,2)*FE(IG)+FF(N)*DGDGL(IG,2))
     &           + FR(I,2)* (DFDI(N,3)*FE(IG)+FF(N)*DGDGL(IG,3)) )/RAC2
              ENDIF
 156        CONTINUE
 155      CONTINUE
          CALL ASSERT(CPT.EQ.DDLD)
 150    CONTINUE
C
C       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
C       ON ENVOIE DFDM2D OU DFDM3D AVEC LES COORD DU SS-ELT
        IF (NDIM.EQ.2) THEN
          CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,COORSE,RBID10,RBID10,JAC)
        ELSEIF (NDIM.EQ.3) THEN
          CALL DFDM3D(NNO,KPG,IPOIDS,IDFDE,COORSE,RBID4,RBID4,RBID4,JAC)
        ENDIF
C
C      CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        IF (RIGI) THEN
          DO 160 I=1,NDIM
            DO 161 N=1,NNOP
              CPT = 1
              PFF(CPT,N,I) =  DFDI(N,I)
              DO 162 IG=1,NFH
                CPT = CPT+1
                PFF(CPT,N,I) = DFDI(N,I) * HE(FISNO(N,IG))
 162          CONTINUE
              DO 163 IG=1,NFE
                CPT = CPT+1
                PFF(CPT,N,I) = DFDI(N,I)*FE(IG) + FF(N)*DGDGL(IG,I)
 163          CONTINUE
              CALL ASSERT(CPT.EQ.DDLDN)
 161        CONTINUE
 160      CONTINUE
        ENDIF
C
C       LOI DE COMPORTEMENT
C       CONTRAINTE CAUCHY -> CONTRAINTE LAGRANGE POUR LDC EN T-
        IF (CPLAN) FM(3,3) = SQRT(ABS(2.D0*EPSM(3)+1.D0))
        CALL MATINV('S',3,FM,FMM,DETF)
        CALL VECINI(6,0.D0,SIGN)
        DO 170 I=1,2*NDIM
          DO 171 L=1,2*NDIM
            FTF = ( FMM(INDI(I),INDI(L)) * FMM(INDJ(I),INDJ(L))
     &           +   FMM(INDI(I),INDJ(L)) * FMM(INDJ(I),INDI(L)))
     &           *   RIND1(L)
            SIGN(I) = SIGN(I) + FTF * SIGM(L,KPG)
 171      CONTINUE
          SIGN(I) = SIGN(I) * DETF
 170    CONTINUE
        IF (NDIM.EQ.2) SIGN(4) = SIGN(4) * RAC2
        IF (NDIM.EQ.3) THEN
          DO 180 M=4,2*NDIM
            SIGN(M) = SIGM(M,KPG) * RAC2
 180      CONTINUE
        ENDIF

C
C       INTEGRATION
C
        CALL R8INIR(6,0.0D0,SIGMA,1)
        CALL NMCOMP('XFEM',IDECPG+KPG,1,NDIM,TYPMOD,IMATE,COMPOR,CRIT,
     &              INSTAM,INSTAP,6,EPSM,DEPS,6,SIGN,VI(1,KPG),OPTION,
     &              ANGMAS,10,ELGEOM(1,KPG),SIGMA,VIP(1,KPG),36,DSIDEP,
     &              1,RBID,CODRET)

C - CALCUL DE LA MATRICE DE RIGIDITE
        IF (RIGI) THEN
C
C          RIGIDITÉ GEOMETRIQUE
          DO 190 N=1,NNOP
            NN=DEC(N)

            DO 191 M=1,N
              MN=DEC(M)
C
              DO 192 I=1,DDLDN
                DO 193 J=1,DDLDN
                  TMP1 = 0.D0
                  IF (OPTION(1:4).EQ.'RIGI') THEN
                    TMP1 = SIGN(1)*PFF(I,N,1)*PFF(J,M,1)
     &                    + SIGN(2)*PFF(I,N,2)*PFF(J,M,2)
     &                    + SIGN(4)*(PFF(I,N,1)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,1))/RAC2
                    IF (NDIM.EQ.3) THEN
                      TMP1 = TMP1 + SIGN(3)*PFF(I,N,3)*PFF(J,M,3)
     &                            + SIGN(5)*(PFF(I,N,1)*PFF(J,M,3)
     &                               +PFF(I,N,3)*PFF(J,M,1))/RAC2
     &                            + SIGN(6)*(PFF(I,N,3)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,3))/RAC2
                    ENDIF
                  ELSE
                    TMP1 = SIGMA(1)*PFF(I,N,1)*PFF(J,M,1)
     &                    + SIGMA(2)*PFF(I,N,2)*PFF(J,M,2)
     &                    + SIGMA(4)*(PFF(I,N,1)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,1))/RAC2
                    IF (NDIM.EQ.3) THEN
                      TMP1 = TMP1 + SIGMA(3)*PFF(I,N,3)*PFF(J,M,3)
     &                            + SIGMA(5)*(PFF(I,N,1)*PFF(J,M,3)
     &                               +PFF(I,N,3)*PFF(J,M,1))/RAC2
     &                            + SIGMA(6)*(PFF(I,N,3)*PFF(J,M,2)
     &                               +PFF(I,N,2)*PFF(J,M,3))/RAC2
                    ENDIF
                  ENDIF
C                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = DDLDN
                  ENDIF
                  IF (J.LE.J1) THEN
                    DO 194 L = 1,NDIM
                      KKD = (NN+(I-1)*NDIM+L-1) * (NN+(I-1)*NDIM+L) /2

                      KK = KKD + MN+(J-1)*NDIM+L

                      MATUU(KK) = MATUU(KK) + TMP1*JAC
 194                CONTINUE
                  ENDIF
 193            CONTINUE
 192          CONTINUE
 191        CONTINUE
 190      CONTINUE
C         RIGIDITE ELASTIQUE
          DO 200 N=1,NNOP
            NN=DEC(N)

            DO 201 I=1,DDLD
              DO 202 L=1,2*NDIM
                SIG(L) = 0.D0
                DO 203 K=1,2*NDIM
                  SIG(L) = SIG(L) + DEF(K,N,I) * DSIDEP(K,L)
 203            CONTINUE
 202          CONTINUE
              DO 204 J=1,DDLD
                DO 205 M=1,N
                  TMP2 = 0.D0
                  MN=DEC(M)
                  DO 206 K=1,2*NDIM
                    TMP2 = TMP2 + SIG(K) * DEF(K,M,J)
 206              CONTINUE

C                STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = DDLD
                  ENDIF
                  IF (J.LE.J1) THEN
                    KKD = (NN+I-1) * (NN+I) /2
                    MATUU(KKD+MN+J) = MATUU(KKD+MN+J) + TMP2*JAC
                  END IF
 205            CONTINUE
 204          CONTINUE
 201        CONTINUE
 200      CONTINUE
        ENDIF
C
C - CALCUL DE LA FORCE INTERIEURE
C
        IF (RESI) THEN
C
          DO 210 N=1,NNOP
            NN=DEC(N)
            DO 211 I=1,DDLD
              DO 212 L=1,2*NDIM
              ZR(IVECTU-1+NN+I)=
     &                       ZR(IVECTU-1+NN+I) + DEF(L,N,I)*SIGMA(L)*JAC
 212          CONTINUE
 211        CONTINUE
 210      CONTINUE
C
C    CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION LAGRANGE -> CAUCHY
C
          IF (CPLAN) F(3,3) = SQRT(ABS(2.D0*EPSP(3)+1.D0))
          DETF = F(3,3) * (F(1,1)*F(2,2)-F(1,2)*F(2,1))
          IF (NDIM.EQ.3) THEN
            DETF = DETF - F(2,3)*(F(1,1)*F(3,2)-F(3,1)*F(1,2))
     &                  + F(1,3)*(F(2,1)*F(3,2)-F(3,1)*F(2,2))
          ENDIF
          DO 220 I = 1,2*NDIM
            SIGP(I,KPG) = 0.D0
            DO 221 L = 1,2*NDIM
              FTF = (F(INDI(I),INDI(L))*F(INDJ(I),INDJ(L)) +
     &               F(INDI(I),INDJ(L))*F(INDJ(I),INDI(L)))*RIND(L)
              SIGP(I,KPG) = SIGP(I,KPG) + FTF*SIGMA(L)
 221        CONTINUE
            SIGP(I,KPG) = SIGP(I,KPG)/DETF
 220      CONTINUE
        ENDIF

 1000 CONTINUE
      END
