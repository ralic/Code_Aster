      SUBROUTINE  XXNMEL(POUM,ELREFP,ELRESE,NDIM,COORSE,IGEOM,
     &                   HE,NFH,DDLC,DDLM,NNOPS,NFE,
     &                   BASLOC,NNOP,NPG,TYPMOD,OPTION,IMATE,COMPOR,
     &                   LGPG,CRIT,IDEPL,LSN,LST,IDECPG,SIG,VI,
     &                   MATUU,IVECTU,CODRET,NFISS,FISNO)

      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER       CODRET,DDLC,DDLM,FISNO(NNOP,NFISS)
      INTEGER       IDECPG,IDEPL,IGEOM,IMATE,IVECTU,NNOPS
      INTEGER       LGPG,NDIM,NFE,NFH,NFISS,NNOP,NPG
      REAL*8        BASLOC(3*NDIM*NNOP),COORSE(*),CRIT(3),HE(NFISS)
      REAL*8        LSN(NNOP),LST(NNOP),SIG(2*NDIM,NPG)
      REAL*8        MATUU(*),VI(LGPG,NPG)
      CHARACTER*(*) POUM
      CHARACTER*8   ELREFP,ELRESE,TYPMOD(*)
      CHARACTER*16  OPTION,COMPOR(4)

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
C RESPONSABLE GENIAUT S.GENIAUT
C TOLE CRP_21 CRP_20 CRS_1404
C
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN HYPER-ELASTICITE AVEC X-FEM EN 2D ET EN 3D
C.......................................................................
C
C IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
C IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
C IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
C IN  NFH     : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
C IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
C IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
C IN  NNOPS   : NOMBRE DE NOEUDS SOMMET ELEMENTS PARENT
C IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
C IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
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
C IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
C               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)

C OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
C OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT IVECTU  : VECTEUR FORCES NODALES (RAPH_MECA ET FULL_MECA)
C......................................................................
C
      CHARACTER*16  COMPO2(4)
      INTEGER  KPG,I,IG,N,NN,M,MN,J,J1,KL,L,KKD,IPG,IRET
      INTEGER  DDLD,DDLS,NNO,NNOS,NPGBIS,CPT,NDIMB,DEC(NNOP)
      INTEGER  IBID,IDFDE,IPOIDS,IVF,JCOOPG,JDFD2,JGANO
      REAL*8   DSIDEP(6,6),EPS(6),SIGMA(6),FTF,DETF
      REAL*8   TMP1,TMP2,SIGP(6,3*(1+NFE+NFH)),RBID33(3,3)
      REAL*8   XG(NDIM),XE(NDIM),FF(NNOP),JAC,LSNG,LSTG,R8BID
      REAL*8   RBID,RBID4(4),RB4(4),RBID10(10),RB10(10),RBD4(4)
      REAL*8   DFDI(NNOP,NDIM),F(3,3),FE(4),BASLOG(3*NDIM)
      REAL*8   DGDGL(4,3),PFF(6,NNOP,NNOP)
      REAL*8   DEF(6,NDIM*(1+NFH+NFE),NNOP)
      REAL*8   UR,R
      LOGICAL  GRDEPL,AXI,CPLAN
C
      INTEGER INDI(6),INDJ(6)
      REAL*8  RIND(6),RAC2,ANGMAS(3)
      DATA    INDI / 1 , 2 , 3 , 1 , 1 , 2 /
      DATA    INDJ / 1 , 2 , 3 , 2 , 3 , 3 /
      DATA    RIND / 0.5D0,0.5D0,0.5D0,0.70710678118655D0,
     &               0.70710678118655D0,0.70710678118655D0 /
      DATA    RAC2 / 1.4142135623731D0 /
      DATA    ANGMAS /0.D0, 0.D0, 0.D0/
C--------------------------------------------------------------------
C
C     ATTENTION, EN 3D, ZR(IDEPL) ET ZR(VECTU) SONT DIMENSIONNÉS DE
C     TELLE SORTE QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES
C     NOEUDS MILIEU
C
C     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
      DDLD = NDIM*(1+NFH+NFE)
C
C     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
      DDLS = DDLD+DDLC
C
C - INITIALISATION
      GRDEPL = COMPOR(3) .EQ. 'GROT_GDEP'
      AXI    = TYPMOD(1) .EQ. 'AXIS'
      CPLAN  = TYPMOD(1) .EQ. 'C_PLAN'
C
      IF (GRDEPL) THEN
        CALL U2MESS('F','XFEM2_2')
      ENDIF
C
C     ADRESSE DES COORD DU SOUS ELT EN QUESTION
      CALL ELREF5(ELRESE,'XINT',NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,
     &            IDFDE,JDFD2,JGANO)

      CALL ASSERT(NPG.EQ.NPGBIS.AND.NDIM.EQ.NDIMB)
C
C DECALAGES CALCULES EN AMONT: PERF
C
        DO 179 N=1,NNOP
          CALL INDENT(N,DDLS,DDLM,NNOPS,DEC(N))
179     CONTINUE
C
C-----------------------------------------------------------------------
C     BOUCLE SUR LES POINTS DE GAUSS
      DO 1000 KPG=1,NPG

C       COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
        CALL VECINI(NDIM,0.D0,XG)
        DO 100 I=1,NDIM
          DO 101 N=1,NNO
           XG(I) = XG(I) + ZR(IVF-1+NNO*(KPG-1)+N)*COORSE(NDIM*(N-1)+I)
 101      CONTINUE
 100    CONTINUE
C
C       JUSTE POUR CALCULER LES FF
C
        CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPL,GRDEPL,
     &              NDIM,HE,RBID,R8BID,FISNO,NFISS,NFH,NFE,DDLS,DDLM,
     &              FE,DGDGL,'NON',XE,FF,DFDI,F,EPS,RBID33)
C

        IF (NFE.GT.0) THEN
C         BASE LOCALE ET LEVEL SETS AU POINT DE GAUSS
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
            CALL XCALF2(HE(1),LSNG,LSTG,BASLOG,FE,DGDGL,IRET)
          ELSEIF (NDIM.EQ.3) THEN
            CALL XCALFE(HE,LSNG,LSTG,BASLOG,FE,DGDGL,IRET)
          ENDIF

C         PB DE CALCUL DES DERIVEES DES FONCTIONS SINGULIERES
C         CAR ON SE TROUVE SUR LE FOND DE FISSURE
          CALL ASSERT(IRET.NE.0)
C
        ENDIF
C
C -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE) ET DU DEPL. RADIAL
        IF (AXI) THEN
          R  = 0.D0
          UR = 0.D0
          DO 120 N=1,NNOP
            R  = R  + FF(N)*ZR(IGEOM-1+2*(N-1)+1)
            UR = UR + FF(N)*ZR(IDEPL-1+DDLS*(N-1)+1)
            DO 121 IG=1,NFH
              UR = UR + FF(N)
     &                     *ZR(IDEPL-1+DDLS*(N-1)+NDIM*IG+1)
     &                     *HE(FISNO(N,IG))

 121        CONTINUE
            DO 122 IG=1,NFE
              UR = UR + FF(N)
     &                     *ZR(IDEPL-1+DDLS*(N-1)+NDIM*(NFH+IG)+1)
     &                     *FE(IG)

 122        CONTINUE

 120      CONTINUE

          CALL ASSERT(R.GT.0D0)
C          ATTENTION : LE POIDS N'EST PAS X R
C          CE SERA FAIT PLUS TARD AVEC JAC = JAC X R
        ENDIF

C       COORDONNÉES DU POINT DE GAUSS DANS L'ÉLÉMENT DE RÉF PARENT : XE
C       ET CALCUL DE FF, DFDI, ET EPS
        IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'
     &  .OR. OPTION(1: 9) .EQ. 'RAPH_MECA') THEN
          CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPL,GRDEPL,
     &                NDIM,HE,R,UR,FISNO,NFISS,NFH,NFE,DDLS,DDLM,
     &                FE,DGDGL,'OUI',XE,FF,DFDI,F,EPS,RBID33)

C       SI OPTION 'RIGI_MECA', ON INITIALISE À 0 LES DEPL
        ELSEIF ( OPTION .EQ. 'RIGI_MECA') THEN
          CALL REEREF(ELREFP,AXI,NNOP,NNOPS,ZR(IGEOM),XG,IDEPL,
     &                .FALSE.,NDIM,HE,R,UR,FISNO,NFISS,NFH,NFE,DDLS,
     &                DDLM,FE,DGDGL,'INI',XE,FF,DFDI,F,EPS,RBID33)
        ENDIF

C - CALCUL DES ELEMENTS GEOMETRIQUES

C
C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 140 N=1,NNOP
          CPT = 0
C         FONCTIONS DE FORME CLASSIQUES
          DO 141 I=1,NDIM
            CPT = CPT+1
            DEF(1,I,N) =  F(I,1)*DFDI(N,1)
            DEF(2,I,N) =  F(I,2)*DFDI(N,2)
            DEF(3,I,N) =  0.D0
            DEF(4,I,N) = (F(I,1)*DFDI(N,2) + F(I,2)*DFDI(N,1))/RAC2
            IF (NDIM.EQ.3) THEN
              DEF(3,I,N) =  F(I,3)*DFDI(N,3)
              DEF(5,I,N) = (F(I,1)*DFDI(N,3) + F(I,3)*DFDI(N,1))/RAC2
              DEF(6,I,N) = (F(I,2)*DFDI(N,3) + F(I,3)*DFDI(N,2))/RAC2
            ENDIF
 141      CONTINUE

C         TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
          IF (AXI) THEN
            DEF(3,1,N) = F(3,3) * FF(N)/R
          ENDIF

C         ENRICHISSEMENT PAR HEAVYSIDE
          DO 142 IG=1,NFH
            DO 143 I=1,NDIM
              CPT = CPT+1
              DO 144 M=1,2*NDIM
                DEF(M,CPT,N) =  DEF(M,I,N) * HE(FISNO(N,IG))
 144          CONTINUE
              IF (NDIM.EQ.2) THEN
                DEF(3,CPT,N) =  0.D0
              ENDIF
 143        CONTINUE

C   TERME DE CORRECTION (3,3) A PORTE SUR LE DDL 1+NDIM*IG
            IF (AXI) THEN
              DEF(3,1+NDIM*IG,N) = F(3,3) * FF(N)/R * HE(FISNO(N,IG))
            ENDIF

 142      CONTINUE
C
C         ENRICHISSEMENT PAR LES NFE FONTIONS SINGULIÈRES
          DO 145 IG=1,NFE
            DO 146 I=1,NDIM
              CPT=CPT+1
              DEF(1,CPT,N) =
     &           F(I,1)* (DFDI(N,1) * FE(IG) + FF(N)*DGDGL(IG,1))

              DEF(2,CPT,N) =
     &           F(I,2)* (DFDI(N,2) * FE(IG) + FF(N)*DGDGL(IG,2))

              DEF(3,CPT,N) =  0.D0

              DEF(4,CPT,N) =
     &         ( F(I,1)* (DFDI(N,2)*FE(IG)+FF(N)*DGDGL(IG,2))
     &         + F(I,2)* (DFDI(N,1)*FE(IG)+FF(N)*DGDGL(IG,1)) )/RAC2

              IF (NDIM.EQ.3) THEN
                DEF(3,CPT,N) =
     &             F(I,3)* (DFDI(N,3) * FE(IG) + FF(N)*DGDGL(IG,3))
                DEF(5,CPT,N) =
     &           ( F(I,1)* (DFDI(N,3)*FE(IG)+FF(N)*DGDGL(IG,3))
     &           + F(I,3)* (DFDI(N,1)*FE(IG)+FF(N)*DGDGL(IG,1)) )/RAC2
                DEF(6,CPT,N) =
     &           ( F(I,3)* (DFDI(N,2)*FE(IG)+FF(N)*DGDGL(IG,2))
     &           + F(I,2)* (DFDI(N,3)*FE(IG)+FF(N)*DGDGL(IG,3)) )/RAC2
              ENDIF
 146        CONTINUE

C   TERME DE CORRECTION (3,3) AXI PORTE SUR LE DDL 1+NDIM*(NFH+IG)
            IF (AXI) THEN
              DEF(3,1+NDIM*(NFH+IG),N) = F(3,3) * FF(N)/R * FE(IG)
            ENDIF

 145      CONTINUE

          CALL ASSERT(CPT.EQ.DDLD)

 140    CONTINUE
C
C       CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
C       AVEC LES COORDONNEES DU SOUS-ELEMENT
        IF (NDIM.EQ.2) THEN
          CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,COORSE,RBID10,RB10,JAC)
        ELSEIF (NDIM.EQ.3) THEN
          CALL DFDM3D(NNO,KPG,IPOIDS,IDFDE,COORSE,RBID4,RB4,RBD4,JAC)
        ENDIF

C       MODIFICATION DU JACOBIEN SI AXI
        IF (AXI) THEN
          JAC = JAC * R
        ENDIF
C
C      TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
C        IF (AXI) THEN
C          DO 150 N=1,NNOP
C            DEF(3,N,1) = F(3,3)* ZR(IVF+N+(KPG-1)*NNO-1)/R
C 150      CONTINUE
C        ENDIF
C
C       CALCUL DES PRODUITS DE FONCTIONS DE FORMES (ET DERIVEES)
        IF ( (OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR.  OPTION(1: 9) .EQ. 'FULL_MECA'    ) .AND. GRDEPL) THEN
          DO 160 N=1,NNOP
            DO 161 M=1,N
              PFF(1,M,N) = DFDI(N,1)*DFDI(M,1)
              PFF(2,M,N) = DFDI(N,2)*DFDI(M,2)
              PFF(3,M,N) = 0.D0
              PFF(4,M,N) =(DFDI(N,1)*DFDI(M,2)+DFDI(N,2)*DFDI(M,1))/RAC2
              IF (NDIM.EQ.3) THEN
               PFF(3,M,N)= DFDI(N,3)*DFDI(M,3)
               PFF(5,M,N)=(DFDI(N,1)*DFDI(M,3)+DFDI(N,3)*DFDI(M,1))/RAC2
               PFF(6,M,N)=(DFDI(N,2)*DFDI(M,3)+DFDI(N,3)*DFDI(M,2))/RAC2
              ENDIF
 161        CONTINUE
 160      CONTINUE
        ENDIF

C - CALCUL DE LA MATRICE DE RIGIDITE POUR L'OPTION RIGI_MECA

C
        IF ( OPTION .EQ. 'RIGI_MECA') THEN
C
C -       LOI DE COMPORTEMENT : ON VA OBTENIR ICI LA MATRICE DE HOOKE
C         POUR LE CAS ELASTIQUE ISOTROPE - DEFO/CONTR PLANES OU 3D
          IPG= IDECPG + KPG
          COMPO2(1)='ELAS'
          COMPO2(2)=' '
          COMPO2(3)=' '
          COMPO2(4)=' '

          CALL NMCPEL('XFEM',IPG,1,POUM,NDIM,TYPMOD,ANGMAS,IMATE,COMPO2,
     &                CRIT,OPTION,EPS,SIGMA,VI(1,KPG),DSIDEP,CODRET)
C
          DO 170 N=1,NNOP
            NN=DEC(N)
            DO 178 I=1,DDLD
            DO 172 KL=1,2*NDIM
               SIGP(KL,I) = 0.D0
               DO 173 L=1,2*NDIM
                  SIGP(KL,I) = SIGP(KL,I) + DEF(L,I,N)*DSIDEP(L,KL)
 173           CONTINUE
 172        CONTINUE
 178        CONTINUE
C
            DO 175 M=1,N
               MN=DEC(M)
               DO 171 I=1,DDLD
                 KKD = (NN+I-1) * (NN+I) /2
                 IF (M.EQ.N) THEN
                   J1 = I
                 ELSE
                   J1 = DDLD
                 ENDIF
C
                 DO 174 J=1,DDLD
C
C                 RIGIDITE ELASTIQUE
                   TMP2 = 0.D0
                   DO 176 L=1,2*NDIM
                      TMP2 = TMP2 + SIGP(L,I)*DEF(L,J,M)
 176               CONTINUE
C
C                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (J.LE.J1) THEN
                    MATUU(KKD+MN+J) = MATUU(KKD+MN+J) + TMP2*JAC
                  END IF
C
 174              CONTINUE
 171            CONTINUE
 175          CONTINUE
 170      CONTINUE
        GOTO 9999
        ENDIF
C
C - LOI DE COMPORTEMENT : CALCUL DE S(E) ET DS/DE À PARTIR DE EPS
C                       {XX YY ZZ SQRT(2)*XY SQRT(2)*XZ SQRT(2)*YZ}

C       POUR LES VARIABLES DE COMMANDES (TEMP...), IL EST NECESSSAIRE
C       DE DONNER LA POSITION DU POINT DE GAUSS COURRANT DANS LA
C       FAMILLE 'XFEM'
        IPG = IDECPG + KPG
        CALL NMCPEL('XFEM',IPG,1,POUM,NDIM,TYPMOD,ANGMAS,IMATE,COMPOR,
     &               CRIT,OPTION,EPS,SIGMA,VI(1,KPG),DSIDEP,CODRET)


C - CALCUL DE LA MATRICE DE RIGIDITE POUR LES OPTIONS RIGI_MECA_TANG
C   ET FULL_MECA

        IF ( OPTION(1:10) .EQ. 'RIGI_MECA_'
     &  .OR. OPTION(1: 9) .EQ. 'FULL_MECA'    ) THEN
C
          DO 180 N=1,NNOP
            NN=DEC(N)
C
            DO 181 I=1,DDLD
              KKD = (NN+I-1) * (NN+I) /2
              DO 182 KL=1,2*NDIM
                SIGP(KL,I) = 0.D0
                DO 183 L=1,2*NDIM
                  SIGP(KL,I) = SIGP(KL,I) + DEF(L,I,N)*DSIDEP(L,KL)
183             CONTINUE
182           CONTINUE
              DO 184 J=1,DDLD
                DO 185 M=1,N
                  MN=DEC(M)
C
                  IF (M.EQ.N) THEN
                    J1 = I
                  ELSE
                    J1 = DDLD
                  ENDIF
C
C                 RIGIDITE GEOMETRIQUE
                  TMP1 = 0.D0
                  IF (GRDEPL .AND. I.EQ.J) THEN
                    TMP1 = 0.D0
                    DO 186 L=1,2*NDIM
                      TMP1 = TMP1 + PFF(L,M,N)*SIGMA(L)
186                 CONTINUE
C
C                  TERME DE CORRECTION AXISYMETRIQUE
C                    IF (AXI .AND. I.EQ.1) THEN
C                      TMP1 = TMP1 + ZR(IVF+N+(KPG-1)*NNO-1) *
C     &                      ZR(IVF+M+(KPG-1)*NNO-1)/(R*R) * SIGMA(3)
C                    END IF
                  ENDIF
C
C                 RIGIDITE ELASTIQUE
                  TMP2 = 0.D0
                  DO 187 L=1,2*NDIM
                    TMP2 = TMP2 + SIGP(L,I)*DEF(L,J,M)
187               CONTINUE
C
C                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                  IF (J.LE.J1) THEN
                    MATUU(KKD+MN+J) = MATUU(KKD+MN+J) + (TMP1+TMP2)*JAC
                  END IF
C
 185            CONTINUE
 184          CONTINUE
 181        CONTINUE
 180      CONTINUE
        ENDIF


C - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
C
        IF(OPTION(1:9).EQ.'FULL_MECA'.OR.
     &     OPTION(1:9).EQ.'RAPH_MECA') THEN
C
          DO 190 N=1,NNOP
            NN=DEC(N)
C
            DO 191 I=1,DDLD
              DO 192 M=1,2*NDIM
                ZR(IVECTU-1+NN+I)=
     &                       ZR(IVECTU-1+NN+I) + DEF(M,I,N)*SIGMA(M)*JAC
 192          CONTINUE
 191        CONTINUE
 190      CONTINUE
C
          IF (GRDEPL) THEN
C          CONVERSION LAGRANGE -> CAUCHY
            IF (CPLAN) F(3,3) = SQRT(ABS(2.D0*EPS(3)+1.D0))
            DETF = F(3,3) * (F(1,1)*F(2,2)-F(1,2)*F(2,1))
            IF (NDIM.EQ.3) THEN
              DETF = DETF - F(2,3)*(F(1,1)*F(3,2)-F(3,1)*F(1,2))
     &                    + F(1,3)*(F(2,1)*F(3,2)-F(3,1)*F(2,2))
            ENDIF
            DO 200 I=1,2*NDIM
              SIG(I,KPG) = 0.D0
              DO 210 L=1,2*NDIM
                FTF = (F(INDI(I),INDI(L))*F(INDJ(I),INDJ(L)) +
     &                 F(INDI(I),INDJ(L))*F(INDJ(I),INDI(L)))*RIND(L)
                SIG(I,KPG) = SIG(I,KPG) + FTF*SIGMA(L)
 210          CONTINUE
              SIG(I,KPG) = SIG(I,KPG)/DETF
 200        CONTINUE
          ELSE
C          SIMPLE CORRECTION DES CONTRAINTES
            DO 300 L=1,3
              SIG(L,KPG) = SIGMA(L)
 300        CONTINUE
            SIG(4,KPG) = SIGMA(4)/RAC2
            IF (NDIM.EQ.3) THEN
              SIG(5,KPG) = SIGMA(5)/RAC2
              SIG(6,KPG) = SIGMA(6)/RAC2
            ENDIF
          ENDIF
        ENDIF

 9999 CONTINUE
C
 1000 CONTINUE

      END
