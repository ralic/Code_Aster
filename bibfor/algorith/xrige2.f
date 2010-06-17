      SUBROUTINE  XRIGE2(ELREFP,ELRESE,NDIM,COORSE,IGEOM,HE,DDLH,
     &                   DDLC,DDLM,NFE,BASLOC,NNOP,NPG,TYPMOD,
     &                   LGPG,LSN,LST,SIG,IDECPG,MATUU,CODRET)

      IMPLICIT NONE
      INTEGER       NDIM,IGEOM,LGPG,CODRET,NNOP,NPG,DDLH,DDLC,NFE
      INTEGER       IDECPG,DDLM
      CHARACTER*8   ELREFP,TYPMOD(*)
      CHARACTER*8   ELRESE      
      CHARACTER*24  COORSE
      REAL*8        BASLOC(6*NNOP),HE
      REAL*8        LSN(NNOP),LST(NNOP),SIG(48),MATUU(*)



C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2010   AUTEUR CARON A.CARON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_21

C.......................................................................
C
C     BUT:  CALCUL  DE l'OPTION RIGI_MECA_GE AVEC X-FEM EN 2D
C.......................................................................
C IN  ELREFP  : ELEMENT DE REFERENCE PARENT
C IN  NDIM    : DIMENSION DE L'ESPACE
C IN  COORSE  : COORDONNEES DES SOMMETS DU SOUS-ELEMENT
C IN  IGEOM   : COORDONNEES DES NOEUDS DE L'ELEMENT PARENT
C IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ELT
C IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
C IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
C IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
C IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE AUX NOEUDS
C IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
C IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ELEMENT
C IN  TYPMOD  : TYPE DE MODELISATION
C IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
C               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
C IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
C IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
C IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
C               DU SOUS ELEMENT COURRANT (EN FAIT IDECPG+1)
C IN SIG     : CONTRAINTES DE CAUCHY
C OUT MATUU   : MATRICE DE MASSE PROFIL 
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


      INTEGER  KPG,KK,N,I,M,J,J1,KKD,INO,IG,IRET
      INTEGER  NNO,NNOS,NPGBIS,DDLS,DDLD,CPT,CPT1,NDIMB,IPG
      INTEGER  JCOOPG,JDFD2,JGANO,JCOORS,IDFDE,IVF,IPOIDS
      REAL*8   F(3,3),EPS(6)
      REAL*8   POIDS,TEMP,TMP1,FE(4),BASLOG(6)
      REAL*8   XG(NDIM),XE(NDIM),FF(NNOP),JAC,LSNG,LSTG
      REAL*8   RBID1(4),RBID2(4)
      REAL*8   DFDI(NNOP,NDIM),PFF(24,NNOP,NNOP),DGDGL(4,3)
      REAL*8   ENR(NNOP,NDIM+DDLH+NDIM*NFE),GRAD(3,3)
      REAL*8   RAC2,RHO
      INTEGER  DDLN,DDLMM,NNON,NNOM,INDENN,INDENM,NNOPS

      INTEGER  IBID
      REAL*8   RBID
      DATA     RAC2 / 1.4142135623731D0 /


C--------------------------------------------------------------------

C     NOMBRE DE DDL DE DEPLACEMENT À CHAQUE NOEUD SOMMET
      DDLD=NDIM+DDLH+NDIM*NFE

C     NOMBRE DE DDL TOTAL (DEPL+CONTACT) À CHAQUE NOEUD SOMMET
      DDLS=DDLD+DDLC

C     ELEMENT DE REFERENCE PARENT : RECUP DE NNOPS
      CALL ELREF4(' ','RIGI',IBID,IBID,NNOPS,IBID,IBID,IBID,IBID,IBID)

C     ADRESSE DES COORD DU SOUS ELT EN QUESTION
      CALL JEVEUO(COORSE,'L',JCOORS)
C
      CALL ELREF5(ELRESE,'XINT',NDIMB,NNO,NNOS,NPGBIS,IPOIDS,JCOOPG,IVF,
     &                  IDFDE,JDFD2,JGANO)
      CALL ASSERT(NPG.EQ.NPGBIS.AND.NDIM.EQ.NDIMB)


C - CALCUL POUR CHAQUE POINT DE GAUSS
      DO 10 KPG=1,NPG

C       COORDONNEES DU PT DE GAUSS DANS LE REPERE REEL : XG
        CALL VECINI(NDIM,0.D0,XG)
        DO 110 I=1,NDIM
          DO 111 N=1,NNO
          XG(I)=XG(I)+ZR(IVF-1+NNO*(KPG-1)+N)*ZR(JCOORS-1+NDIM*(N-1)+I)
 111      CONTINUE
 110    CONTINUE
 
C       JUSTE POUR CALCULER LES FF
        CALL REEREF(ELREFP,NNOP,NNOPS,IGEOM,XG,IBID,.FALSE.,NDIM,HE,
     &       DDLH,NFE,DDLS,DDLM,FE,DGDGL,'NON',XE,FF,DFDI,RBID,RBID,
     &       RBID)

        IF (NFE.GT.0) THEN
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
C       ET CALCUL DE FF, DFDI, ET EPS

        CALL REEREF(ELREFP,NNOP,NNOPS,IGEOM,XG,IBID,.FALSE.,NDIM,HE,
     &       DDLH,NFE,DDLS,DDLM,FE,DGDGL,'DFF',XE,FF,DFDI,RBID,RBID,
     &       RBID)


C       POUR CALCULER LE JACOBIEN DE LA TRANSFO SSTET->SSTET REF
C       ON ENVOIE DFDM2D AVEC LES COORD DU SS-ELT
        CALL DFDM2D(NNO,KPG,IPOIDS,IDFDE,ZR(JCOORS),
     &                                   RBID1,RBID2,JAC)

C--------CALCUL DES FONCTIONS ENRICHIES--------------------------

        DO 126 N=1,NNOP
          CPT=0
          DO 127 M=1,N
C----------LA PARTIE CORRESPONDANTE AU FF CLASSIQUES
              PFF(1,N,M) =  DFDI(N,1)*DFDI(M,1)
              PFF(2,N,M) =  DFDI(N,2)*DFDI(M,2)
              PFF(3,N,M) = 0.D0
              PFF(4,N,M) =(DFDI(N,1)*DFDI(M,2)+DFDI(N,2)*DFDI(M,1))/RAC2
 
C----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT HEAVISIDE
              CPT=4
              IF(DDLH.NE.0) THEN
                PFF(CPT+1,N,M) =  DFDI(N,1)*DFDI(M,1)
                PFF(CPT+2,N,M) =  DFDI(N,2)*DFDI(M,2)
                PFF(CPT+3,N,M) =  0.D0             
                PFF(CPT+4,N,M) =(DFDI(N,1)*DFDI(M,2)+
     &                               DFDI(N,2)*DFDI(M,1))/RAC2
   
                CPT=8
              ENDIF
C----------LA PARTIE CORRESPONDANTE A L'ENRICHEMENT CRACK-TIP
              DO 128 IG=1,NFE

                PFF(CPT+4*(IG-1)+1,N,M) =  (DFDI(N,1)*FE(IG)+
     &                            FF(N)*DGDGL(IG,1))*(DFDI(M,1)*FE(IG)+
     &                                           FF(M)*DGDGL(IG,1))
                PFF(CPT+4*(IG-1)+2,N,M) =  (DFDI(N,2)*FE(IG)+
     &                            FF(N)*DGDGL(IG,2))*(DFDI(M,2)*FE(IG)+
     &                                           FF(M)*DGDGL(IG,2))
                PFF(CPT+4*(IG-1)+3,N,M) =0.D0  

                PFF(CPT+4*(IG-1)+4,N,M) =((DFDI(N,1)*FE(IG)+
     &                            FF(N)*DGDGL(IG,1))*(DFDI(M,2)*FE(IG)+
     &                                         FF(M)*DGDGL(IG,2))+
     &                                    (DFDI(N,2)*FE(IG)+
     &                            FF(N)*DGDGL(IG,2))*(DFDI(M,1)*FE(IG)+
     &                                         FF(M)*DGDGL(IG,1)))/RAC2
  

 128         CONTINUE
 127      CONTINUE
 126    CONTINUE

        DO 230 N=1,NNOP
          CPT1=0
          IF (N.LE.NNOPS) THEN
            NNON=0
            DDLN=DDLS
          ELSEIF (N.GT.NNOPS) THEN
            NNON=NNOPS
            DDLN=DDLM
          ENDIF
          INDENN = DDLS*NNON+DDLN*(N-NNON-1)

          DO 231 I=1,DDLD
            KKD = (INDENN+I-1) * (INDENN+I) /2
            DO 240 J=1,DDLD
              DO 241 M=1,N
                IF (M.LE.NNOPS) THEN
                  NNOM=0
                  DDLMM=DDLS
                ELSEIF (M.GT.NNOPS) THEN
                  NNOM=NNOPS
                  DDLMM=DDLM
                ENDIF
                INDENM = DDLS*NNOM+DDLMM*(M-NNOM-1)

                IF (M.EQ.N) THEN
                  J1 = I
                ELSE
                  J1 = DDLD
                ENDIF
                TMP1 = 0.D0

                TMP1 = TMP1+PFF(1,N,M)*SIG((KPG-1)*4+1)
                TMP1 = TMP1+PFF(2,N,M)*SIG((KPG-1)*4+2)
                TMP1 = TMP1+PFF(3,N,M)*SIG((KPG-1)*4+3)
                TMP1 = TMP1+PFF(4,N,M)*SIG((KPG-1)*4+4)
                CPT1=4
                IF(DDLH.NE.0) THEN
                  TMP1 = TMP1+PFF(CPT1+1,N,M)*SIG((KPG-1)*4+1)
                  TMP1 = TMP1+PFF(CPT1+2,N,M)*SIG((KPG-1)*4+2)
                  TMP1 = TMP1+PFF(CPT1+3,N,M)*SIG((KPG-1)*4+3)
                  TMP1 = TMP1+PFF(CPT1+4,N,M)*SIG((KPG-1)*4+4)
                CPT1=8
                ENDIF
                DO 242 IG=1,NFE
                  TMP1 = TMP1+PFF(CPT1+4*(IG-1)+1,N,M)*SIG((KPG-1)*4+1) 
                  TMP1 = TMP1+PFF(CPT1+4*(IG-1)+2,N,M)*SIG((KPG-1)*4+2)
                  TMP1 = TMP1+PFF(CPT1+4*(IG-1)+3,N,M)*SIG((KPG-1)*4+3)
                  TMP1 = TMP1+PFF(CPT1+4*(IG-1)+4,N,M)*SIG((KPG-1)*4+4) 

 242            CONTINUE
C
                IF (J.LE.J1) THEN
                  KK = KKD + INDENM + J
                  MATUU(KK)= MATUU(KK)+TMP1*JAC
                END IF
C  
 241          CONTINUE
 240        CONTINUE
 231      CONTINUE
 230    CONTINUE

 
 
 10   CONTINUE

      END
