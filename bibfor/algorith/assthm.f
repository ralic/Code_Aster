      SUBROUTINE  ASSTHM(NNO,NNOS,NNOM ,NPG,NPI, IPOIDS,IPOID2,
     &                   IVF,IVF2, IDFDE, IDFDE2,
     &                   GEOM,CRIT,DEPLM,DEPLP,
     &                   CONTM,CONTP,VARIM,VARIP,
     &                   DEFGEM,DEFGEP,
     &                   DRDS,DRDSR,DSDE,B,DFDI,DFDI2,R,SIGBAR,C,CK,CS,
     &                   MATUU,VECTU,RINSTM,RINSTP,
     &                   OPTION,IMATE,MECANI,PRESS1,PRESS2,TEMPE,
     &                   DIMDEF,DIMCON,DIMUEL,NBVARI,NDDLS,NDDLM,
     &                   NMEC,NP1,NP2,NDIM,COMPOR,
     &                   TYPMOD,AXI,PERMAN,MODINT,
     &                   CODRET)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C RESPONSABLE UFBHHLL C.CHAVANT
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
C ======================================================================
      IMPLICIT      NONE
C
      CHARACTER*6 NOMPRO
      PARAMETER (NOMPRO='ASSTHM')
C
      INTEGER       DIMMAT,NPG,IPOID2,IVF2,IDFDE2,DIMUEL,NNOM
      PARAMETER    (DIMMAT=120)
      INTEGER       NNO,NNOS,NPI,IPOIDS,IVF,IDFDE,IMATE,DIMDEF,DIMCON
      INTEGER       NBVARI,NDDLS,NDDLM,NMEC,NP1,NP2,NDIM,CODRET
      INTEGER       MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER       YAMEC,YAP1,YAP2,YATE
      INTEGER       ADDEME,ADDEP1,ADDEP2,ADDETE ,II,JJ
      INTEGER       KPI,IPI
      INTEGER       I,J,N,K,KJI
      REAL*8        DFDI(NNO,3),DFDI2(NNOS,3)
      REAL*8        GEOM(NDIM,NNO),CRIT(*),POIDS,POIDS2
      REAL*8        DEPLP(DIMUEL),DEPLM(DIMUEL)
      REAL*8        CONTM(DIMCON*NPI),CONTP(DIMCON*NPI)
      REAL*8        VARIM(NBVARI*NPI),VARIP(NBVARI*NPI)
      REAL*8        MATUU(DIMUEL*DIMUEL),MATRI(DIMMAT,DIMMAT)
      REAL*8        RINSTP,RINSTM,A(2),AS(2),AK(2),VECTU(DIMUEL)
      REAL*8        DEFGEM(DIMDEF),DEFGEP(DIMDEF)
      REAL*8        DRDS(DIMDEF+1,DIMCON),DRDSR(DIMDEF,DIMCON)
      REAL*8        DSDE(DIMCON,DIMDEF),B(DIMDEF,DIMUEL)
      REAL*8        R(DIMDEF+1),SIGBAR(DIMDEF),C(DIMDEF)
      REAL*8        DT,TA,TA1,RTHMC,R8PREM,CK(DIMDEF),CS(DIMDEF)
      LOGICAL       AXI, PERMAN
      CHARACTER*2   CODMES(1)
      CHARACTER*3   MODINT
      CHARACTER*8   TYPMOD(2)
      CHARACTER*16  OPTION,COMPOR(*),THMC,LOI
C
C =====================================================================
C.......................................................................
C
C     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
C           EN MECANIQUE DES MILIEUX POREUX AVEC COUPLAGE THM
C.......................................................................
C =====================================================================
C IN AXI       AXISYMETRIQUE?
C IN TYPMOD    MODELISATION (D_PLAN, AXI, 3D ?)
C IN MODINT    METHODE D'INTEGRATION (CLASSIQUE,LUMPEE(D),REDUITE(R) ?)
C IN NNO       NB DE NOEUDS DE L'ELEMENT
C IN NNOS      NB DE NOEUDS SOMMETS DE L'ELEMENT
C IN NNOM      NB DE NOEUDS MILIEUX DE L'ELEMENT
C IN NDDLS     NB DE DDL SUR LES SOMMETS
C IN NDDLM     NB DE DDL SUR LES MILIEUX
C IN NPI       NB DE POINTS D'INTEGRATION DE L'ELEMENT
C IN NPG       NB DE POINTS DE GAUSS     POUR CLASSIQUE(=NPI)
C                    SOMMETS             POUR LUMPEE   (=NPI=NNOS)
C                    POINTS DE GAUSS     POUR REDUITE  (<NPI)
C IN NDIM      DIMENSION DE L'ESPACE
C IN DIMUEL    NB DE DDL TOTAL DE L'ELEMENT
C IN DIMCON    DIMENSION DES CONTRAINTES GENERALISEES ELEMENTAIRES
C IN DIMDEF    DIMENSION DES DEFORMATIONS GENERALISEES ELEMENTAIRES
C IN IVF       FONCTIONS DE FORMES QUADRATIQUES
C IN IVF2      FONCTIONS DE FORMES LINEAIRES
C =====================================================================
C IN  DFDE    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
C IN  DFDN    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
C IN  DFDK    : DERIVEE DES FTION FORME QUAD ELEMENT DE REFERENCE
C IN  DFDE2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
C IN  DFDN2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
C IN  DFDK2   : DERIVEE DES FTION FORME LINE ELEMENT DE REFERENCE
C IN  GEOM    : COORDONNEES DES NOEUDS
C IN  OPTION  : OPTION DE CALCUL
C IN  IMATE   : MATERIAU CODE
C IN  COMPOR  : COMPORTEMENT
C IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX + THETA
C IN  DEPLP   : DEPLACEMENT A L INSTANT PLUS
C IN  DEPLM   : DEPLACEMENT A L INSTANT MOINS
C IN  RINSTM  : INSTANT PRECEDENT
C IN  RINSTP  : INSTANT COURANT
C IN  MECANI  : TABLEAU CONTENANT
C               YAMEC = MECA(1), YAMEC = 1 >> IL Y A UNE EQUATION MECANI
C               ADDEME = MECA(2), ADRESSE DANS LES TABLEAUX DES DEFORMAT
C               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
C               DEFORMATIONS CORRESPONDANT A LA MECANIQUE
C               ADCOME = MECA(3), ADRESSE DANS LES TABLEAUX DES CONTRAIN
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA MECANIQUE
C               NDEFME = MECA(4), NOMBRE DE DEFORMATIONS MECANIQUES
C               NCONME = MECA(5), NOMBRE DE CONTRAINTES MECANIQUES
C IN  PRESS1    : TABLEAU CONTENANT
C               YAP1 = PRESS1(1), YAP1 = 1 >> IL Y A UNE EQUATION DE PRE
C               NBPHA1=PRESS1(2) NOMBRE DE PHASES POUR LE CONSTITUANT 1
C               ADDEP1 = PRESS1(3), ADRESSE DANS LES TABLEAUX DES DEFORM
C               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
C               DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
C               ADCP11=PRESS1(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
C               PREMIER CONSTITUANT
C               ADCP12=PRESS1(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
C               PREMIER CONSTITUANT
C               NDEFP1 = PRESS1(6), NOMBRE DE DEFORMATIONS PRESSION 1
C               NCONP1 = PRESS1(7), NOMBRE DE CONTRAINTES POUR
C               CHAQUE PHASE DU CONSTITUANT 1
C IN  PRESS2    : TABLEAU CONTENANT
C               YAP2 = PRESS2(1), YAP2 = 1 >> IL Y A UNE EQUATION DE PRE
C               NBPHA1=PRESS2(2) NOMBRE DE PHASES POUR LE CONSTITUANT 1
C               ADDEP2 = PRESS2(3), ADRESSE DANS LES TABLEAUX DES DEFORM
C               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
C               DEFORMATIONS CORRESPONDANT A LA PREMIERE PRESSION
C               ADCP21=PRESS2(4), ADRESSE DANS LES TABLEAUX DES CONTRAIN
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA PREMIERE PHASE DU
C               SECOND CONSTITUANT
C               ADCP22=PRESS2(5), ADRESSE DANS LES TABLEAUX DES CONTRAIN
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA DEUXIEME PHASE DU
C               SECOND CONSTITUANT
C               NDEFP2 = PRESS2(6), NOMBRE DE DEFORMATIONS PRESSION 2
C               NCONP2 = PRESS2(7), NOMBRE DE CONTRAINTES POUR
C               CHAQUE PHASE DU CONSTITUANT 2
C
C IN  TEMPE    : TABLEAU CONTENANT
C               YATE = TEMPE(1), YAMEC = 1 >> IL Y A UNE EQUATION THERMI
C               ADDETE = TEMPE(2), ADRESSE DANS LES TABLEAUX DES DEFORMA
C               GENERALISEES AU POINT DE GAUSS DEFGEP ET DEFGEM DES
C               DEFORMATIONS CORRESPONDANT A LA THERMIQUE
C               ADCOTE = TEMPE(3), ADRESSE DANS LES TABLEAUX DES CONTRAI
C               GENERALISEES AU POINT DE GAUSS CONGEP ET CONGEM DES
C               CONTRAINTES CORRESPONDANT A LA THERMIQUE
C               NDEFTE = TEMPE(4), NOMBRE DE DEFORMATIONS THERMIQUES
C               NCONTE = TEMPE(5), NOMBRE DE CONTRAINTES THERMIQUES
C OUT CODRET  : CODE RETOUR LOIS DE COMPORTEMENT
C OUT DFDI    : DERIVEE DES FCT FORME
C OUT CONTP   : CONTRAINTES
C OUT VARIP   : VARIABLES INTERNES
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
C......................................................................
C
      IF(NDDLS*NNO.GT.DIMMAT) THEN
         CALL U2MESS('F','ALGORITH_33')
      ENDIF
C
      IF(DIMUEL.GT.DIMMAT) THEN
         CALL U2MESS('F','ALGORITH_33')
      ENDIF
C =====================================================================
C --- DETERMINATION DES VARIABLES CARACTERISANT LE MILIEU -------------
C =====================================================================
      YAMEC  = MECANI(1)
      ADDEME = MECANI(2)
      YAP1   = PRESS1(1)
      ADDEP1 = PRESS1(3)
      YAP2   = PRESS2(1)
      ADDEP2 = PRESS2(3)
      YATE   = TEMPE(1)
      ADDETE = TEMPE(2)
C =====================================================================
C --- CALCUL DE CONSTANTES TEMPORELLES --------------------------------
C =====================================================================
      DT = RINSTP-RINSTM
      TA= CRIT(4)
      TA1 = 1.D0-TA
C =====================================================================
C --- CREATION DES MATRICES DE SELECTION ------------------------------
C --- (MATRICES DIAGONALES) C,D,F,CS,DS,FS ----------------------------
C --- CREATION DE L'OPERATEUR A, AS -----------------------------------
C --- CES MATRICES SELECTIONNENT LES COMPOSANTES UTILES POUR ----------
C --- POUR CHAQUE TYPE DE POINT D'INTEGRATION -------------------------
C =====================================================================
C --- POUR LES METHODES CLASSIQUE ET LUMPEE ---------------------------
C --- A,C,D,F NE COMPORTENT QUE DES 1 ---------------------------------
C =====================================================================
C --- INITIALISATION --------------------------------------------------
C =====================================================================
      DO 201 I=1,DIMDEF
          C(I) = 1.D0
          CS(I) = 1.D0
 201  CONTINUE
      A(1)= 1.D0
      A(2)= 1.D0
      AS(1) = 1.D0
      AS(2) = 1.D0
C =====================================================================
C --- SI INTEGRATION REDUITE, ON MET A 0 CERTAINS COEFFICIENTS --------
C =====================================================================
       IF (MODINT .EQ. 'RED') THEN
          IF (YAMEC .EQ. 1) THEN
             DO 203 I=1,NDIM
                CS(ADDEME-1+I) = 0.D0
 203         CONTINUE
             DO 213 I=1,6
                CS(ADDEME-1+NDIM+I) = 0.D0
 213         CONTINUE
          ENDIF
          IF (YAP1 .EQ. 1) THEN
             C(ADDEP1) = 0.D0
             DO 204 I=1,NDIM
                CS(ADDEP1-1+1+I) = 0.D0
 204         CONTINUE
          ENDIF
          IF (YAP2 .EQ. 1) THEN
             C(ADDEP2) = 0.D0
             DO 206 I=1,NDIM
                CS(ADDEP2-1+1+I) = 0.D0
 206         CONTINUE
          ENDIF
          IF (YATE .EQ.1) THEN
             A(2) = 0.D0
             AS(1) = 0.D0
             DO 207 I=1,NDIM
                CS(ADDETE-1+1+I) = 0.D0
 207         CONTINUE
          ENDIF
       ENDIF
C ======================================================================
C --- FIN CALCUL C,D,F -------------------------------------------------
C ======================================================================
C --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
C ======================================================================
      IF (OPTION(1:9).NE.'RIGI_MECA') THEN
         DO 1 I=1,DIMUEL
            VECTU(I)=0.D0
 1       CONTINUE
      ENDIF
C ======================================================================
C --- INITIALISATION DF(MATUU) ET MATRI --------------------------------
C ======================================================================
      IF ((OPTION(1:9).EQ.'RIGI_MECA') .OR.
     &    (OPTION(1:9).EQ.'FULL_MECA')) THEN
         DO 3 I=1,DIMUEL*DIMUEL
               MATUU(I)=0.D0
 3       CONTINUE
         DO 5 I=1,DIMMAT
            DO 6 J=1,DIMMAT
               MATRI(I,J)=0.D0
 6          CONTINUE
 5       CONTINUE
      ENDIF
C ======================================================================
C --- CALCUL POUR CHAQUE POINT D'INTEGRATION: BOUCLE SUR KPI -----------
C ======================================================================
      LOI = ' '
      CALL RCVALA(IMATE,' ','THM_INIT', 0, ' ', 0.D0, 1, 'COMP_THM',
     &                                              RTHMC, CODMES, 'FM')
      THMC = COMPOR(8)
      IF ( (RTHMC-1.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_SATU'
      ELSE IF ( (RTHMC-2.0D0).LT.R8PREM() ) THEN
         LOI = 'GAZ'
      ELSE IF ( (RTHMC-3.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_VAPE'
      ELSE IF ( (RTHMC-4.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_VAPE_GAZ'
      ELSE IF ( (RTHMC-5.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_GAZ'
      ELSE IF ( (RTHMC-6.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_GAZ_ATM'
      ELSE IF ( (RTHMC-9.0D0).LT.R8PREM() ) THEN
         LOI = 'LIQU_AD_GAZ_VAPE'
      ENDIF
      IF (THMC.NE.LOI) THEN
         CALL UTMESS('F',NOMPRO,'IL Y A INCOHRENCE ENTRE LA LOI'//
     &         ' DE COUPLAGE DE DEFI_MATERIAU '//LOI//' ET LA LOI'//
     &         ' DE COUPLAGE DANS STAT_NON_LINE '//THMC)
C        CALL U2MESK('F','ALGORITH_34', 2 ,VALK)
      ENDIF
C =====================================================================
C --- BOUCLE SUR LES POINTS D'INTEGRATION -----------------------------
C =====================================================================
      DO 10 IPI=1,NPI
        KPI = IPI
C =====================================================================
C --- CALCUL DE LA MATRICE B AU POINT D'INTEGRATION -------------------
C =====================================================================
         CALL CABTHM(NDDLS,NDDLM,NNO,NNOS,NNOM,DIMUEL,
     &               DIMDEF,NDIM,NPI,KPI,IPOIDS,IPOID2,IVF,IVF2,
     &               IDFDE,IDFDE2,DFDI,DFDI2,
     &               GEOM,POIDS,POIDS2,B,NMEC,YAMEC,ADDEME,YAP1,
     &               ADDEP1,YAP2,ADDEP2,YATE,ADDETE,NP1,NP2,AXI)
C =====================================================================
C --- CALCUL DES DEFORMATIONS GENERALISEES E=BU -----------------------
C =====================================================================
         DO 108 I=1,DIMDEF
            DEFGEM(I)=0.D0
            DEFGEP(I)=0.D0
            DO 109 N=1,DIMUEL
               DEFGEM(I)=DEFGEM(I)+B(I,N)*DEPLM(N)
               DEFGEP(I)=DEFGEP(I)+B(I,N)*DEPLP(N)
 109        CONTINUE
 108     CONTINUE
C ======================================================================
C --- APPEL A LA ROUTINE EQUTHP OU EQUTHM ------------------------------
C ======================================================================
C --- CALCUL DES CONTRAINTES (VIRTUELLES ET GENERALISEES) --------------
C --- ET DE LEURS DERIVEES ---------------------------------------------
C ======================================================================
         IF (PERMAN) THEN
           CALL EQUTHP(
     &          IMATE,OPTION,NDIM,COMPOR,TYPMOD,
     &          KPI,NPG,
     &          DIMDEF,DIMCON,NBVARI,
     &          DEFGEM,CONTM((KPI-1)*DIMCON+1),
     &          VARIM((KPI-1)*NBVARI+1),
     &          DEFGEP,CONTP((KPI-1)*DIMCON+1),
     &          VARIP((KPI-1)*NBVARI+1),
     &          MECANI,PRESS1,PRESS2,TEMPE,
     &          CRIT,RINSTM,RINSTP,
     &          R,DRDS,DSDE,CODRET)
         ELSE
           CALL EQUTHM(
     &          IMATE,OPTION,TA,TA1,NDIM,COMPOR,TYPMOD,
     &          KPI,NPG,
     &          DIMDEF,DIMCON,NBVARI,
     &          DEFGEM,CONTM((KPI-1)*DIMCON+1),
     &          VARIM((KPI-1)*NBVARI+1),
     &          DEFGEP,CONTP((KPI-1)*DIMCON+1),
     &          VARIP((KPI-1)*NBVARI+1),
     &          MECANI,PRESS1,PRESS2,TEMPE,
     &          CRIT,RINSTM,RINSTP,DT,
     &          R,DRDS,DSDE,CODRET)
         ENDIF
       IF ( CODRET.NE.0) THEN
         GOTO 9000
       ENDIF
C ======================================================================
C --- CONTRIBUTION DU POINT D'INTEGRATION KPI A LA MATRICE TANGENTE ET -
C --- AU RESIDU --------------------------------------------------------
C ----------------------------------------------------------------------
C --- MATRICE TANGENTE : REMPLISSAGE EN NON SYMETRIQUE -----------------
C ======================================================================
C --- CHOIX DU JEU DE MATRICES ADAPTE AU POINT D'INTEGRATION -----------
C --- SI KPI<NPG ALORS ON EST SUR UN POINT DE GAUSS: CK = C  -----------
C --- SINON ON EST SUR UN SOMMET                   : CK = CS -----------
C ======================================================================
          IF (KPI .LE. NPG) THEN
              CALL LCEQVN(DIMDEF,C,CK)
              CALL LCEQVN(2,A,AK)
          ELSE
              CALL LCEQVN(DIMDEF,CS,CK)
              CALL LCEQVN(2,AS,AK)
          ENDIF
C ======================================================================
C --- CALCUL DE MATUU (MATRI) ------------------------------------------
C --- ON MODIFIE LA 7EME LIGNE (TERME EN TEMPERATURE) DE LA MATRICE ----
C --- DE MANIERE A L'ADAPTER AU PI -------------------------------------
C ======================================================================
       IF (OPTION(1:9).EQ.'RIGI_MECA' .OR.
     &     OPTION(1:9).EQ.'FULL_MECA') THEN
           DO 199 I=1,DIMDEF
              DO 198 J=1,DIMCON
                 DRDSR(I,J)=DRDS(I,J)
 198          CONTINUE
 199       CONTINUE

            IF (YATE .EQ. 1) THEN
                DO 200 I=1,DIMCON
                   DRDSR(ADDETE,I) = AK(1)*DRDS(ADDETE,I)
     &                              + AK(2)*DRDS(DIMDEF+1,I)
 200            CONTINUE
            ENDIF
C ======================================================================
C --- ON ASSEMBLE: DF=BT.CK.DRDSR.DK.DSDE.FK.B.POIDS -------------------
C ======================================================================
            CALL PMATHM(DIMMAT,DIMDEF,DIMCON,DIMUEL,
     &                                      DSDE,DRDSR,CK,B,POIDS,MATRI)
      ENDIF
C ======================================================================
C --- CALCUL DE VECTUU -------------------------------------------------
C ======================================================================
C --- ON SELECTIONNE LES COMPOSANTES UTILES DE R POUR CE PI ------------
C ======================================================================
         IF ((OPTION(1:9).EQ.'FULL_MECA' .OR.
     &        OPTION(1:9).EQ.'RAPH_MECA')) THEN
            DO 20 I=1,DIMDEF
               SIGBAR(I) = CK(I)*R(I)
 20         CONTINUE
C ======================================================================
C --- ON SELECTIONNE LA BONNE COMPOSANTE 7 POUR CE PI ------------------
C ======================================================================
            IF (YATE .EQ. 1) THEN
                SIGBAR(ADDETE) = AK(1)*R(ADDETE) + AK(2)*R(DIMDEF+1)
            ENDIF
C ======================================================================
C --- ON ASSEMBLE R=BT.SIGBAR.POIDS ------------------------------------
C ======================================================================
            DO 117 I=1,DIMUEL
               DO 118 K=1,DIMDEF
                     VECTU(I)=VECTU(I)+B(K,I)*SIGBAR(K)*POIDS
 118           CONTINUE
 117        CONTINUE
         ENDIF
 10   CONTINUE
C ======================================================================
C --- SORTIE DE BOUCLE SUR LES POINTS D'INTEGRATION --------------------
C ======================================================================
        IF(OPTION(1:9).EQ.'RIGI_MECA' .OR.
     &     OPTION(1:9).EQ.'FULL_MECA') THEN
               KJI=1
               DO 115 II=1,DIMUEL
                  DO 116 JJ=1,DIMUEL
                     MATUU(KJI) = MATRI(II,JJ)
                     KJI= KJI + 1
 116              CONTINUE
 115           CONTINUE
        ENDIF
C ======================================================================
 9000  CONTINUE
C ======================================================================
       END
