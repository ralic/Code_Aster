      SUBROUTINE TE0102( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
C                          OPTION : 'MASS_THER      '
C                          CAS COQUE
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      PARAMETER          (NDIMAX=27)
      PARAMETER          (NBRES=6)
      PARAMETER          (NBVAR= 2)
      CHARACTER*2        CODRET(NBRES),NUM
      CHARACTER*8        NOMRES(NBRES), NOMPAR(NBVAR),ELREFE
      CHARACTER*16       PHENOM
      CHARACTER*24       CARAC,FF
      REAL*8             M(3,3),ROC,H
      REAL*8             VALRES(NBRES)
      REAL*8             COOR2D(18)
      REAL*8             DFDX(9),DFDY(9),POIDS,PM,DELTAT
      REAL*8             MUN,ZERO,DEUX,QUATRE,CINQ
      REAL*8             QUINZE,SEIZE,COUR,COSA,SINA,R
      REAL*8             VALPAR(NBVAR), TEMPE, INSTAN
      REAL*8             MASSE(NDIMAX,NDIMAX)
      INTEGER            NNO,KP,NPG1,NPG2,GI,PI,GJ,PJ,K,IMATTT
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM
      INTEGER            IMATE,ITEMPS
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
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
C --- INITIALISATIONS :
C     ---------------
      CALL ELREF1(ELREFE)
      MUN    = -1.0D0
      ZERO   =  0.0D0
      UNDEMI =  0.5D0
      UN     =  1.0D0
      DEUX   =  2.0D0
      QUATRE =  4.0D0
      QUINZE = 15.0D0
      SEIZE  = 16.0D0
C
      TEMPE  = ZERO
      INSTAN = ZERO
      NOMPAR(1) = 'INST'
      NOMPAR(2) = 'TEMP'
      VALPAR(1) = INSTAN
      VALPAR(2) = TEMPE
C
      DO 10 I=1,3
        DO 10 J=1,3
          M(I,J) =ZERO
 10   CONTINUE
C
      DO 20 I = 1, NDIMAX
        DO 20 J = 1, NDIMAX
          MASSE(I,J) = ZERO
 20   CONTINUE
C

C --- RECUPERATION DES CARACTERISTIQUES DE L'ELEMENT :
C --- NOMBRE DE NOEUDS, NOMBRE DE POINTS D'INTEGRATION :
C     ------------------------------------------------
      CARAC='&INEL.'//ELREFE//'.CARAC'
      CALL JEVETE(CARAC,'L',ICARAC)
      NNO  = ZI(ICARAC)
      NPG1 = ZI(ICARAC+2)
      NPG2 = ZI(ICARAC+3)
C
C --- RECUPERATION DES POIDS, DES FONCTIONS DE FORME ET DE LEURS
C --- DERIVEES AUX POINTS D'INTEGRATION (ON UTILISE LA SECONDE
C --- FAMILLE DE POINTS D'INTEGRATION DE L'ELEMENT) :
C     ---------------------------------------------
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF+(3*NNO+1)*NPG1
      IVF   =IPOIDS + NPG2
      IDFDE =IVF    + NPG2*NNO
      IDFDK =IDFDE  + NPG2*NNO
C
C --- DANS LE CAS DES ELEMENTS LINEIQUES EN CONTRAINTES PLANES ET AXI
C --- ON UTILISE LA PREMIERE FAMILLE DE POINTS D'INTEGRATION
C --- DE L'ELEMENT :
C     ------------
      IF (NOMTE(1:8).EQ.'THCPSE3 ' .OR. NOMTE(1:8).EQ.'THCASE3 ') THEN
         IPOIDS=IFF
         IVF   =IPOIDS+NPG1
         IDFDE =IVF   +NPG1*NNO
      ENDIF
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
C     ----------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C --- RECUPERATION DE L'EPAISSEUR DE LA COQUE :
C     ---------------------------------------
      CALL JEVECH('PCACOQU','L',ICACOQ)
C
C --- RECUPERATION DE L'INSTANT DU CALCUL ET DU PAS DE TEMPS :
C     ------------------------------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)
      VALPAR(1) = ZR(ITEMPS)
      DELTAT    = ZR(ITEMPS+1)
C
C --- RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM :
C     -------------------------------------------------
      CALL RCCOMA(ZI(IMATE),'THER',PHENOM,CODRET)
C
C --- DETERMINATION DU TENSEUR DE CAPACITE THERMIQUE :
C     ==============================================
C
C --- CAS DES COQUES THERMIQUES MULTI-COUCHES :
C     ---------------------------------------
      IF (PHENOM.EQ.'THER_COQMU') THEN
C
C ---   NOM DES COMPOSANTES DU TENSEUR DE CAPACITE
C ---   THERMIQUE HOMOGENEISE :
C       ---------------------
        DO 30 I=1, NBRES
          CALL CODENT(I+24,'G',NUM)
          NOMRES(I) = 'HOM_'//NUM
 30     CONTINUE
C
C ---   INTERPOLATION DES TERMES DU TENSEUR DE CAPACITE THERMIQUE
C ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        CALL RCVALA ( ZI(IMATE),'THER_COQMU',NBVAR,NOMPAR,VALPAR,
     +                NBRES,NOMRES,VALRES,CODRET, 'FM' )
C
C ---   TENSEUR DE CAPACITE THERMIQUE :
C       -----------------------------
        M(1,1) = VALRES(1)
        M(2,1) = VALRES(2)
        M(3,1) = VALRES(3)
        M(2,2) = VALRES(4)
        M(3,2) = VALRES(5)
        M(3,3) = VALRES(6)
C
C --- CAS DES COQUES THERMIQUES ISOTROPES :
C     ===================================
      ELSEIF (PHENOM.EQ.'THER') THEN
C
C ---   INTERPOLATION DE LA CAPACITE THERMIQUE EN FONCTION DU TEMPS
C ---   ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        NBV = 1
        NOMRES(1)='RHO_CP'
        CALL RCVALA ( ZI(IMATE),'THER',NBVAR,NOMPAR,VALPAR,
     +                NBV,NOMRES,VALRES,CODRET, 'FM' )
C
C ---   CAPACITE THERMIQUE :
C       ------------------
        ROCP = VALRES(1)
C
C ---   DEMI-EPAISSEUR  :
C       --------------
        H = UNDEMI*ZR(ICACOQ)
C
C ---   TENSEUR DE CAPACITE THERMIQUE :
C       -----------------------------
        M(1,1) = SEIZE *ROCP*H/QUINZE
        M(2,1) = DEUX  *ROCP*H/QUINZE
        M(3,1) = DEUX  *ROCP*H/QUINZE
        M(2,2) = QUATRE*ROCP*H/QUINZE
        M(3,2) = MUN   *ROCP*H/QUINZE
        M(3,3) = QUATRE*ROCP*H/QUINZE
C
C --- CAS DES COQUES THERMIQUES HETEROGENES :
C     -------------------------------------
      ELSEIF (PHENOM.EQ.'THER_COQUE') THEN
C
C ---   NOM DES COMPOSANTES DU TENSEUR DE CAPACITE
C ---   THERMIQUE HOMOGENEISE
C ---   EN NOTANT RHOCP LA CAPACITE THERMIQUE EN CHAQUE POINT
C ---             P1(X3), P2(X3), P3(X3) LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
C ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   (I.E. T(X1,X2,X3) =    P1(X3)*TMOY(X1,X2)
C ---                        + P2(X3)*TINF(X1,X2)
C ---                        + P3(X3)*TSUP(X1,X2)
C ---   LES TERMES DU TENSEUR DE CAPACITE THERMIQUE HOMOGENEISE
C ---   SONT ALORS :
C       ----------
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P1(X3).DX3) :
        NOMRES(1) = 'CMAS_MM'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P2(X3).DX3) :
        NOMRES(2) = 'CMAS_MP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P2(X3).DX3) :
        NOMRES(3) = 'CMAS_PP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P3(X3).DX3) :
        NOMRES(4) = 'CMAS_SI'
C
C ---   INTERPOLATION DES COMPOSANTES DU TENSEUR DE CAPACITE THERMIQUE
C ---   EN FONCTION DU TEMPS ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        NBV = 4
        CALL RCVALA ( ZI(IMATE),PHENOM,NBVAR,NOMPAR,VALPAR,
     +                NBV,NOMRES,VALRES,CODRET, 'FM' )
C
        M(1,1) = VALRES(1)
        M(1,2) = VALRES(2)
        M(1,3) = VALRES(2)
        M(2,2) = VALRES(3)
        M(2,3) = VALRES(4)
        M(3,3) = VALRES(3)
        M(2,1) = M(1,2)
        M(3,1) = M(1,3)
        M(3,2) = M(2,3)
C
      ELSE
        CALL UTMESS('F','TE0102','LE MATERIAU '//PHENOM//' N''EST '
     +            //'PAS CONNU. SEULS SONT ADMIS LES MATERIAUX '
     +            //' ''THER'', ''THER_COQMU'' ET ''THER_COQUE'' '
     +            //'POUR LES COQUES THERMIQUES .')
      ENDIF
C
C
C===================================
C --- CALCUL DE LA MASSE THERMIQUE =
C===================================
C
C --- CAS DES COQUES SURFACIQUES :
C     --------------------------
      IF (NOMTE(1:8).NE.'THCPSE3 ' .AND. NOMTE(1:8).NE.'THCASE3 ') THEN
C
C --- DETERMINATION DES COORDONNEES COOR2D DES NOEUDS DE L'ELEMENT
C --- DANS LE REPERE DE L'ELEMENT :
C     ---------------------------
        CALL CQ3D2D(NNO,ZR(IGEOM),UN,ZERO,COOR2D)
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 40 KP=1, NPG2
          K=(KP-1)*NNO
C
C ---   DERIVEES DES FONCTIONS DE FORME ET PRODUIT JACOBIEN*POIDS
C ---   (DANS POIDS)  SUR L'ELEMENT :
C       ---------------------------
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                  COOR2D,DFDX,DFDY,POIDS )
          DO 50 GI=1,NNO
             DO 60 GJ=1,GI
                DO 70 PI=1,3
                   DO 80 PJ=1,PI
                      PM =  M(PI,PJ)*ZR(IVF+K+GI-1)*ZR(IVF+K+GJ-1)*POIDS
C
C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                      IF ((PI.NE.PJ).AND.(GI.NE.GJ)) THEN
                        I=3*(GI-1)+PJ
                        J=3*(GJ-1)+PI
                        MASSE(I,J)  = MASSE(I,J)  + PM
                      ENDIF
C
C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                      I=3*(GI-1)+PI
                      J=3*(GJ-1)+PJ
                     MASSE(I,J)  = MASSE(I,J)  + PM
 80                CONTINUE
 70              CONTINUE
 60          CONTINUE
 50       CONTINUE
 40     CONTINUE
C
      ELSE
C
C --- CAS DES COQUES LINEIQUES :
C     ------------------------
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION :
C      -----------------------------------
        DO 90 KP=1,NPG2
          K=(KP-1)*NNO
          CALL DFDM1D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),
     +                  ZR(IGEOM),DFDX,COUR,POIDS,COSA,SINA)
C
          IF (NOMTE(3:4).EQ.'CA') THEN
             R=ZERO
             DO 100 I=1,NNO
                R=R+ZR(IGEOM+2*I-2)*ZR(IVF+K+I-1)
 100         CONTINUE
             POIDS=POIDS*R
          ENDIF
C
          DO 110 GI=1,NNO
             DO 120 GJ=1,GI
                DO 130 PI=1,3
                   DO 140 PJ=1,PI
                      PM =  M(PI,PJ)*ZR(IVF+K+GI-1)*ZR(IVF+K+GJ-1)*POIDS
C
C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                      IF ((PI.NE.PJ).AND.(GI.NE.GJ)) THEN
                        I=3*(GI-1)+PJ
                        J=3*(GJ-1)+PI
                        MASSE(I,J)  = MASSE(I,J)  + PM
                      ENDIF
C
C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                      I=3*(GI-1)+PI
                      J=3*(GJ-1)+PJ
                      MASSE(I,J)  = MASSE(I,J)  + PM
 140                CONTINUE
 130              CONTINUE
 120          CONTINUE
 110       CONTINUE
  90     CONTINUE
C
      ENDIF
C
C --- RECUPERATION DE LA MATRICE DE MASSE THERMIQUE EN SORTIE DU TE :
C     -------------------------------------------------------------
      CALL JEVECH('PMATTTR','E',IMATTT)
C
C --- AFFECTATION DE LA MATRICE DE MASSE THERMIQUE EN SORTIE DU TE :
C     ------------------------------------------------------------
      NBDDL = 3*NNO
      IND = 0
      DO 150 I = 1, NBDDL
        DO 160 J = 1, I
          IND = IND + 1
          ZR(IMATTT+IND-1) = MASSE(I,J)/DELTAT
 160    CONTINUE
 150  CONTINUE
C
      END
