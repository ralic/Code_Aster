      SUBROUTINE TE0110( OPTION , NOMTE )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16       OPTION , NOMTE
C ......................................................................
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
C TOLE  CRP_20
C
C    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
C                          OPTION : 'CHAR_THER_EVOL'
C                          CAS COQUES SURFACIQUES ET LEURS BORDS
C                         (CAS COQUES LINEIQUES NON FAIT)
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
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
      PARAMETER          (NBRES=30)
      PARAMETER          (NBPAR=4)
      PARAMETER          (NDIMAX=27)
      CHARACTER*2        CODRET(NBRES),NUM, CODMAT
      CHARACTER*8        HFMOIN, HFPLUS, HFBORD,ELREFE
      CHARACTER*8        NOMRES(NBRES),NOMPAR(NBPAR)
      CHARACTER*16       PHENOM
      CHARACTER*24       CARAC,FF
      REAL*8             VALRES(NBRES), VALPAR(NBPAR), HOM(NBRES)
      REAL*8             M(3,3),ROC,H,TPG(3),LAM,DTPGDX(3),B(3,3)
      REAL*8             COOR2D(18),DFDX(9),DFDY(9),POIDS,DTPGDY(3)
      REAL*8             AXE(3,3),ANG(2), A(3,3,2,2)
      REAL*8             MATN(3,3), MATP(3,3)
      REAL*8             MATREF(3), MATELE(3)
      REAL*8             RIGITH(NDIMAX,NDIMAX), MASSE(NDIMAX,NDIMAX)
      REAL*8             LONG, HMOIN, HPLUS, HBORD
      INTEGER            I,J,NNO,KP,NPG1,NPG2,GI,PI,IVECTT,ITEMP,ICACOQ
      INTEGER            ICARAC,IFF,IPOIDS,IVF,IDFDE,IDFDK,IGEOM,IMATE
      INTEGER            ITEMPS,K,PJ,GJ
C
C
C --- DETERMINATION DU SECOND MEMBRE CHAR_THER_EVOL :
C --- F =   1/DT*MASSE_THER*(T-) - (1-THETA)*RIGI_THER*(T-)
C ---    - (1-THETA)*HPLUS*(T-)  - (1-THETA)*HMOIN*(T-)
C --- MASSE_THER DESIGNE LA MASSE THERMIQUE
C --- RIGI_THER EST LA RIGIDITE THERMIQUE
C --- LES AUTRES TERMES PRIS EN COMPTE SONT SEULEMENT DES TERMES
C --- D'ECHANGE.
C --- T- DESIGNE LE CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
C --- HPLUS EST LE COEFFICIENT D'ECHANGE DE LA SURFACE SUPERIEURE
C --- DE LA COQUE AVEC L'EXTERIEUR
C --- HMOIN EST LE COEFFICIENT D'ECHANGE DE LA SURFACE INFERIEURE
C --- DE LA COQUE AVEC L'EXTERIEUR
C --- EN FAIT LES EXPRESSIONS HPLUS ET HMOIN SONT
C --- DISCRETISEES ET INTEGREES SUR LES SURFACES SUR LESQUELLES
C --- ELLES S'APPLIQUENT .
C --- LES OPERATEURS DE MASSE THERMIQUE, RIGIDITE THERMIQUE ET
C --- D'ECHANGE S'APPLIQUENT SUR LE TRIPLET (T_MOY,T_INF,T_SUP)
C --- DANS CET ORDRE OU
C --- T_MOY EST LA TEMPERATURE SUR LE FEUILLET MOYEN DE LA COQUE
C --- T_INF EST LA TEMPERATURE SUR LE FEUILLET INFERIEUR DE LA COQUE
C --- T_SUP EST LA TEMPERATURE SUR LE FEUILLET SUPERIEUR DE LA COQUE
C     ==============================================================
C --- INITIALISATIONS :
C     ---------------
      CALL ELREF1(ELREFE)
      ZERO      =  0.0D0
      UN        =  1.0D0
C

      NOMPAR(1) = 'INST'
      NOMPAR(2) = 'X'
      NOMPAR(3) = 'Y'
      NOMPAR(4) = 'Z'
      VALPAR(1) = ZERO
      VALPAR(2) = ZERO
      VALPAR(3) = ZERO
      VALPAR(4) = ZERO
C
      HPLUS  = ZERO
      HMOIN  = ZERO
      HBORD  = ZERO
C
      MATREF(1) = ZERO
      MATREF(2) = ZERO
      MATREF(3) = ZERO
      MATELE(1) = ZERO
      MATELE(2) = ZERO
      MATELE(3) = ZERO
C
      DO 10 I = 1, NDIMAX
        DO 10 J = 1, NDIMAX
          RIGITH(I,J) = ZERO
          MASSE(I,J)  = ZERO
 10   CONTINUE
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
C --- DERIVEES AUX POINTS D'INTEGRATION (ON CHOISIT LA SECONDE
C --- FAMILLE DE POINTS D'INTEGRATION :
C     -------------------------------
      FF   ='&INEL.'//ELREFE//'.FF'
      CALL JEVETE(FF,'L',IFF)
      IPOIDS=IFF+(3*NNO+1)*NPG1
      IVF   =IPOIDS+NPG2
      IDFDE =IVF   +NPG2*NNO
      IDFDK =IDFDE +NPG2*NNO
C
C --- RECUPERATION DES COORDONNEES DES NOEUDS DE L'ELEMENT :
C     ----------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION DU MATERIAU :
C     ------------------------
      CALL JEVECH('PMATERC','L',IMATE)
C
C --- RECUPERATION DE L'EPAISSEUR DE LA COQUE ET DES 2 ANGLES
C --- PERMETTANT DE PASSER DU REPERE GLOBAL AU REPERE DE REFERENCE
C --- TANGENT A LA COQUE :
C     ------------------
      CALL JEVECH('PCACOQU','L',ICACOQ)
C
C --- RECUPERATION DE L'INSTANT DU CALCUL, DU PAS DE TEMPS ET
C --- DU PARAMETRE THETA DE LA METHODE 'THETA' UTILISEE
C --- POUR RESOUDRE L'EQUATION DIFFERENTIELLE EN TEMPS DE LA
C --- TEMPERATURE (EN STATIONNAIRE THETA =1 ) :
C     ---------------------------------------
      CALL JEVECH('PTEMPSR','L',ITEMPS)
C
C --- RECUPERATION DU CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT :
C     ----------------------------------------------------------
      CALL JEVECH('PTEMPER','L',ITEMP)
C
      VALPAR(1) = ZR(ITEMPS)
      DELTAT    = ZR(ITEMPS+1)
      THETA     = ZR(ITEMPS+2)
C
C --- RECUPERATION EVENTUELLE DES COEFFICIENTS D'ECHANGE AVEC
C --- L'EXTERIEUR :
C     -----------
      CALL TECACH(.FALSE.,.FALSE.,'PCOEFHR',1,ICOEHR)
      CALL TECACH(.FALSE.,.FALSE.,'PCOEFHF',1,ICOEHF)
      IF (NOMTE(1:8).NE.'THCPSE3 '.AND. NOMTE(1:8).NE.'THCASE3 ' .AND.
     +    NOMTE(1:8).NE.'THCOSE3 '.AND. NOMTE(1:8).NE.'THCOSE2 ') THEN
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        IF (ICOEHR.GT.0) THEN
          HMOIN  = ZR(ICOEHR)
          HPLUS  = ZR(ICOEHR+1)
        ENDIF
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        IF (ICOEHF.GT.0) THEN
          HFMOIN = ZK8(ICOEHF)
          HFPLUS = ZK8(ICOEHF+1)
        ENDIF
      ELSEIF (NOMTE(1:8).EQ.'THCOSE3 '.OR. NOMTE(1:8).EQ.'THCOSE2 ')
     +    THEN
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES REELS :
        IF (ICOEHR.GT.0) THEN
          H = ZR(ICOEHR)
        ENDIF
C ---   CAS OU LES COEFFICIENTS D'ECHANGE SONT DES FONCTIONS :
        IF (ICOEHF.GT.0) THEN
          HFBORD = ZK8(ICOEHF)
        ENDIF
      ENDIF
C
C --- NOMBRE DE NOEUDS SOMMETS :
C     ------------------------
      IF (NOMTE(5:6).EQ.'TR') THEN
        NBNOSO = 3
      ELSEIF (NOMTE(5:6).EQ.'QU') THEN
        NBNOSO = 4
      ENDIF
C
C..................................................................
C.    CAS DES COQUES SURFACIQUES                                  .
C..................................................................
C
      IF (NOMTE(1:8).NE.'THCPSE3 '.AND. NOMTE(1:8).NE.'THCASE3 ' .AND.
     +    NOMTE(1:8).NE.'THCOSE3 '.AND. NOMTE(1:8).NE.'THCOSE2 ') THEN
C
C ---   RECUPERATION DE LA NATURE DU MATERIAU DANS PHENOM
C       -------------------------------------------------
        CALL RCCOMA(ZI(IMATE),'THER',PHENOM,CODMAT)
C
C ---   DETERMINATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE
C ---   ET TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE
C ---
C
C ---   CAS DES COQUES THERMIQUES MULTI-COUCHES
C       =======================================
        IF (PHENOM.EQ.'THER_COQMU') THEN
C
C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT
C       -----------------------------------
          CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     +                ANG)
C
C ---   NOM DES COMPOSANTES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE :
C       ----------------------------------------------------------
          DO 20 I=1, NBRES
             CALL CODENT(I,'G',NUM)
             NOMRES(I) = 'HOM_'//NUM
20        CONTINUE
C
C ---   INTERPOLATION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---   EN FONCTION DU TEMPS :
C       --------------------
          NBVAR = 1
          CALL RCVALA ( ZI(IMATE),'THER_COQMU',NBVAR,NOMPAR,VALPAR,
     +                  NBRES,NOMRES,VALRES,CODRET, 'FM' )
C
C ---   CONSTRUCTION DE LA MATRICE DE PASSAGE DU REPERE UTILISATEUR
C ---   AU REPERE ELEMENT :
C       -----------------
          CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     +                ANG)
C
C ---   VALEURS DES CARACTERISIQUES DU MATERIAU DANS LE REPERE
C ---   DE L'ELEMENT ( PARCE QUE C'EST DANS CE REPERE QUE LE
C ---   FLUX THERMIQUE EST LE PLUS SIMPLE A ECRIRE) :
C       -------------------------------------------
          DO 30 I=1,6
            CALL REFLTH(ANG,VALRES(3*(I-1)+1),HOM(3*(I-1)+1))
30        CONTINUE
C
C ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -----------------------------------
          A(1,1,1,1) = HOM(1)
          A(1,1,2,2) = HOM(2)
          A(1,1,1,2) = HOM(3)
          A(2,1,1,1) = HOM(4)
          A(2,1,2,2) = HOM(5)
          A(2,1,1,2) = HOM(6)
          A(3,1,1,1) = HOM(7)
          A(3,1,2,2) = HOM(8)
          A(3,1,1,2) = HOM(9)
          A(2,2,1,1) = HOM(10)
          A(2,2,2,2) = HOM(11)
          A(2,2,1,2) = HOM(12)
          A(3,2,1,1) = HOM(13)
          A(3,2,2,2) = HOM(14)
          A(3,2,1,2) = HOM(15)
          A(3,3,1,1) = HOM(16)
          A(3,3,2,2) = HOM(17)
          A(3,3,1,2) = HOM(18)
C
C ---   LES TERMES DE CONDUCTIVITE TRANSVERSE NE SE TRANSFORMENT PAS
C ---   PAR CHANGEMENT DE REPERE :
C       ------------------------
          B(1,1) = VALRES(19)
          B(2,1) = VALRES(20)
          B(3,1) = VALRES(21)
          B(2,2) = VALRES(22)
          B(3,2) = VALRES(23)
          B(3,3) = VALRES(24)
C
C ---   LES TERMES RELATIFS A LA CAPACITE CALORIFIQUE NE SE
C ---   TRANSFORMENT PAS PAR CHANGEMENT DE REPERE :
C       -----------------------------------------
          M(1,1) = VALRES(25)
          M(2,1) = VALRES(26)
          M(3,1) = VALRES(27)
          M(2,2) = VALRES(28)
          M(3,2) = VALRES(29)
          M(3,3) = VALRES(30)
C
C ---   CAS DES COQUES THERMIQUES ISOTROPES
C       ===================================
        ELSEIF (PHENOM.EQ.'THER') THEN
C
C ---   INTERPOLATION DE LA CONDUCTIVITE EN FONCTION DU TEMPS
C ---   ET DE LA TEMPERATURE
C ---   (L'INTERPOLATION EN FONCTION DE LA TEMPERATURE EST
C ---    INACTIVE POUR LE MOMENT) :
C       -------------------------
        NBV = 2
        NOMRES(1)='RHO_CP'
        NOMRES(2)='LAMBDA'
        NBVAR = 1
        CALL RCVALA ( ZI(IMATE),'THER',NBVAR,NOMPAR,VALPAR,
     +                NBV,NOMRES,VALRES,CODRET, 'FM' )
C
C ---   INITIALISATION DES TENSEURS DE CONDUCTIVITE MEMBRANAIRE,
C ---   TRANSVERSE ET DU TENSEUR DE CAPACITE THERMIQUE :
C       ----------------------------------------------
          DO 40 L = 1, 2
            DO 40 K = 1, L
              DO 40 I = 1, 3
                DO 40 J = 1, I
                  A(I,J,K,L) = ZERO
                  B(I,J) =     ZERO
                  M(I,J) =     ZERO
 40       CONTINUE
C
C ---   CAPACITE THERMIQUE :
C       ------------------
          ROC = VALRES(1)
C
C ---   CONDUCTIVITE :
C       ------------
          LAM = VALRES(2)
C
C ---   DEMI-EPAISSEUR :
C       --------------
          H = ZR(ICACOQ)/2.D0
C
C ---   TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -----------------------------------
          A(1,1,1,1) = 16.D0*LAM*H/15.D0
          A(1,1,2,2) = A(1,1,1,1)
          A(2,2,1,1) = 4.D0*LAM*H/15.D0
          A(2,2,2,2) = A(2,2,1,1)
          A(3,3,1,1) = 4.D0*LAM*H/15.D0
          A(3,3,2,2) = A(2,2,1,1)
          A(2,1,1,1) = 2.D0*LAM*H/15.D0
          A(2,1,2,2) = A(2,1,1,1)
          A(3,1,1,1) = 2.D0*LAM*H/15.D0
          A(3,1,2,2) = A(3,1,1,1)
          A(3,2,1,1) = -LAM*H/15.D0
          A(3,2,2,2) = A(3,2,1,1)
C
C ---   MATRICE DE CONDUCTIVITE TRANSVERSE :
C       ----------------------------------
          B(1,1) =  16.D0*LAM/(6.D0*H)
          B(2,1) = -8.D0*LAM/(6.D0*H)
          B(3,1) =  B(2,1)
          B(2,2) =  7.D0*LAM/(6.D0*H)
          B(3,2) =  LAM/(6.D0*H)
          B(3,3) =  B(2,2)
          B(1,2) =  B(2,1)
          B(1,3) =  B(3,1)
          B(2,3) =  B(3,2)
C
C ---   TENSEUR DE CAPACITE THERMIQUE ISOTROPE :
C       --------------------------------------
          M(1,1) = 16.D0*ROC*H /15.D0
          M(2,1) = 2.D0*ROC*H  /15.D0
          M(3,1) = 2.D0*ROC*H  /15.D0
          M(2,2) = 4.D0*ROC*H/15.D0
          M(3,2) =-ROC*H    /15.D0
          M(3,3) = 4.D0*ROC*H/15.D0
          M(1,2) = M(2,1)
          M(1,3) = M(3,1)
          M(2,3) = M(3,2)
C
C ---   CAS DES COQUES THERMIQUES HETEROGENES HOMOGENEISEES
C       ===================================================
        ELSEIF (PHENOM.EQ.'THER_COQUE') THEN
C
C ---   LES DIRECTIONS 1 ET 2 DESIGNENT CELLES DU PLAN DE LA PLAQUE
C ---   LA DIRECTION 3 EST PERPENDICULAIRE
C ---   ON ADMET QUE LE TENSEUR DE CONDUCTIVITE EN CHAQUE POINT
C ---   EST DIAGONAL ET QUE SES VALEURS PROPRES SONT
C ---   LAMBDA_1 , LAMBDA_2 ET LAMBDA_3
C ---   D'AUTRE PART, SOIENT P1(X3), P2(X3), P3(X3) LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS
C ---   A L'INTERPOLATION DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   LES TERMES DU TENSEUR DE CONDUCTIVITE HOMOGENEISE SONT ALORS
C ---   POUR LE TENSEUR DE CONDUCTIVITE MEMBRANAIRE :
C       -------------------------------------------
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P1(X3).DX3)
          NOMRES(1)  = 'COND_LMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P1(X3)*P2(X3).DX3)
          NOMRES(2)  = 'COND_LMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P2(X3).DX3)
          NOMRES(3)  = 'COND_LPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_1*P2(X3)*P3(X3).DX3)
          NOMRES(4)  = 'COND_LSI'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P1(X3).DX3)
          NOMRES(5)  = 'COND_TMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P1(X3)*P2(X3).DX3)
          NOMRES(6)  = 'COND_TMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P2(X3).DX3)
          NOMRES(7)  = 'COND_TPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_2*P2(X3)*P3(X3).DX3)
          NOMRES(8)  = 'COND_TSI'
C
C ---   ON NE DONNE QUE 8 TERMES CAR ON A LES EGALITES SUIVANTES :
C ---   SOMME_EP(P2(X3)*P2(X3).DX3) = SOMME_EP(P3(X3)*P3(X3).DX3)
C ---   SOMME_EP(P1(X3)*P2(X3).DX3) = SOMME_EP(P1(X3)*P3(X3).DX3)

C ---   POUR LE TENSEUR DE CONDUCTIVITE TRANSVERSE :
C       ------------------------------------------
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P1'(X3).DX3)
          NOMRES(9)  = 'COND_NMM'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P1'(X3)*P2'(X3).DX3)
          NOMRES(10) = 'COND_NMP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P2'(X3).DX3)
          NOMRES(11) = 'COND_NPP'
C ---   TERME SOMME_EPAISSEUR(LAMBDA_3*P2'(X3)*P3'(X3).DX3)
          NOMRES(12) = 'COND_NSI'
C
C ---   ON N'A DONNE QUE 4 TERMES CAR ON A LES EGALITES SUIVANTES :
C ---   SOMME_EP(P2'(X3)*P2'(X3).DX3) = SOMME_EP(P3'(X3)*P3'(X3).DX3)
C ---   SOMME_EP(P1'(X3)*P2'(X3).DX3) = SOMME_EP(P1'(X3)*P3'(X3).DX3)

C ---   POUR LE TENSEUR DE CAPACITE THERMIQUE :
C       -------------------------------------
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P1(X3).DX3) :
          NOMRES(13) = 'CMAS_MM'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P1(X3)*P2(X3).DX3) :
          NOMRES(14) = 'CMAS_MP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P2(X3).DX3) :
          NOMRES(15) = 'CMAS_PP'
C ---   TERME SOMME_EPAISSEUR(RHOCP*P2(X3)*P3(X3).DX3) :
          NOMRES(16) = 'CMAS_SI'
C
C ---  ON N'A DONNE QUE 4 TERMES A CAUSE DES 2 EGALITES DONNEES
C ---  APRES LA DEFINITION DES TERMES DU TENSEUR DE CONDUCTIVITE
C ---  MEMBRANAIRE
C
C ---   INTERPOLATION DES TERMES DES TENSEURS DE CONDUCTIVITE ET DE
C ---   CAPACITE THERMIQUE EN FONCTION DU TEMPS :
C       ---------------------------------------
         NBV = 16
         NBVAR = 1
         CALL RCVALA ( ZI(IMATE),PHENOM,NBVAR,NOMPAR,VALPAR,NBV,NOMRES,
     +                 VALRES,CODRET, 'FM' )
C
C ---   DETERMINATION DE LA ROTATION FAISANT PASSER DU REPERE
C ---   DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------
         CALL MUDIRX(NBNOSO,ZR(IGEOM),3,ZR(ICACOQ+1),ZR(ICACOQ+2),AXE,
     +               ANG)
C
C ---   PASSAGE DU REPERE DE REFERENCE AU REPERE DE L'ELEMENT :
C       -----------------------------------------------------
C
C ---   TERMES DE CONDUCTIVITE MEMBRANAIRE DANS LE REPERE DE L'ELEMENT :
C       --------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P1*P1.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P1.DX3))
C       --------------------------------------------------------------
         MATREF(1) = VALRES(1)
         MATREF(2) = VALRES(5)
         MATREF(3) = ZERO
         CALL REFLTH(ANG, MATREF, MATELE)
C
         A(1,1,1,1) = MATELE(1)
         A(1,1,2,2) = MATELE(2)
         A(1,1,1,2) = MATELE(3)
         A(1,1,2,1) = MATELE(3)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P1*P2.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P1*P2.DX3))
C       --------------------------------------------------------------
         MATREF(1) = VALRES(2)
         MATREF(2) = VALRES(6)
         MATREF(3) = ZERO
         CALL REFLTH(ANG, MATREF, MATELE)
C
         A(1,2,1,1) = MATELE(1)
         A(1,2,2,2) = MATELE(2)
         A(1,2,1,2) = MATELE(3)
         A(1,2,2,1) = MATELE(3)
C
         A(2,1,1,1) = A(1,2,1,1)
         A(2,1,2,2) = A(1,2,2,2)
         A(2,1,1,2) = A(1,2,1,2)
         A(2,1,2,1) = A(1,2,2,1)
C
         A(1,3,1,1) = MATELE(1)
         A(1,3,2,2) = MATELE(2)
         A(1,3,1,2) = MATELE(3)
         A(1,3,2,1) = MATELE(3)
C
         A(3,1,1,1) = A(1,3,1,1)
         A(3,1,2,2) = A(1,3,2,2)
         A(3,1,1,2) = A(1,3,1,2)
         A(3,1,2,1) = A(1,3,2,1)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P2*P2.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P2.DX3))
C       --------------------------------------------------------------
         MATREF(1) = VALRES(3)
         MATREF(2) = VALRES(7)
         MATREF(3) = ZERO
         CALL REFLTH(ANG, MATREF, MATELE)
C
         A(2,2,1,1) = MATELE(1)
         A(2,2,2,2) = MATELE(2)
         A(2,2,1,2) = MATELE(3)
         A(2,2,2,1) = MATELE(3)
C
         A(3,3,1,1) = MATELE(1)
         A(3,3,2,2) = MATELE(2)
         A(3,3,1,2) = MATELE(3)
         A(3,3,2,1) = MATELE(3)
C  ------------------------------------------------------------------
C ---   PASSAGE DANS LE REPERE DE L'ELEMENT DE :
C ---      ( SOMME_EP(LAMBDA_1*P2*P3.DX3)     0.                     )
C ---      (         0.                  SOMME_EP(LAMBDA_2*P2*P3.DX3))
C       --------------------------------------------------------------
         MATREF(1) = VALRES(4)
         MATREF(2) = VALRES(8)
         MATREF(3) = ZERO
         CALL REFLTH(ANG, MATREF, MATELE)
C
         A(2,3,1,1) = MATELE(1)
         A(2,3,2,2) = MATELE(2)
         A(2,3,1,2) = MATELE(3)
         A(2,3,2,1) = MATELE(3)
C
         A(3,2,1,1) = A(2,3,1,1)
         A(3,2,2,2) = A(2,3,2,2)
         A(3,2,1,2) = A(2,3,1,2)
         A(3,2,2,1) = A(2,3,2,1)
C  ------------------------------------------------------------------
C
C ---   TERMES DE CONDUCTIVITE TRANSVERSE :
C       ---------------------------------
         B(1,1) = VALRES(9)
         B(1,2) = VALRES(10)
         B(1,3) = VALRES(10)
         B(2,2) = VALRES(11)
         B(2,3) = VALRES(12)
         B(3,3) = VALRES(11)
         B(2,1) = B(1,2)
         B(3,1) = B(1,3)
         B(3,2) = B(2,3)
C
C ---   TERMES DE MASSE :
C       ---------------
         M(1,1) = VALRES(13)
         M(1,2) = VALRES(14)
         M(1,3) = VALRES(14)
         M(2,2) = VALRES(15)
         M(2,3) = VALRES(16)
         M(3,3) = VALRES(15)
         M(2,1) = M(1,2)
         M(3,1) = M(1,3)
         M(3,2) = M(2,3)
C
       ELSE
         CALL UTMESS('F','TE0101','LE MATERIAU '//PHENOM//' N''EST '
     +             //'PAS CONNU. SEULS SONT ADMIS LES MATERIAUX '
     +             //' ''THER'', ''THER_COQMU'' ET ''THER_COQUE'' '
     +             //'POUR LES COQUES THERMIQUES .')
       ENDIF
C
C --- PRISE EN COMPTE DANS LE TENSEUR DE CONDUCTIVITE TRANSVERSE DES
C --- ECHANGES DES PEAUX INFERIEURE ET SUPERIEURE AVEC L'EXTERIEUR :
C     ------------------------------------------------------------
C --- CAS OU LES COEFFICIENTS D'ECHANGES SONT DES FONCTIONS :
       IF (ICOEHF.GT.0) THEN
         IPOIDS=IFF+(3*NNO+1)*NPG1
         IVF   =IPOIDS+NPG2
         IDFDE =IVF   +NPG2*NNO
         IDFDK =IDFDE +NPG2*NNO
         DO 80 KP=1,NPG2
           K=(KP-1)*NNO
           CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                   COOR2D,DFDX,DFDY,POIDS )
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
           XGAU = ZERO
           YGAU = ZERO
           ZGAU = ZERO
           DO 90 I = 1,NNO
             XGAU = XGAU + ZR(IGEOM+3*(I-1)  ) * ZR(IVF+K+I-1)
             YGAU = YGAU + ZR(IGEOM+3*(I-1)+1) * ZR(IVF+K+I-1)
             ZGAU = ZGAU + ZR(IGEOM+3*(I-1)+2) * ZR(IVF+K+I-1)
 90        CONTINUE
C
           VALPAR(2) = XGAU
           VALPAR(3) = YGAU
           VALPAR(4) = ZGAU
           NBVAR = 4
           CALL FOINTE('FM',HFMOIN,NBVAR,NOMPAR,VALPAR,HMOIN,IER)
           CALL FOINTE('FM',HFPLUS,NBVAR,NOMPAR,VALPAR,HPLUS,IER)
 80      CONTINUE
       ENDIF
C
C ---  CONTRIBUTION AU TENSEUR DE CONDUCTIVITE TRANSVERSE B DES
C ---  ECHANGES AVEC L'EXTERIEUR
C ---             (0 0  0 )
C ---    B_ECH =  (0 H- 0 )
C ---             (0 0  H+)
C      --------------------
       B(2,2) = B(2,2) + HMOIN
       B(3,3) = B(3,3) + HPLUS
C
C --- CALCUL DES COORDONNEES DES CONNECTIVITES DANS LE REPERE
C --- DE L'ELEMENT :
C     ------------
       CALL CQ3D2D(NNO,ZR(IGEOM),UN,ZERO,COOR2D)
C
        IPOIDS=IFF
        IVF   =IPOIDS+NPG1
        IDFDE =IVF   +NPG1*NNO
        IDFDK =IDFDE +NPG1*NNO
C
C --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE MEMBRANAIRE
C --- DU TENSEUR DE CONDUCTIVITE :
C     ==========================
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 100 KP=1,NPG1
          K=(KP-1)*NNO
C
C ---   DERIVEES DES FONCTIONS DE FORME ET PRODUIT JACOBIEN*POIDS
C ---   (DANS POIDS)  SUR L'ELEMENT :
C       ---------------------------
          CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                 COOR2D,DFDX,DFDY,POIDS )
         DO 110 PI=1,3
           TPG(PI)    = ZERO
           DTPGDX(PI) = ZERO
           DTPGDY(PI) = ZERO
 110     CONTINUE
C
C ---   TEMPERATURES ET GRADIENTS THERMIQUES AUX POINTS D'INTEGRATION :
C       -------------------------------------------------------------
         DO 120 GI=1,NNO
           DO 130 PI=1,3
             I          = 3*(GI-1)+PI-1+ITEMP
             TPG(PI)    = TPG(PI)    + ZR(I)*ZR(IVF+K+GI-1)
             DTPGDX(PI) = DTPGDX(PI) + ZR(I)*DFDX(GI)
             DTPGDY(PI) = DTPGDY(PI) + ZR(I)*DFDY(GI)
 130       CONTINUE
 120     CONTINUE
         DO 140 GI= 1, NNO
           DO 150 GJ= 1, GI
             DO 160 PI = 1, 3
               DO 170 PJ = 1, PI
                  PK =  A(PI,PJ,1,1)  *DFDX(GI)*DFDX(GJ)
     +                + A(PI,PJ,2,2)  *DFDY(GI)*DFDY(GJ)
     +                + A(PI,PJ,1,2)  *DFDX(GI)*DFDY(GJ)
     +                + A(PI,PJ,1,2)  *DFDY(GI)*DFDX(GJ)
C
                  IF ((PI.NE.PJ).AND.(GI.NE.GJ)) THEN
                      I=3*(GI-1)+PJ
                      J=3*(GJ-1)+PI
                      RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                  ENDIF
                  I = 3*(GI-1) + PI
                  J = 3*(GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
 170           CONTINUE
 160         CONTINUE
 150       CONTINUE
 140     CONTINUE
 100   CONTINUE
C
C --- CALCUL DE LA RIGIDITE THERMIQUE DUE A LA PARTIE TRANSVERSE
C --- DU TENSEUR DE CONDUCTIVITE ET CALCUL DE LA MASSE THERMIQUE :
C     ==========================================================
C
C --- ON PREND LA SECONDE FAMILLE DE POINTS D'INTEGRATION QUI
C --- EST D'UN ORDRE PLUS ELEVE :
C     -------------------------
       IPOIDS=IDFDK + NPG1*NNO
       IVF   =IPOIDS+ NPG2
       IDFDE =IVF   + NPG2*NNO
       IDFDK =IDFDE + NPG2*NNO
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
       DO 180 KP=1,NPG2
         K=(KP-1)*NNO
         CALL DFDM2D ( NNO,ZR(IPOIDS+KP-1),ZR(IDFDE+K),ZR(IDFDK+K),
     +                 COOR2D,DFDX,DFDY,POIDS )
C
         DO 190 GI= 1, NNO
           IVF1 = IVF+K+GI-1
           DO 200 GJ= 1, GI
             IVF2 = IVF+K+GJ-1
             DO 210 PI = 1, 3
               DO 220 PJ=1,PI
                 PK = B(PI,PJ)*ZR(IVF1)*ZR(IVF2)
                 PM = M(PI,PJ)*ZR(IVF1)*ZR(IVF2)
C
C ---     AFFECTATION DES TERMES HORS DIAGONAUX DE LA TRIANGULAIRE
C ---     INFERIEURE DE LA SOUS-MATRICE :
C         -----------------------------
                 IF ((PI.NE.PJ).AND.(GI.NE.GJ)) THEN
                     I=3*(GI-1)+PJ
                     J=3*(GJ-1)+PI
                     RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                     MASSE(I,J)  = MASSE(I,J)  + POIDS*PM
                 ENDIF
C
C ---     AFFECTATION DES TERMES DE LA TRIANGULAIRE SUPERIEURE
C ---     DE LA SOUS-MATRICE :
C         ------------------
                  I = 3*(GI-1) + PI
                  J = 3*(GJ-1) + PJ
                  RIGITH(I,J) = RIGITH(I,J) + POIDS*PK
                  MASSE(I,J)  = MASSE(I,J)  + POIDS*PM
 220            CONTINUE
 210          CONTINUE
 200        CONTINUE
 190      CONTINUE
 180    CONTINUE
        DO 230 I = 1, 3*NNO
          DO 230 J = 1, I-1
            RIGITH(J,I) = RIGITH(I,J)
            MASSE(J,I)  = MASSE(I,J)
 230    CONTINUE
C..................................................................
C.    CAS DES BORDS DES COQUES SURFACIQUES                        .
C.    ILS INTERVIENNENT PAR LEUR CONTRIBUTION A L'ECHANGE LATERAL .
C..................................................................

C
      ELSEIF (NOMTE(1:8).EQ.'THCOSE3 '.OR.NOMTE(1:8).EQ.'THCOSE2 ') THEN
        CALL JEVETE('&INEL.'//ELREFE//'.DEMR',' ', MZR )
CCC     CALL JEVECH('PCACOQU','L',ICACOQ)
C
CCC     EP=ZR(ICACOQ)
C
        LONG=(ZR(IGEOM+3)-ZR(IGEOM))**2+(ZR(IGEOM+3+1)-ZR(IGEOM+1))**2
     &                                 +(ZR(IGEOM+3+2)-ZR(IGEOM+2))**2
C
        LONG = SQRT(LONG)/2.D0
C       EP  =EP/2.D0
C
C ---   DETERMINATION DE LA 'PART' DE RIGIDITE THERMIQUE DU A L'ECHANGE
C ---   AVEC L'EXTERIEUR POUR LES COQUES LINEIQUES
C ---   ON RAPPELLE QUE LE TERME GENERIQUE DE CETTE MATRICE A POUR
C ---   EXPRESSION :
C ---   B(I,J) = SOMME_VOLUME(H*NI(X,Y,Z)*NJ(X,Y,Z).DX.DY.DZ)
C ---   SOIT
C ---   B(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY)
C ---           *SOMME_EPAISSEUR(PK(Z)*PL(Z).DZ)
C ---   OU LES PK ET PL SONT LES FONCTIONS D'INTERPOLATION DANS
C ---   L'EPAISSEUR
C ---   PLUS EXACTEMENT P1(Z), P2(Z), P3(Z) SONT LES POLYNOMES
C ---   DE LAGRANGE (OU AUTRES) DE DEGRE 2 RELATIFS A L'INTERPOLATION
C ---   DE LA TEMPERATURE DANS L'EPAISSEUR TELS QUE
C ---   P1 EST RELATIF A LA TEMPERATURE MOYENNE
C ---   P2 EST RELATIF A LA TEMPERATURE INFERIEURE
C ---   P3 EST RELATIF A LA TEMPERATURE SUPERIEURE
C ---   (I.E. T(X,Y,Z) =    P1(Z)*TMOY(X,Y)
C ---                     + P2(Z)*TINF(X,Y)
C ---                     + P3(Z)*TSUP(X,Y)) :
C       ------------------------------------
        CALL R8INIR(9,ZERO,MATP,1)
        CALL R8INIR(9,ZERO,MATN,1)
C
C ---   DETERMINATION DE LA MATRICE MATP DONT LE TERME GENERIQUE
C ---   EST MATP(I,J) = SOMME_EPAISSEUR(PI(Z)*PJ(Z).DZ) :
C       -----------------------------------------------
        DO 240 KP=1,NPG2
           KQ=(KP-1)*3
C
           POI1=ZR(MZR-1+12+KP)
C
           MATP(1,1)=MATP(1,1)+POI1*ZR(MZR-1+KQ+1)**2
           MATP(1,2)=MATP(1,2)+POI1*ZR(MZR-1+KQ+1)*ZR(MZR-1+KQ+2)
           MATP(1,3)=MATP(1,3)+POI1*ZR(MZR-1+KQ+1)*ZR(MZR-1+KQ+3)
           MATP(2,1)=MATP(1,2)
           MATP(2,2)=MATP(2,2)+POI1*ZR(MZR-1+KQ+2)**2
           MATP(2,3)=MATP(2,3)+POI1*ZR(MZR-1+KQ+2)*ZR(MZR-1+KQ+3)
           MATP(3,1)=MATP(1,3)
           MATP(3,2)=MATP(2,3)
           MATP(3,3)=MATP(3,3)+POI1*ZR(MZR-1+KQ+3)**2
 240    CONTINUE
C
C ---   DETERMINATION DE LA MATRICE MATN DONT LE TERME GENERIQUE
C ---   EST MATN(I,J) = SOMME_LONGUEUR (H*NI(X,Y)*NJ(X,Y).DX.DY) :
C       --------------------------------------------------------
        DO 250 KP=1,NPG1
           K=(KP-1)*NNO
C
           POI2=ZR(IPOIDS-1+KP)
C
           IF (ICOEHF.GT.0) THEN
C
C ---   COORDONNEES DU POINT D'INTEGRATION COURANT :
C       ------------------------------------------
             XGAU = ZERO
             YGAU = ZERO
             ZGAU = ZERO
             DO 260 I = 1,NNO
               XGAU = XGAU + ZR(IGEOM+3*(I-1)  )*ZR(IVF+K+I-1)
               YGAU = YGAU + ZR(IGEOM+3*(I-1)+1)*ZR(IVF+K+I-1)
               ZGAU = ZGAU + ZR(IGEOM+3*(I-1)+2)*ZR(IVF+K+I-1)
 260         CONTINUE
C
             VALPAR(2) = XGAU
             VALPAR(3) = YGAU
             VALPAR(4) = ZGAU
             NBVAR = 4
C
             CALL FOINTE('FM',HFBORD,NBVAR,NOMPAR,VALPAR,HBORD,IER)
           ENDIF
C
C
C      IMPORTANT: LAMB = CONV * EPAISSEUR
C
C          LAMB=LAMB*LONG*THETA*EP
           HBORD=HBORD*LONG*THETA/2.D0
C
           MATN(1,1)=POI2*HBORD*ZR(IVF-1+K+1)**2
           MATN(1,2)=POI2*HBORD*ZR(IVF-1+K+1)*ZR(IVF-1+K+2)
           MATN(2,1)=MATN(1,2)
           MATN(2,2)=POI2*HBORD*ZR(IVF-1+K+2)**2
C
           IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN
               MATN(1,3)=POI2*HBORD*ZR(IVF-1+K+1)*ZR(IVF-1+K+3)
               MATN(2,3)=POI2*HBORD*ZR(IVF-1+K+2)*ZR(IVF-1+K+3)
               MATN(3,1)=MATN(1,3)
               MATN(3,2)=MATN(2,3)
               MATN(3,3)=POI2*HBORD*ZR(IVF-1+K+3)**2
           ENDIF
C
           RIGITH(1,1)=RIGITH(1,1)+MATN(1,1)*MATP(1,1)
           RIGITH(1,2)=RIGITH(1,2)+MATN(1,1)*MATP(1,2)
           RIGITH(1,3)=RIGITH(1,3)+MATN(1,1)*MATP(1,3)
           RIGITH(1,4)=RIGITH(1,4)+MATN(1,2)*MATP(1,1)
           RIGITH(1,5)=RIGITH(1,5)+MATN(1,2)*MATP(1,2)
           RIGITH(1,6)=RIGITH(1,6)+MATN(1,2)*MATP(1,3)
C
           RIGITH(2,1)=RIGITH(1,2)
           RIGITH(2,2)=RIGITH(2,2)+MATN(1,1)*MATP(2,2)
           RIGITH(2,3)=RIGITH(2,3)+MATN(1,1)*MATP(2,3)
           RIGITH(2,4)=RIGITH(2,4)+MATN(1,2)*MATP(2,1)
           RIGITH(2,5)=RIGITH(2,5)+MATN(1,2)*MATP(2,2)
           RIGITH(2,6)=RIGITH(2,6)+MATN(1,2)*MATP(2,3)
C
           RIGITH(3,1)=RIGITH(1,3)
           RIGITH(3,2)=RIGITH(2,3)
           RIGITH(3,3)=RIGITH(3,3)+MATN(1,1)*MATP(3,3)
           RIGITH(3,4)=RIGITH(3,4)+MATN(1,2)*MATP(3,1)
           RIGITH(3,5)=RIGITH(3,5)+MATN(1,2)*MATP(3,2)
           RIGITH(3,6)=RIGITH(3,6)+MATN(1,2)*MATP(3,3)
C
           RIGITH(4,1)=RIGITH(1,4)
           RIGITH(4,2)=RIGITH(2,4)
           RIGITH(4,3)=RIGITH(3,4)
           RIGITH(4,4)=RIGITH(4,4)+MATN(2,2)*MATP(1,1)
           RIGITH(4,5)=RIGITH(4,5)+MATN(2,2)*MATP(1,2)
           RIGITH(4,6)=RIGITH(4,6)+MATN(2,2)*MATP(1,3)
C
           RIGITH(5,1)=RIGITH(1,5)
           RIGITH(5,2)=RIGITH(2,5)
           RIGITH(5,3)=RIGITH(3,5)
           RIGITH(5,4)=RIGITH(4,5)
           RIGITH(5,5)=RIGITH(5,5)+MATN(2,2)*MATP(2,2)
           RIGITH(5,6)=RIGITH(5,6)+MATN(2,2)*MATP(2,3)
C
           RIGITH(6,1)=RIGITH(1,6)
           RIGITH(6,2)=RIGITH(2,6)
           RIGITH(6,3)=RIGITH(3,6)
           RIGITH(6,4)=RIGITH(4,6)
           RIGITH(6,5)=RIGITH(5,6)
           RIGITH(6,6)=RIGITH(6,6)+MATN(2,2)*MATP(3,3)
C
           IF (NOMTE(1:8).EQ.'THCOSE3 ') THEN
C
             RIGITH(1,7)=RIGITH(1,7)+MATN(1,3)*MATP(1,1)
             RIGITH(1,8)=RIGITH(1,8)+MATN(1,3)*MATP(1,2)
             RIGITH(1,9)=RIGITH(1,9)+MATN(1,3)*MATP(1,3)
C
             RIGITH(2,7)=RIGITH(2,7)+MATN(1,3)*MATP(2,1)
             RIGITH(2,8)=RIGITH(2,8)+MATN(1,3)*MATP(2,2)
             RIGITH(2,9)=RIGITH(2,9)+MATN(1,3)*MATP(2,3)
C
             RIGITH(3,7)=RIGITH(3,7)+MATN(1,3)*MATP(3,1)
             RIGITH(3,8)=RIGITH(3,8)+MATN(1,3)*MATP(3,2)
             RIGITH(3,9)=RIGITH(3,9)+MATN(1,3)*MATP(3,3)
C
             RIGITH(4,7)=RIGITH(4,7)+MATN(2,3)*MATP(1,1)
             RIGITH(4,8)=RIGITH(4,8)+MATN(2,3)*MATP(1,2)
             RIGITH(4,9)=RIGITH(4,9)+MATN(2,3)*MATP(1,3)
C
             RIGITH(5,7)=RIGITH(5,7)+MATN(2,3)*MATP(2,1)
             RIGITH(5,8)=RIGITH(5,8)+MATN(2,3)*MATP(2,2)
             RIGITH(5,9)=RIGITH(5,9)+MATN(2,3)*MATP(2,3)
C
             RIGITH(6,7)=RIGITH(6,7)+MATN(2,3)*MATP(3,1)
             RIGITH(6,8)=RIGITH(6,8)+MATN(2,3)*MATP(3,2)
             RIGITH(6,9)=RIGITH(6,9)+MATN(2,3)*MATP(3,3)
C
             RIGITH(7,1)=RIGITH(1,7)
             RIGITH(7,2)=RIGITH(2,7)
             RIGITH(7,3)=RIGITH(3,7)
             RIGITH(7,4)=RIGITH(4,7)
             RIGITH(7,5)=RIGITH(5,7)
             RIGITH(7,6)=RIGITH(6,7)
             RIGITH(7,7)=RIGITH(7,7)+MATN(3,3)*MATP(1,1)
             RIGITH(7,8)=RIGITH(7,8)+MATN(3,3)*MATP(1,2)
             RIGITH(7,9)=RIGITH(7,9)+MATN(3,3)*MATP(1,3)
C
             RIGITH(8,1)=RIGITH(1,8)
             RIGITH(8,2)=RIGITH(2,8)
             RIGITH(8,3)=RIGITH(3,8)
             RIGITH(8,4)=RIGITH(4,8)
             RIGITH(8,5)=RIGITH(5,8)
             RIGITH(8,6)=RIGITH(6,8)
             RIGITH(8,7)=RIGITH(7,8)
             RIGITH(8,8)=RIGITH(8,8)+MATN(3,3)*MATP(2,2)
             RIGITH(8,9)=RIGITH(8,9)+MATN(3,3)*MATP(2,3)
C
             RIGITH(9,1)=RIGITH(1,9)
             RIGITH(9,2)=RIGITH(2,9)
             RIGITH(9,3)=RIGITH(3,9)
             RIGITH(9,4)=RIGITH(4,9)
             RIGITH(9,5)=RIGITH(5,9)
             RIGITH(9,6)=RIGITH(6,9)
             RIGITH(9,7)=RIGITH(7,9)
             RIGITH(9,8)=RIGITH(8,9)
             RIGITH(9,9)=RIGITH(9,9)+MATN(3,3)*MATP(3,3)
           ENDIF
C
 250    CONTINUE
C
C..................................................................
      ENDIF
C..................................................................
C
C --- RECUPERATION DU VECTEUR SECOND MEMBRE EN SORTIE DE CHAR_THER_EVOL:
C     ----------------------------------------------------------------
      CALL JEVECH('PVECTTR','E',IVECTT)
C
C --- AFFECTATION DU VECTEUR SECOND MEMBRE EN SORTIE DE
C --- CHAR_THER_EVOL :
C     --------------
      NBDDL = 3*NNO
      DO 270 I = 1, NBDDL
        DO 280 J = 1, NBDDL
           ZR(IVECTT+I-1) = ZR(IVECTT+I-1) +
     +                 (MASSE(J,I)/DELTAT-(UN-THETA)*RIGITH(J,I))
     +                *ZR(ITEMP+J-1)
 280    CONTINUE
 270  CONTINUE
C
      END
