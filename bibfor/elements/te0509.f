      SUBROUTINE TE0509 ( OPTION , NOMTE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*16        OPTION , NOMTE
C ......................................................................
C    - FONCTION REALISEE:  CALCUL DES CARACTERISTIQUES SUIVANTES :
C               .LA CONSTANTE DE TORSION         (OPTION 'CARA_TORSION')
C
C              .LE CENTRE DE TORSION/CISAILLEMENT
C              .LES COEFFICIENTS DE CISAILLEMENT (OPTION 'CARA_CISA')
C
C               .L'INERTIE DE GAUCHISSEMENT      (OPTION 'CARA_GAUCHI')
C
C          .LE DOMAINE SUR-LEQUEL ON TRAVAILLE REPRESENTE LA
C           SECTION DE LA POUTRE MAILLEE AVEC DES ELEMENTS 2D
C           ISOPARAMETRIQUES THERMIQUES (THERMIQUES CAR ON
C           DOIT RESOUDRE DES EQUATIONS DE LAPLACE).
C
C-------------------------------------------------------------------
C  OPTION : 'CARA_TORSION' :
C
C          .LA CONSTANTE DE TORSION CT EST DETERMINEE EN FAISANT
C           LA RESOLUTION DE L'EQUATION :
C                LAPLACIEN(PHI) = -2     DANS LA SECTION
C       AVEC     PHI = 0                 SUR LE CONTOUR DE LA SECTION
C           ON A ALORS CT = 2*SOMME_S(PHI.DS)
C
C-------------------------------------------------------------------
C  OPTION : 'CARA_CISA' :
C
C          .LES COEFFICIENTS DE CISAILLEMENT AY ET AZ SONT
C           DETERMINES EN FAISANT RESPECTIVEMENT LA RESOLUTION
C           DE L' EQUATION :
C                G*LAPLACIEN(PSI_Z) = -Z*TZ/IY     DANS LA SECTION
C       AVEC     D(PSI_Z )/DN = 0     SUR LE CONTOUR DE LA SECTION
C       ET       PSI_Z = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
C
C           ET DE L' EQUATION :
C                G*LAPLACIEN(PSI_Y) = -Y*TY/IZ     DANS LA SECTION
C       AVEC     D(PSI_Y )/DN = 0     SUR LE CONTOUR DE LA SECTION
C       ET       PSI_Y = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION
C
C               AY = 2*S*U1_Y/TY**2
C               AZ = 2*S*U1_Z/TZ**2
C       AVEC U1_Y = 0.5*SOMME_S(G*(GRAD(PSI_Y)**2).DS)
C       AVEC U1_Z = 0.5*SOMME_S(G*(GRAD(PSI_Z)**2).DS)
C
C          X DESIGNE L'AXE DE LA POUTRE
C          Y ET Z DESIGNENT LES AXES PRINCIPAUX D'INERTIE
C          DU PLAN DE LA SECTION
C          DANS LE ROUTINE CES AXES SERONT NOMMES RESPECTIVEMENT X ET Y
C          L'ORIGINE DES AXES DE COORDONNEESEST SITUEE AU CENTRE DE
C          GRAVITE DE LA SECTION
C          N DESIGNE LE VECTEUR NORMAL A LA FRONTIERE
C
C         TY ET TZ DESIGNENT LES EFFORTS TRANCHANTS
C         ON PREND TY = 1 ET TZ = 1
C         ON FAIT L'HYPOTHESE QUE LE MATERIAU EST ISOTROPE
C         AUQUEL CAS LE MODULE DE CISAILLEMENT G N'INTERVIENT PAS
C         LES INERTIES IY ET IZ SONT PRISES EN COMPTE ULTERIEUREMENT
C         AU MOMENT OU L'ON FAIT LA SOMMATION SUR LA SECTION TOTALE
C         DES QUANTITES ELEMENTAIRES.
C
C
C          .LES COORDONNEES DU CENTRE DE TORSION/CISAILLEMENT
C           SONT EGALES A :
C             EY =  MX0_Y/TZ
C             EZ = -MX0_Z/TY
C
C           AVEC MX0_Y = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
C           SACHANT QUE SIGMA_XY = G*D(PSI_Z)/DY
C                   ET  SIGMA_XZ = G*D(PSI_Z)/DZ
C
C           ET  MX0_Z = SOMME_S((SIGMA_XZ*Y - SIGMA_XY*Z).DS)
C           SACHANT QUE SIGMA_XY = G*D(PSI_Y)/DY
C                   ET  SIGMA_XZ = G*D(PSI_Y)/DZ
C
C-------------------------------------------------------------------
C  OPTION : 'CARA_GAUCHI' :
C
C          .LA CONSTANTE DE GAUCHISSEMENT I_OMEGA EST DETERMINEE
C           EN FAISANT LA RESOLUTION DE L'EQUATION :
C
C                LAPLACIEN(OMEGA) = 0     DANS LA SECTION
C          AVEC :
C     1) D(OMEGA)/D(N) = Z*NY-Y*NZ   SUR LE CONTOUR DE LA SECTION
C     NY ET NZ ETANT LES COMPOSANTES DU VECTEUR N NORMAL A CE CONTOUR
C
C     2) SOMME_S(OMEGA.DS) = 0
C        (VOIR L'ENTETE DE LA ROUTINE PECAP3 POUR PLUS D'EXPLICATIONS)
C
C           ON A ALORS I_OMEGA = SOMME_S(OMEGA**2.DS)
C
C           OMEGA  EST LA FONCTION DE GAUCHISSEMENT
C           I_OMEGA EST L'INERTIE DE GAUCHISSEMENT
C
C-------------------------------------------------------------------
C
C    - ARGUMENTS:
C        DONNEES:      OPTION       -->  OPTION DE CALCUL
C                      NOMTE        -->  NOM DU TYPE ELEMENT
C ......................................................................
C
      INTEGER            NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO
      REAL*8             MX0Y, MX0Z
      REAL*8             DFDX(9),DFDY(9)
C
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
C --- INITIALISATIONS :
C     ---------------
      CALL ELREF4(' ','RIGI',NDIM,NNO,NNOS,NPG,IPOIDS,IVF,IDFDE,JGANO)
      ZERO   = 0.0D0
      SPHIDS = ZERO
      MX0Y   = ZERO
      MX0Z   = ZERO
      U1Y    = ZERO
      U1Z    = ZERO
      SOMEG2 = ZERO
C
C --- RECUPERATION DES COORDONNEES DES CONNECTIVITES DE L'ELEMENT :
C     -----------------------------------------------------------
      CALL JEVECH('PGEOMER','L',IGEOM)
C
C --- RECUPERATION DU CHAMP DE SACLAIRES EN SORTIE DU TE :
C     --------------------------------------------------
      CALL JEVECH('PCASECT','E',ICASE)
C
C----------------------------------------
C --- OPTION : 'CARA_TORSION'           -
C --- CALCUL DE LA CONSTANTE DE TORSION -
C----------------------------------------
      IF (OPTION.EQ.'CARA_TORSION') THEN
C
C   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
C   --- L'EQUATION  : LAPLACIEN(PHI) = -2     DANS LA SECTION
C   --- AVEC          PHI = 0     SUR LE CONTOUR DE LA SECTION :
C       ------------------------------------------------------
        CALL JEVECH('PTEMPER','L',ITEMPE)
C
C         -----------------------------------
C   ---   -CALCUL DE SOMME/S_ELEMENT(PHI.DS) :
C         -----------------------------------
C
C   --- BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
          DO 10 IGAU = 1, NPG
            K=(IGAU-1)*NNO
C
C   ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
C   ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C   ---    AU POINT D'INTEGRATION COURANT :
C          ------------------------------
            CALL DFDM2D(NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                  POIDS)
C
C   ---    CALCUL DE SOMME/S_ELEMENT(PHI.DS) :
C          ---------------------------------
            DO 20 INO = 1, NNO
               SPHIDS = SPHIDS + ZR(IVF+K+INO-1)*ZR(ITEMPE+INO-1)*POIDS
 20         CONTINUE
 10       CONTINUE
C
C   --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE
C   --- A LA VALEUR LA VALEUR SOMME/S_ELEMENT(PHI.DS) :
C       ---------------------------------------------
          ZR(ICASE) = SPHIDS
C
C--------------------------------------------------------------
C --- OPTION : 'CARA_CISA'                                    -
C --- CALCUL DES COORDONNES DU CENTRE DE CISAILLEMENT/TORSION -
C --- ET DES COEFFICIENTS DE CISAILLEMENT                     -
C--------------------------------------------------------------
      ELSEIF (OPTION.EQ.'CARA_CISA') THEN
C
C   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
C   --- L'EQUATION  : LAPLACIEN(PSI_Y) = -Y    DANS LA SECTION
C   --- AVEC  D(PSI_Y )/DN = 0  SUR LE CONTOUR DE LA SECTION
C   --- (C'EST LA CONDITION PAR DEFAUT)
C   --- ET PSI_Y = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION :
C       ----------------------------------------------------
        CALL JEVECH('PTEMPE1','L',ITEMP1)
C
C       ---------------------------------------------------------------
C       -CALCUL DE MX0_Z=SOMME/S_ELEMENT((SIGMA_XZ*Y - SIGMA_XY*Z).DS)-
C ---   - AVEC SIGMA_XY = D(PSI_Y)/DY                                 -
C       -  ET  SIGMA_XZ = D(PSI_Y)/DZ                                 -
C       ---------------------------------------------------------------
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 30 IGAU = 1, NPG
          K=(IGAU-1)*NNO
C
C ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
C ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C ---    AU POINT D'INTEGRATION COURANT :
C        ------------------------------
          CALL DFDM2D(NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                POIDS)
C
C ---    CALCUL DES CONTRAINTES SIGMA_XY = D(PSI_Y)/DY ET
C ---    SIGMA_XZ = D(PSI_Y)/DZ  AU POINT D'INTEGRATION COURANT :
C        ------------------------------------------------------
         SIGMXY = ZERO
         SIGMXZ = ZERO
         XGAU   = ZERO
         YGAU   = ZERO
C
          DO 40 INO = 1, NNO
             I = IGEOM + 2*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+K+INO-1)*ZR(I+1)
             YGAU = YGAU + ZR(IVF+K+INO-1)*ZR(I+2)
C
             SIGMXY = SIGMXY + DFDX(INO)*ZR(ITEMP1+INO-1)
             SIGMXZ = SIGMXZ + DFDY(INO)*ZR(ITEMP1+INO-1)
 40       CONTINUE
C
C ---    CALCUL DE SOMME/S_ELEMENT(SIGMA_XZ*X - SIGMA_XY*Y).DS)
C ---    (Y EST DEVENU X ET Z EST DEVENU Y) :
C        ----------------------------------
         MX0Z = MX0Z + (SIGMXZ*XGAU - SIGMXY*YGAU)*POIDS
 30     CONTINUE
C
C --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC LA COORDONNEE
C --- SELON Z DU CENTRE DE CISAILLEMENT/TORSION :
C     -----------------------------------------
        EZ = -MX0Z
        ZR(ICASE+2-1) = EZ
C
C --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
C --- L'EQUATION  : LAPLACIEN(PSI_Z) = -Z    DANS LA SECTION
C --- AVEC  D(PSI_Z )/DN = 0  SUR LE CONTOUR DE LA SECTION
C --- (C'EST LA CONDITION PAR DEFAUT)
C --- ET PSI_Z = 0    EN UN NOEUD ARBITRAIRE DE LA SECTION :
C     ----------------------------------------------------
        CALL JEVECH('PTEMPE2','L',ITEMP2)
C
C       ---------------------------------------------------------------
C       -CALCUL DE MX0_Y=SOMME/S_ELEMENT((SIGMA_XZ*Y - SIGMA_XY*Z).DS)-
C ---   - AVEC SIGMA_XY = D(PSI_Z)/DY                                 -
C       -  ET  SIGMA_XZ = D(PSI_Z)/DZ                                 -
C       ---------------------------------------------------------------
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 50 IGAU = 1, NPG
          K=(IGAU-1)*NNO
C
C ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
C ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C ---    AU POINT D'INTEGRATION COURANT :
C        ------------------------------
          CALL DFDM2D(NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)
C
C ---    CALCUL DES CONTRAINTES SIGMA_XY = D(PSI_Z)/DY ET
C ---    SIGMA_XZ = D(PSI_Z)/DZ  AU POINT D'INTEGRATION COURANT :
C        ------------------------------------------------------
         SIGMXY = ZERO
         SIGMXZ = ZERO
         XGAU   = ZERO
         YGAU   = ZERO
C
          DO 60 INO = 1, NNO
             I = IGEOM + 2*(INO-1) -1
C
             XGAU = XGAU + ZR(IVF+K+INO-1)*ZR(I+1)
             YGAU = YGAU + ZR(IVF+K+INO-1)*ZR(I+2)
C
             SIGMXY = SIGMXY + DFDX(INO)*ZR(ITEMP2+INO-1)
             SIGMXZ = SIGMXZ + DFDY(INO)*ZR(ITEMP2+INO-1)
 60       CONTINUE
C
C ---    CALCUL DE SOMME/S_ELEMENT(SIGMA_XZ*X - SIGMA_XY*Y).DS)
C ---    (Y EST DEVENU X ET Z EST DEVENU Y) :
C        ----------------------------------
          MX0Y = MX0Y + (SIGMXZ*XGAU - SIGMXY*YGAU)*POIDS
 50     CONTINUE
C
C --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC LA COORDONNEE
C --- SELON Z DU CENTRE DE CISAILLEMENT/TORSION :
C     -----------------------------------------
        EY = MX0Y
        ZR(ICASE+1-1) = EY
C
C----------------------------------------------
C --- CALCUL DES COEFFICIENTS DE CISAILLEMENT -
C----------------------------------------------
C
C ---  CALCUL DE U1_Y =  SOMME_S_ELEMENT(GRAD(PSI_Y)**2.DS)
C ---  ET        U1_Z =  SOMME_S_ELEMENT(GRAD(PSI_Z)**2.DS) :
C       ---------------------------------------------------
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION :
C     -----------------------------------
        DO 70 IGAU = 1, NPG
          K=(IGAU-1)*NNO*2
C
C ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
C ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C ---    AU POINT D'INTEGRATION COURANT :
C        ------------------------------
          CALL DFDM2D(NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,POIDS)

C
C ---    CALCUL D(PSI_Y)/DY, D(PSI_Y)/DZ ET D(PSI_Z)/DY, D(PSI_Z)/DZ
C ---    AU POINT D'INTEGRATION COURANT :
C        -----------------------------
         DPSYDY = ZERO
         DPSYDZ = ZERO
         DPSZDY = ZERO
         DPSZDZ = ZERO
C
          DO 80 INO = 1, NNO
C
             DPSYDY = DPSYDY + DFDX(INO)*ZR(ITEMP1+INO-1)
             DPSYDZ = DPSYDZ + DFDY(INO)*ZR(ITEMP1+INO-1)
C
             DPSZDY = DPSZDY + DFDX(INO)*ZR(ITEMP2+INO-1)
             DPSZDZ = DPSZDZ + DFDY(INO)*ZR(ITEMP2+INO-1)
 80       CONTINUE
C
C ---    CALCUL DE U1_Y ET U1_Z :
C        ----------------------
             U1Y = U1Y + (DPSYDY*DPSYDY + DPSYDZ*DPSYDZ)*POIDS
             U1Z = U1Z + (DPSZDY*DPSZDY + DPSZDZ*DPSZDZ)*POIDS
 70     CONTINUE
C
C --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE AVEC U1Y ET U1Z
C --- QUI SONT LES CONTRIBUTIONS DE L'ELEMENT AUX COEFFICIENTS DE
C --- CISAILLEMENT DE LA POUTRE A UN COEFFICIENT MULTIPLICATIF PRES :
C     -------------------------------------------------------------
        ZR(ICASE+3-1) = U1Z
        ZR(ICASE+4-1) = U1Y
C----------------------------------------------
C --- OPTION : 'CARA_GAUCHI'                  -
C --- CALCUL DE LA CONSTANTE DE GAUCHISSEMENT -
C --- SOMME/S__ELEMENT(OMEGA**2.DS)           -
C----------------------------------------------
      ELSEIF (OPTION.EQ.'CARA_GAUCHI') THEN
C
C   --- RECUPERATION DU CHAMP D'INCONNUES SCALAIRES SOLUTION DE
C   --- L'EQUATION  : LAPLACIEN(OMEGA) = 0     DANS LA SECTION
C   --- AVEC     1) D(OMEGA)/D(N) = Z*NY-Y*NZ
C   --- SUR LE CONTOUR DE LA SECTION
C   --- NY ET NZ ETANT LES COMPOSANTES DU VECTEUR N NORMAL A CE CONTOUR
C   ---     ET   2) SOMME_S(OMEGA.DS) = 0 :
C       ---------------------------------
        CALL JEVECH('PTEMPER','L',ITEMPE)
C
C         ----------------------------------------
C   ---   -CALCUL DE SOMME/S_ELEMENT(OMEGA**2.DS) :
C         ----------------------------------------
C
C   --- BOUCLE SUR LES POINTS D'INTEGRATION :
C       -----------------------------------
          DO 90 IGAU = 1, NPG
            K=(IGAU-1)*NNO
C
C   ---    CALCUL DES DERIVEES DES FONCTIONS DE FORME  ET DU PRODUIT
C   ---    JACOBIEN*POIDS_INTEGRATION (DANS LA VARIABLE POIDS)
C   ---    AU POINT D'INTEGRATION COURANT :
C          ------------------------------
            CALL DFDM2D(NNO,IGAU,IPOIDS,IDFDE,ZR(IGEOM),DFDX,DFDY,
     &                  POIDS)
C
C   ---    CALCUL DE SOMME/S_ELEMENT(OMEGA**2.DS) :
C          --------------------------------------
            DO 100 INO = 1, NNO
               SOMEG2 = SOMEG2 + ZR(IVF+K+INO-1)*
     +                           ZR(ITEMPE+INO-1)*ZR(ITEMPE+INO-1)*POIDS
 100        CONTINUE
 90       CONTINUE
C
C   --- AFFECTATION DU CHAMP DE SCALAIRES EN SORTIE
C   --- A LA VALEUR LA VALEUR SOMME/S_ELEMENT(OMEGA**2.DS) :
C       --------------------------------------------------
          ZR(ICASE+1-1) = SOMEG2
      ENDIF
C
      END
