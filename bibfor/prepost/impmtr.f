      SUBROUTINE IMPMTR ( IFM, NOMSDZ, GRAINZ, OPTIOZ, NBNO, LISNOZ,
     &                    NBCMP, LISCMZ, NBCHIF, EPS )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      IMPMTR -- IMPRESSION DE LA MATR_ASSE DE NOM NOMSD
C                DANS LE FICHIER DE NOM FICHIER.
C
C
C   ARGUMENT        E/S  TYPE         ROLE
C    IFM             IN    I      UNITE LOGIQUE D'IMPRESSION DE
C                                 LA MATR_ASSE,
C    NOMSDZ          IN    K*     NOM DE LA MATR_ASSE
C    GRAINZ          IN    K*     'GRAIN' DE L'IMPRESSION
C                                     = 'VALEUR'
C                                  OU = 'NOEUD'
C                                  SI = 'VALEUR' ON IMPRIME UNE VALEUR
C                                                PAR LIGNE
C                                  SI = 'NOEUD' LE GRAIN D'IMPRESSION
C                                               EST CONSTITUE PAR UNE
C                                               SOUS-MATRICE DONT LES
C                                               TERMES COUPLENT 2 NOEUDS
C    OPTIOZ          IN    K*     OPTION D'IMPRESSION
C                                     = 'SOUS_MATRICE'
C                                  OU = 'LIGNE'
C                                  OU = 'COLONNE'
C                                  SI = 'SOUS_MATRICE' ON IMPRIME
C                                       UNIQUEMENT LES COMPOSANTES
C                                       LICITES DES NOEUDS DE LA LISTE
C                                       LISNOZ ET SI CELLE-CI EST VIDE
C                                       (NBNO = 0)
C                                       ON IMPRIME CELLES DE TOUS LES
C                                       NOEUDS (PHYSIQUES) DU MAILLAGE
C                                  SI = 'LIGNE' ON IMPRIME TOUTES LES
C                                       LES LIGNES RELATIVES AUX
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST
C                                       VIDE (NBNO =0 )
C                                  SI = 'COLONNE' ON IMPRIME TOUTES LES
C                                       LES COLONNES RELATIVES AUX
C                                       COMPOSANTES LICITES DES NOEUDS
C                                       DE LA LISTE LISNOZ ET DE TOUS
C                                       LES NOEUDS SI CETTE LISTE EST
C                                       VIDE (NBNO =0)
C    NBNO            IN    I     NOMBRE DE NOEUDS DE LA LISTE LISNOZ
C                                SI = 0 LA LISTE LISNOZ EST VIDE ET
C                                L'ON PREND EN COMPTE TOUS LES NOEUDS
C                                (PHYSIQUES) DU MAILLAGE
C    LISNOZ          IN    K*    LISTE DES NOEUDS POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES A METTRE APRES
C                                LA VIRGULE DANS LE FORMAT D'IMPRESSION
C    EPS             IN    R     PRECISION SERVANT A TESTER LA NULLITE
C                                D'UN TERME DE LA MATRICE
C.========================= DEBUT DES DECLARATIONS ====================
C ----- COMMUNS NORMALISES  JEVEUX
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM, JEXNOM
C -----  ARGUMENTS
      CHARACTER*(*)  NOMSDZ, GRAINZ, OPTIOZ, LISNOZ, LISCMZ
C -----  VARIABLES LOCALES
      CHARACTER*6   GRAIN
      CHARACTER*19  NOMSD
      INTEGER        TYPSYM, TYPVAL
C.========================= DEBUT DU CODE EXECUTABLE ==================

      CALL JEMARQ ( )

C --- INITIALISATIONS :
C     ---------------
      NOMSD  = NOMSDZ
      GRAIN  = GRAINZ

C --- CONSTRUCTION DU DESCRIPTEUR DE LA MATRICE :
C     -----------------------------------------
      CALL MTDSCR(NOMSD)

C --- RECUPERATION DU DESCRIPTEUR DE LA MATRICE :
C     -----------------------------------------
      CALL JEVEUO(NOMSD//'.&INT','L',LMAT)

C --- INDICATEUR DU TYPE DE VALEURS (REEL OU COMPLEXE) DE LA MATRICE:
C     --------------------------------------------------------------
      TYPVAL = ZI(LMAT+3)

C --- INDICATEUR DE LA SYMETRIE DE LA MATRICE :
C     ---------------------------------------
      TYPSYM = ZI(LMAT+4)


C --- CAS D'UNE MATR_ASSE A VALEURS REELLES :
C     -------------------------------------
      IF (TYPVAL.EQ.1) THEN

C ---  CAS D'UNE MATR_ASSE SYMETRIQUE :
C      ------------------------------
       IF (TYPSYM.EQ.1) THEN

C ---     IMPRESSION SELON LE GRAIN NOEUD :
C         -------------------------------
          IF (GRAIN(1:5).EQ.'NOEUD') THEN
              CALL IMRSMN (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )

C ---     IMPRESSION SELON LE GRAIN VALEUR :
C         --------------------------------
          ELSEIF (GRAIN.EQ.'VALEUR') THEN
              CALL IMRSMV (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )
          ELSE
              CALL U2MESK('F','PREPOST_66',1,GRAIN)
          ENDIF


C ---  CAS D'UNE MATR_ASSE NON-SYMETRIQUE :
C      ----------------------------------
       ELSEIF (TYPSYM.EQ.0) THEN
          IF (GRAIN(1:5).EQ.'NOEUD') THEN
              CALL IMRNMN (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )

C ---     IMPRESSION SELON LE GRAIN VALEUR :
C         --------------------------------
          ELSEIF (GRAIN.EQ.'VALEUR') THEN
              CALL IMRNMV (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )
          ELSE
              CALL U2MESK('F','PREPOST_66',1,GRAIN)
          ENDIF


       ELSE
              CALL U2MESS('F','PREPOST_67')
       ENDIF

C --- CAS D'UNE MATR_ASSE A VALEURS COMPLEXES :
C     ---------------------------------------
      ELSEIF (TYPVAL.EQ.2) THEN

C ---  CAS D'UNE MATR_ASSE SYMETRIQUE :
C      ------------------------------
       IF (TYPSYM.EQ.1) THEN

C ---     IMPRESSION SELON LE GRAIN NOEUD :
C         -------------------------------
          IF (GRAIN(1:5).EQ.'NOEUD') THEN
              CALL IMCSMN (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )

C ---     IMPRESSION SELON LE GRAIN VALEUR :
C         --------------------------------
          ELSEIF (GRAIN.EQ.'VALEUR') THEN
              CALL IMCSMV (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )
          ELSE
              CALL U2MESK('F','PREPOST_66',1,GRAIN)
          ENDIF


C ---  CAS D'UNE MATR_ASSE NON-SYMETRIQUE :
C      ----------------------------------
       ELSEIF (TYPSYM.EQ.0) THEN

C ---     IMPRESSION SELON LE GRAIN NOEUD :
C         -------------------------------
          IF (GRAIN(1:5).EQ.'NOEUD') THEN
              CALL IMCNMN (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )

C ---     IMPRESSION SELON LE GRAIN VALEUR :
C         --------------------------------
          ELSEIF (GRAIN.EQ.'VALEUR') THEN
              CALL IMCNMV (IFM, NOMSDZ, OPTIOZ, NBNO,
     &                     LISNOZ, NBCMP, LISCMZ, NBCHIF, EPS )
          ELSE
              CALL U2MESK('F','PREPOST_66',1,GRAIN)
          ENDIF


       ELSE
              CALL U2MESS('F','PREPOST_67')
       ENDIF
      ELSE
            CALL U2MESS('F','PREPOST_68')
      ENDIF

      CALL JEDEMA()

      END
