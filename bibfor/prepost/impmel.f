      SUBROUTINE IMPMEL ( FICHIE, NOMSDZ, GRAINZ, NBMA, LISMAZ,
     +                    NBCMP, LISCMZ, NBCHIF )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 07/04/98   AUTEUR CIBHHGB G.BERTRAND 
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
C      IMPMEL -- IMPRESSION DU MATR_ELEM DE NOM NOMSD
C                DANS LE FICHIER DE NOM FICHIER.
C                
C
C   ARGUMENT        E/S  TYPE         ROLE
C    FICHIE          IN    K*     NOM DU FICHIER OU L'ON IMPRIME
C                                 LE MATR_ELEM
C    NOMSDZ          IN    K*     NOM DU MATR_ELEM
C    GRAINZ          IN    K*     'GRAIN' DE L'IMPRESSION
C                                     = 'VALEUR'
C                                  OU = 'NOEUD'
C                                  OU = 'MAILLE'
C                                  SI = 'VALEUR' ON IMPRIME UNE VALEUR
C                                                PAR LIGNE
C                                  SI = 'NOEUD' LE GRAIN D'IMPRESSION 
C                                               EST CONSTITUE PAR UNE
C                                               SOUS-MATRICE DONT LES 
C                                               TERMES COUPLENT 2 NOEUDS
C                                  SI = 'MAILLE' LE GRAIN D'IMPRESSION 
C                                               EST CONSTITUE PAR LA
C                                               MATRICE ELLE-MEME 
C    NBMA            IN    I     NOMBRE DE MAILLES DE LA LISTE LISMAZ
C                                SI = 0 LA LISTE LISMAZ EST VIDE ET
C                                L'ON PREND EN COMPTE TOUTES LES
C                                MAILLES DU MATR_ELEM
C    LISMAZ          IN    K*    LISTE DES MAILLES POUR-LESQUELS ON
C                                DESIRE L'IMPRESSION DES VALEURS
C                                DES MATRICES DU MATR_ELEM
C    NBCMP           IN    I     NOMBRE DE COMPOSANTES DE LA LISTE
C                                LISCMZ DES COMPOSANTES
C    LISCMZ          IN    K*    LISTE DES COMPOSANTES POUR-LESQUELLES
C                                ON DESIRE L'IMPRESSION DES VALEURS
C                                DE LA MATRICE
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES A METTRE APRES
C                                LA VIRGULE DANS LE FORMAT D'IMPRESSION
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
      CHARACTER*(*) FICHIE, NOMSDZ, GRAINZ, LISMAZ, LISCMZ
C -----  VARIABLES LOCALES
      CHARACTER*6   GRAIN 
      CHARACTER*19  NOMSD
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      CALL JEMARQ ( )
C
C --- INITIALISATIONS :
C     ---------------
      GRAIN  = GRAINZ

C
C ---   IMPRESSION SELON LE GRAIN NOEUD :
C       -------------------------------
          IF (GRAIN(1:5).EQ.'NOEUD') THEN
              CALL IMELNN (FICHIE, NOMSDZ, NBCMP, LISCMZ,
     +                     NBMA, LISMAZ, NBCHIF )
C
C ---   IMPRESSION SELON LE GRAIN VALEUR :
C       --------------------------------
          ELSEIF (GRAIN.EQ.'VALEUR') THEN
              CALL IMELNV (FICHIE, NOMSDZ, NBCMP, LISCMZ,
     +                     NBMA, LISMAZ, NBCHIF )
C
C ---   IMPRESSION SELON LE GRAIN MAILLE :
C       --------------------------------
          ELSEIF (GRAIN.EQ.'MAILLE') THEN
              CALL IMELNM (FICHIE, NOMSDZ, NBCMP, LISCMZ,
     +                     NBMA, LISMAZ, NBCHIF )
          ELSE
              CALL UTMESS ('F','IMPMEL','LA VALEUR DU GRAIN '//
     +                     'D''IMPRESSION EST '//GRAIN//' ALORS '//
     +                     'QUE LES SEULES VALEURS POSSIBLES SONT '//
     +                     '"NOEUD" OU "VALEUR"  OU "MAILLE".')
          ENDIF
C
      CALL JEDEMA()
C
      END
