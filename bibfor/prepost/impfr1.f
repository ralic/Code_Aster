      SUBROUTINE IMPFR1 (NOMNOZ, NOMCMZ, LONLIS, NOMNOE, NOMCMP,
     +                   VALE, NBCHIF, FICHIE )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 05/11/96   AUTEUR CIBHHGB G.BERTRAND 
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
C      IMPFR1 -- IMPRESSION D'UNE LIGNE OU D'UNE COLONNE D'UNE
C                MATR_ASSE SELON LE GRAIN 'VALEUR'
C                (I.E. ON IMPRIME UNE SEULE VALEUR PAR LIGNE)
C                LA LIGNE OU LA COLONNE NE CONTIENT QUE LES VALEURS
C                DES COMPOSANTES LICITES DES NOEUDS LICITES
C                (LICITE VEUT DIRE 'CHOISI PAR L'UTILISATEUR')
C
C   ARGUMENT        E/S  TYPE         ROLE
C    NOMNOZ          IN   K*     NOM DU NOEUD POUR LEQUEL ON VA 
C                                IMPRIMER LA LIGNE DE LA MATRICE
C    NOMCMZ          IN   K*     NOM DE LA COMPOSANTE POUR LAQUELLLE  
C                                ON VA IMPRIMER LA LIGNE DE LA MATRICE
C    LONLIS          IN   I      LONGUEUR 'UTILE' DES TABLEAUX NOMNOE,
C                                NOMCMP ET  VALE
C    NOMNOE(*)       IN   K8     LISTE DES NOEUDS SUR-LESQUELS PORTENT
C                                LES TERMES DU TABLEAU VALE
C    NOMCMP(*)       IN   K8     LISTE DES COMPOSANTES SUR-LESQUELLES 
C                                PORTENT LES TERMES DU TABLEAU VALE
C    VALE(*)         IN   R      TABLEAU DES VALEURS DE LA LIGNE OU
C                                DE LA COLONNE A IMPRIMER 
C    GRAINZ          IN    K*     'GRAIN' DE L'IMPRESSION
C                                     = 'VALEUR'
C                                  OU = 'NOEUD'
C                                  SI = 'VALEUR' ON IMPRIME UNE VALEUR
C                                                PAR LIGNE
C                                  SI = 'NOEUD' LE GRAIN D'IMPRESSION 
C                                               EST CONSTITUE PAR UNE
C                                               SOUS-MATRICE DONT LES 
C                                               TERMES COUPLENT 2 NOEUDS
C    NBCHIF          IN    I     NOMBRE DE CHIFFRES SIGNIFICATIFS
C    FICHIE          IN    K*     NOM DU FICHIER OU L'ON IMPRIME
C                                 LA MATR_ASSE,
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      CHARACTER*(*) NOMNOZ, NOMCMZ, FICHIE
      CHARACTER*8   NOMNOE(*), NOMCMP(*)
      REAL*8        VALE(*)
C -----  VARIABLES LOCALES
      CHARACTER*1   SLACH 
      CHARACTER*2   KNBCH, KLVALR
      CHARACTER*8   NOMNO1, NOMCM1, FORVAR
      CHARACTER*26  FORM1
      CHARACTER*37  FORMAV
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
      SLACH  = '/'
      NOMNO1 = NOMNOZ
      NOMCM1 = NOMCMZ
C
C --- UNITE LOGIQUE DU FICHIER D'IMPRESSION :
C     -------------------------------------
      IFM = IUNIFI(FICHIE)
C
C --- NOMBRE DE CHIFFRES A METTRE APRES LA VIRGULE
C --- ON RAPPELLE QUE LA FONCTION NDR8EM() RENVOIE LE NOMBRE 
C --- DE CHIFFRES EN DECIMAL D'UN REEL :
C     --------------------------------
C      NBCH = MIN (NDR8EM( ), NBCHIF)
      NBCH = MIN (14, NBCHIF)
      NBCH1 = NBCH - 1
C
C --- LONGUEUR PRISE POUR REPRESENTER UNE VALEUR REELLE :
C     -------------------------------------------------
      LVALRE = NBCH + 6
C
C --- FORMAT D'ECRITURE DES VALEURS REELLES :
C     -------------------------------------
      CALL CODENT(NBCH1,'G',KNBCH)
      CALL CODENT(LVALRE,'D',KLVALR)
C
      FORVAR = '1PD'//KLVALR//'.'//KNBCH
C
C --- FORMAT D'ECRITURE D'UNE LIGNE POUR LE GRAIN = 'VALEUR' :
C     -----------------------------------------------------
      FORM1 = '2X,A8,A1,A8,2X,A8,A1,A8,2X'
      FORMAV = '('//FORM1//','//FORVAR//')'

C
C ---   BOUCLE SUR LES VALEURS DE LA LIGNE DE LA MATRICE :
C       ------------------------------------------------
      DO 10 I = 1, LONLIS
             WRITE(IFM,FORMAV) NOMNO1,SLACH,NOMCM1,NOMNOE(I),SLACH,
     +                         NOMCMP(I),VALE(I)
 10   CONTINUE     
C
      END
