      SUBROUTINE RRRSSL(IABLO, IHCOL, IDLEXC, COEF, MATIN, MATRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER          IABLO(1), IHCOL(1), IDLEXC(1)
      REAL*8           COEF, MATIN(1), MATRES(1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 12/05/95   AUTEUR CIBHHGB G.BERTRAND 
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
C -------------------------------------------------------
C     COMBINAISON LINEAIRE DES BLOCS COURANTS DE MATRICES SYMETRIQUES 
C     STOCKEES EN LIGNE DE CIEL DONT LES VALEURS SONT RELLES
C     DANS UNE MATRICE RESULTAT A VALEURS REELLES,
C     LE FACTEUR MULTIPLICATIF DE LA COMBINAISON ETANT REEL
C -------------------------------------------------------
C  IABLO(1)      - IN    - I  - : IABLO(1) EST LE NUMERO DE LA
C                -       -    -   PREMIERE EQUATION DU BLOC COURANT
C                -       -    -   IABLO(2) EST LE NUMERO DE LA
C                -       -    -   DERNIERE EQUATION DU BLOC COURANT
C -------------------------------------------------------
C  IHCOL(1)      - IN    - I  - : IHCOL EST LE TABLEAU DE LA 
C                -       -    -   LONGUEUR DES LIGNES (OU DES COLONNES)
C -------------------------------------------------------
C  IDLEXC(1)     - IN    - I  - : IDLEXC EST LE TABLEAU DES INDICES 
C                -       -    -   DES DDLS EXCLUS (ICI, LES LAGRANGE)
C                -       -    -   IDLEXC(I) =1 SI L'INCONNUE I EST
C                -       -    -                UN LAGRANGE
C                -       -    -   IDLEXC(I) =0 SINON
C -------------------------------------------------------
C  COEF          - IN    - R  - : COEFFICIENT MULTIPLICATEUR DE LA
C                -       -    -   MATRICE MATIN
C -------------------------------------------------------
C  MATIN(1)      - IN    - R  - : MATRICE DONNEE EN ENTREE EN  
C                -       -    -   ARGUMENT DE LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C  MATRES(1)     - VAR   - R  - : MATRICE RESULTAT DE LA COMBINAISON 
C                -       
C -------------------------------------------------------
C
C- RECUPERATION NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT
C
      IL1 = IABLO(1) +1
C
C- RECUPERATION NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT
C
      IL2 = IABLO(2)
      KIN1 = 0
      DO 10 IEQUA = IL1, IL2
C
C- RECUPERATION DE LA LONGUEUR DE LA LIGNE COURANTE
C
             ILONG = IHCOL(IEQUA)
C
C- BOUCLE SUR LES TERMES DE LA LIGNE COURANTE
C- LES LIGNES CORRESPONDANT A DES MULTIPLICATEURS DE LAGRANGE
C- SONT MISES A 0
C
             DO 20 IND = 1,ILONG
                    KIN1 = KIN1+1
                    MATRES(KIN1) = MATRES(KIN1) + COEF*MATIN(KIN1)*
     +                              (1-IDLEXC(IEQUA-ILONG+IND))*
     +                              (1-IDLEXC(IEQUA))
  20         CONTINUE
C
  10  CONTINUE
C
      END
