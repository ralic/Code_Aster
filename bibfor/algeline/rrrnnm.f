      SUBROUTINE RRRNNM(NEQ, IHCOL, IADIA, IDLEXC, COEF, MATINS,
     +                  MATINI, MATRES, MATREI)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER          NEQ, IHCOL(1), IADIA(1), IDLEXC(1)
      REAL*8           COEF, MATINS(1), MATINI(1), MATRES(1), MATREI(1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 28/02/2006   AUTEUR VABHHTS J.PELLET 
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
C     COMBINAISON LINEAIRE D'UNE MATRICE NON-SYMETRIQUE
C     STOCKEE MORSE DONT LES VALEURS SONT RELLES DANS UNE MATRICE
C     RESULTAT NON-SYMETRIQUE STOCKEE MORSE A VALEURS REELLES,
C     LE FACTEUR MULTIPLICATIF DE LA COMBINAISON ETANT REEL
C     (IL N'Y A EN FAIT QU'UN SEUL BLOC)
C -------------------------------------------------------
C -------------------------------------------------------
C  IHCOL(1)      - IN    - I  - : IHCOL EST LE TABLEAU DES INDICES
C                -       -    -   DE COLONNES DES TERMES DE LA
C                -       -    -   MATRICE
C -------------------------------------------------------
C  IADIA(1)      - IN    - I  - : IADIA EST LE TABLEAU DES ADRESSES
C                -       -    -   DES TERMES DIAGONAUX DANS LA MATRICE
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
C  MATINS(1)     - IN    - R  - : BLOC SUP DE LA MATRICE DONNEE
C                -       -    -   EN ENTREE EN ARGUMENT DE LA
C                -       -    -   DE LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C  MATINI(1)     - IN    - R  - : BLOC INF DE LA MATRICE DONNEE
C                -       -    -   EN ENTREE EN ARGUMENT DE LA
C                -       -    -   DE LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C  MATRES(1)     - VAR   - R  - : BLOC SUP DE LA MATRICE RESULTAT DE
C                -       -    -   LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C  MATREI(1)     - VAR   - R  - : BLOC INF DE LA MATRICE RESULTAT DE
C                -       -    -   LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C
C- RECUPERATION NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT
C
      IL1 = 1
C
C- RECUPERATION NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT
C
      IL2 = NEQ
      KIN = 0
      IDEBLI = 1
      DO 10 IEQUA = IL1, IL2
C
C- ADRESSE DU TERME DIAGONAL DE LA LIGNE IEQUA DANS LA MATRICE
C
             IFINLI = IADIA(IEQUA)

C
C- BOUCLE SUR LES TERMES DE LA LIGNE COURANTE
C- LES LIGNES CORRESPONDANT A DES MULTIPLICATEURS DE LAGRANGE
C- SONT MISES A 0
C
             DO 20 IND = IDEBLI,IFINLI
                    KIN = KIN+1
C
C-    INDICE DE COLONNE DU TERME COURANT DE LA LIGNE
C
                    INDCOL = IHCOL(IND)
C
                    MATRES(KIN) = MATRES(KIN) + COEF*MATINS(KIN)*
     +                             (1-IDLEXC(INDCOL))*(1-IDLEXC(IEQUA))
C
                    MATREI(KIN) = MATREI(KIN) + COEF*MATINI(KIN)*
     +                             (1-IDLEXC(INDCOL))*(1-IDLEXC(IEQUA))
  20         CONTINUE
C
             IDEBLI = IADIA(IEQUA) +1
C
  10  CONTINUE
C
      END
