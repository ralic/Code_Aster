      SUBROUTINE RRRSS2(IABLO, IHCOL, IHCOIN, IABLIN, IBLOIN, 
     +                  IDLEXC, COEF, MATIN, MATRES)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER          IABLO(1),IHCOL(1),IHCOIN(1),IDLEXC(1)
      INTEGER          IABLIN(1), IBLOIN(1)
      REAL*8           COEF, MATIN(1), MATRES(1)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 20/04/99   AUTEUR CIBHHGB G.BERTRAND 
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
C     LES MATRICES QUE L'ON COMBINE SONT STOCKEES EN LIGNE
C     DE CIEL MAIS LEURS LONGUEURS DE LIGNES SONT DIFFERENTES
C -------------------------------------------------------
C  IABLO(1)      - IN    - I  - : IABLO(1) EST LE NUMERO DE LA
C                -       -    -   PREMIERE EQUATION DU BLOC COURANT
C                -       -    -   IABLO(2) EST LE NUMERO DE LA
C                -       -    -   DERNIERE EQUATION DU BLOC COURANT
C -------------------------------------------------------
C  IHCOL(1)      - IN    - I  - : IHCOL EST LE TABLEAU DE LA 
C                -       -    -   LONGUEUR DES LIGNES (OU DES COLONNES)
C -------------------------------------------------------
C  IHCOIN(1)     - IN    - I  - : IHCOIN EST LE TABLEAU DE LA 
C                -       -    -   LONGUEUR DES LIGNES (OU DES COLONNES)
C                                 DE MATRICE COURANTE A COMBINER
C -------------------------------------------------------
C  IABLIN(1)     - IN    - I  - : IABLIN EST LE TABLEAU ASSOCIANT 
C                -       -    -   A CHAQUE LIGNE DE LA MATRICE COURANTE
C                                 A COMBINER LE NUMERO DE BLOC AUQUEL
C                                 ELLE APPARTIENT
C -------------------------------------------------------
C  IBLOIN(1)     - IN    - I  - : IBLOIN EST LE TABLEAU DES NUMEROS 
C                -       -    -   DE LIGNE DE DEBUT ET DE FIN DE
C                                 CHAQUE BLOC DE LA MATRICE A COMBINER
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
C --- RECUPERATION NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT 
C --- DE LA MATRICE RESULTANTE :
C     ------------------------
      IL1 = IABLO(1) +1
C
C --- RECUPERATION NUMERO DE LA DERNIERE EQUATION DU BLOC COURANT :
C --- DE LA MATRICE RESULTANTE :
C     ------------------------
      IL2  = IABLO(2)
C
C --- NUMERO DE BLOC DE LA MATRICE A COMBINER AUQUEL APPARTIENT 
C --- LA PREMIERE LIGNE DU BLOC COURANT DE LA MATRICE RESULTANTE :
C     ----------------------------------------------------------
      JBLOC = IABLIN(IL1)
C
C --- RECUPERATION NUMERO DE LA PREMIERE EQUATION DU BLOC COURANT 
C --- DE LA MATRICE A COMBINER :
C     ------------------------
      JL1   = IBLOIN(JBLOC) + 1
C
C --- INITIALISATIONS :
C     ---------------
      KIN1 = 0
      KIN2 = 0
      IDEC = 0
C
C --- ACTUALISATION DE L'INDICE DU PREMIER TERME DE LA MATRICE
C --- A COMBINER A PRENDRE EN COMPTE DANS LA COMBINAISON :
C     --------------------------------------------------
      DO 10 IEQUA = JL1, IL1-1
        KIN2 = KIN2 + IHCOIN(IEQUA)
 10   CONTINUE
C
C --- BOUCLE SUR LES LIGNES DE LA MATRICE RESULTANTE :
C     ----------------------------------------------
      DO 20 IEQUA = IL1, IL2
C
C ---   RECUPERATION DE LA LONGUEUR DE LA LIGNE COURANTE DE LA 
C ---   MATRICE RESULTANTE :
C       ------------------
        ILONG = IHCOL(IEQUA)
C
C ---   RECUPERATION DE LA LONGUEUR DE LA LIGNE COURANTE DE LA 
C ---   MATRICE A COMBINER COURANTE :
C       ---------------------------
        ILONG1 = IHCOIN(IEQUA)
C
C ---   REACTUALISATION DU DECALAGE ENTRE L'INDICE DES TERMES
C ---   DANS LA MATRICE RESULTANTE ET L'INDICE DES TERMES
C ---   DE LA MATRICE A COMBINER :
C       ------------------------
        IDEC = IDEC + ILONG - ILONG1
C
C ---   BOUCLE SUR LES TERMES DE LA LIGNE COURANTE
C ---   LES LIGNES CORRESPONDANT A DES MULTIPLICATEURS DE LAGRANGE
C ---   SONT MISES A 0 :
C       --------------
           DO 30 IND = 1,ILONG1
              KIN1 = KIN1+1
              KIN2 = KIN2+1
              MATRES(KIN1+IDEC) = MATRES(KIN1+IDEC) + 
     +                               COEF*MATIN(KIN2)*
     +                              (1-IDLEXC(IEQUA-ILONG1+IND))*
     +                              (1-IDLEXC(IEQUA))
  30       CONTINUE
C
  20  CONTINUE
C
      END
