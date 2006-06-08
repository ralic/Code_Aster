      SUBROUTINE RRSSM2(NEQ,IHCOL, IHCOIN, IADIA, IADIN, 
     +                  IDLEXC, COEF, INDIRE, MATIN, MATRES)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 21/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C--------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER          IHCOL(1),IHCOIN(1),IDLEXC(1)
      INTEGER          IADIA(1),IADIN(1),INDIRE(1)
      REAL*8           COEF, MATIN(1), MATRES(1)
C--------------------------------------------------------
C     COMBINAISON LINEAIRE DE MATRICES SYMETRIQUES 
C     STOCKEES MORSE DONT LES VALEURS SONT RELLES
C     DANS UNE MATRICE RESULTAT A VALEURS REELLES,
C     LE FACTEUR MULTIPLICATIF DE LA COMBINAISON ETANT REEL
C     LES MATRICES QUE L'ON COMBINE SONT STOCKEES MORSE
C     MAIS LEURS LONGUEURS DE LIGNES SONT DIFFERENTES
C -------------------------------------------------------
C  NEQ           - IN    - I  - : NEQ EST LE NOMBRE DE LIGNES DES 
C                -       -    -   MATRICES
C -------------------------------------------------------
C  IHCOL(1)      - IN    - I  - : IHCOL EST LE TABLEAU DES NUMEROS 
C                -       -    -   DE COLONNES DES TERMES NON NUL
C                -       -    -   DE LA TRIANGULAIRE INFERIEURE
C                -       -    -   DE LA MATRICE RESULTANTE
C -------------------------------------------------------
C  IHCOIN(1)     - IN    - I  - : IHCOIN EST LE TABLEAU DES NUMEROS 
C                -       -    -   DE COLONNES DES TERMES NON NUL
C                -       -    -   DE LA TRIANGULAIRE INFERIEURE
C                -       -    -   DE LA MATRICE  A COMBINER
C -------------------------------------------------------
C  IADIA (1)     - IN    - I  - : IADIA EST LE TABLEAU DE L'INDICE  
C                -       -    -   CUMMULE DES TERMES DIAGONAUX
C                -       -    -   DE LA MATRICE RESULTANTE
C -------------------------------------------------------
C  IADIN(1)      - IN    - I  - : IADIN EST LE TABLEAU DE L'INDICE
C                -       -    -   CUMMULE DES TERMES DIAGONAUX
C                                 DE LA MATRICE A COMBINER
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
C  INDIRE(1)     - IN    - I  - : TABLEAU DE TRAVAIL
C -------------------------------------------------------
C  MATIN(1)      - IN    - R  - : MATRICE DONNEE EN ENTREE EN  
C                -       -    -   ARGUMENT DE LA COMBINAISON LINEAIRE
C -------------------------------------------------------
C  MATRES(1)     - VAR   - R  - : MATRICE RESULTAT DE LA COMBINAISON 
C                -       
C -------------------------------------------------------
C
C --- INITIALISATIONS :
C     ---------------
      IDEBL1 = 1
      IDEBL2 = 1
      KIN1   = 0
      KIN2   = 0
C
C --- BOUCLE SUR LES LIGNES DE LA MATRICE RESULTANTE :
C     ----------------------------------------------
      DO 10 IEQUA = 1, NEQ
C
C ---   INDICE DU TERME DIAGONAL DE LA MATRICE RESULTANTE
C ---   POUR LA LIGNE IEQUA :
C       -------------------
        IFINL1 = IADIA(IEQUA)
C
C ---   INDICE DU TERME DIAGONAL DE LA MATRICE A COMBINER
C ---   POUR LA LIGNE IEQUA :
C       -------------------
        IFINL2 = IADIN(IEQUA)
C
C ---   INITIALISATION DU TABLEAU D'INDIRECTION :
C       ---------------------------------------
        DO 20 I = 1, NEQ
          INDIRE(I) = 0
  20    CONTINUE
C
C ---   BOUCLE SUR LES TERMES DE LA LIGNE COURANTE A COMBINER :
C       -----------------------------------------------------
        DO 30 J2 = IDEBL2, IFINL2
C
          K = 0
C
C ---     NUMERO DE COLONNE DU TERME COURANT DE LA MATRICE :
C         ------------------------------------------------ 
          I2 = IHCOIN(J2)    
C
C ---     BOUCLE SUR LES TERMES DE LA LIGNE COURANTE RESULTANTE :
C         -----------------------------------------------------
          DO 40 J1 = IDEBL1, IFINL1
C
C ---       NUMERO DE COLONNE DU TERME COURANT DE LA MATRICE :
C           ------------------------------------------------ 
            I1 = IHCOL(J1)
C
            K = K + 1
            IF (I1.EQ.I2) GOTO 50
C
  40      CONTINUE
  50      CONTINUE
C
          INDIRE(I2) = K
C
  30    CONTINUE
C
C ---   AFFECTATION DE LA MATRICE RESULTANTE :
C       ====================================
        KIN1 = IDEBL1 - 1
C
C ---   BOUCLE SUR LES TERMES DE LA LIGNE COURANTE A COMBINER :
C       -----------------------------------------------------
        DO 60 J2 = IDEBL2, IFINL2 
C
          KIN2 =  KIN2 + 1
          I2   = IHCOIN(J2)    
          IND1 = INDIRE(I2)
C       
          MATRES(KIN1+IND1) = MATRES(KIN1+IND1) + 
     +                               COEF*MATIN(KIN2)*
     +                              (1-IDLEXC(I2))*
     +                              (1-IDLEXC(IEQUA))
C
  60    CONTINUE
C  
C
C ---  REACTUALISATION DES INDICES DES DEBUTS DE LIGNES
C ---  DE LA MATRICE RESULTANTE ET DE LA MATRICE A COMBINER :
C      ---------------------------------------------------
        IDEBL1 = IADIA(IEQUA) + 1
        IDEBL2 = IADIN(IEQUA) + 1
C
  10  CONTINUE
C
      END
