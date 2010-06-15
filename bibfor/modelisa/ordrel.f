      SUBROUTINE ORDREL (NUMNOE, NOMNOE, DDL, COEF, COEFC, NBOCNO,
     +                   NBTERM, NOMCMP, NDDLA)
      IMPLICIT REAL*8 (A-H,O-Z)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C
      INTEGER      NUMNOE(NBTERM), NBOCNO(NBTERM)
      REAL*8       COEF(NBTERM)
      COMPLEX*16   COEFC(NBTERM)
      CHARACTER*8  NOMNOE(NBTERM), DDL(NBTERM), NOMCMP(NDDLA)
C
C ------------------------------------------------------------------
C     REARRANGEMENT DES TABLEAUX D'UNE RELATION LINEAIRE PAR ORDRE
C     DE NOEUD CROISSANT ET DE NUMERO DE DDL CROISSANT POUR UN
C     NOEUD DONNE
C ------------------------------------------------------------------
C  NUMNOE(NBTERM) - VAR    - I    - : NUMEROS DES NOEUDS DE LA
C                 -        -      -   RELATION EN ENTREE
C                 -        -      -   CONTIENT PAR LA SUITE  LES
C                 -        -      -   NUMEROS DES DDLS
C                 -        -      -   SERT DE CLE POUR LE TRI
C -----------------------------------------------------------------
C  NOMNOE(NBTERM) - VAR    - K8   - : NOMS DES NOEUDS DE LA
C                 -        -      -   RELATION
C -----------------------------------------------------------------
C  DDL(NBTERM)    - VAR    - K8   - : NOMS DES DDLS DE LA
C                 -        -      -   RELATION
C -----------------------------------------------------------------
C  COEF(NBTERM)   - VAR    - R    - : COEFFICIENTS REELS DES TERMES
C                 -        -      -   DE LA RELATION
C -----------------------------------------------------------------
C  COEFC(NBTERM)  - VAR    - C    - : COEFFICIENTS COMPLEXES DES
C                 -        -      -   TERMES DE LA RELATION
C -----------------------------------------------------------------
C  NBOCNO(NBTERM) - VAR    - I    - : NOMBRE D'OCCURENCES DE CHAQUE
C                 -        -      -   TERME DANS LA RELATION
C -----------------------------------------------------------------
C  NBTERM         - IN     - I    - : NOMBRE DE TERMES DE LA
C                 -        -      -   RELATION
C -----------------------------------------------------------------
C  NOMCMP(NDDLA)  - IN     - K8   - : NOMS DES COMPOSANTES POSSIBLES
C                 -        -      -   AUX NOEUDS DU MAILLAGE
C -----------------------------------------------------------------
C  NDDLA          - IN     - I    - : NOMBRE MAX DE COMPOSANTES
C                 -        -      -   POSSIBLES EN UN NOEUD
C -----------------------------------------------------------------
C
C --- REARRANGEMENT DES TABLEAUX DES NOEUDS, DES DDLS ET DES ---
C --- COEFFICIENTS DE LA RELATION SELON L'ORDRE CROISSANT    ---
C --- DES NOEUDS                                             ---
C
      CALL ORDRE1(NUMNOE,NOMNOE,DDL,COEF,COEFC,NBTERM)
C
C --- DETERMINATION DU TABLEAU NBOCNO DONNANT LE NOMBRE ---
C --- D'OCCURENCES DES NOEUDS DANS LA LISTE_RELA
C
      DO 10 I = 1, NBTERM
         NBOCNO(I) = 1
  10  CONTINUE
      DO 20 I = 1, NBTERM - 1
         DO 30 J = I+1, NBTERM
            IF (NUMNOE(I).EQ.NUMNOE(J)) THEN
               NBOCNO(I) = NBOCNO(I) + 1
            ENDIF
  30     CONTINUE
  20  CONTINUE
C
C --- REARRANGEMENT DES TABLEAUX DES NOEUDS, DES DDLS ET DES ---
C --- COEFFICIENTS DE LA RELATION SELON L'ORDRE CROISSANT    ---
C --- DES DDLS POUR UN NOEUD DONNE                           ---
C
      K = 0
C
C --- CAS DU PREMIER NOEUD ---
C
      IF (NBOCNO(1).GT.1) THEN
         DO 40 J = 1, NBOCNO(1)
            K = K+1
C --- ICI NUMNOE CONTIENT LES NUMEROS DES DDLS POUR UN NOEUD DONNE
            NUMNOE(K) = INDIK8(NOMCMP,DDL(K),1,NDDLA)
   40    CONTINUE
         CALL ORDRE1(NUMNOE,NOMNOE,DDL,COEF,COEFC,NBOCNO(1))
      ELSE
         K = K+1
      ENDIF
C
C --- CAS DES AUTRES NOEUDS  ---
C
      DO 50 I = 2, NBTERM
C
C --- ON UTILISE LE FAIT QUE SI DES NOEUDS SONT EGAUX, ILS  ---
C --- SONT CONSECUTIFS                                      ---
C
         IF (NOMNOE(I).EQ.NOMNOE(I-1)) GOTO 50
         IF (NBOCNO(I).GT.1) THEN
             DO 60 J = 1, NBOCNO(I)
               K = K + 1
C --- ICI NUMNOE CONTIENT LES NUMEROS DES DDLS POUR UN NOEUD DONNE
               NUMNOE(K) = INDIK8(NOMCMP,DDL(K),1,NDDLA)
   60       CONTINUE
            IND = K - NBOCNO(I) + 1
            CALL ORDRE1(NUMNOE(IND),NOMNOE(IND),DDL(IND),
     +                  COEF(IND),COEFC(IND),NBOCNO(I))
         ELSE
            K = K+1
         ENDIF
   50 CONTINUE
      END
