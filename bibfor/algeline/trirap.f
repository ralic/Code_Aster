      SUBROUTINE TRIRAP(CLEF,TAB,NTAB,N,G,D,M)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ----------------------------------------------------------------------
C             TRI RAPIDE : CHOIX PIVOT ET EXPLORATION (CF TRI)
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE / SORTIE
C INTEGER CLEF(N)         : VECTEUR CLEF
C INTEGER TAB(N,NTAB)     : TABLEAU A TRIER EN MEME TEMPS QUE CLEF
C                           (SI NTAB = 0, PAS PRIS EN COMPTE)
C VARIABLES D'ENTREE
C INTEGER NTAB            : NOMBRE DE COLONNES DE TAB
C INTEGER N               : NOMBRE DE LIGNES DE CLEF
C INTEGER G               : INDICE GAUCHE DE CLEF
C INTEGER D               : INDICE DROITE DE CLEF
C
C VARIABLE DE SORTIE
C INTEGER M               : INDICE DU PIVOT
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER N,NTAB,G,D,M,CLEF(*),TAB(N,*)
      INTEGER PIVOT,GP,DP,I,TMP

C --- CHOIX DU PIVOT

      M = (D+G)/2

      IF (CLEF(G).LT.CLEF(M)) THEN
        IF (CLEF(D).LT.CLEF(M)) THEN
          IF (CLEF(D).LT.CLEF(G)) THEN
            M = G
          ELSE
            M = D
          ENDIF
        ENDIF
      ELSE
        IF (CLEF(M).LT.CLEF(D)) THEN
          IF (CLEF(G).LT.CLEF(D)) THEN
            M = G
          ELSE
            M = D
          ENDIF
        ENDIF
      ENDIF

      PIVOT = CLEF(M)
      CLEF(M) = CLEF(G)
      CLEF(G) = PIVOT

      DO 10 I = 1, NTAB
        TMP = TAB(M,I)
        TAB(M,I) = TAB(G,I)
        TAB(G,I) = TMP
 10   CONTINUE

C --- EXPLORATION

      GP = G
      DP = D + 1

 20   CONTINUE

      GP = GP + 1
      IF (CLEF(GP).LT.PIVOT) GOTO 20

 30   CONTINUE

      DP = DP - 1
      IF (CLEF(DP).GT.PIVOT) GOTO 30

      IF (GP.LT.DP) THEN

        TMP = CLEF(GP)
        CLEF(GP) = CLEF(DP)
        CLEF(DP) = TMP

        DO 40 I = 1, NTAB
          TMP = TAB(GP,I)
          TAB(GP,I) = TAB(DP,I)
          TAB(DP,I) = TMP
 40     CONTINUE

        GOTO 20

      ENDIF

C --- PLACEMENT DU PIVOT

      M = GP - 1

      CLEF(G) = CLEF(M)
      CLEF(M) = PIVOT

      DO 50 I = 1, NTAB
        TMP = TAB(G,I)
        TAB(G,I) = TAB(M,I)
        TAB(M,I) = TMP
 50   CONTINUE

      END
