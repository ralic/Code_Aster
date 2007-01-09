      SUBROUTINE PLINT2(SC    ,NSOM  ,AS    ,AQ    ,NCMAX  ,
     &                  NARE1 ,NARE2 ,TRAVR ,TRAVI ,TRAVL  ,
     &                  NC)
C       
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8  SC(2,*)
      INTEGER NSOM      
      INTEGER AS(2,*)
      REAL*8  AQ(3,*)  
      INTEGER NCMAX          
      INTEGER NARE1
      INTEGER NARE2
      REAL*8  TRAVR(*)
      INTEGER TRAVI(*)      
      LOGICAL TRAVL(*)
      INTEGER NC      
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C INTERSECTION DE DEUX POLYGONES (2D)
C
C ----------------------------------------------------------------------
C 
C      
C I/O SC     : IN  - COORDONNEES DES SOMMETS DES DEUX POLYGONES
C              OUT - COORDONNEES DES SOMMETS DE L'INTERSECTION
C IN  NSOM   : NOMBRE DE SOMMETS DES DEUX POLYGONES
C IN  AS     : SOMMETS DES ARETES DES DEUX POLYGONES
C                       SENS DE PARCOURS, NORMALE SORTANTE A DROITE
C IN  AQ     : EQUATION DE DROITE ASSOCIEE AUX ARETES
C                       AQ1 * X + AQ2 * Y + AQ3 = 0
C                       AVEC (AQ1, AQ2) NORMALE SORTANTE
C IN  NCMAX  : NOMBRE MAXI DE COMPOSANTES CONNEXES
C IN  NARE1  : NOMBRE D'ARETES PREMIER POLYGONE CONVEXE
C IN  NARE2  : NOMBRE D'ARETES SECOND  POLYGONE CONVEXE
C I/O TRAVR  : VECTEUR DE TRAVAIL DE REELS
C                     DIM: 2*NNS
C I/O TRAVI  : VECTEUR DE TRAVAIL D'ENTIERS
C                     DIM: MAX(NA+4*NNS,NSOM+NNS+NCMAX+2*NA+2*NNA)
C              OUT : CONNECTIVITE DES COMPOSANTES CONNEXES DE L'INTER
C                (NOMBRE SOMMETS COMPOSANTE 1, SOMMET.1, SOMMET.2,...
C                 NOMBRE SOMMETS COMPOSANTE 2, SOMMET.1, ...)
C I/O TRAVL  : VECTEUR DE TRAVAIL DE BOOLEENS
C                     DIM: NSOM+NNS
C OUT NC     : NOMBRE DE COMPOSANTES CONNEXES
C
C DIMENSIONS :
C NOUVEAUX SOMMETS : NNS = MAX(NARE1,NARE2) (APPROCHE)
C NOUVELLES ARETES : NNA = NNS/2
C
C ----------------------------------------------------------------------
C
      INTEGER N,NARE,NA0,NS0,NI
      INTEGER A1,A2,S0,S1,S2,S3,S4
      INTEGER I,J,P0,P1,P2
      REAL*8  R1,R2,R3,R4
      LOGICAL IR
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      NI   = 0
      NC   = 0
      NARE = NARE1 + NARE2
      P0   = NARE + 1
C
      DO 10 I = 1, NARE
        TRAVI(I) = 0
 10   CONTINUE
C
C --- 1. NOUVEAUX SOMMETS
C
      NS0 = NSOM
      DO 20 A1 = 1, NARE1

        S1 = AS(1,A1)
        S2 = AS(2,A1)
        DO 20 J = 1, NARE2

          A2 = NARE1 + J
          S3 = AS(1,A2)
          S4 = AS(2,A2)

C ------- TEST INTERSECTION

          R3 = AQ(1,A1)*SC(1,S3) + AQ(2,A1)*SC(2,S3) + AQ(3,A1)
          R4 = AQ(1,A1)*SC(1,S4) + AQ(2,A1)*SC(2,S4) + AQ(3,A1)
          
          IF (((R3.LT.0.D0).EQV.(R4.LT.0.D0)).OR.(R3.EQ.0.D0)
     &        .OR.(R4.EQ.0.D0)) GOTO 20

          R1 = AQ(1,A2)*SC(1,S1) + AQ(2,A2)*SC(2,S1) + AQ(3,A2)
          R2 = AQ(1,A2)*SC(1,S2) + AQ(2,A2)*SC(2,S2) + AQ(3,A2)
          
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(R1.EQ.0.D0)
     &        .OR.(R2.EQ.0.D0)) GOTO 20

C ------- STOCKAGE POSITION DE L'INTERSECTION SUR LES DEUX ARETES

          NI = NI + 1
          R1 = R1/(R1-R2)
          R2 = R3/(R3-R4)
          TRAVR(2*NI-1) = R1
          TRAVR(2*NI  ) = R2

C ------- STOCKAGE COORDONNEES SOMMET INTERSECTION

          NSOM = NSOM + 1
          SC(1,NSOM) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          SC(2,NSOM) = (1-R1)*SC(2,S1) + R1*SC(2,S2)

C ------- INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A1)

          P1 = TRAVI(A1)
          P2 = 0

 30       CONTINUE

          IF (P1.NE.0) THEN
            R3 = TRAVR(2*ABS(TRAVI(P1))-1)
            IF (R3.LT.R1) THEN
              P2 = P1
              P1 = TRAVI(P1+1)
              GOTO 30
            ENDIF
          ENDIF

          TRAVI(P0+1) = P1
          IF (P2.NE.0) THEN
            TRAVI(P2+1) = P0
          ELSE
            TRAVI(A1) = P0
          ENDIF

C ------- INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A2)

          P1 = TRAVI(A2)
          P2 = 0

 40       CONTINUE

          IF (P1.NE.0) THEN
            R3 = TRAVR(2*ABS(TRAVI(P1)))
            IF (R3.LT.R2) THEN
              P2 = P1
              P1 = TRAVI(P1+1)
              GOTO 40
            ENDIF
          ENDIF

          TRAVI(P0+3) = P1
          IF (P2.NE.0) THEN
            TRAVI(P2+1) = P0 + 2
          ELSE
            TRAVI(A2) = P0 + 2
          ENDIF

C ------- SIGNE DU SOMMET INTERSECTION

          IF (AQ(1,A1)*AQ(2,A2).GT.AQ(2,A1)*AQ(1,A2)) THEN
            TRAVI(P0)   = NI
            TRAVI(P0+2) = -NI
          ELSE
            TRAVI(P0)   = -NI
            TRAVI(P0+2) = NI
          ENDIF

          P0 = P0 + 4

 20   CONTINUE

      IF (NI.EQ.0) GOTO 120
C
C --- 2. NOUVELLES ARETES
C
      NA0 = NARE
      DO 50 I = 1, NA0

        P1 = TRAVI(I)
        S1 = AS(1,I)
        S2 = AS(2,I)
        IR = .TRUE.

C ----- PARCOURS DE LA STRUCTURE AI(I) ET DECOUPAGE DES ARETES

 60     CONTINUE

        IF (P1.NE.0) THEN

          S0 = TRAVI(P1)
          IF (S0.GT.0) THEN
            IF (IR) THEN
              AS(1,I) = S1
              AS(2,I) = NS0 + S0
              IR = .FALSE.
            ELSE
              NARE = NARE + 1
              AS(1,NARE) = S1
              AS(2,NARE) = NS0 + S0
            ENDIF
          ELSE
            S1 = NS0 - S0
          ENDIF

          P1 = TRAVI(P1+1)
          GOTO 60

        ENDIF

        IF (S0.LT.0) THEN
          IF (IR) THEN
            AS(1,I) = S1
            AS(2,I) = S2
          ELSE
            NARE = NARE + 1
            AS(1,NARE) = S1
            AS(2,NARE) = S2
          ENDIF
        ENDIF

 50   CONTINUE
C
C --- 3. GRAPHE SOMMET - ARETES
C
      P0 = NSOM + NCMAX

      DO 70 I = 1, NSOM
        TRAVI(P0+2*I-1) = 0
        TRAVI(P0+2*I  ) = 0
 70   CONTINUE

      DO 80 I = 1, NARE

        S1 = AS(1,I)
        IF (TRAVI(P0+2*S1-1).EQ.0) THEN
          TRAVI(P0+2*S1-1) = I
        ELSE
          TRAVI(P0+2*S1  ) = I
        ENDIF

        S2 = AS(2,I)
        IF (TRAVI(P0+2*S2-1).EQ.0) THEN
          TRAVI(P0+2*S2-1) = I
        ELSE
          TRAVI(P0+2*S2  ) = I
        ENDIF

 80   CONTINUE
C
C --- 4. COMPOSANTES CONNEXES DE L'INTERSECTION
C
      DO 90 I = 1, NSOM
        TRAVL(I) = .FALSE.
 90   CONTINUE

      N = 0
      P1 = 0

      DO 100 I = 1, NI

        S0 = NS0 + I
        IF (TRAVL(S0)) GOTO 100

        NC = NC + 1
        P1 = P1 + N + 1
        N = 0

 110    CONTINUE

        TRAVL(S0) = .TRUE.

        IF (AS(1,TRAVI(P0+2*S0-1)).EQ.S0) THEN
          S0 = AS(2,TRAVI(P0+2*S0-1))
        ELSEIF (AS(1,TRAVI(P0+2*S0)).EQ.S0) THEN
          S0 = AS(2,TRAVI(P0+2*S0))
        ENDIF

        IF (S0.EQ.0) CALL U2MESS('F','ARLEQUIN_23')

        N = N + 1
        TRAVI(P1+N) = S0
        IF (.NOT.TRAVL(S0)) GOTO 110

        TRAVI(P1) = N

 100  CONTINUE
C
 120  CONTINUE

      END
