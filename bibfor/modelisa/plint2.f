      SUBROUTINE PLINT2(SC,NS,AS,AQ,NA1,NA2,ZR,ZI,ZL,NC)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C                     INTERSECTION DE DEUX POLYGONES
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C REAL*8     SC(2,*)  : COORDONNEES DES SOMMETS DES DEUX POLYGONES
C INTEGER    NS       : NOMBRE DE SOMMETS DES DEUX POLYGONES
C INTEGER    AS(2,*)  : SOMMETS DES ARETES DES DEUX POLYGONES
C                       SENS DE PARCOURS, NORMALE SORTANTE A DROITE
C REAL*8     AQ(3,*)  : EQUATION DE DROITE ASSOCIEE AUX ARETES
C                       AQ1 * X + AQ2 * Y + AQ3 = 0
C                       AVEC (AQ1, AQ2) NORMALE SORTANTE
C INTEGER    NA1      : NOMBRE D'ARETES POLYGONE CONVEXE
C INTEGER    NA2      : NOMBRE D'ARETES AUTRE POLYGONE
C
C VECTEURS DE TRAVAIL
C REAL*8     ZR(*)    : 2*NNS
C INTEGER    ZI(*)    : MAX(NA+4*NNS,NS+NNS+NCMAX+2*NA+2*NNA)
C LOGICAL    ZL(*)    : NS+NNS
C
C VARIABLES EN SORTIE
C REAL*8     SC(2,*)  : COORDONNEES DES SOMMETS DE L'INTERSECTION
C INTEGER    ZI(*)    : CONNECTIVITE DES COMPOSANTES CONNEXES DE L'INTER
C                      (NOMBRE SOMMETS COMPOSANTE 1, SOMMET.1, SOMMET.2,
C                       ..., NOMBRE SOMMETS COMPOSANTE 2, SOMMET.1, ...)
C INTEGER    NC       : NOMBRE DE COMPOSANTES CONNEXES
C
C DIMENSION
C NOUVEAUX SOMMETS : NNS = MAX(NA1,NA2) (APPROCHE)
C NOUVELLES ARETES : NNA = NNS/2
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE (NOMBRE DE COMPOSANTE CONNEXE MAX)
      INTEGER NCMAX
      PARAMETER (NCMAX = 4)

C --- VARIABLES
      INTEGER ZI(*),AS(2,*),NA1,NA2,NS,NC,N
      INTEGER NA,NA0,NS0,NI,A1,A2,S0,S1,S2,S3,S4,I,J,P0,P1,P2
      REAL*8  SC(2,*),AQ(3,*),ZR(*),R1,R2,R3,R4
      LOGICAL ZL(*),IR

      NI = 0
      NC = 0
      NA = NA1 + NA2
      P0 = NA + 1

      DO 10 I = 1, NA
        ZI(I) = 0
 10   CONTINUE

C --- 1. NOUVEAUX SOMMETS

      NS0 = NS
      DO 20 A1 = 1, NA1

        S1 = AS(1,A1)
        S2 = AS(2,A1)

        DO 20 J = 1, NA2

          A2 = NA1 + J
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
          ZR(2*NI-1) = R1
          ZR(2*NI  ) = R2

C ------- STOCKAGE COORDONNEES SOMMET INTERSECTION

          NS = NS + 1
          SC(1,NS) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          SC(2,NS) = (1-R1)*SC(2,S1) + R1*SC(2,S2)

C ------- INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A1)

          P1 = ZI(A1)
          P2 = 0

 30       CONTINUE

          IF (P1.NE.0) THEN
            R3 = ZR(2*ABS(ZI(P1))-1)
            IF (R3.LT.R1) THEN
              P2 = P1
              P1 = ZI(P1+1)
              GOTO 30
            ENDIF
          ENDIF

          ZI(P0+1) = P1
          IF (P2.NE.0) THEN
            ZI(P2+1) = P0
          ELSE
            ZI(A1) = P0
          ENDIF

C ------- INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A2)

          P1 = ZI(A2)
          P2 = 0

 40       CONTINUE

          IF (P1.NE.0) THEN
            R3 = ZR(2*ABS(ZI(P1)))
            IF (R3.LT.R2) THEN
              P2 = P1
              P1 = ZI(P1+1)
              GOTO 40
            ENDIF
          ENDIF

          ZI(P0+3) = P1
          IF (P2.NE.0) THEN
            ZI(P2+1) = P0 + 2
          ELSE
            ZI(A2) = P0 + 2
          ENDIF

C ------- SIGNE DU SOMMET INTERSECTION

          IF (AQ(1,A1)*AQ(2,A2).GT.AQ(2,A1)*AQ(1,A2)) THEN
            ZI(P0) = NI
            ZI(P0+2) = -NI
          ELSE
            ZI(P0) = -NI
            ZI(P0+2) = NI
          ENDIF

          P0 = P0 + 4

 20   CONTINUE

      IF (NI.EQ.0) GOTO 120

C --- 2. NOUVELLES ARETES

      NA0 = NA
      DO 50 I = 1, NA0

        P1 = ZI(I)
        S1 = AS(1,I)
        S2 = AS(2,I)
        IR = .TRUE.

C ----- PARCOURS DE LA STRUCTURE AI(I) ET DECOUPAGE DES ARETES

 60     CONTINUE

        IF (P1.NE.0) THEN

          S0 = ZI(P1)
          IF (S0.GT.0) THEN
            IF (IR) THEN
              AS(1,I) = S1
              AS(2,I) = NS0 + S0
              IR = .FALSE.
            ELSE
              NA = NA + 1
              AS(1,NA) = S1
              AS(2,NA) = NS0 + S0
            ENDIF
          ELSE
            S1 = NS0 - S0
          ENDIF

          P1 = ZI(P1+1)
          GOTO 60

        ENDIF

        IF (S0.LT.0) THEN
          IF (IR) THEN
            AS(1,I) = S1
            AS(2,I) = S2
          ELSE
            NA = NA + 1
            AS(1,NA) = S1
            AS(2,NA) = S2
          ENDIF
        ENDIF

 50   CONTINUE

C --- 3. GRAPHE SOMMET - ARETES

      P0 = NS + NCMAX

      DO 70 I = 1, NS
        ZI(P0+2*I-1) = 0
        ZI(P0+2*I  ) = 0
 70   CONTINUE

      DO 80 I = 1, NA

        S1 = AS(1,I)
        IF (ZI(P0+2*S1-1).EQ.0) THEN
          ZI(P0+2*S1-1) = I
        ELSE
          ZI(P0+2*S1  ) = I
        ENDIF

        S2 = AS(2,I)
        IF (ZI(P0+2*S2-1).EQ.0) THEN
          ZI(P0+2*S2-1) = I
        ELSE
          ZI(P0+2*S2  ) = I
        ENDIF

 80   CONTINUE

C --- 4. COMPOSANTES CONNEXES DE L'INTERSECTION

      DO 90 I = 1, NS
        ZL(I) = .FALSE.
 90   CONTINUE

      N = 0
      P1 = 0

      DO 100 I = 1, NI

        S0 = NS0 + I
        IF (ZL(S0)) GOTO 100

        NC = NC + 1
        P1 = P1 + N + 1
        N = 0

 110    CONTINUE

        ZL(S0) = .TRUE.

        IF (AS(1,ZI(P0+2*S0-1)).EQ.S0) THEN
          S0 = AS(2,ZI(P0+2*S0-1))
        ELSEIF (AS(1,ZI(P0+2*S0)).EQ.S0) THEN
          S0 = AS(2,ZI(P0+2*S0))
        ENDIF

        IF (S0.EQ.0) CALL U2MESS('F','MODELISA6_23')

        N = N + 1
        ZI(P1+N) = S0
        IF (.NOT.ZL(S0)) GOTO 110

        ZI(P1) = N

 100  CONTINUE

      IF (NC.GT.NCMAX) CALL U2MESS('A','MODELISA6_24')

 120  CONTINUE

      END
