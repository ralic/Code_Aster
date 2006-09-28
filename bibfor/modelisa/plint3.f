      SUBROUTINE PLINT3(SC,NS,FS,FQ,NF1,NF2,AS,AF,NA1,NA2,ZR,ZI,ZL,NC)
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
C TOLE CRP_20
C ----------------------------------------------------------------------
C                     INTERSECTION DE DEUX POLYEDRES
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C REAL*8     SC(3,*)  : COORDONNEES DES SOMMETS DES DEUX POLYEDRES
C INTEGER    NS       : NOMBRE DE SOMMETS DES DEUX POLYEDREES
C INTEGER    FS(3,*)  : SOMMETS DES FACES TRIANGLES
C                       SENS DE PARCOURS, NORMALE SORTANTE A DROITE
C REAL*8     FQ(4,*)  : EQUATION DE DROITE ASSOCIEE AUX FACE
C                       FQ1 * X + FQ2 * Y + FQ3 * Z + FQ4 = 0
C                       AVEC (FQ1, FQ2, FQ3) NORMALE SORTANTE
C INTEGER    NF1      : NOMBRE DE FACES DU POLYEDRE CONVEXE
C INTEGER    NF2      : NOMBRE DE FACES DE L'AUTRE POLYEDRE
C INTEGER    AS(2,*)  : SOMMETS COMPOSANT LES ARETES DES POLYEDRES
C INTEGER    AF(2,*)  : FACES ADJACENTES AUX ARETES DES POLYEDRES
C INTEGER    NA1      : NOMBRE D'ARETES DU POLYEDRE CONVEXE
C INTEGER    NA2      : NOMBRE D'ARETES DE L'AUTRE POLYEDRE
C DANS AS ET AF : NA1 ARETES POLY CONVEXE PUIS NA2 ARETES AUTRE POLYEDRE
C
C VECTEURS DE TRAVAIL
C REAL*8     ZR(*)    : NNS
C INTEGER    ZI(*)    : 8*NS + 16*NNS + 4*(NA1+NA2) + 3*(NF1+NF2)
C LOGICAL    ZL(*)    : NF1 + NF2
C
C VARIABLES EN SORTIE
C REAL*8     SC(3,*)  : COORDONNEES DES SOMMETS DE L'INTERSECTION
C INTEGER    FS(3,*)  : SOMMETS DES FACES TRIANGLES DE L'INTERSECTION
C                       RANGES PAR COMPOSANTES CONNEXES
C INTEGER    ZI(*)    : NOMBRE DE FACES DES COMPOSANTES CONNEXES DE
C                       L'INTERSECTION
C INTEGER    NC       : NOMBRE DE COMPOSANTES CONNEXES
C
C DIMENSION
C NOUVEAUX SOMMETS : NNS = 3*MAX(NF1,NF2) (APPROCHE)
C NOUVELLES ARETES : NNA = 2*NNS
C NOUVELLES FACES  : NNF = 2*(NS+NNS)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE
      INTEGER NCMAX
      PARAMETER (NCMAX = 8)

C --- FONCTION
      REAL*8  DDOT

C --- VARIABLES
      INTEGER AS(2,*),AF(2,*),FS(3,*),ZI(*),NSC(NCMAX),NC
      INTEGER NA0,NA1,NA2,NA,A0,A1,A2,NS0,NSF,NS,S0,S1,S2,N,NT
      INTEGER NF0,NF1,NF2,NF,F0,F1,F2,F3,F4,NI,I,J,P0,P1,P2,P3,P4
      REAL*8  SC(3,*),FQ(4,*),ZR(*),SI(3),V(3),R0,R1,R2
      LOGICAL ZL(*),IR,IS

      NA = NA1 + NA2
      NF = NF1 + NF2
      NC = 0

C --- 1. NOUVEAUX SOMMETS

      NS0 = NS

      P0 = 6*MAX(NF1,NF2)
      DO 10 A0 = 1, NA
        ZI(P0+A0) = 0
 10   CONTINUE

      P3 = P0 + NA + 1
      NI = 0

C --- 1.1 NOUVEAUX SOMMETS INTERSECTION DES ARETES 2 ET DES FACES 1

      DO 20 I = 1, NA2

        A2 = NA1 + I
        S1 = AS(1,A2)
        S2 = AS(2,A2)

        DO 20 F1 = 1, NF1

C ------- 1.1.1 S1 ET S2 DE PAR ET D'AUTRE DU PLAN DEFINI PAR F1 ?

          R1 = DDOT(3,FQ(1,F1),1,SC(1,S1),1) + FQ(4,F1)
          R2 = DDOT(3,FQ(1,F1),1,SC(1,S2),1) + FQ(4,F1)
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(R1.EQ.0.D0)
     &        .OR.(R2.EQ.0.D0)) GOTO 20

C ------- 1.1.2 COORDONNEES DE L'INTERSECTION

          R1 = R1/(R1 - R2)
          SI(1) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          SI(2) = (1-R1)*SC(2,S1) + R1*SC(2,S2)
          SI(3) = (1-R1)*SC(3,S1) + R1*SC(3,S2)

C ------- 1.1.3 INTERSECTION APPARTIENT A F1 ?

          CALL PROVE3(SC(1,FS(1,F1)),SC(1,FS(2,F1)),SI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

          CALL PROVE3(SC(1,FS(2,F1)),SC(1,FS(3,F1)),SI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

          CALL PROVE3(SC(1,FS(3,F1)),SC(1,FS(1,F1)),SI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

C ------- 1.1.4 STOCKAGE SOMMET INTERSECTION

          NI = NI + 1
          ZR(NI) = R1

          ZI(2*NI-1) = A2
          ZI(2*NI  ) = F1

          NS = NS + 1
          CALL DCOPY(3,SI,1,SC(1,NS),1)

C ------- 1.1.5 INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A2)

          P1 = ZI(P0+A2)
          P2 = 0

 30       CONTINUE

          IF (P1.NE.0) THEN
            R0 = ZR(ABS(ZI(P1)))
            IF (R0.LT.R1) THEN
              P2 = P1
              P1 = ZI(P1+1)
              GOTO 30
            ENDIF
          ENDIF

          ZI(P3+1) = P1
          IF (P2.NE.0) THEN
            ZI(P2+1) = P3
          ELSE
            ZI(P0+A2) = P3
          ENDIF

C ------- 1.1.6 SIGNE DU SOMMET INTERSECTION

          R0 = FQ(1,F1)*(SC(1,S2)-SC(1,S1))
     &       + FQ(2,F1)*(SC(2,S2)-SC(2,S1))
     &       + FQ(3,F1)*(SC(3,S2)-SC(3,S1))

          IF (R0.GT.0.D0) THEN
            ZI(P3) = NI
          ELSE
            ZI(P3) = -NI
          ENDIF

          P3 = P3 + 2

 20   CONTINUE

C --- 1.2 NOUVEAUX SOMMETS INTERSECTION DES ARETES 1 ET DES FACES 2

      DO 40 I = 1, NF2

        F2 = NF1 + I

        DO 40 A1 = 1, NA1

          S1 = AS(1,A1)
          S2 = AS(2,A1)

C ------- 1.2.1 S1 ET S2 DE PAR ET D'AUTRE DU PLAN DEFINI PAR F2 ?

          R1 = DDOT(3,FQ(1,F2),1,SC(1,S1),1) + FQ(4,F2)
          R2 = DDOT(3,FQ(1,F2),1,SC(1,S2),1) + FQ(4,F2)
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(R1.EQ.0.D0)
     &        .OR.(R2.EQ.0.D0)) GOTO 40

C ------- 1.2.2 COORDONNEES DE L'INTERSECTION

          R1 = R1/(R1 - R2)
          SI(1) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          SI(2) = (1-R1)*SC(2,S1) + R1*SC(2,S2)
          SI(3) = (1-R1)*SC(3,S1) + R1*SC(3,S2)

C ------- 1.2.3 INTERSECTION APPARTIENT A F2 ?

          CALL PROVE3(SC(1,FS(1,F2)),SC(1,FS(2,F2)),SI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,FS(2,F2)),SC(1,FS(3,F2)),SI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,FS(3,F2)),SC(1,FS(1,F2)),SI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

C ------- 1.2.4 STOCKAGE SOMMET INTERSECTION

          NI = NI + 1
          ZR(NI) = R1

          ZI(2*NI-1) = A1
          ZI(2*NI  ) = F2

          NS = NS + 1
          CALL DCOPY(3,SI,1,SC(1,NS),1)

C ------- 1.2.5 INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A1)

          P1 = ZI(P0+A1)
          P2 = 0

 50       CONTINUE

          IF (P1.NE.0) THEN
            R0 = ZR(ABS(ZI(P1)))
            IF (R0.LT.R1) THEN
              P2 = P1
              P1 = ZI(P1+1)
              GOTO 50
            ENDIF
          ENDIF

          ZI(P3+1) = P1
          IF (P2.NE.0) THEN
            ZI(P2+1) = P3
          ELSE
            ZI(P0+A1) = P3
          ENDIF

C ------- 1.2.6 SIGNE DU SOMMET INTERSECTION

          R0 = FQ(1,F2)*(SC(1,S2)-SC(1,S1))
     &       + FQ(2,F2)*(SC(2,S2)-SC(2,S1))
     &       + FQ(3,F2)*(SC(3,S2)-SC(3,S1))

          IF (R0.GT.0.D0) THEN
            ZI(P3) = NI
          ELSE
            ZI(P3) = -NI
          ENDIF

          P3 = P3 + 2

 40   CONTINUE

      IF (NI.EQ.0) GOTO 270

C --- 2. NOUVELLES ARETES

      NA0 = NA

C --- 2.1 DECOUPAGE DES ARETES A PARTIR DE LA STRUCTURE AI

      DO 60 I = 1, NA0

        P1 = ZI(P0+I)
        S1 = AS(1,I)
        S2 = AS(2,I)
        IR = .TRUE.

C ----- 2.1.1 PARCOURS DE LA STRUCTURE AI(I)

 70     CONTINUE

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
              AF(1,NA) = AF(1,I)
              AF(2,NA) = AF(2,I)
            ENDIF
          ELSE
            S1 = NS0 - S0
          ENDIF

          P1 = ZI(P1+1)
          GOTO 70

        ENDIF

        IF (S0.LT.0) THEN
          IF (IR) THEN
            AS(1,I) = S1
            AS(2,I) = S2
          ELSE
            NA = NA + 1
            AS(1,NA) = S1
            AS(2,NA) = S2
            AF(1,NA) = AF(1,I)
            AF(2,NA) = AF(2,I)
          ENDIF
        ENDIF

 60   CONTINUE

C --- 2.2 NOUVELLES ARETES RELIANT NOUVEAUX SOMMETS

      DO 80 I = 1, 2*NI
        ZI(P0+4*I) = 0
 80   CONTINUE

      N = 0
      P3 = 1

      DO 90 I = 1, NI

        J = ZI(P3)
        F0 = ZI(P3+1)
        F1 = AF(1,J)
        F2 = AF(2,J)
        S0 = NS0 + I
        P3 = P3 + 2

C ----- 2.2.1 STRUCTURE AT (FACE, FACE, SOMMET, SOMMET) SOMMETS NOUVEAUX
        IF (F0.LT.F1) THEN
          F3 = F0
          F4 = F1
        ELSE
          F3 = F1
          F4 = F0
        ENDIF

        DO 100 J = 1, N
          IF (ZI(P0+4*J-3).NE.F3) GOTO 100
          IF (ZI(P0+4*J-2).NE.F4) GOTO 100
          ZI(P0+4*J) = S0
          GOTO 110
 100    CONTINUE

        N = N + 1
        ZI(P0+4*N-3) = F3
        ZI(P0+4*N-2) = F4
        ZI(P0+4*N-1) = S0

 110    CONTINUE

        IF (F0.LT.F2) THEN
          F3 = F0
          F4 = F2
        ELSE
          F3 = F2
          F4 = F0
        ENDIF

        DO 120 J = 1, N
          IF (ZI(P0+4*J-3).NE.F3) GOTO 120
          IF (ZI(P0+4*J-2).NE.F4) GOTO 120
          ZI(P0+4*J) = S0
          GOTO 90
 120    CONTINUE

        N = N + 1
        ZI(P0+4*N-3) = F3
        ZI(P0+4*N-2) = F4
        ZI(P0+4*N-1) = S0

 90   CONTINUE

C --- 2.2.2 PARCOURS AT ET CREATION ARETES LIANT SOMMETS NOUVEAUX

      DO 130 I = 1, N

        IF (ZI(P0+4*I).EQ.0) GOTO 130

        NA = NA + 1
        AF(1,NA) = ZI(P0+4*I-3)
        AF(2,NA) = ZI(P0+4*I-2)
        AS(1,NA) = ZI(P0+4*I-1)
        AS(2,NA) = ZI(P0+4*I  )

 130  CONTINUE

C --- 3. CONNECTIVITE DES FACES

C --- 3.1 GRAPHE FACE - ARETES (FA)

      P0 = NF + 1

      DO 140 I = 1, NF
        ZI(I) = 0
 140  CONTINUE

      DO 150 I = 1, NA

        F1 = AF(1,I)
        F2 = AF(2,I)

        ZI(P0) = I
        ZI(P0+1) = ZI(F1)
        ZI(F1) = P0
        P0 = P0 + 2

        IF (F2.EQ.0) GOTO 150

        ZI(P0) = I
        ZI(P0+1) = ZI(F2)
        ZI(F2) = P0
        P0 = P0 + 2

 150  CONTINUE

C --- 3.2 CONNECTIVITE : GRAPHE FACE - SOMMETS

      P4 = P0 + 6*NF
      NF0 = NF
      NF = 1

      DO 160 F0 = 1, NF0

C ----- 3.2.1. BOUCLE SUR LES COMPOSANTES CONNEXES DE LA FACE

        IR = .FALSE.
        P3 = ZI(F0)

 170    CONTINUE

        P1 = P3
        IF (P1.EQ.0) GOTO 160

        P3 = ZI(P1+1)
        IF (ZI(P1).LE.0) GOTO 170

        A0 = ZI(P1)
        ZI(P1) = -A0
        S0 = AS(1,A0)
        S1 = AS(2,A0)
        ZI(P4) = S0
        NSF = 1

        IS = S0.LE.NS0

        IF (AF(1,A0).NE.F0) AF(2,A0) = AF(1,A0)
        AF(1,A0) = NF

C ----- 3.2.2. PARCOURS DE AS(FA(I)) POUR TROUVER SOMMET SUIVANT S1

        P2 = P3

 180    CONTINUE

        P1 = P2
        IF (P1.EQ.0) THEN
          P1 = ZI(F0)
 190      CONTINUE
          IF (P1.EQ.0) GOTO 170
          A0 = -ZI(P1)
          IF ((A0.GT.0).AND.(AF(1,A0).EQ.NF)) ZI(P1) = 0
          P1 = ZI(P1+1)
          GOTO 190
        ENDIF

        A0 = ZI(P1)
        P2 = ZI(P1+1)
        IF (A0.LE.0) GOTO 180

        IF (AS(1,A0).EQ.S1) THEN
          S2 = AS(2,A0)
          ZI(P1) = -A0
        ELSEIF (AS(2,A0).EQ.S1) THEN
          S2 = AS(1,A0)
          ZI(P1) = -A0
        ELSE
          GOTO 180
        ENDIF

        IF (AF(1,A0).NE.F0) AF(2,A0) = AF(1,A0)
        AF(1,A0) = NF

        ZI(P4+NSF) = S1
        NSF = NSF + 1

        IF (S1.GT.NS0) IS = .FALSE.

        IF (S2.NE.S0) THEN
          S1 = S2
          P2 = P3
          GOTO 180
        ENDIF

        IF (IR.AND.IS) THEN
          P1 = ZI(F0)
 200      CONTINUE
          IF (P1.EQ.0) GOTO 170
          A0 = -ZI(P1)
          IF ((A0.GT.0).AND.(AF(1,A0).EQ.NF)) ZI(P1) = 0
          P1 = ZI(P1+1)
          GOTO 200
        ENDIF

        ZI(P0+3*NF-3) = NSF
        ZI(P0+3*NF-2) = P4
        ZI(P0+3*NF-1) = F0
        P4 = P4 + NSF
        NF = NF + 1

        IR = .TRUE.

        GOTO 170

 160  CONTINUE

C --- 4. COMPOSANTES CONNEXES ET TRIANGULATION DES FACES

      NF0 = NF - 1
      NF = 0

      DO 210 F0 = 1, NF0
        ZL(F0) = .TRUE.
 210  CONTINUE

      DO 220 F0 = 1, NF0

        IF (.NOT.ZL(F0)) GOTO 220

        NSF = ZI(P0+3*F0-3)
        P1 = ZI(P0+3*F0-2)
        F1 = ZI(P0+3*F0-1)

        DO 230 J = 1, NSF

          IF (ZI(P1-1+J).GT.NS0) THEN

            NF1 = NF
            NC = NC + 1

            ZL(F0) = .FALSE.
            CALL PLTRI2(3,SC,FQ(1,F1),ZI(P1),NSF,FS(1,NF+1),NT)
            NF = NF + NT
            ZI(P4) = F1
            N = 1
            I = 0
            GOTO 240

          ENDIF

 230    CONTINUE

        GOTO 220

 240    CONTINUE

        P1 = ZI(ZI(P4+I))
        I = I + 1

 250    CONTINUE

        IF (P1.NE.0) THEN

          A0 = -ZI(P1)
          P1 = ZI(P1+1)
          IF (A0.EQ.0) GOTO 250

          F1 = AF(1,A0)
          F2 = AF(2,A0)

          IF (ZL(F1)) THEN
            ZL(F1) = .FALSE.
            NSF = ZI(P0+3*F1-3)
            F3 = ZI(P0+3*F1-1)
          CALL PLTRI2(3,SC,FQ(1,F3),ZI(ZI(P0+3*F1-2)),NSF,FS(1,NF+1),NT)
            NF = NF + NT
            ZI(P4+N) = F3
            N = N + 1
          ENDIF

          IF ((F2.NE.0).AND.(ZL(F2))) THEN
            ZL(F2) = .FALSE.
            NSF = ZI(P0+3*F2-3)
            F4 = ZI(P0+3*F2-1)
          CALL PLTRI2(3,SC,FQ(1,F4),ZI(ZI(P0+3*F2-2)),NSF,FS(1,NF+1),NT)
            NF = NF + NT
            ZI(P4+N) = F4
            N = N + 1
          ENDIF

          GOTO 250

        ELSE

          IF (I.NE.N) GOTO 240

        ENDIF

        NSC(NC) = NF - NF1

 220  CONTINUE

      IF (NC.GT.NCMAX) CALL U2MESS('A','MODELISA6_24')

      DO 260 I = 1, NC
        ZI(I) = NSC(I)
 260  CONTINUE

 270  CONTINUE

      END
