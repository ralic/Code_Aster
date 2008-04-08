      SUBROUTINE PLINT3(SC    ,NBNO  ,FS    ,FQ    ,NCMAX ,
     &                  PRECTR,NFAC1 ,NFAC2 ,AS    ,AF    ,
     &                  NARE1 ,NARE2 ,SI    ,IS    ,TRAVL ,
     &                  NC)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      REAL*8  SC(3,*)
      REAL*8  PRECTR
      INTEGER NBNO
      INTEGER AS(2,*)
      INTEGER AF(2,*)
      INTEGER FS(3,*)
      INTEGER NCMAX
      INTEGER NARE1
      INTEGER NARE2
      INTEGER NFAC1
      INTEGER NFAC2
      REAL*8  SI(*)
      INTEGER IS(*)
      LOGICAL TRAVL(*)
      INTEGER NC
      REAL*8  FQ(4,*)
C
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C INTERSECTION DE DEUX POLYEDRES (3D)
C
C ----------------------------------------------------------------------
C
C
C I/O SC     : IN  - COORDONNEES DES SOMMETS DES DEUX POLYEDRES
C              OUT - COORDONNEES DES SOMMETS DE L'INTERSECTION
C IN  NBNO   : NOMBRE DE SOMMETS DES DEUX POLYEDREES
C                (AVEC ECHANTILLONNAGE)
C I/O FS     : NOEUDS DEFINISSANT LES FACETTES DES DEUX POLYEDRES
C                (FACE1.ND1,FACE1.ND2,FACE1.ND3,
C                 FACE2.ND1,FACE2.ND2,FACE2.ND3,...)
C              OUT - NOEUDS DEFINISSANT LES FACETTES DE L'INTERSECTION
C                       RANGES PAR COMPOSANTES CONNEXES
C IN  FQ     : EQUATION DE DROITE ASSOCIEE AUX FACES
C                       FQ1 * X + FQ2 * Y + FQ3 * Z + FQ4 = 0
C                       AVEC (FQ1, FQ2, FQ3) NORMALE SORTANTE
C IN  NCMAX  : NOMBRE MAXI DE COMPOSANTES CONNEXES
C IN  PRECTR : PRECISION POUR TRIANGULATION
C OUT SI     : COORD. PARAMETRIQUE SUR ARETE DE L'INTERSECTION
C                FACE/ARETE
C                (KSI.1,
C                 KSI.2,)
C              INDEXEE PAR NUMERO D'INTERSECTION
C              DIM: NNS
C OUT IS     : NOMBRE DE FACES DES COMPOSANTES CONNEXES DE
C               L'INTERSECTION
C                     DIM: 8*NBNO + 16*NNS + 4*(NARE1+NARE2) +
C                          3*(NFAC1+NFAC2)
C I/O TRAVL  : VECTEUR DE TRAVAIL DE BOOLEENS POUR PARCOURS
C              UNIQUE DES INTERSECTIONS (INUTILE POUR LA SUITE)
C                     DIM: NFAC1 + NFAC2
C IN  NFAC1  : NOMBRE DE FACETTES DU PREMIER POLYEDRE CONVEXE
C IN  NFAC2  : NOMBRE DE FACETTES DU SECOND POLYEDRE CONVEXE
C IN  AS     : SOMMETS COMPOSANT LES ARETES DES POLYEDRES
C                (ARETE1.ND1,ARETE1.ND2,
C                 ARETE2.ND1,ARETE2.ND2...)
C                NARE1 ARETES PREMIER POLY. CONVEXE PUIS
C                NARE2 ARETES SECOND POLY. CONVEXE
C IN  AF     : FACES ADJACENTES AUX ARETES DES POLYEDRES
C                (ARETE1.FACE1,ARETE1.FACE2,
C                 ARETE2.FACE1,ARETE2.FACE2...)
C                NARE1 ARETES PREMIER POLY. CONVEXE PUIS
C                NARE2 ARETES SECOND POLY. CONVEXE
C IN  NARE1  : NOMBRE D'ARETES DU PREMIER POLYEDRE CONVEXE
C IN  NARE2  : NOMBRE D'ARETES DU SECOND POLYEDRE CONVEXE
C OUT NC     : NOMBRE DE COMPOSANTES CONNEXES
C
C DIMENSIONS :
C NOUVEAUX SOMMETS : NNS = 3*MAX(NFAC1,NFAC2) (APPROCHE)
C NOUVELLES ARETES : NNA = 2*NNS
C NOUVELLES FACES  : NNF = 2*(NBNO+NNS)
C
C
C ----------------------------------------------------------------------
C
      REAL*8  DDOT
      INTEGER INSC(NCMAX)
      INTEGER NA0,NARE,A0,A1,A2,NS0,NSF,S0,S1,S2,N,NTRI
      INTEGER NF0,NFAC,F0,F1,F2,F3,F4,NI,I,J,P0,P1,P2,P3,P4
      REAL*8  CSI(3),V(3),R0,R1,R2
      LOGICAL LIR,LIS
      INTEGER     IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C
C --- INITIALISATIONS
C
      NARE  = NARE1 + NARE2
      NFAC  = NFAC1 + NFAC2
      NC    = 0
      NS0   = NBNO
      P0    = 6*MAX(NFAC1,NFAC2)
      DO 10 A0 = 1, NARE
        IS(P0+A0) = 0
 10   CONTINUE
C
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> *** CALCUL INTERSECTION '//
     &                'DE DEUX POLYEDRES '
      ENDIF
C
C --- 1. NOUVEAUX SOMMETS
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> ... NOUVEAUX SOMMETS ?'
      ENDIF
      P3 = P0 + NARE + 1
      NI = 0

C --- 1.1 NOUVEAUX SOMMETS INTERSECTION DES ARETES 2 ET DES FACES 1

      DO 20 I = 1, NARE2

        A2 = NARE1 + I
        S1 = AS(1,A2)
        S2 = AS(2,A2)

        DO 20 F1 = 1, NFAC1

C ------- 1.1.1 S1 ET S2 DE PAR ET D'AUTRE DU PLAN DEFINI PAR F1 ?

          R1 = DDOT(3,FQ(1,F1),1,SC(1,S1),1) + FQ(4,F1)
          R2 = DDOT(3,FQ(1,F1),1,SC(1,S2),1) + FQ(4,F1)
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(R1.EQ.0.D0)
     &        .OR.(R2.EQ.0.D0)) GOTO 20

C ------- 1.1.2 COORDONNEES DE L'INTERSECTION

          R1 = R1/(R1 - R2)
          CSI(1) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          CSI(2) = (1-R1)*SC(2,S1) + R1*SC(2,S2)
          CSI(3) = (1-R1)*SC(3,S1) + R1*SC(3,S2)

C ------- 1.1.3 INTERSECTION APPARTIENT A F1 ?

          CALL PROVE3(SC(1,FS(1,F1)),SC(1,FS(2,F1)),CSI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

          CALL PROVE3(SC(1,FS(2,F1)),SC(1,FS(3,F1)),CSI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

          CALL PROVE3(SC(1,FS(3,F1)),SC(1,FS(1,F1)),CSI,V)
          IF (DDOT(3,FQ(1,F1),1,V,1).LT.0.D0) GOTO 20

C ------- 1.1.4 STOCKAGE SOMMET INTERSECTION

          NI = NI + 1
          SI(NI) = R1

          IS(2*NI-1) = A2
          IS(2*NI  ) = F1

          NBNO = NBNO + 1
          CALL DCOPY(3,CSI,1,SC(1,NBNO),1)

C ------- 1.1.5 INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A2)

          P1 = IS(P0+A2)
          P2 = 0

 30       CONTINUE

          IF (P1.NE.0) THEN
            R0 = SI(ABS(IS(P1)))
            IF (R0.LT.R1) THEN
              P2 = P1
              P1 = IS(P1+1)
              GOTO 30
            ENDIF
          ENDIF

          IS(P3+1) = P1
          IF (P2.NE.0) THEN
            IS(P2+1) = P3
          ELSE
            IS(P0+A2) = P3
          ENDIF

C ------- 1.1.6 SIGNE DU SOMMET INTERSECTION

          R0 = FQ(1,F1)*(SC(1,S2)-SC(1,S1))
     &       + FQ(2,F1)*(SC(2,S2)-SC(2,S1))
     &       + FQ(3,F1)*(SC(3,S2)-SC(3,S1))

          IF (R0.GT.0.D0) THEN
            IS(P3) = NI
          ELSE
            IS(P3) = -NI
          ENDIF

          P3 = P3 + 2

 20   CONTINUE

C --- 1.2 NOUVEAUX SOMMETS INTERSECTION DES ARETES 1 ET DES FACES 2

      DO 40 I = 1, NFAC2

        F2 = NFAC1 + I

        DO 40 A1 = 1, NARE1

          S1 = AS(1,A1)
          S2 = AS(2,A1)

C ------- 1.2.1 S1 ET S2 DE PAR ET D'AUTRE DU PLAN DEFINI PAR F2 ?

          R1 = DDOT(3,FQ(1,F2),1,SC(1,S1),1) + FQ(4,F2)
          R2 = DDOT(3,FQ(1,F2),1,SC(1,S2),1) + FQ(4,F2)
          IF (((R1.LT.0.D0).EQV.(R2.LT.0.D0)).OR.(R1.EQ.0.D0)
     &        .OR.(R2.EQ.0.D0)) GOTO 40

C ------- 1.2.2 COORDONNEES DE L'INTERSECTION

          R1 = R1/(R1 - R2)
          CSI(1) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          CSI(2) = (1-R1)*SC(2,S1) + R1*SC(2,S2)
          CSI(3) = (1-R1)*SC(3,S1) + R1*SC(3,S2)

C ------- 1.2.3 INTERSECTION APPARTIENT A F2 ?

          CALL PROVE3(SC(1,FS(1,F2)),SC(1,FS(2,F2)),CSI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,FS(2,F2)),SC(1,FS(3,F2)),CSI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

          CALL PROVE3(SC(1,FS(3,F2)),SC(1,FS(1,F2)),CSI,V)
          IF (DDOT(3,FQ(1,F2),1,V,1).LT.0.D0) GOTO 40

C ------- 1.2.4 STOCKAGE SOMMET INTERSECTION

          NI = NI + 1
          SI(NI) = R1

          IS(2*NI-1) = A1
          IS(2*NI  ) = F2

          NBNO = NBNO + 1
          CALL DCOPY(3,CSI,1,SC(1,NBNO),1)

C ------- 1.2.5 INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A1)

          P1 = IS(P0+A1)
          P2 = 0

 50       CONTINUE

          IF (P1.NE.0) THEN
            R0 = SI(ABS(IS(P1)))
            IF (R0.LT.R1) THEN
              P2 = P1
              P1 = IS(P1+1)
              GOTO 50
            ENDIF
          ENDIF

          IS(P3+1) = P1
          IF (P2.NE.0) THEN
            IS(P2+1) = P3
          ELSE
            IS(P0+A1) = P3
          ENDIF

C ------- 1.2.6 SIGNE DU SOMMET INTERSECTION

          R0 = FQ(1,F2)*(SC(1,S2)-SC(1,S1))
     &       + FQ(2,F2)*(SC(2,S2)-SC(2,S1))
     &       + FQ(3,F2)*(SC(3,S2)-SC(3,S1))

          IF (R0.GT.0.D0) THEN
            IS(P3) = NI
          ELSE
            IS(P3) = -NI
          ENDIF

          P3 = P3 + 2

 40   CONTINUE
C
      IF (NIV.GE.2) THEN
      IF (NI.EQ.0) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> ...... PAS D''INTERSECTIONS '//
     &               'DETECTEES'
        GOTO 270
      ELSE
        WRITE(IFM,*) '<ARLEQUIN><INT> ...... ',NI,
     &               ' INTERSECTIONS DETECTEES'
      ENDIF
      ENDIF
C
C --- 2. NOUVELLES ARETES
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> ... CREATION NOUVELLES ARETES'
      ENDIF
      NA0 = NARE

C --- 2.1 DECOUPAGE DES ARETES A PARTIR DE LA STRUCTURE AI

      DO 60 I = 1, NA0

        P1 = IS(P0+I)
        S1 = AS(1,I)
        S2 = AS(2,I)
        LIR = .TRUE.

C ----- 2.1.1 PARCOURS DE LA STRUCTURE AI(I)

 70     CONTINUE

        IF (P1.NE.0) THEN

          S0 = IS(P1)
          IF (S0.GT.0) THEN
            IF (LIR) THEN
              AS(1,I) = S1
              AS(2,I) = NS0 + S0
              LIR = .FALSE.
            ELSE
              NARE = NARE + 1
              AS(1,NARE) = S1
              AS(2,NARE) = NS0 + S0
              AF(1,NARE) = AF(1,I)
              AF(2,NARE) = AF(2,I)
            ENDIF
          ELSE
            S1 = NS0 - S0
          ENDIF

          P1 = IS(P1+1)
          GOTO 70

        ENDIF

        IF (S0.LT.0) THEN
          IF (LIR) THEN
            AS(1,I) = S1
            AS(2,I) = S2
          ELSE
            NARE = NARE + 1
            AS(1,NARE) = S1
            AS(2,NARE) = S2
            AF(1,NARE) = AF(1,I)
            AF(2,NARE) = AF(2,I)
          ENDIF
        ENDIF

 60   CONTINUE

C --- 2.2 NOUVELLES ARETES RELIANT NOUVEAUX SOMMETS

      DO 80 I = 1, 2*NI
        IS(P0+4*I) = 0
 80   CONTINUE

      N = 0
      P3 = 1

      DO 90 I = 1, NI

        J = IS(P3)
        F0 = IS(P3+1)
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
          IF (IS(P0+4*J-3).NE.F3) GOTO 100
          IF (IS(P0+4*J-2).NE.F4) GOTO 100
          IS(P0+4*J) = S0
          GOTO 110
 100    CONTINUE

        N = N + 1
        IS(P0+4*N-3) = F3
        IS(P0+4*N-2) = F4
        IS(P0+4*N-1) = S0

 110    CONTINUE

        IF (F0.LT.F2) THEN
          F3 = F0
          F4 = F2
        ELSE
          F3 = F2
          F4 = F0
        ENDIF

        DO 120 J = 1, N
          IF (IS(P0+4*J-3).NE.F3) GOTO 120
          IF (IS(P0+4*J-2).NE.F4) GOTO 120
          IS(P0+4*J) = S0
          GOTO 90
 120    CONTINUE

        N = N + 1
        IS(P0+4*N-3) = F3
        IS(P0+4*N-2) = F4
        IS(P0+4*N-1) = S0

 90   CONTINUE
C
C --- 2.2.2 PARCOURS AT ET CREATION ARETES LIANT SOMMETS NOUVEAUX
C
      DO 130 I = 1, N
        IF (IS(P0+4*I).EQ.0) GOTO 130
        NARE = NARE + 1
        AF(1,NARE) = IS(P0+4*I-3)
        AF(2,NARE) = IS(P0+4*I-2)
        AS(1,NARE) = IS(P0+4*I-1)
        AS(2,NARE) = IS(P0+4*I  )
 130  CONTINUE
C
C --- 3. CONNECTIVITE DES FACES
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> ... CREATION CONNECTIVITE FACES'
      ENDIF
C --- 3.1 GRAPHE FACE - ARETES (FA)
C
      P0 = NFAC + 1

      DO 140 I = 1, NFAC
        IS(I) = 0
 140  CONTINUE
      DO 150 I = 1, NARE

        F1 = AF(1,I)
        F2 = AF(2,I)

        IS(P0) = I
        IS(P0+1) = IS(F1)
        IS(F1) = P0
        P0 = P0 + 2

        IF (F2.EQ.0) GOTO 150

        IS(P0) = I
        IS(P0+1) = IS(F2)
        IS(F2) = P0
        P0 = P0 + 2

 150  CONTINUE
C
C --- 3.2 CONNECTIVITE : GRAPHE FACE - SOMMETS
C
      P4 = P0 + 6*NFAC
      NF0 = NFAC
      NFAC = 1

      DO 160 F0 = 1, NF0

C ----- 3.2.1. BOUCLE SUR LES COMPOSANTES CONNEXES DE LA FACE

        LIR = .FALSE.
        P3 = IS(F0)

 170    CONTINUE

        P1 = P3
        IF (P1.EQ.0) GOTO 160

        P3 = IS(P1+1)
        IF (IS(P1).LE.0) GOTO 170

        A0 = IS(P1)
        IS(P1) = -A0
        S0 = AS(1,A0)
        S1 = AS(2,A0)
        IS(P4) = S0
        NSF = 1

        LIS = S0.LE.NS0

        IF (AF(1,A0).NE.F0) AF(2,A0) = AF(1,A0)
        AF(1,A0) = NFAC

C ----- 3.2.2. PARCOURS DE AS(FA(I)) POUR TROUVER SOMMET SUIVANT S1

        P2 = P3

 180    CONTINUE

        P1 = P2
        IF (P1.EQ.0) THEN
          P1 = IS(F0)
 190      CONTINUE
          IF (P1.EQ.0) GOTO 170
          A0 = -IS(P1)
          IF ((A0.GT.0).AND.(AF(1,A0).EQ.NFAC)) IS(P1) = 0
          P1 = IS(P1+1)
          GOTO 190
        ENDIF

        A0 = IS(P1)
        P2 = IS(P1+1)
        IF (A0.LE.0) GOTO 180

        IF (AS(1,A0).EQ.S1) THEN
          S2 = AS(2,A0)
          IS(P1) = -A0
        ELSEIF (AS(2,A0).EQ.S1) THEN
          S2 = AS(1,A0)
          IS(P1) = -A0
        ELSE
          GOTO 180
        ENDIF

        IF (AF(1,A0).NE.F0) AF(2,A0) = AF(1,A0)
        AF(1,A0) = NFAC

        IS(P4+NSF) = S1
        NSF = NSF + 1

        IF (S1.GT.NS0) LIS = .FALSE.

        IF (S2.NE.S0) THEN
          S1 = S2
          P2 = P3
          GOTO 180
        ENDIF

        IF (LIR.AND.LIS) THEN
          P1 = IS(F0)
 200      CONTINUE
          IF (P1.EQ.0) GOTO 170
          A0 = -IS(P1)
          IF ((A0.GT.0).AND.(AF(1,A0).EQ.NFAC)) IS(P1) = 0
          P1 = IS(P1+1)
          GOTO 200
        ENDIF

        IS(P0+3*NFAC-3) = NSF
        IS(P0+3*NFAC-2) = P4
        IS(P0+3*NFAC-1) = F0
        P4 = P4 + NSF
        NFAC = NFAC + 1

        LIR = .TRUE.

        GOTO 170

 160  CONTINUE
C
C --- 4. COMPOSANTES CONNEXES ET TRIANGULATION DES FACES
C
      NF0 = NFAC - 1
      NFAC = 0

      DO 210 F0 = 1, NF0
        TRAVL(F0) = .TRUE.
 210  CONTINUE

      DO 220 F0 = 1, NF0

        IF (.NOT.TRAVL(F0)) GOTO 220

        NSF = IS(P0+3*F0-3)
        P1 = IS(P0+3*F0-2)
        F1 = IS(P0+3*F0-1)

        DO 230 J = 1, NSF

          IF (IS(P1-1+J).GT.NS0) THEN

            NFAC1 = NFAC
            NC = NC + 1

            TRAVL(F0) = .FALSE.

            CALL PLTRI2(3,SC,FQ(1,F1),IS(P1),NSF,
     &                  PRECTR,FS(1,NFAC+1),NTRI)
            NFAC = NFAC + NTRI
            IS(P4) = F1
            N = 1
            I = 0

            GOTO 240

          ENDIF

 230    CONTINUE

        GOTO 220

 240    CONTINUE

        P1 = IS(IS(P4+I))
        I = I + 1

 250    CONTINUE

        IF (P1.NE.0) THEN

          A0 = -IS(P1)
          P1 = IS(P1+1)
          IF (A0.EQ.0) THEN
            GOTO 250
           ENDIF
          F1 = AF(1,A0)
          F2 = AF(2,A0)

          IF (TRAVL(F1)) THEN
            TRAVL(F1) = .FALSE.
            NSF = IS(P0+3*F1-3)
            F3 = IS(P0+3*F1-1)
            CALL PLTRI2(3,SC,FQ(1,F3),IS(IS(P0+3*F1-2)),NSF,
     &                  PRECTR,FS(1,NFAC+1),NTRI)
            NFAC = NFAC + NTRI
            IS(P4+N) = F3
            N = N + 1
          ENDIF

          IF ((F2.NE.0).AND.TRAVL(F2)) THEN
            TRAVL(F2) = .FALSE.
            NSF = IS(P0+3*F2-3)
            F4 = IS(P0+3*F2-1)
            CALL PLTRI2(3,SC,FQ(1,F4),IS(IS(P0+3*F2-2)),NSF,
     &                  PRECTR,FS(1,NFAC+1),NTRI)
            NFAC = NFAC + NTRI
            IS(P4+N) = F4
            N = N + 1
          ENDIF

          GOTO 250

        ELSE

          IF (I.NE.N) GOTO 240

        ENDIF

        INSC(NC) = NFAC - NFAC1

 220  CONTINUE
C
      DO 260 I = 1, NCMAX
        IS(I) = INSC(I)
 260  CONTINUE
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><INT> ... NBRE COMPOSANTES CONNEXES: ',
     &                 NC
        WRITE(IFM,*) '<ARLEQUIN><INT> *** FIN CALCUL INTERSECTION '//
     &                'DE DEUX POLYEDRES '
      ENDIF
C

 270  CONTINUE

      END
