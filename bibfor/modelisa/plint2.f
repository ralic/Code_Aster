      SUBROUTINE PLINT2(SC    ,NBNO  ,FS    ,DQ    ,NCMAX  ,
     &                  NSEG1 ,NSEG2 ,SI    ,
     &                  PREC,
     &                  IS    ,TRAVL  , NCMPCO)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/05/2009   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE FS PUBLISHED BY
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      REAL*8  SC(2,*)
      INTEGER NBNO
      INTEGER FS(2,*)
      REAL*8  DQ(3,*)
      INTEGER NCMAX
      INTEGER NSEG1
      INTEGER NSEG2
      REAL*8  SI(*)
      INTEGER IS(*)
      LOGICAL TRAVL(*)
      INTEGER NCMPCO
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
C I/O NBNO   : IN  - NOMBRE DE SOMMETS DES DEUX POLYGONES
C              OUT - NOMBRE DE SOMMETS DES DEUX POLYGONES +
C                    NOMBRE INTERSECTIONS
C IN  FS     : NOEUDS DEFINISSANT LES SEGMENTS DES DEUX POLYGONES
C                (SEG1.ND1,SEG1.ND2,
C                 SEG2.ND1,SEG2.ND2,...)
C IN  DQ     : EQUATION DE DROITE ASSOCIEE AUX ARETES
C                       AQ1 * X + AQ2 * Y + AQ3 = 0
C                       AVEC (AQ1, AQ2) NORMALE SORTANTE
C IN  NCMAX  : NOMBRE MAXI DE COMPOSANTES CONNEXES
C IN  NSEG1  : NOMBRE DE SEGMENTS PREMIER POLYGONE CONVEXE
C IN  NSEG2  : NOMBRE DE SEGMENTS SECOND  POLYGONE CONVEXE
C IN  PREC  : PRECISION DES TESTS
C OUT SI     : COORD. PARAMETRIQUE DE L'INTERSECTION SUR LES DEUX ARETES
C                (KSI.1.1,KSI.1.2,
C                 KSI.2.1,KSI.2.2,...)
C              INDEXEE PAR NUMERO D'INTERSECTION
C              DIM: 2*NNS
C OUT IS    : CONNECTIVITE DES COMPOSANTES CONNEXES DE L'INTERSECTION
C                     DIM: MAX(NA+4*NNS,NBNO+NNS+NCMAX+2*NA+2*NNA)
C                (NOMBRE SOMMETS COMPOSANTE 1, SOMMET.1, SOMMET.2,...
C                 NOMBRE SOMMETS COMPOSANTE 2, SOMMET.1, ...)
C              L'INDEX DU SOMMET SE REFERE A SC
C I/O TRAVL  : VECTEUR DE TRAVAIL DE BOOLEENS POUR PARCOURS
C              UNIQUE DES INTERSECTIONS (INUTILE POUR LA SUITE)
C                     DIM: NBNO+NNS
C OUT NCMPCO     : NOMBRE DE COMPOSANTES CONNEXES
C
C DIMENSIONS :
C NOUVEAUX SOMMETS : NNS = MAX(NSEG1,NSEG2) (APPROCHE)
C NOUVELLES ARETES : NNA = NNS/2
C
C ----------------------------------------------------------------------
C
      INTEGER N,NARE,NA0,NS0,NBRINT
      INTEGER A1,A2,S0,S1,S2,S3,S4
      INTEGER I,J,P0,P1,P2
      REAL*8  R1,R2,R3,R4
      REAL*8 PREC
      LOGICAL IR
      INTEGER     IFM,NIV

      INTEGER      NI,NR,NK
      PARAMETER   ( NI = 1 , NR = 2 , NK = 1 )
      INTEGER      VALI(NI)
      REAL*8       VALR(NR)
      CHARACTER*24 VALK(NK)

      CHARACTER*6  NOMPRO
      PARAMETER   (NOMPRO='PLINT2')
         PREC=0.D0
C
C ----------------------------------------------------------------------
C====
C 1. INITIALISATIONS
C====
C
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C
      NBRINT   = 0
      NCMPCO   = 0
      S0       = -123
      NARE = NSEG1 + NSEG2
      P0   = NARE + 1
C
      DO 10 , I = 1, NARE
        IS(I) = 0
   10 CONTINUE
C
C --- AFFICHAGE
C
      CALL ARLDBG(NOMPRO,NIV,IFM,1,NI,VALI,NR,VALR,NK,VALK)
C
C====
C 2. NOUVEAUX SOMMETS ?
C====
C
      CALL ARLDBG(NOMPRO,NIV,IFM,2,NI,VALI,NR,VALR,NK,VALK)
      NS0 = NBNO
C
C ON PARCOURT CHAQUE SEGMENT DECRIVANT LA FRONTIERE DE LA MAILLE 1
C
      DO 20 , A1 = 1, NSEG1
C
C S1 ET S2 SONT LES EXTREMITES DU SEGMENT A1
C
        S1 = FS(1,A1)
        S2 = FS(2,A1)
C
C ON PARCOURT CHAQUE SEGMENT DECRIVANT LA FRONTIERE DE LA MAILLE 2
C
        DO 21 , J = 1, NSEG2
C
C S3 ET S4 SONT LES EXTREMITES DU SEGMENT A2
C
          A2 = NSEG1 + J
          S3 = FS(1,A2)
          S4 = FS(2,A2)

C 2.1. ==> TEST INTERSECTION ENTRE LES SEGMENTS A1 ET A2
C 2.1.1. ==> S3 ET S4 SONT-ILS DU MEME COTE DU SEGMENT A1 ?
          R3 = DQ(1,A1)*SC(1,S3) + DQ(2,A1)*SC(2,S3) + DQ(3,A1)
          R4 = DQ(1,A1)*SC(1,S4) + DQ(2,A1)*SC(2,S4) + DQ(3,A1)
          IF ( ( (R3.LT.0.D0) .EQV. (R4.LT.0.D0) ) .OR.
     >         ( ABS(R3).LE.PREC ) .OR.
     >         ( ABS(R4).LE.PREC ) ) GOTO 21

C 2.1.2. ==> S1 ET S2 SONT-ILS DU MEME COTE DU SEGMENT A2 ?
          R1 = DQ(1,A2)*SC(1,S1) + DQ(2,A2)*SC(2,S1) + DQ(3,A2)
          R2 = DQ(1,A2)*SC(1,S2) + DQ(2,A2)*SC(2,S2) + DQ(3,A2)

          IF ( ( (R1.LT.0.D0) .EQV. (R2.LT.0.D0) ) .OR.
     >         ( ABS(R1).LE.PREC ) .OR.
     >         ( ABS(R2).LE.PREC ) ) GOTO 21

C 2.2. ==> IL Y A UNE INTERSECTION
C 2.2.1. ==> STOCKAGE POSITION DE L'INTERSECTION SUR LES DEUX ARETES

          NBRINT = NBRINT + 1
          R1 = R1/(R1-R2)
          R2 = R3/(R3-R4)
          SI(2*NBRINT-1) = R1
          SI(2*NBRINT  ) = R2

C 2.2.2. ==> STOCKAGE COORDONNEES SOMMET INTERSECTION

          NBNO = NBNO + 1
          SC(1,NBNO) = (1-R1)*SC(1,S1) + R1*SC(1,S2)
          SC(2,NBNO) = (1-R1)*SC(2,S1) + R1*SC(2,S2)
          IF (NIV.GE.2) THEN
            VALI(1) = NBNO
            VALR(1) = SC(1,NBNO)
            VALR(2) = SC(2,NBNO)
            CALL ARLDBG(NOMPRO,NIV,IFM,8,NI,VALI,NR,VALR,NK,VALK)
          ENDIF

C 2.2.3. ==> INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A1)

          P1 = IS(A1)
          P2 = 0

  223     CONTINUE

          IF (P1.NE.0) THEN
            R3 = SI(2*ABS(IS(P1))-1)
            IF (R3.LT.R1) THEN
              P2 = P1
              P1 = IS(P1+1)
              GOTO 223
            ENDIF
          ENDIF

          IS(P0+1) = P1
          IF (P2.NE.0) THEN
            IS(P2+1) = P0
          ELSE
            IS(A1) = P0
          ENDIF

C 2.2.4. ==> INSERTION TRIEE DU SOMMET INTERSECTION DANS AI(A2)

          P1 = IS(A2)
          P2 = 0

 224     CONTINUE

          IF (P1.NE.0) THEN
            R3 = SI(2*ABS(IS(P1)))
            IF (R3.LT.R2) THEN
              P2 = P1
              P1 = IS(P1+1)
              GOTO 224
            ENDIF
          ENDIF

          IS(P0+3) = P1
          IF (P2.NE.0) THEN
            IS(P2+1) = P0 + 2
          ELSE
            IS(A2) = P0 + 2
          ENDIF

C C 2.2.5. ==> SIGNE DU SOMMET INTERSECTION

          IF ( DQ(1,A1)*DQ(2,A2) .GT. DQ(2,A1)*DQ(1,A2) ) THEN
            IS(P0)   = NBRINT
            IS(P0+2) = -NBRINT
          ELSE
            IS(P0)   = -NBRINT
            IS(P0+2) = NBRINT
          ENDIF

          P0 = P0 + 4

   21   CONTINUE

   20 CONTINUE
C
      IF (NIV.GE.2) THEN
      IF (NBRINT.EQ.0) THEN
        CALL ARLDBG(NOMPRO,NIV,IFM,3,NI,VALI,NR,VALR,NK,VALK)
      ELSE
        VALI(1)=NBRINT
        CALL ARLDBG(NOMPRO,NIV,IFM,4,NI,VALI,NR,VALR,NK,VALK)
      ENDIF
      ENDIF
C
C====
C 3. NOUVELLES ARETES
C====
C
      IF ( NBRINT.GT.0 ) THEN
C
      CALL ARLDBG(NOMPRO,NIV,IFM,5,NI,VALI,NR,VALR,NK,VALK)
      NA0 = NARE
      DO 30 , I = 1, NA0

        P1 = IS(I)
        S1 = FS(1,I)
        S2 = FS(2,I)
        IR = .TRUE.

C ----- PARCOURS DE LA STRUCTURE AI(I) ET DECOUPAGE DES ARETES

   31   CONTINUE

        IF (P1.NE.0) THEN

          S0 = IS(P1)
          IF (S0.GT.0) THEN
            IF (IR) THEN
              FS(1,I) = S1
              FS(2,I) = NS0 + S0
              IR = .FALSE.
            ELSE
              NARE = NARE + 1
              FS(1,NARE) = S1
              FS(2,NARE) = NS0 + S0
            ENDIF
          ELSE
            S1 = NS0 - S0
          ENDIF

          P1 = IS(P1+1)
          GOTO 31

        ENDIF

        IF (S0.LT.0) THEN
          IF (IR) THEN
            FS(1,I) = S1
            FS(2,I) = S2
          ELSE
            NARE = NARE + 1
            FS(1,NARE) = S1
            FS(2,NARE) = S2
          ENDIF
        ENDIF

   30 CONTINUE
C
      ENDIF
C
C====
C 4. GRAPHE SOMMET - ARETES
C====
C
      IF ( NBRINT.GT.0 ) THEN
C
      CALL ARLDBG(NOMPRO,NIV,IFM,6,NI,VALI,NR,VALR,NK,VALK)
      P0 = NBNO + NCMAX

      DO 41 , I = 1, NBNO
        IS(P0+2*I-1) = 0
        IS(P0+2*I  ) = 0
   41 CONTINUE

      DO 42 , I = 1, NARE

        S1 = FS(1,I)
        IF (IS(P0+2*S1-1).EQ.0) THEN
          IS(P0+2*S1-1) = I
        ELSE
          IS(P0+2*S1  ) = I
        ENDIF

        S2 = FS(2,I)
        IF (IS(P0+2*S2-1).EQ.0) THEN
          IS(P0+2*S2-1) = I
        ELSE
          IS(P0+2*S2  ) = I
        ENDIF

   42 CONTINUE
C
      ENDIF
C
C====
C 5. COMPOSANTES CONNEXES DE L'INTERSECTION
C====
C
      IF ( NBRINT.GT.0 ) THEN
C
      DO 51 , I = 1, NBNO
        TRAVL(I) = .FALSE.
   51 CONTINUE

      N = 0
      P1 = 0

      DO 52 , I = 1, NBRINT
C
C --- ACCES A L'INTERSECTION I STOCKEE APRES LES NBNO INITIAUX
C
        S0 = NS0 + I
        IF (TRAVL(S0)) GOTO 52

        NCMPCO = NCMPCO + 1
        P1 = P1 + N + 1
        N = 0

  521   CONTINUE

        TRAVL(S0) = .TRUE.

        IF (FS(1,IS(P0+2*S0-1)).EQ.S0) THEN
          S0 = FS(2,IS(P0+2*S0-1))
        ELSEIF (FS(1,IS(P0+2*S0)).EQ.S0) THEN
          S0 = FS(2,IS(P0+2*S0))
        ENDIF

        IF (S0.EQ.0) CALL U2MESS('F','ARLEQUIN_23')

        N = N + 1
        IS(P1+N) = S0
        IF (.NOT.TRAVL(S0)) GOTO 521

        IS(P1) = N

   52 CONTINUE
C
      VALI(1)=NCMPCO
      CALL ARLDBG(NOMPRO,NIV,IFM,7,NI,VALI,NR,VALR,NK,VALK)
C
      ENDIF
C
      END
