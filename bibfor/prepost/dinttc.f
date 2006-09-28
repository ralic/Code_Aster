      SUBROUTINE DINTTC(COORD1,COORD2,XO1O2,YO1O2,ZO1O2,DO1O2,
     &                   R,NORM,NINT,NHOP,NPIR,COORD,NBI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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

C***************************************************************
C              BUT DE CETTE ROUTINE :                          *
C CALCULER LES INTERSECTIONS ENTRE UN TETRAEDRE ET UN CYLINDRE *
C***************************************************************
C IN COORD1         : COORDONNES DES NOEUDS EXTREMES DU CYLINDRE NOEU1
C IN COORD2         : COORDONNES DES NOEUDS EXTREMES DU CYLINDRE NOEU2
C IN *O1O2          : DISTANCE DES NOEUDS DU TETRA A L AXE DU CYLINDRE
C IN DO102          : DISTANCE DES NOEUDS DU TETRA A L AXE DU CYLINDRE
C IN R              : RAYON DE LA SPHERE
C IN NINT,NHOP,NPIR : PLACEMENT DES NOEUDS PAR RAPPORT AU CYLINDRE
C OUT COORD         : COORDONNEES DES NOEUDS DU TETRA/INTERSECTIONS
C OUT NBI           : NBR D INTERSECTION

      IMPLICIT NONE

C - DECLARATION GLOBALE

      REAL*8 COORD1(3), COORD2(3), XO1O2,YO1O2,ZO1O2, DO1O2
      REAL*8 R, COORD(3,12)
      INTEGER NORM(2,4), NINT, NHOP, NPIR
      INTEGER NBI

C - DECLARATION LOCALE

      INTEGER I, J, K, L, IA, IB
      REAL*8 XO1A, YO1A, ZO1A, XO2A, YO2A, ZO2A, XAB, YAB, ZAB
      REAL*8 XXO1, YXO1, ZXO1, XXO2, YXO2, ZXO2
      REAL*8 A, B, C, DELTA, LAMDA1, LAMDA2, LAMBDA
      REAL*8 DAO1, DAO2, DXO1, DXO2, PAO1O2, PAO2O1, PBO1O2
      REAL*8 COS1, COS2, N2, N3, N4
      LOGICAL LNOEU(2,4),LINTCY

      L = 4
      NBI = 0

C VERIFICATION SI UN DES NOEUDS DU TETRA EST CONFONDU
C AVEC NOEU1 OU NOEU2

      DO 10 J = 1,4
        LNOEU(1,J) = .FALSE.
        LNOEU(2,J) = .FALSE.
        XO1A  = COORD(1,J) - COORD1(1)
        YO1A  = COORD(2,J) - COORD1(2)
        ZO1A  = COORD(3,J) - COORD1(3)
        XO2A  = COORD(1,J) - COORD2(1)
        YO2A  = COORD(2,J) - COORD2(2)
        ZO2A  = COORD(3,J) - COORD2(3)
        DAO1 = SQRT( XO1A**2 + YO1A**2 + ZO1A**2 )
        DAO2 = SQRT( XO2A**2 + YO2A**2 + ZO2A**2 )
        IF ( ABS(DAO1).LE.1.0D-6 ) LNOEU(1,J) = .TRUE.
        IF ( ABS(DAO2).LE.1.0D-6 ) LNOEU(2,J) = .TRUE.
 10   CONTINUE

C RECHERCHE DES INTERSECTIONS DES ARETES DU TETRA AVEC LE CYLINDRE
      DO 20 J = 1,3
        DO 30 K = J+1,4
          L = L+1
C LES 2 NOEUDS SONT EXTERIEURS OU INTERIEURS
          IF ((NORM(1,J)*NORM(1,K)).GT.0 ) THEN
            DO 35 I = 1,3
              COORD(I,L) = 1.0D30
 35         CONTINUE
          ELSE
C UN POINT EST EXTERIEUR ET L AUTRE INTERIEUR
            NBI = NBI + 1
            LINTCY = .TRUE.
C ON PREND COMME REFERENCE LE POINT EXTERNE
            IF ( NORM(1,J).EQ.1 ) THEN
              IA = K
              IB = J
            ELSE
              IA = J
              IB = K
            ENDIF
C COORDONNES DES VECTEURS ET DISTANCES
            XO1A  = COORD(1,IA) - COORD1(1)
            YO1A  = COORD(2,IA) - COORD1(2)
            ZO1A  = COORD(3,IA) - COORD1(3)
            XO2A  = COORD(1,IA) - COORD2(1)
            YO2A  = COORD(2,IA) - COORD2(2)
            ZO2A  = COORD(3,IA) - COORD2(3)
            XAB   = COORD(1,IB) - COORD(1,IA)
            YAB   = COORD(2,IB) - COORD(2,IA)
            ZAB   = COORD(3,IB) - COORD(3,IA)
            DAO1 = SQRT( XO1A**2 + YO1A**2 + ZO1A**2 )
            DAO2 = SQRT( XO2A**2 + YO2A**2 + ZO2A**2 )
            IF ( ABS(DAO1).LE.1.0D-6 ) DAO1 = 0.0D0
            IF ( ABS(DAO2).LE.1.0D-6 ) DAO2 = 0.0D0
            PAO1O2 =    XO1A*XO1O2 + YO1A*YO1O2 + ZO1A*ZO1O2
            PAO2O1 = -( XO2A*XO1O2 + YO2A*YO1O2 + ZO2A*ZO1O2 )
            A =(YAB*ZO1O2-ZAB*YO1O2)**2 +(ZAB*XO1O2-XAB*ZO1O2)**2 +
     &        (XAB*YO1O2-YAB*XO1O2)**2
            B =( YAB*ZO1O2-ZAB*YO1O2 ) *( YO1A*ZO1O2-ZO1A*YO1O2 ) +
     &        ( ZAB*XO1O2-XAB*ZO1O2 ) *( ZO1A*XO1O2-XO1A*ZO1O2 ) +
     &        ( XAB*YO1O2-YAB*XO1O2 ) *( XO1A*YO1O2-YO1A*XO1O2 )
            C =( YO1A*ZO1O2-ZO1A*YO1O2 )**2 +
     &        ( ZO1A*XO1O2-XO1A*ZO1O2 )**2 +
     &        ( XO1A*YO1O2-YO1A*XO1O2 )**2 - R*R * DO1O2**2
            DELTA = B*B - A*C
            IF ( DELTA.LT.0.D0 ) THEN
              CALL U2MESS('F','PREPOST_22')
            ENDIF
            IF ( A.EQ.0.0D0 ) THEN
C AB ET O1O2 SONT COLINEAIRES : INTERSECTION SUR LES FACES
              LINTCY = .FALSE.
            ELSE
C 2 POINTS D INTERSECTION
              LAMDA1 =( - B - SQRT(DELTA) ) / A
              IF ( ABS(1.D0-LAMDA1).LE.1.0D-6 ) THEN
                LAMDA1 = 1.0D0
              ELSE IF ( ABS(LAMDA1).LE.1.0D-6 ) THEN
                LAMDA1 = 0.0D0
              ENDIF
              LAMDA2 =( - B + SQRT(DELTA) ) / A
              IF ( ABS(1.D0-LAMDA2).LE.1.0D-6 ) THEN
                LAMDA2 = 1.0D0
              ELSE IF ( ABS(LAMDA2).LE.1.0D-6 ) THEN
                LAMDA2 = 0.0D0
              ENDIF
C CAS OU L INTERSECTION EST SUR LA PARTIE CYLINDRIQUE
              IF ( LAMDA1.GE.0.D0 .AND. LAMDA1.LE.1.D0 ) THEN
                COORD(1,L) = LAMDA1*XAB + COORD(1,IA)
                COORD(2,L) = LAMDA1*YAB + COORD(2,IA)
                COORD(3,L) = LAMDA1*ZAB + COORD(3,IA)
              ELSE IF ( LAMDA2.GE.0.D0 .AND. LAMDA2.LE.1.D0 ) THEN
                COORD(1,L) = LAMDA2*XAB + COORD(1,IA)
                COORD(2,L) = LAMDA2*YAB + COORD(2,IA)
                COORD(3,L) = LAMDA2*ZAB + COORD(3,IA)
              ELSE
C L INTERSECTION COUPE UNE DES SURFACES EXTREMITES DU CYLINDRE
                LINTCY = .FALSE.
              ENDIF
            ENDIF

C VERIFICATION SI LA PROJECTION DU POINT D INTERSECTION SUR 0102
C SE TROUVE ENTRE 01 ET 02

            IF ( LINTCY ) THEN
              XXO1 = COORD(1,L)-COORD1(1)
              YXO1 = COORD(2,L)-COORD1(2)
              ZXO1 = COORD(3,L)-COORD1(3)
              XXO2 = COORD(1,L)-COORD2(1)
              YXO2 = COORD(2,L)-COORD2(2)
              ZXO2 = COORD(3,L)-COORD2(3)
              DXO1 = SQRT( XXO1**2 + YXO1**2 + ZXO1**2 )
              DXO2 = SQRT( XXO2**2 + YXO2**2 + ZXO2**2 )
              COS1 =  ( XXO1*XO1O2+YXO1*YO1O2+ZXO1*ZO1O2 )/(DO1O2*DXO1)
              COS2 = -( XXO2*XO1O2+YXO2*YO1O2+ZXO2*ZO1O2 )/(DO1O2*DXO2)
              N2 =( DXO1*COS1 ) / DO1O2
              N3 =( DXO2*COS2 ) / DO1O2
              N4 = N2 + N3
              IF ( ABS(1.D0-N2).LE.1.0D-6 ) N2 = 1.0D0
              IF ( ABS(1.D0-N3).LE.1.0D-6 ) N3 = 1.0D0
              IF ( ABS(1.D0-N4).LE.1.0D-6 ) N4 = 1.0D0
              IF ( N4.NE.1.0D0 ) THEN
                CALL U2MESS('F','PREPOST_23')
              ENDIF
              IF ( N2.GT.1.0D0 .OR. N3.GT.1.0D0 ) THEN
                LINTCY = .FALSE.
              ENDIF
C SUIVANT LES CAS, ON CHERCHE OU PAS L INTERSECTION SUR LES FACES
              IF ( NINT.EQ.1 ) THEN
                IF ( LNOEU(1,IB) .AND. N2.LT.0.0D0 ) THEN
                  LINTCY = .TRUE.
                  NBI = NBI-1
                ELSE IF (LNOEU(2,IB) .AND. N3.LT.0.0D0) THEN
                  LINTCY = .TRUE.
                  NBI = NBI-1
                ENDIF
              ELSE IF ( NINT.EQ.2 .AND. NHOP.LE.1 ) THEN
                IF (LNOEU(1,IB) .AND. N2.LT.0.0D0) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD1(1)
                  COORD(2,L) = COORD1(2)
                  COORD(3,L) = COORD1(3)
                  NBI = NBI-1
                ELSE IF (LNOEU(2,IB) .AND. N3.LT.0.0D0) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD2(1)
                  COORD(2,L) = COORD2(2)
                  COORD(3,L) = COORD2(3)
                  NBI = NBI-1
                ENDIF
              ELSE IF (NINT.EQ.2 .AND. NHOP.EQ.2 .AND. NPIR.EQ.2) THEN
                IF ( LNOEU(1,IB) .AND. N2.LT.0.0D0 ) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD1(1)
                  COORD(2,L) = COORD1(2)
                  COORD(3,L) = COORD1(3)
                  NBI = NBI-1
                ELSE IF (LNOEU(2,IB) .AND. N3.LT.0.0D0) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD2(1)
                  COORD(2,L) = COORD2(2)
                  COORD(3,L) = COORD2(3)
                  NBI = NBI-1
                ENDIF
              ELSE IF (NINT.EQ.2 .AND. NHOP.EQ.2 .AND. NPIR.GT.2) THEN
                IF ( LNOEU(1,IB) .AND. N2.LT.0.0D0 ) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD1(1)
                  COORD(2,L) = COORD1(2)
                  COORD(3,L) = COORD1(3)
                ELSE IF (LNOEU(2,IB) .AND. N3.LT.0.0D0) THEN
                  LINTCY = .TRUE.
                  COORD(1,L) = COORD2(1)
                  COORD(2,L) = COORD2(2)
                  COORD(3,L) = COORD2(3)
                ENDIF
              ENDIF
            ENDIF

C CAS OU L INTERSECTION COUPE UNE DES SURFACES EXTREMITES DU CYLINDRE
C IL FAUT CHERCHER DE QUEL COTE DU CYLINDRE EST LE POINT A

            IF ( .NOT.LINTCY ) THEN
              IF ( DAO1.GT.DAO2 .AND. DAO2.NE.0.0D0 ) THEN
                LAMBDA = PAO2O1 /(XAB*XO1O2 + YAB*YO1O2+ ZAB*ZO1O2)
              ELSE IF ( DAO2.GT.DAO1 .AND. DAO1.NE.0.0D0 ) THEN
                LAMBDA =  -PAO1O2 /(XAB*XO1O2 + YAB*YO1O2 + ZAB*ZO1O2)
              ELSE IF ( DAO1.EQ.0.0D0 .OR. DAO2.EQ.0.0D0 ) THEN
                LAMBDA = 0.0D0
              ELSE
                CALL U2MESS('F','PREPOST_24')
              ENDIF
              IF ( ABS(1.D0-LAMBDA).LE.1.0D-10 ) THEN
                LAMBDA = 1.0D0
              ELSE IF ( ABS(LAMBDA).LE.1.0D-10 ) THEN
                LAMBDA = 0.0D0
              ENDIF
              IF ( LAMBDA.GE.0.D0 .AND. LAMBDA.LE.1.D0 ) THEN
                COORD(1,L) = LAMBDA*XAB + COORD(1,IA)
                COORD(2,L) = LAMBDA*YAB + COORD(2,IA)
                COORD(3,L) = LAMBDA*ZAB + COORD(3,IA)
              ELSE
                CALL U2MESS('F','PREPOST_25')
              ENDIF
            ENDIF
          ENDIF
 30     CONTINUE
 20   CONTINUE

C SUIVANT LES CAS IL RESTE DES CALCULS A FAIRE
      IF ( NINT.EQ.1 .AND. NBI.GE.1 ) THEN
        NBI = 3

C CALCUL LA PROJECTION DE L INTERSECTION SUR LA FACE
C RECHERCHE DU NUMERO DU POINT INTERNE

        DO 40 I = 1,4
          IF ( NORM(1,I).EQ.1 ) IB = I
 40     CONTINUE
        DO 50 I = 1,4
          IF ( NORM(1,I).EQ.-1 .AND. NORM(2,I).NE.0 ) THEN

C DE QUEL COTE SE TROUVE LES POINTS EXTERNES 01 OU 02
            IF ( NORM(2,I).EQ.1 .AND. LNOEU(1,IB) ) THEN
              XO1A  = COORD(1,I) - COORD1(1)
              YO1A  = COORD(2,I) - COORD1(2)
              ZO1A  = COORD(3,I) - COORD1(3)
              LAMBDA = -( XO1A*XO1O2 + YO1A*YO1O2 + ZO1A*ZO1O2 )
              IF ( IB.EQ.1 ) THEN
                L = 4+IB+I-2
              ELSE
                L = 4+IB+I-1
              ENDIF
              COORD(1,L) = LAMBDA*XO1O2/DO1O2 + COORD(1,I)
              COORD(2,L) = LAMBDA*YO1O2/DO1O2 + COORD(2,I)
              COORD(3,L) = LAMBDA*ZO1O2/DO1O2 + COORD(3,I)
            ELSE IF ( NORM(2,I).EQ.-1 .AND. LNOEU(2,IB) ) THEN
              XO2A  = COORD(1,IA) - COORD2(1)
              YO2A  = COORD(2,IA) - COORD2(2)
              ZO2A  = COORD(3,IA) - COORD2(3)
              LAMBDA = -( XO2A*XO1O2 + YO2A*YO1O2 + ZO2A*ZO1O2 )
              IF ( IB.EQ.1 ) THEN
                L = 4+I+J-2
              ELSE
                L = 4+I+J-1
              ENDIF
              COORD(1,L) = -LAMBDA*XO1O2/DO1O2 + COORD(1,I)
              COORD(2,L) = -LAMBDA*YO1O2/DO1O2 + COORD(2,I)
              COORD(3,L) = -LAMBDA*ZO1O2/DO1O2 + COORD(3,I)
            ENDIF
          ENDIF
 50     CONTINUE
      ELSE IF ( NINT.EQ.2 .AND. NHOP.EQ.2 .AND. NPIR.GE.3 ) THEN

C SI LES 2 POINTS EXTERNES SONT DU MEME COTE ON NE FAIT RIEN
C SINON CALCUL DE L INTERSECTION POUR L ARETE AVEC 2 POINTS EXTERNES
C ON CHERCHE LES POINTS EXTERNES

        IA = 0
        DO 60 I = 1,4
          IF ( NORM(1,I).EQ.-1 .AND. IA.EQ.0 ) THEN
            IA = I
          ELSE IF ( NORM(1,I).EQ.-1 ) THEN
            IB = I
          ENDIF
 60     CONTINUE
C SI LES 2 POINTS NE SONT PAS DU MEME COTE
        IF ( NORM(2,IA).NE.NORM(2,IB) ) THEN
          XAB    = COORD(1,IB) - COORD(1,IA)
          YAB    = COORD(2,IB) - COORD(2,IA)
          ZAB    = COORD(3,IB) - COORD(3,IA)
          XO1A  = COORD(1,IA) - COORD1(1)
          YO1A  = COORD(2,IA) - COORD1(2)
          ZO1A  = COORD(3,IA) - COORD1(3)
          XO2A  = COORD(1,IA) - COORD2(1)
          YO2A  = COORD(2,IA) - COORD2(2)
          ZO2A  = COORD(3,IA) - COORD2(3)
          PAO1O2  = XO1A*XO1O2 + YO1A*YO1O2 + ZO1A*ZO1O2
          PBO1O2 = XAB*XO1O2 + YAB*YO1O2 + ZAB*ZO1O2
          LAMDA1 = - PAO1O2 / PBO1O2
          PAO2O1  = XO2A*XO1O2 + YO2A*YO1O2 + ZO2A*ZO1O2
          LAMDA2 = - PAO2O1 / PBO1O2
          COORD(1,11) = LAMDA1*XAB + COORD(1,IA)
          COORD(2,11) = LAMDA1*YAB + COORD(2,IA)
          COORD(3,11) = LAMDA1*ZAB + COORD(3,IA)
          COORD(1,12) = LAMDA2*XAB + COORD(1,IA)
          COORD(2,12) = LAMDA2*YAB + COORD(2,IA)
          COORD(3,12) = LAMDA2*ZAB + COORD(3,IA)
        ENDIF
      ENDIF
      END
