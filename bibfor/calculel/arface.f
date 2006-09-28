      SUBROUTINE ARFACE(FS,NF,AS,FA,NA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C       CALCUL DES ARETES D'UN ENSEMBLE DE FACES 3D QUELCONQUES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      FS(*)     : CONNECTIVITE DES FACES
C                         ( NB NOEUDS FACE.1, FACE.1.NO1, FACE.1.NO2,
C                           ..., NB NOEUDS FACE.2, ... )
C                           NB NOEUDS FACE.* < 0 : TRIANGLE
C                           NB NOEUDS FACE.* > 0 : QUADRANGLE
C INTEGER      NF        : NOMBRE DE FACES
C
C VARIABLES DE SORTIE
C INTEGER      AS(*)     : CONNECTIVITE DES ARETES
C                         ( NB NOEUDS ARETE.1, ARETE.1.NO1, ARETE.1.NO2,
C                           ..., NB NOEUDS ARETE.2, ... )
C INTEGER      FA(*)     : ARETES BORDANT LES FACES
C                         ( NB ARETES FACE.1, FACE.1.ARETE.1,
C                           FACE.1.ARETE.2, ..., NB ARETES FACE.2, ...)
C                           SIGNE DE FACE.*.ARETE.* : SENS DE PARCOURS
C INTEGER      NA        : NOMBRE D'ARETES
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER FS(*),AS(*),FA(*),NA,NF,NSF,NSA,NS2,NARE
      INTEGER ARE(16),S0,S1,S2,S,I,F,A,P0,P1,P2,Q0,Q1

      P0 = 1
      Q0 = 0
      Q1 = 1
      NA = 0

C --- CALCUL DES ARETES

      DO 10 F = 1, NF

        NSF = FS(P0)

        IF (NSF.EQ.-3) THEN
          CALL NOARET('TRIA3   ',ARE,NARE)
        ELSEIF (NSF.LE.-6) THEN
          CALL NOARET('TRIA6   ',ARE,NARE)
        ELSEIF (NSF.EQ.4) THEN
          CALL NOARET('QUAD4   ',ARE,NARE)
        ELSEIF (NSF.EQ.6) THEN
          CALL NOARET('QUAD6   ',ARE,NARE)
        ELSEIF (NSF.GE.8) THEN
          CALL NOARET('QUAD8   ',ARE,NARE)
        ELSE
          CALL U2MESS('F','CALCULEL_15')
        ENDIF

        Q0 = Q0 + 1
        FA(Q0) = NARE

        P1 = 1

C ----- PARCOURS DES ARETES DE LA FACE F

        DO 20 I = 1, NARE

          NSA = ARE(P1)
          S0 = FS(P0+ARE(P1+1))
          S1 = FS(P0+ARE(P1+2))

          Q0 = Q0 + 1

C ------- TEST EXISTENCE DE L'ARETE (S0,S1)

          P2 = 1

          DO 30 A = 1, NA

            NS2 = AS(P2)

            IF ((AS(P2+1).EQ.S1).AND.(AS(P2+2).EQ.S0)) THEN

              FA(Q0) = -A
              P1 = P1 + NSA + 1
              GOTO 20

            ENDIF

            P2 = P2 + NS2 + 1

 30       CONTINUE

C ------- NOUVELLE ARETE

          FA(Q0) = A
          NA = NA + 1

          AS(Q1) = NSA
          AS(Q1+1) = S0
          AS(Q1+2) = S1
          IF (NSA.EQ.3) AS(Q1+3) = FS(P0+ARE(P1+3))

          P1 = P1 + NSA + 1
          Q1 = Q1 + NSA + 1

 20     CONTINUE

        P0 = P0 + ABS(NSF) + 1

 10   CONTINUE

      END
