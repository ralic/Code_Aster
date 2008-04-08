      SUBROUTINE PLTRI2(DIME  ,SC    ,NORM  ,IS    ,NBNO  ,
     &                  PRECTR,TRI   ,NTRI)
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER DIME
      INTEGER NBNO
      REAL*8  SC(DIME,*)
      REAL*8  NORM(*)
      REAL*8  PRECTR
      INTEGER IS(*)
      INTEGER TRI(3,*)
      INTEGER NTRI
C
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C TRIANGULATION D'UN POLYGONE QUELCONQUE PAR ESSORILLAGE
C ESSORILLAGE: COUPER LES OREILLES DE CERTAINS CHIENS EN POINTE
C MERCI CHRISTOPHE DURAND POUR CETTE DEFINITION
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  SC     : COORDONNEES DES SOMMETS
C IN  NBNO   : NOMBRE DE SOMMETS
C IN  NORM   : (EN 3D) NORMALE SORTANTE DU POLYGONE
C I/O IS     : CONNECTIVITE DE LA COMPOSANTE CONNEXE
C                (NOMBRE SOMMETS COMPOSANTE, SOMMET.1, SOMMET.2,...)
C              L'INDEX DU SOMMET SE REFERE A SC
C IN  PRECTR : PRECISION POUR TRIANGULATION
C OUT NTRI   : NOMBRE DE TRIANGLES OBTENUS
C OUT TRI    : CONNECTIVITE TRIANGULATION (UNE LIGNE PAR TRIANGLE)
C                (SOMMET.1.1,SOMMET.1.2,SOMMET.1.3,
C                (SOMMET.2.1,SOMMET.2.2,SOMMET.2.3,...)
C              L'INDEX DU SOMMET SE REFERE A SC
C
C ----------------------------------------------------------------------
C
      REAL*8  PROVE2,DDOT,PLVOL2
      INTEGER N0,I,J,K,A,B,C,D,E,F
      REAL*8  V(3),R0,R1,R2,SM
C
C ----------------------------------------------------------------------
C
      N0   = NBNO
      NTRI = 0

      IF (N0.LT.3) GOTO 80

C --- VERIFICATION ORIENTATION

      IF (DIME.EQ.3) THEN

        R0 = 0.D0
        B  = IS(NBNO)

        DO 10 I = 1, NBNO
          A  = B
          B  = IS(I)
          R0 = R0 + NORM(1)*SC(2,A)*SC(3,B) - NORM(2)*SC(1,A)*SC(3,B)
     &            + NORM(2)*SC(3,A)*SC(1,B) - NORM(3)*SC(2,A)*SC(1,B)
     &            + NORM(3)*SC(1,A)*SC(2,B) - NORM(1)*SC(3,A)*SC(2,B)
 10     CONTINUE

        IF (R0.LT.0.D0) THEN

          DO 20 I = 1, NBNO/2
            A = IS(NBNO+1-I)
            IS(NBNO+1-I) = IS(I)
            IS(I) = A
 20       CONTINUE

        ENDIF

      ENDIF
C --- SURFACE DU POLYGONE

      SM = PRECTR*PLVOL2(DIME  ,SC    ,NORM  ,IS    ,NBNO  )

C --- TRIANGULATION

      IF (N0.EQ.3) GOTO 70

      I = 0

 30   CONTINUE

      I = I + 1
      B = IS(1+MOD(I  ,N0))
      C = IS(1+MOD(I+1,N0))
      D = IS(1+MOD(I+2,N0))

C --- ALIGNEMENT DE B, C ET D

      IF (DIME.EQ.2) THEN
        R2 = PROVE2(SC(1,B),SC(1,D),SC(1,C))
      ELSE
        CALL PROVE3(SC(1,B),SC(1,D),SC(1,C),V)
        R2 = DDOT(3,NORM,1,V,1)
      ENDIF

      IF (ABS(R2).LT.SM) THEN

        K = 1 + MOD(I+1,N0)
        N0 = N0 - 1

        DO 40 J = K, N0
          IS(J) = IS(J+1)
 40     CONTINUE

        IF (N0.EQ.3) GOTO 70
        GOTO 30

      ENDIF

C --- ORIENTATION INTERIEURE DE LA DIAGONALE BD

      A = IS(1+MOD(I-1,N0))

      IF (DIME.EQ.2) THEN

        R0 = PROVE2(SC(1,A),SC(1,B),SC(1,C))
        R1 = PROVE2(SC(1,B),SC(1,D),SC(1,A))

      ELSE

        CALL PROVE3(SC(1,A),SC(1,B),SC(1,C),V)
        R0 = DDOT(3,NORM,1,V,1)
        CALL PROVE3(SC(1,B),SC(1,D),SC(1,A),V)
        R1 = DDOT(3,NORM,1,V,1)

      ENDIF

      IF (R0.GE.0.D0) THEN
        IF ((R1.LE.0.D0).OR.(R2.GE.0.D0)) GOTO 30
      ELSE
        IF ((R1.LT.0.D0).AND.(R2.GT.0.D0)) GOTO 30
      ENDIF

C --- PAS D'INTERSECTION DE LA DIAGONALE BD

      K = I + 3
      F = IS(1+MOD(K,N0))

      DO 50 J = 5, N0

        E = F
        K = K + 1
        F = IS(1+MOD(K,N0))

        IF (DIME.EQ.2) THEN

          R1 = PROVE2(SC(1,B),SC(1,D),SC(1,E))
          R2 = PROVE2(SC(1,B),SC(1,D),SC(1,F))

        ELSE

          CALL PROVE3(SC(1,B),SC(1,D),SC(1,E),V)
          R1 = DDOT(3,NORM,1,V,1)
          CALL PROVE3(SC(1,B),SC(1,D),SC(1,F),V)
          R2 = DDOT(3,NORM,1,V,1)

        ENDIF

        IF (((R1.GT.0.D0).AND.(R2.GT.0.D0))
     &  .OR.((R1.LT.0.D0).AND.(R2.LT.0.D0))) GOTO 50

        IF (DIME.EQ.2) THEN

          R1 = PROVE2(SC(1,E),SC(1,F),SC(1,B))
          R2 = PROVE2(SC(1,E),SC(1,F),SC(1,D))

        ELSE

          CALL PROVE3(SC(1,E),SC(1,F),SC(1,B),V)
          R1 = DDOT(3,NORM,1,V,1)
          CALL PROVE3(SC(1,E),SC(1,F),SC(1,D),V)
          R2 = DDOT(3,NORM,1,V,1)

        ENDIF

        IF (((R1.GT.0.D0).AND.(R2.GT.0.D0))
     &  .OR.((R1.LT.0.D0).AND.(R2.LT.0.D0))) GOTO 50

        GOTO 30

 50   CONTINUE

C
C --- ESSORILLAGE DU POLYGONE SUIVANT LA DIAGONALE BD
C
      NTRI = NTRI + 1
      TRI(1,NTRI) = B
      TRI(2,NTRI) = C
      TRI(3,NTRI) = D

      K = 1+MOD(I+1,N0)
      N0 = N0 - 1
      DO 60 J = K, N0
        IS(J) = IS(J+1)
 60   CONTINUE

      IF (N0.GT.3) GOTO 30
C
C --- DERNIER TRIANGLE
C
 70   CONTINUE

      IF (DIME.EQ.2) THEN
        R0 = PROVE2(SC(1,IS(1)),SC(1,IS(2)),SC(1,IS(3)))
      ELSE
        CALL PROVE3(SC(1,IS(1)),SC(1,IS(2)),SC(1,IS(3)),V)
        R0 = DDOT(3,NORM,1,V,1)
      ENDIF

      IF ((NTRI.EQ.0).OR.(R0.GT.SM)) THEN
        NTRI = NTRI + 1
        TRI(1,NTRI) = IS(1)
        TRI(2,NTRI) = IS(2)
        TRI(3,NTRI) = IS(3)
      ENDIF

 80   CONTINUE

      END
