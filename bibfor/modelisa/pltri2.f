      SUBROUTINE PLTRI2(DIM,SC,N,IS,NS,TRI,NTRI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C         TRIANGULATION D'UN POLYGONE QUELCONQUE PAR ESSORILLAGE
C ----------------------------------------------------------------------
C VARIABLES EN ENTREE
C INTEGER    DIM        : DIMENSION DE L'ESPACE
C REAL*8     SC(DIM,*)  : COORDONNEES DES SOMMETS
C REAL*8     N(3)       : (EN 3D) NORMALE SORTANTE DU POLYGONE
C INTEGER    NS         : NOMBRE DE SOMMETS
C
C VARIABLES D'ENTREE / SORTIE
C INTEGER    IS(*)      : INDEX DANS SC DES SOMMETS DU POLYGONE
C
C VARIABLES DE SORTIE
C INTEGER    TRI(3,*)   : CONNECTIVITE TRIANGULATION (INDEX DANS SC)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE
      REAL*8  PREC
      PARAMETER (PREC = 1.D-7)

C --- FONCTIONS
      REAL*8  PROVE2,R8DOT,PLVOL2

C --- VARIABLES
      INTEGER IS(*),TRI(3,*),DIM,NS,NTRI,N0,I,J,K,A,B,C,D,E,F
      REAL*8  SC(DIM,*),N(*),V(3),R0,R1,R2,SM

      N0 = NS
      NTRI = 0

      IF (N0.LT.3) GOTO 80

C --- VERIFICATION ORIENTATION
      
      IF (DIM.EQ.3) THEN

        R0 = 0.D0
        B = IS(NS)

        DO 10 I = 1, NS
          A = B
          B = IS(I)
          R0 = R0 + N(1)*SC(2,A)*SC(3,B) - N(2)*SC(1,A)*SC(3,B)
     &            + N(2)*SC(3,A)*SC(1,B) - N(3)*SC(2,A)*SC(1,B)
     &            + N(3)*SC(1,A)*SC(2,B) - N(1)*SC(3,A)*SC(2,B)
 10     CONTINUE

        IF (R0.LT.0.D0) THEN

          DO 20 I = 1, NS/2
            A = IS(NS+1-I)
            IS(NS+1-I) = IS(I)
            IS(I) = A
 20       CONTINUE
            
        ENDIF

      ENDIF

C --- SURFACE DU POLYGONE

      SM = PREC*PLVOL2(DIM,SC,N,IS,NS)

C --- TRIANGULATION

      IF (N0.EQ.3) GOTO 70

      I = 0

 30   CONTINUE

      I = I + 1
      B = IS(1+MOD(I  ,N0))
      C = IS(1+MOD(I+1,N0))
      D = IS(1+MOD(I+2,N0))

C --- ALIGNEMENT DE B, C ET D

      IF (DIM.EQ.2) THEN
        R2 = PROVE2(SC(1,B),SC(1,D),SC(1,C))
      ELSE
        CALL PROVE3(SC(1,B),SC(1,D),SC(1,C),V)
        R2 = R8DOT(3,N,1,V,1)
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

      IF (DIM.EQ.2) THEN

        R0 = PROVE2(SC(1,A),SC(1,B),SC(1,C))
        R1 = PROVE2(SC(1,B),SC(1,D),SC(1,A))

      ELSE

        CALL PROVE3(SC(1,A),SC(1,B),SC(1,C),V)
        R0 = R8DOT(3,N,1,V,1)
        CALL PROVE3(SC(1,B),SC(1,D),SC(1,A),V)
        R1 = R8DOT(3,N,1,V,1)

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

        IF (DIM.EQ.2) THEN

          R1 = PROVE2(SC(1,B),SC(1,D),SC(1,E))
          R2 = PROVE2(SC(1,B),SC(1,D),SC(1,F))

        ELSE

          CALL PROVE3(SC(1,B),SC(1,D),SC(1,E),V)
          R1 = R8DOT(3,N,1,V,1)
          CALL PROVE3(SC(1,B),SC(1,D),SC(1,F),V)
          R2 = R8DOT(3,N,1,V,1)
      
        ENDIF

        IF (((R1.GT.0.D0).AND.(R2.GT.0.D0))
     &  .OR.((R1.LT.0.D0).AND.(R2.LT.0.D0))) GOTO 50

        IF (DIM.EQ.2) THEN

          R1 = PROVE2(SC(1,E),SC(1,F),SC(1,B))
          R2 = PROVE2(SC(1,E),SC(1,F),SC(1,D))

        ELSE

          CALL PROVE3(SC(1,E),SC(1,F),SC(1,B),V)
          R1 = R8DOT(3,N,1,V,1)
          CALL PROVE3(SC(1,E),SC(1,F),SC(1,D),V)
          R2 = R8DOT(3,N,1,V,1)
        
        ENDIF

        IF (((R1.GT.0.D0).AND.(R2.GT.0.D0))
     &  .OR.((R1.LT.0.D0).AND.(R2.LT.0.D0))) GOTO 50

        GOTO 30

 50   CONTINUE

C --- ESSORILLAGE DU POLYGONE SUIVANT LA DIAGONALE BD

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

C --- DERNIER TRIANGLE

 70   CONTINUE

      IF (DIM.EQ.2) THEN
        R0 = PROVE2(SC(1,IS(1)),SC(1,IS(2)),SC(1,IS(3)))
      ELSE
        CALL PROVE3(SC(1,IS(1)),SC(1,IS(2)),SC(1,IS(3)),V)
        R0 = R8DOT(3,N,1,V,1)
      ENDIF

      IF ((NTRI.EQ.0).OR.(R0.GT.SM)) THEN
     
        NTRI = NTRI + 1
        TRI(1,NTRI) = IS(1)
        TRI(2,NTRI) = IS(2)
        TRI(3,NTRI) = IS(3)

      ENDIF

 80   CONTINUE

      END
