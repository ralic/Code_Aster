      SUBROUTINE DIVITR(TMA,DIM,DENO,NDENO,DEMA,DEDIME,NDEMA,
     &            DEPAN,DEPANO,NDEPAN,PILE,LPAN,NITER,NLPAN0)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C          DECOUPAGE DE LA MAILLE DE REFERENCE EN SOUS-MAILLES
C            SUIVANT UNE FRONTIERE ECHANTILLONEE (CF DIVISE)       
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8  TMA             : TYPE DE LA MAILLE
C INTEGER      DIM             : DIMENSION DE LA MAILLE
C INTEGER      NITER           : NOMBRE D'ITERATIONS MAX. DE DECOUPAGE
C
C VARIABLES D'ENTREE / SORTIE
C REAL*8       DENO(DIM,*)     : COORD. NOEUDS DES SOUS-MAILLES
C INTEGER      NDENO           : NOMBRE DE NOEUDS DANS DENO
C INTEGER      DEMA(8,*)       : CONNECTIVITE DES SOUS-MAILLES
C INTEGER      DEDIME(*)       : NOMBRE DE NOEUDS DES SOUS-MAILLES
C INTEGER      NDEMA           : NOMBRE DE SOUS-MAILLES
C REAL*8       DEPAN(DIM+1,*)  : EQUATIONS DES PANS DE LA FRONTIERE
C INTEGER      DEPANO(DIM,*)   : NOEUDS DE DENO DEFINISSANT DEPAN
C INTEGER      NDEPAN          : NOMBRE DE PANS DE LA FRONTIERE
C
C VARIABLES DE TRAVAIL
C INTEGER      PILE(*)
C INTEGER      LPAN(*)
C INTEGER      NLPAN0
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      INTEGER NESSAI
      PARAMETER (NESSAI = 5)

      REAL*8 RGAMMA
      PARAMETER (RGAMMA = 0.4D0)

C --- FONCTION
      INTEGER  POSPAN

C --- VARIABLES
      CHARACTER*8 TMA
      INTEGER  DIM,NN0,NDENO,NDEMA,NDEPAN,NLPAN0,NITER,MODPAN
      INTEGER  DEMA(8,*),DEDIME(*),DEPANO(DIM,*),PILE(4,*),LPAN(*)
      INTEGER  IPAN,IPAN1,IPAN2,NLPAN,NPAN1,NPAN2,LPAN1,LPAN2(19)
      INTEGER  GAMMA,GAMMA0,GAMMA1,NPILE,NSEP,MASEP(4,18),NMASEP(18)
      INTEGER  MA0,MA1,NN1,NMA(2),NMAIL,LMAIL(18),NMAPAN(18)
      INTEGER  POS1,POS2(13),I,J,K,L,P,ITER
      REAL*8   DENO(DIM,*),DEPAN(DIM+1,*),SEP(4,13)

C --- GENERATEUR PSEUDO-ALEATOIRE
      INTEGER GRAIN0,GRAIN1,GRAINS(32),GRAMAX
      PARAMETER (GRAMAX = 2147483647)

C --- TABLES
      INTEGER TYPE(4),INDEX(4),REGLE(26)
      DATA TYPE / 7, 10, 12, 14 /
      DATA INDEX / 0, 4, 10, 18 /
      DATA REGLE / 3,3,2,1,4,4,4,3,2,1,4,4,4,4,3,3,2,1,6,6,5,4,3,3,2,1 /

      IF (NDEPAN.EQ.0) THEN
        NDEMA = 0
        GOTO 240
      ENDIF

      GRAIN1 = 0

C --- INITIALISATION PILE

      NPILE = 1
      PILE(1,1) = 1
      PILE(2,1) = 1
      PILE(3,1) = NDEPAN
      PILE(4,1) = NLPAN0

      DO 10 I = 1, NDEPAN
        LPAN(I) = I
 10   CONTINUE

C --- INITIALISATION PREMIERE MAILLE

      IF (TMA(1:4).EQ.'TRIA') THEN
        CALL NOEUD('TRIA3   ',DENO(1,NDENO+1),NN0,DIM)
      ELSEIF (TMA(1:4).EQ.'QUAD') THEN
        CALL NOEUD('QUAD4   ',DENO(1,NDENO+1),NN0,DIM)
      ELSEIF (TMA(1:5).EQ.'TETRA') THEN
        CALL NOEUD('TETRA4  ',DENO(1,NDENO+1),NN0,DIM)
      ELSEIF (TMA(1:5).EQ.'PENTA') THEN
        CALL NOEUD('PENTA6  ',DENO(1,NDENO+1),NN0,DIM)
      ELSEIF (TMA(1:4).EQ.'HEXA') THEN
        CALL NOEUD('HEXA8   ',DENO(1,NDENO+1),NN0,DIM)
      ENDIF

      NDEMA = 1
      DEDIME(1) = NN0

      DO 20 I = 1, NN0
        DEMA(I,1) = NDENO + I
 20   CONTINUE
      NDENO = NDENO + NN0

C --- ITERATIONS

      ITER = 0

 30   CONTINUE

      ITER = ITER + 1

      IF ((ITER.LE.NITER).AND.(NPILE.NE.0)) THEN

        MA1   = PILE(1,NPILE)
        LPAN1 = PILE(2,NPILE)
        NPAN1 = PILE(3,NPILE)
        NLPAN = PILE(4,NPILE)
        NN1   = DEDIME(MA1)
        NPILE = NPILE - 1

C ----- CHOIX DU PAN SEPARATEUR

        GAMMA = -1
        GAMMA0 = RGAMMA * NPAN1
        MODPAN = 1 + (GRAMAX - 1) / NPAN1

        DO 40 I = 1, NESSAI

          GAMMA1 = 0
          CALL HASARD(GRAIN0,GRAIN1,GRAINS,32)
          IPAN1 = LPAN(LPAN1+GRAIN1/MODPAN)
C          WRITE(*,*) 'PAN',IPAN1,MA1
       
          DO 50 J = 1, NPAN1
           
            IPAN2 = LPAN(LPAN1-1+J)
            K = POSPAN(DIM,DENO,DEPANO(1,IPAN2),DIM,DEPAN(1,IPAN1))

            IF (K.LT.0) THEN
              GAMMA1 = GAMMA1 + 1
            ELSEIF (K.EQ.0) THEN
              GAMMA1 = GAMMA1 - 1
            ENDIF
 
 50       CONTINUE

C ------- SORTIE ET OPTIMISATION
         
C          WRITE(*,*) I,IPAN1,GAMMA1
          IF (GAMMA1.GE.GAMMA0) THEN
            IPAN = IPAN1
            GOTO 60
          ENDIF

          IF (GAMMA1.GT.GAMMA) THEN
            GAMMA = GAMMA1
            IPAN = IPAN1
          ENDIF

 40     CONTINUE

C ----- DECOUPE

 60     CONTINUE

C        WRITE(*,*) 'DECOUP',MA1,IPAN,I
        MA0 = NDEMA
        CALL DECOUP(MA1,IPAN,DIM,DENO,NDENO,DEMA,DEDIME,
     &              NDEMA,DEPAN,SEP,NSEP,MASEP,NMASEP,NMA)

C ----- NOUVELLES MAILLES

        LMAIL(1) = MA1
        NMAPAN(1) = 0
        NMAIL = NDEMA - MA0 + 1

        DO 70 I = 2, NMAIL
          LMAIL(I) = MA0 + I - 1
          NMAPAN(I) = 0
 70     CONTINUE

C ----- COMPTE REPARTITION DES PANS
      
        NPAN2 = 0
        DO 80 I = 1, NPAN1
        
          IPAN2 = LPAN(LPAN1-1+I)
          POS1 = POSPAN(DIM,DENO,DEPANO(1,IPAN2),DIM,DEPAN(1,IPAN))
          LPAN(NLPAN0-1+I) = IPAN2
          IF (POS1.LT.0) GOTO 80

          DO 90 J = 1, NSEP
            POS2(J) = POSPAN(DIM,DENO,DEPANO(1,IPAN2),DIM,SEP(1,J))
            IF (POS2(J).LT.0) GOTO 80
 90       CONTINUE
          
          DO 100 J = 1, NMAIL     
            
            IF((POS1.NE.0).AND.((J.LE.NMA(1)).NEQV.(POS1.EQ.1)))GOTO 100

            L = LMAIL(J)
            K = POSPAN(DIM,DENO,DEMA(1,L),DEDIME(L),DEPAN(1,IPAN2))
            IF (K.NE.0) GOTO 100
     
            DO 110 K = 1, NMASEP(J)
              L = MASEP(K,J)
              P = POS2(ABS(L))
              IF ((P.NE.0).AND.((L.GT.0).NEQV.(P.EQ.1))) GOTO 100
 110        CONTINUE

            NMAPAN(J) = NMAPAN(J) + 1
            NPAN2 = NPAN2 + 1
            
 100      CONTINUE

 80     CONTINUE

        IF (NPAN2.EQ.0) GOTO 180

C ----- REPARTITION

        LPAN2(1) = LPAN1
        DO 120 I = 1, NMAIL-1
          LPAN2(I+1) = LPAN2(I) + (NMAPAN(I)*NLPAN)/NPAN2
 120    CONTINUE
        LPAN2(NMAIL+1) = LPAN1 + NLPAN

C ----- MISE SUR LA PILE

        DO 130 I = 1, NMAIL
          IF (NMAPAN(I).EQ.0) GOTO 130
          NPILE = NPILE + 1
          PILE(1,NPILE) = LMAIL(I)
          PILE(2,NPILE) = LPAN2(I)
          PILE(3,NPILE) = NMAPAN(I)
          PILE(4,NPILE) = LPAN2(I+1) - LPAN2(I)
 130    CONTINUE
        
C ----- ECRITURE DANS LPAN

        DO 140 I = 1, NPAN1
        
          IPAN2 = LPAN(NLPAN0-1+I)
          POS1 = POSPAN(DIM,DENO,DEPANO(1,IPAN2),DIM,DEPAN(1,IPAN))
C          WRITE(*,*) '?',0,IPAN2,POS1
          IF (POS1.LT.0) GOTO 140
 
          DO 150 J = 1, NSEP
            POS2(J) = POSPAN(DIM,DENO,DEPANO(1,IPAN2),DIM,SEP(1,J))
C            WRITE(*,*) '?',J,IPAN2,POS2(J)
            IF (POS2(J).LT.0) GOTO 140
 150      CONTINUE
          
          DO 160 J = 1, NMAIL
            
            IF((POS1.NE.0).AND.((J.LE.NMA(1)).NEQV.(POS1.EQ.1)))GOTO 160
     
            L = LMAIL(J)
            K = POSPAN(DIM,DENO,DEMA(1,L),DEDIME(L),DEPAN(1,IPAN2))
            IF (K.NE.0) GOTO 160

            DO 170 K = 1, NMASEP(J)
              L = MASEP(K,J)
              P = POS2(ABS(L))
              IF ((P.NE.0).AND.((L.GT.0).NEQV.(P.EQ.1))) GOTO 160
 170        CONTINUE

            LPAN(LPAN2(J)) = IPAN2
            LPAN2(J) = LPAN2(J) + 1
       
 160      CONTINUE

 140    CONTINUE

 180    CONTINUE

C        DO 1 I = 1,NDEMA
C          WRITE(*,*) '>',(DEMA(J,I),J=1,DEDIME(I))
C 1      CONTINUE

C        WRITE(*,*) 'PILE',((PILE(J,I),J=1,4),'|',I=1,NPILE)

C        DO 2 I = 1,NPILE
C          WRITE(*,*) '&',(LPAN(PILE(2,I)-1+J),J=1,PILE(3,I))
C 2      CONTINUE

        GOTO 30

      ENDIF

C --- DECOUPE 

      DO 190 I = 1, NDEMA

        NN1 = DEDIME(I)
        IF (NN1.GT.NN0) THEN

          DO 200 J = 1, NN1
            LMAIL(J) = DEMA(J,I)
 200      CONTINUE

          K = NN1/2
          CALL DECOU2(DIM,NN0,I,LMAIL,NN1,K,1,DENO,
     &             DEMA,DEDIME,NDEMA,SEP,J,J,J,NMA)

        ENDIF

 190  CONTINUE

C --- DEGENERESCENCE

      DO 210 I = 1, NDEMA

        NN1 = DEDIME(I)
        IF (NN1.EQ.NN0) GOTO 210 
 
        K = 0
        L = NN1 + NN0
 220    CONTINUE
        K = K + 1
        IF (TYPE(K).NE.L) GOTO 220
        K = INDEX(K)

        L = NN0
        DO 230 J = 1, NN0
          DEMA(L,I) = DEMA(REGLE(K+J),I)
          L = L - 1
 230    CONTINUE

        DEDIME(I) = NN0

 210  CONTINUE

 240  CONTINUE

      END
