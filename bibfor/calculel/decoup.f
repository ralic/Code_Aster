      SUBROUTINE DECOUP(MA,IPAN,DIM,DENO,NDENO,DEMA,DEDIME,
     &                  NDEMA,DEPAN,SEP,NSEP,MASEP,NMASEP,NMA)

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
C        DECOUPAGE D'UNE SOUS-MAILLE EN SOUS-MAILLES PAR UN PAN
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  MA            : INDEX DE LA SOUS-MAILLE A DECOUPER DANS DEMA
C INTEGER  IPAN          : INDEX DU PAN SEPARATEUR DANS DEPAN
C INTEGER  DIM           : DIMENSION D'ESPACE
C
C VARIABLES D'ENTREE / SORTIE
C REAL*8       DENO(DIM,*)     : COORD. NOEUDS DES SOUS-MAILLES
C INTEGER      NDENO           : NOMBRE DE NOEUDS DANS DENO
C INTEGER      DEMA(8,*)       : CONNECTIVITE DES SOUS-MAILLES
C INTEGER      DEDIME(*)       : NOMBRE DE NOEUDS DES SOUS-MAILLES
C INTEGER      NDEMA           : NOMBRE DE SOUS-MAILLES
C REAL*8       DEPAN(DIM+1,*)  : EQUATIONS DES PANS DE LA FRONTIERE
C 
C VARIABLES DE SORTIE
C REAL*8       SEP(DIM+1,*)    : EQUATIONS PANS SEPARATEURS SUPPLEMENT.
C INTEGER      NSEP            : NOMBRE DE PANS SEPARATEURS SUPPLEMENT.
C INTEGER      MASEP(4,*)      : INDEX DANS SEP PANS POUR NOUV S-MAILLES
C INTEGER      NMASEP(*)       : NOMBRE DE PANS SEP POUR NOUV S-MAILLES
C INTEGER      NMA(*)          : NOMBRE DE NOUV S-MAILLES
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      REAL*8   PREC1,PREC2
      PARAMETER (PREC1 = 0.0000001D0)
      PARAMETER (PREC2 = 0.000001D0)
      
C --- FONCTION
      REAL*8   R8DOT

C --- VARIABLES
      INTEGER  MA,IPAN,DIM,NDENO,NDEMA,NSEP
      INTEGER  NMA(*),DEDIME(*),DEMA(8,*),MASEP(*),NMASEP(*)
      INTEGER  INTENO(12),NSONO(2),SONO(10,2),IDARNO(2,13),TYPE(2)
      INTEGER  NFANO(7,2),FANO(6,7,2),FACEAR(30),PANAR(2,6),NOAR(2,12)
      INTEGER  NFACE,NARE,NNO,IARE,NRG,COTE,I,J,K,L,N,P0,P,Q,N0,N1,N2
      REAL*8   DENO(DIM,*),DEPAN(DIM+1,*),SEP(*),R(8),R0   
      LOGICAL  LFACE(7)

C --- TABLES
      INTEGER  ID3D(7),INDEX1(7),REGLE1(107)
      DATA ID3D / 81,576,4096,3600,20250,24000,25600 /
      DATA INDEX1 / 1,10,23,38,55,72,91 /
      DATA REGLE1 / 
     & 3, 1, 3,1,         4,  1,3,2,5,           
     & 3, 2, 4,1,3,5,     6,  1,3,2,5,8,6,      
     & 4, 2, 4,1,4,6,     8,  1,4,3,2,8,9,10,7, 
     & 3, 3, 4,1,4,5,3,8, 8,  3,2,1,11,9,10,5,6,
     & 6, 2, 5,1,3,4,     10, 6,5,4,3,2,1,12,10,8,9,
     & 3, 3, 5,1,4,5,4,9, 10, 13,12,11,10,7,2,3,4,5,6,
     & 5, 2, 4,1,5,7,     10, 5,4,3,2,1,10,11,12,8,9 / 

C --- DECOUPE 1

      NNO = DEDIME(MA)
      CALL NOAREL(DIM,NNO,NOAR,NARE)

 10   CONTINUE

      DO 20 I = 1, NNO
        R(I) = R8DOT(DIM,DEPAN(1,IPAN),1,DENO(1,DEMA(I,MA)),1)
     &       + DEPAN(DIM+1,IPAN)
        IF (ABS(R(I)).LT.PREC1) THEN
          DEPAN(DIM+1,IPAN) = DEPAN(DIM+1,IPAN) + PREC2
          GOTO 10
        ENDIF
 20   CONTINUE

      DO 30 I = 1, NARE

        N1 = NOAR(1,I)
        N2 = NOAR(2,I)

        IF ((R(N1).LT.0.D0).NEQV.(R(N2).LT.0.D0)) THEN

          R0 = 0.5D0*(R(N1)+R(N2))/(R(N1)-R(N2))
          NDENO = NDENO + 1
          N1 = DEMA(N1,MA)
          N2 = DEMA(N2,MA)
          DO 40 J = 1, DIM
            DENO(J,NDENO) = (0.5D0-R0)*DENO(J,N1)+(0.5D0+R0)*DENO(J,N2)
 40       CONTINUE
          INTENO(I) = NDENO

        ELSE

          INTENO(I) = 0

        ENDIF

 30   CONTINUE

C --- IDENTIFICATION

      IF (DIM.EQ.2) THEN
         
C ----- DIMENSION 2

        NSONO(1) = 0
        NSONO(2) = 0

        DO 50 I = 1, NARE

          J = NOAR(1,I)
          N1 = DEMA(J,MA)
          N2 = INTENO(I)

          IF (R(J).LT.0.D0) THEN
            K = 1
          ELSE
            K = 2
          ENDIF

          NSONO(K) = NSONO(K) + 1
          SONO(NSONO(K),K) = N1
          
          IF (N2.NE.0) THEN
            DO 60 L = 1, 2
              NSONO(L) = NSONO(L) + 1
              SONO(NSONO(L),L) = N2
 60         CONTINUE
          ENDIF
            
 50     CONTINUE

      ELSE

C ----- DIMENSION 3

        CALL ARLPAN(NNO,FACEAR,NFACE)
        
C ----- IDENTIFICATION DES FACES

        DO 70 I = 1, NFACE
          NFANO(I,1) = 0
          NFANO(I,2) = 0
          PANAR(2,I) = 0
 70     CONTINUE

        P = 0
        DO 80 I = 1, NFACE

          P = P + 1
          NARE = FACEAR(P)

          DO 80 J = 1, NARE

            P = P + 1
            IARE = FACEAR(P)
            IF (IARE.GT.0) THEN
              N = NOAR(1,IARE)
            ELSE
              N = NOAR(2,-IARE)
            ENDIF
            N1 = DEMA(N,MA)
            N2 = INTENO(ABS(IARE))

            IF (R(N).LT.0.D0) THEN
              L = 1
            ELSE
              L = 2
            ENDIF

            NFANO(I,L) = NFANO(I,L) + 1
            FANO(NFANO(I,L),I,L) = N1

            IF (N2.NE.0) THEN
 
              NFANO(I,1) = NFANO(I,1) + 1
              FANO(NFANO(I,1),I,1) = N2
              NFANO(I,2) = NFANO(I,2) + 1
              FANO(NFANO(I,2),I,2) = N2
            
              PANAR(L,I) = N2

            ENDIF

 80     CONTINUE
        
C ----- IDENTIFICATION DU PAN SEPARATEUR

        N = 0
        J = NFACE + 1

        DO 90 I = 1, NFACE
          P = PANAR(2,I)
          IF (P.NE.0) GOTO 100
 90     CONTINUE

 100    CONTINUE
        N = N + 1
        FANO(N,J,1) = P
        PANAR(2,I) = 0
        P = PANAR(1,I)

        DO 110 I = 1, NFACE
          IF (PANAR(2,I).EQ.P) GOTO 100
 110    CONTINUE
        
        NFANO(J,1) = N
        NFANO(J,2) = N
        NFACE = NFACE + 1
        
        P = N
        DO 120 I = 1, N
          FANO(I,J,2) = FANO(P,J,1)
          P = P - 1
 120    CONTINUE

C ----- IDENTIFICATION DU SOLIDE

        DO 130 COTE = 1, 2

C ------- NUMERO

          N = 1
          DO 140 I = 1, NFACE
            IF (NFANO(I,COTE).NE.0) N = N * NFANO(I,COTE)
 140      CONTINUE

          J = 0
 150      CONTINUE
          J = J + 1
          IF (N.NE.ID3D(J)) GOTO 150
          TYPE(COTE) = 1 + J

C ------- REGLE D'IDENTIFICATION

          DO 160 I = 1, NFACE
            LFACE(I) = .FALSE.
 160      CONTINUE

          P0 = INDEX1(J)
          N0 = REGLE1(P0)
          NRG = REGLE1(P0+1)
 
          I = 0
 170      CONTINUE
          I = I + 1
          IF (NFANO(I,COTE).NE.N0) GOTO 170

          LFACE(I) = .TRUE.   
          IDARNO(1,1) = FANO(N0,I,COTE)
          IDARNO(2,1) = FANO(1,I,COTE)
          DO 180 J = 2, N0
            IDARNO(1,J) = FANO(J-1,I,COTE)
            IDARNO(2,J) = FANO(J,I,COTE)
 180      CONTINUE
          NARE = N0

 190      CONTINUE

          P = P0 + 2
          
          DO 200 I = 1, NRG
            
            N = REGLE1(P)
            P = P + 1
            IARE = REGLE1(P)
            P = P + 1

            N1 = IDARNO(1,IARE)
            N2 = IDARNO(2,IARE)
  
            DO 210 J = 1, NFACE

              IF ((NFANO(J,COTE).NE.N).OR.LFACE(J)) GOTO 210

              DO 220 K = 1, N
                   
                IF ((FANO(K,J,COTE).NE.N2).OR.
     &              (FANO(1+MOD(K,N),J,COTE).NE.N1)) GOTO 220

                LFACE(J) = .TRUE.               
                L = K + 1
      
                DO 230 Q = 2, N

                  L = L + 1
                  IF (L.GT.N) L = L - N
                  NARE = NARE + 1
                  N2 = FANO(L,J,COTE)
                  IDARNO(1,NARE) = N1
                  IDARNO(2,NARE) = N2
                  N1 = N2

 230            CONTINUE

                GOTO 200
               
 220          CONTINUE
 210        CONTINUE

            IDARNO(1,N0) = IDARNO(1,1)
            DO 240 J = 1, N0-1
              IDARNO(1,J) = IDARNO(2,J)
              IDARNO(2,J) = IDARNO(2,J+1)
 240        CONTINUE
            IDARNO(2,N0) = IDARNO(1,1)
 
            GOTO 190

 200      CONTINUE

          N = REGLE1(P) 
          
          NSONO(COTE) = N
          DO 130 I = 1, N
            P = P + 1
            SONO(I,COTE) = IDARNO(1,REGLE1(P))
 130    CONTINUE

      ENDIF

C      WRITE(*,*) '1%',(SONO(I,1),I=1,NSONO(1))
C      WRITE(*,*) '2%',(SONO(I,2),I=1,NSONO(2))

      CALL DECOU2(DIM,0,MA,SONO,NSONO,TYPE,2,DENO,DEMA,
     &          DEDIME,NDEMA,SEP,NSEP,MASEP,NMASEP,NMA)

      END
