      SUBROUTINE DIVISE(MA1,TMA1,NO1,DIM1,H1,MM1,PAN1,
     &                  MA2,TMA2,NO2,DIM2,H2,MM2,PAN2,
     &                  DIM,NH,NI,NL,DENO,DEMA,DEDIME,
     &                  NDEMA,DEPAN,DEPANO,NODANS,PILE,LPAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 04/04/2002   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_21
C A_UTIL
C ----------------------------------------------------------------------
C                  CONSTRUCTION DES SOUS-MAILLES DE MA2 
C                 PAVANT L'INTERSECTION DE MA1 ET DE MA2
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER      MA1         : INDEX MAILLE 1
C CHARACTER*8  TMA1        : TYPE DE MAILLE DE MA1
C REAL*8       NO1         : COORDONNEES DES NOEUDS DE MA1
C INTEGER      DIM1        : SD BOITE.DIME ASSOCIEE A MA1 (CF BOITE)
C REAL*8       H1          : SD BOITE.H ASSOCIEE A MA1 (CF BOITE)
C REAL*8       MM1         : SD BOITE.MINMAX ASSOCIEE A MA1 (CF BOITE)
C REAL*8       PAN1        : SD BOITE.PAN ASSOCIEE A MA1 (CF BOITE)
C INTEGER      MA2         : INDEX MAILLE 2
C CHARACTER*8  TMA2        : TYPE DE MAILLE DE MA2
C REAL*8       NO2         : COORDONNEES DES NOEUDS DE MA2
C INTEGER      DIM2        : SD BOITE.DIME ASSOCIEE A MA2 (CF BOITE)
C REAL*8       H2          : SD BOITE.H ASSOCIEE A MA2 (CF BOITE)
C REAL*8       MM2         : SD BOITE.MINMAX ASSOCIEE A MA2 (CF BOITE)
C REAL*8       PAN2        : SD BOITE.PAN ASSOCIEE A MA2 (CF BOITE)
C INTEGER      DIM         : DIMENSION DE L'ESPACE
C INTEGER      NH          : NOMBRE D'ECHANTILLONAGE (CF PREDIV)
C INTEGER      NI          : NOMBRE D'ITERATIONS DECOUPAGE (CF PREDIV)
C
C VARIABLES DE SORTIE 
C REAL*8       DENO(DIM,*) : COORD. NOEUDS SOUS-MAILLES (CF PREDIV)
C INTEGER      DEMA(8,*)   : CONNECTIVITE DES SOUS-MAILLES (CF PREDIV)
C INTEGER      DEDIME(*)   : NOMBRE DE NOEUDS DE CHAQUE SOUS-MAILLE
C INTEGER      NDEMA       : NOMBRE DE SOUS-MAILLES
C
C VARIABLES DE TRAVAIL
C INTEGER      NL
C REAL*8       DEPAN(*)   
C INTEGER      DEPANO(*)
C LOGICAL      NODANS(*)
C INTEGER      PILE(*)
C INTEGER      LPAN(*)
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE
      REAL*8  PREC
      PARAMETER (PREC = 0.D0)

C --- FONCTION
      REAL*8   R8NRM1

C --- VARIABLES
      CHARACTER*8 TMA1,TMA2,TMAP
      INTEGER  DIM,DIM1(*),DIM2(*),NH,NI,NL,MA1,MA2
      INTEGER  NDEMA,DEPANO(DIM,*),DEMA(*),DEDIME(*)
      INTEGER  PILE(*),LPAN(*),I,J,K,L,N,P,P0,Q0,Q1,Q2
      INTEGER  NDEPAN,NOEPAN(60),NPAN,NNP,N1(4),N2(3,2)
      REAL*8   NO1(*),NO2(*),MM1(*),MM2(*),PAN1(*),PAN2(*)
      REAL*8   DENO(DIM,*),DEPAN(DIM+1,*),H,H1(*),H2(*)
      REAL*8   MH(3),M(3),W(27),DHQ,DHT,R0,R(3),S(3),T(3)
      REAL*8   D1(6),D2(7),S1(3,7)
      LOGICAL  NODANS(2,*),SENS,IR

C --- TABLE
      INTEGER TRI(3,4)
      DATA TRI / 2,4,3, 1,4,3, 1,2,4, 1,2,3 /

C --- INITIALISATION
 
      CALL NOPAN(TMA1,NOEPAN,NPAN)

      DHQ = 2.D0 / (NH-1)
      DHT = 1.D0 / (NH-1)
      H = H2(MA2)

      NDEPAN = 0
      P0 = 1
      Q0 = 0

C --- DIMENSION 2

      IF (DIM.EQ.2) THEN

C ----- ORIENTATION DE LA MAILLE

        SENS = ((NO1(3)-NO1(1))*(NO1(6)-NO1(2))
     &     .GT. (NO1(4)-NO1(2))*(NO1(5)-NO1(1)))

C ----- ECHANTILLONAGE

        DO 10 I = 1, NPAN

          NNP = NOEPAN(P0)
          P0 = P0 + 1

          IF (NNP.EQ.2) THEN
            TMAP = 'SEG2'
          ELSE
            TMAP = 'SEG3'
          ENDIF

C ------- POINT COURANT

          Q2 = Q0
          MH(1) = -1.D0 - DHQ

          DO 20 J = 1, NH

            Q0 = Q0 + 1
            MH(1) = MH(1) + DHQ
            CALL FORME0(MH,TMAP,W,NNP)
            CALL MMPROD(NO1,2,0,2,NOEPAN(P0),NNP,W,NNP,0,0,1,M)
            CALL LOCALI(M,DIM,DIM2,H,DHQ,MA2,0,MM2,PAN2,NO2,L,
     &                  L,R,TMA2,.TRUE.,DENO(1,Q0),NODANS(1,J))
            IF (NODANS(1,J)) 
     &           CALL DEDANS(DENO(1,Q0),TMA2,PREC,NODANS(2,J))

 20       CONTINUE

          P0 = P0 + NNP

C ------- CALCUL DES PANS

          DO 10 J = 2, NH

            IR = (NODANS(1,J).AND.NODANS(1,J-1).AND.
     &           (NODANS(2,J).OR.NODANS(2,J-1)))

            IF ((.NOT.IR).AND.NODANS(1,J).AND.NODANS(1,J-1)) THEN

              CALL POSREF(DENO(1,Q2+J-1),TMA2,D1,N)
              CALL POSREF(DENO(1,Q2+J),TMA2,D2,N)

              DO 30 K = 1, N
                S1(1,K) = D1(K)-D2(K)
 30           CONTINUE

              N = N + 1
              S1(1,N) = 1.D0
              D2(N) = -1.D0
              
              CALL SMPLX2(S1,D2,3,1,N,IR)

            ENDIF

            IF (IR) THEN

              IF (SENS) THEN
                N2(1,1) = Q2 + J - 1
                N2(2,1) = Q2 + J
              ELSE
                N2(1,1) = Q2 + J
                N2(2,1) = Q2 + J - 1
              ENDIF

              NDEPAN = NDEPAN + 1
              DEPANO(1,NDEPAN) = N2(1,1)
              DEPANO(2,NDEPAN) = N2(2,1)            

              DEPAN(1,NDEPAN) = DENO(2,N2(2,1)) - DENO(2,N2(1,1))
              DEPAN(2,NDEPAN) = DENO(1,N2(1,1)) - DENO(1,N2(2,1))       
              DEPAN(3,NDEPAN) = DENO(1,N2(2,1)) * DENO(2,N2(1,1)) 
     &                        - DENO(1,N2(1,1)) * DENO(2,N2(2,1))

              R0 = 1.D0/R8NRM1(2,DEPAN(1,NDEPAN),1)
              CALL R8SCAL(3,R0,DEPAN(1,NDEPAN),1)

            ENDIF

 10     CONTINUE
 
C --- DIMENSION 3

      ELSE

C ----- ECHANTILLONAGE
        
        DO 40 I = 1, NPAN

          NNP = NOEPAN(P0)
          P0 = P0 + 1

C ------- TRIANGLE

          IF (NNP.LT.0) THEN

            IF (NNP.EQ.-3) THEN
              TMAP = 'TRIA3'
            ELSEIF (NNP.EQ.-6) THEN
              TMAP = 'TRIA6'
            ELSE
              TMAP = 'TRIA7'
            ENDIF

C --------- POINT COURANT

            Q1 = 0
            Q2 = Q0
            MH(2) = -DHT

            DO 50 J = 1, NH
              
              MH(2) = MH(2) + DHT
              MH(1) = -DHT

              DO 50 K = J, NH

                Q0 = Q0 + 1
                Q1 = Q1 + 1
                MH(1) = MH(1) + DHT
                CALL FORME0(MH,TMAP,W,NNP)
                CALL MMPROD(NO1,3,0,3,NOEPAN(P0),NNP,W,NNP,0,0,1,M)
                CALL LOCALI(M,DIM,DIM2,H,DHT,MA2,0,MM2,PAN2,NO2,L,
     &                      L,R,TMA2,.TRUE.,DENO(1,Q0),NODANS(1,Q1))
                IF (NODANS(1,Q1)) 
     &              CALL DEDANS(DENO(1,Q0),TMA2,PREC,NODANS(2,Q1))

 50         CONTINUE

C --------- CALCUL DES PANS

            N1(1) = 0
            N1(2) = 1
            N1(3) = NH + 1 
            N1(4) = NH + 2

            DO 60 J = 2, NH

              N1(1) = N1(1) + 1
              N1(2) = N1(2) + 1

              DO 60 K = J, NH

                IF (K.NE.NH) THEN
                  N = 1
                  DO 70 L = 1, 4
                    IF (.NOT.NODANS(1,N1(L))) N = N * (L+1)  
 70               CONTINUE
                ELSE
                  DO 80 L = 1, 3
                    IF (.NOT.NODANS(1,N1(L))) GOTO 160
 80               CONTINUE
                  N = 5
                ENDIF

                IF (N.EQ.1) THEN

                  DO 90 L = 1, 4
                  
                    IF (NODANS(2,N1(L))) THEN

                      N = 1 + MOD(6-L,4)
                      N2(1,1) = N1(TRI(1,N))
                      N2(2,1) = N1(TRI(2,N))
                      N2(3,1) = N1(TRI(3,N))

                      N = 1 + MOD(L+1,4)
                      N2(1,2) = N1(TRI(1,N))
                      N2(2,2) = N1(TRI(2,N))
                      N2(3,2) = N1(TRI(3,N))

                      N = 2
                      GOTO 110
   
                    ENDIF

 90               CONTINUE

                ELSEIF (N.LT.6) THEN
                   
                  N2(1,1) = N1(TRI(1,N-1))
                  N2(2,1) = N1(TRI(2,N-1))
                  N2(3,1) = N1(TRI(3,N-1))
                  
                  N = 1

                  DO 100 L = 1, 3
                    IF (NODANS(2,N2(L,1))) GOTO 110
 100              CONTINUE

                ENDIF

                GOTO 160
                 
 110            CONTINUE

                DO 120 L = 1, N
 
                  NDEPAN = NDEPAN + 1
                      
                  DO 130 P = 1, 3
                    N2(P,L) = N2(P,L) + Q2
                    DEPANO(P,NDEPAN) = N2(P,L)
 130              CONTINUE

                  DO 140 P = 1, 3
                    R(P) = DENO(P,N2(2,L)) - DENO(P,N2(1,L))
                    S(P) = DENO(P,N2(3,L)) - DENO(P,N2(1,L))
 140              CONTINUE

                  T(1) = R(2)*S(3) - R(3)*S(2)
                  T(2) = R(3)*S(1) - R(1)*S(3)
                  T(3) = R(1)*S(2) - R(2)*S(1)
 
                  R0 = 0.D0
                  DO 150 P = 1, 3
                    DEPAN(P,NDEPAN) = T(P)
                    R0 = R0 - T(P)*DENO(P,N2(1,L))
 150              CONTINUE
                
                  DEPAN(4,NDEPAN) = R0
                  
                  R0 = 1.D0/R8NRM1(3,DEPAN(1,NDEPAN),1)
                  CALL R8SCAL(4,R0,DEPAN(1,NDEPAN),1)

 120            CONTINUE

 160            CONTINUE

                DO 60 L = 1, 4
                  N1(L) = N1(L) + 1
 60         CONTINUE

C ------- QUADRANGLE

          ELSE

            IF (NNP.EQ.4) THEN
              TMAP = 'QUAD4'
            ELSEIF (NNP.EQ.6) THEN
              TMAP = 'QUAD6'
            ELSEIF (NNP.EQ.8) THEN
              TMAP = 'QUAD4'
            ELSE
              TMAP = 'QUAD9'
            ENDIF

C --------- POINT COURANT

            Q1 = 0
            Q2 = Q0
            MH(2) = -1.D0 - DHQ

            DO 170 J = 1, NH
              
              MH(2) = MH(2) + DHQ
              MH(1) = -1.D0 - DHQ

              DO 170 K = 1, NH

                Q0 = Q0 + 1
                Q1 = Q1 + 1
                MH(1) = MH(1) + DHQ
                CALL FORME0(MH,TMAP,W,NNP)
                CALL MMPROD(NO1,3,0,3,NOEPAN(P0),NNP,W,NNP,0,0,1,M)
                CALL LOCALI(M,DIM,DIM2,H,DHQ,MA2,0,MM2,PAN2,NO2,L,
     &                      L,R,TMA2,.TRUE.,DENO(1,Q0),NODANS(1,Q1))
                IF (NODANS(1,Q1)) 
     &              CALL DEDANS(DENO(1,Q0),TMA2,PREC,NODANS(2,Q1))
 170        CONTINUE

C --------- CALCUL DES PANS

            N1(1) = -1
            N1(2) = 0
            N1(3) = NH - 1 
            N1(4) = NH

            DO 180 J = 2, NH

              DO 190 K = 1, 4  
                N1(K) = N1(K) + 1
 190          CONTINUE

              DO 180 K = 2, NH
                
                N = 1
                DO 200 L = 1, 4
                  N1(L) = N1(L) + 1 
                  IF (.NOT.NODANS(1,N1(L))) N = N * (L+1)  
 200            CONTINUE

                IF (N.EQ.1) THEN
                   
                  DO 210 L = 1, 4
                   
                    IF (NODANS(2,N1(L))) THEN

                      N = 1 + MOD(6-L,4)
                      N2(1,1) = N1(TRI(1,N))
                      N2(2,1) = N1(TRI(2,N))
                      N2(3,1) = N1(TRI(3,N))

                      N = 1 + MOD(L+1,4)
                      N2(1,2) = N1(TRI(1,N))
                      N2(2,2) = N1(TRI(2,N))
                      N2(3,2) = N1(TRI(3,N))

                      N = 2
                      GOTO 230

                    ENDIF

 210              CONTINUE

                ELSEIF (N.LT.6) THEN
                   
                  N2(1,1) = N1(TRI(1,N-1))
                  N2(2,1) = N1(TRI(2,N-1))
                  N2(3,1) = N1(TRI(3,N-1))

                  N = 1

                  DO 220 L = 1, 3
                    IF (NODANS(2,N2(L,1))) GOTO 230
 220              CONTINUE
                                   
                ENDIF

                N = 0

 230            CONTINUE

                DO 180 L = 1, N

                  NDEPAN = NDEPAN + 1
                      
                  SENS = .FALSE.

                  DO 240 P = 1, 3
                    N2(P,L) = N2(P,L) + Q2
                    DEPANO(P,NDEPAN) = N2(P,L)
 240              CONTINUE

                  DO 250 P = 1, 3
                    R(P) = DENO(P,N2(2,L)) - DENO(P,N2(1,L))
                    S(P) = DENO(P,N2(3,L)) - DENO(P,N2(1,L))
 250              CONTINUE  

                  T(1) = R(2)*S(3) - R(3)*S(2)
                  T(2) = R(3)*S(1) - R(1)*S(3)
                  T(3) = R(1)*S(2) - R(2)*S(1)
                  
                  R0 = 0.D0
                  DO 260 P = 1, 3
                    DEPAN(P,NDEPAN) = T(P)
                    R0 = R0 - T(P)*DENO(P,N2(1,L))
 260              CONTINUE
                
                  DEPAN(4,NDEPAN) = R0

                  R0 = 1.D0/R8NRM1(3,DEPAN(1,NDEPAN),1)
                  CALL R8SCAL(4,R0,DEPAN(1,NDEPAN),1)

 180        CONTINUE

          ENDIF

          P0 = P0 + NNP

 40     CONTINUE

      ENDIF

C --- DIVISION DE LA MAILLE

      CALL DIVITR(TMA2,DIM,DENO,Q0,DEMA,DEDIME,NDEMA,
     &            DEPAN,DEPANO,NDEPAN,PILE,LPAN,NI,NL)

C --- SELECTION DES SOUS-MAILLES

      CALL DIVSEL(DIM,NO1,TMA1,MA1,DIM1,H1,MM1,PAN1,
     &              NO2,TMA2,DENO,DEMA,DEDIME,NDEMA)

      END
