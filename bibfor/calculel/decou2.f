      SUBROUTINE DECOU2(DIM,NN0,MA,SONO,NSONO,TYPE,NCOTE,DENO,DEMA,
     &                      DEDIME,NDEMA,SEP,NSEP,MASEP,NMASEP,NMA)

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
C          REGULARISATION DES SOUS-MAILLES PRODUITES PAR DECOUP
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  DIM           : DIMENSION D'ESPACE
C INTEGER  NNO           : NOMBRE MAX DE NOEUDS POUR LES NOUV S-MAILLES
C                          SI NNO = 0 : MAXIMUM POSSIBLE
C INTEGER  MA            : INDEX DANS DEMA DE LA SOUS-MAILLE DECOUPEE 
C INTEGER  SONO(10,*)    : CONNECTIVITE DES SOUS-MAILLES A DECOUPER
C INTEGER  NSONO(*)      : NOMBRE DE NOEUDS DES SOUS-MAILLES A DECOUPER
C INTEGER  TYPE(*)       : TYPE DES SOUS-MAILLES A DECOUPER (CF DECOUP)
C INTEGER  NCOTE         : NOMBRE DE SOUS-MAILLES A DECOUPER
C REAL*8   DENO(DIM,*)   : COORD. NOEUDS DES SOUS-MAILLES
C INTEGER  NDENO         : NOMBRE DE NOEUDS DANS DENO
C
C VARIABLES D'ENTREE / SORTIE
C INTEGER  DEMA(8,*)     : CONNECTIVITE DES SOUS-MAILLES
C INTEGER  DEDIME(*)     : NOMBRE DE NOEUDS DES SOUS-MAILLES
C INTEGER  NDEMA         : NOMBRE DE SOUS-MAILLES
C 
C VARIABLES DE SORTIE
C REAL*8   SEP(DIM+1,*)  : EQUATIONS PANS SEPARATEURS SUPPLEMENTAIRES
C INTEGER  NSEP          : NOMBRE DE PANS SEPARATEURS SUPPLEMENTAIRES
C INTEGER  MASEP(4,*)    : INDEX DANS SEP PANS POUR NOUV SOUS-MAILLES
C INTEGER  NMASEP(*)     : NOMBRE DE PANS SEP POUR NOUV SOUS-MAILLES
C INTEGER  NMA(*)        : NOMBRE DE NOUVELLES SOUS-MAILLES
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTION
      REAL*8   R8DOT, R8NRM1

C --- VARIABLES
      INTEGER  DIM,NN0,MA,NDEMA,NSEP,NSONO(*),NMA(*),TYPE(*),NCOTE
      INTEGER  DEDIME(*),DEMA(8,*),MASEP(4,*),NMASEP(*),SONO(10,*)
      INTEGER  NMAIL,COTE,I,J,N,P,P0,Q,N0,N1,N2,N3,NRG
      REAL*8   SEP(4,*),DENO(DIM,*),R(3),S(3),R0

C --- TABLES
      INTEGER INDEX2(8),REGLE2(485)
      DATA INDEX2 / 1,17,34,64,132,200,274,380 /
      DATA REGLE2 / 001,2, 2,4, 1,1, 1,-1, 3,1,2,4, 3,2,3,4, 001,2, 2,5,
     & 1,1, 1,-1, 3,1,2,5, 4,2,3,4,5, 02,3, 2,3,4, 2,4,6, 1,1, 2,-1,-2,
     & 1,2, 4,1,2,3,4, 4,2,3,4,6, 4,2,4,5,6, 006,6, 2,4,8, 2,6,8, 2,4,5,
     & 2,5,8, 2,7,4, 2,7,8, 1,3, 3,1,-3,-4, 2,-2,4, 1,5, 3,-1,-5,6, 
     & 2,2,-6, 4,1,2,4,5, 4,2,4,5,8, 4,2,5,6,8, 4,3,4,2,7, 4,2,7,4,8,
     & 4,2,6,7,8, 006,6, 2,3,6, 2,5,6, 2,3,4, 2,4,6, 6,3,8, 6,2,8, 1,3,
     & 3,1,-3,-4, 2,-2,4, 1,5, 3,-1,-5,6, 2,2,-6, 4,1,2,3,4, 4,2,3,4,6,
     & 4,2,4,5,6, 4,7,6,3,8, 4,6,3,8,2, 4,6,8,5,2, 006,7, 3,10,2, 
     & 5,10,4, 1,10,6, 3,10,1, 5,10,3, 1,10,5, 1,-1, 1,-2, 1,-3, 2,1,-4,
     & 2,2,-5, 2,3,-6, 3,4,5,6, 4,7,3,2,10, 4,8,5,4,10, 4,9,1,6,10,
     & 4,2,3,1,10, 4,4,5,3,10, 4,6,1,5,10, 4,3,5,1,10, 0010,9, 1,3,9, 
     & 1,5,9, 1,3,10, 1,9,10, 1,6,9, 9,6,3, 1,3,6, 3,6,8, 1,3,7, 3,6,7,
     & 1,3, 3,1,-3,4, 2,-2,-4, 2,2,-5, 4,-1,5,6,7, 2,-6,-8, 1,-9, 3,-7,
     & 9,10, 2,8,-10, 4,1,3,4,10, 4,3,9,10,1, 4,1,10,5,9, 4,1,5,6,9,
     & 4,1,6,3,9, 4,3,6,8,9, 4,7,2,1,3, 4,1,6,7,3, 4,8,7,6,3, 0010,9,
     & 5,9,1, 9,10,1, 1,4,9, 1,6,9, 1,3,8, 1,6,8, 1,8,9, 1,3,9, 1,2,8,
     & 2,6,8, 2,1,2, 2,-1,3, 2,-2,-4, 2,-3,8, 3,4,-6,-7, 3,5,7,-8,
     & 2,-5,9, 3,6,-9,-10, 1,10, 4,5,9,10,1, 4,5,4,9,1, 4,9,6,10,1,
     & 4,1,3,4,9, 4,9,8,6,1, 4,3,8,9,1, 4,1,2,3,8, 4,6,2,1,8, 4,2,6,7,8/

C --- INITIALISATION

      NSEP = 0
      NMAIL = 0

      IF (NN0.EQ.0) THEN
        IF (DIM.EQ.2) THEN
          N0 = 1
        ELSE
          N0 = 4
        ENDIF
      ELSE
        IF (DIM.EQ.2) THEN
          N0 = NN0 - 3
        ELSE
          N0 = NN0 / 2
        ENDIF
      ENDIF

      NMA(1) = 0
      NMA(2) = 0

      DO 10 COTE = 1, NCOTE

C ----- TEST BESOIN DECOUPAGE

        IF (DIM.EQ.2) THEN
          N = NSONO(COTE) - 3
        ELSE
          N = TYPE(COTE)
        ENDIF

C ----- PAS DE DECOUPAGE

        IF (N.LE.N0) THEN

          IF (NMAIL.NE.0) THEN 
            NDEMA = NDEMA + 1 
            Q = NDEMA
          ELSE
            Q = MA
          ENDIF

          NMA(COTE) = NMA(COTE) + 1
          NMAIL = NMAIL + 1
          IF (NN0.EQ.0) NMASEP(NMAIL) = 0

          N = NSONO(COTE)
          DO 20 I = 1, N
            DEMA(I,Q) = SONO(I,COTE)
 20       CONTINUE
          DEDIME(Q) = N

C ----- DECOUPAGE

        ELSE

          P = INDEX2(N)
          Q = REGLE2(P)
          N = REGLE2(P+1)
          P = P + 1

C ------- CALCUL DES PANS SEPARATEURS
          
          IF (NN0.EQ.0) THEN

            IF (DIM.EQ.2) THEN
            
              DO 30 I = 1, Q

                N1 = SONO(REGLE2(P+1),COTE)
                N2 = SONO(REGLE2(P+2),COTE)
                P = P + 2

                SEP(1,NSEP+I) = DENO(2,N2) - DENO(2,N1)
                SEP(2,NSEP+I) = DENO(1,N1) - DENO(1,N2)
                SEP(3,NSEP+I) = DENO(1,N2) * DENO(2,N1) 
     &                        - DENO(1,N1) * DENO(2,N2)

                R0 = 1.D0/R8NRM1(2,SEP(1,NSEP+I),1)
                CALL R8SCAL(3,R0,SEP(1,NSEP+I),1)

 30           CONTINUE

            ELSE

              DO 40  I = 1, Q

                N1 = SONO(REGLE2(P+1),COTE)
                N2 = SONO(REGLE2(P+2),COTE)
                N3 = SONO(REGLE2(P+3),COTE)
                P = P + 3

                DO 50 J = 1, 3
                  R(J) = DENO(J,N2) - DENO(J,N1)
                  S(J) = DENO(J,N3) - DENO(J,N1)
 50             CONTINUE  

                SEP(1,NSEP+I) = R(2)*S(3) - R(3)*S(2)
                SEP(2,NSEP+I) = R(3)*S(1) - R(1)*S(3)
                SEP(3,NSEP+I) = R(1)*S(2) - R(2)*S(1)
                SEP(4,NSEP+I) =-R8DOT(3,SEP(1,NSEP+I),1,DENO(1,N1),1)

                R0 = 1.D0/R8NRM1(3,SEP(1,NSEP+I),1)
                CALL R8SCAL(4,R0,SEP(1,NSEP+I),1)

 40           CONTINUE          

            ENDIF

C --------- CALCUL MASEP

            DO 60 I = 1, N

              P = P + 1
              NRG = REGLE2(P)
              NMASEP(NMAIL+I) = NRG

              DO 60 J = 1, NRG
                P = P + 1
                P0 = REGLE2(P)
                IF (P0.GT.0) THEN
                  MASEP(J,NMAIL+I) = NSEP + REGLE2(P)
                ELSE
                  MASEP(J,NMAIL+I) = REGLE2(P) - NSEP 
                ENDIF
 60         CONTINUE

            NSEP = NSEP + Q

          ELSE

            P = P + (DIM+2)*Q + N

          ENDIF

C ------- CALCUL DEMA

          NMA(COTE) = NMA(COTE) + N

          DO 70 I = 1, N
             
            IF (NMAIL.NE.0) THEN 
              NDEMA = NDEMA + 1 
              Q = NDEMA
            ELSE
              Q = MA
            ENDIF

            NMAIL = NMAIL + 1
       
            P = P + 1
            NRG = REGLE2(P)
            DEDIME(Q) = NRG

            DO 70 J = 1, NRG
              P = P + 1
              DEMA(J,Q) = SONO(REGLE2(P),COTE)
 70       CONTINUE

        ENDIF
              
 10   CONTINUE

      END
