      SUBROUTINE TANGNT(NO,NNR,DIME,L1,L2,TANG)

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
C A_UTIL
C ----------------------------------------------------------------------
C       TANGENTES AUX NOEUDS OU TANGENTE CONSTANTE D'UNE MAILLE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE 
C INTEGER  L1                 : L1 = 0 : TANGENTES AUX NOEUDS
C                               SINON  : TANGENTE MOYENNE CONSTANTE
C INTEGER  L2                 : L2 = 0 : NOEUDS MAILLE MOYENNE
C                               SINON  : NOEUDS MAILLE SOLIDE
C REAL*8   NO(DIME,*)         : COORDONNEES DES NOEUDS
C INTEGER  NNR                : NOMBRE DE NOEUDS MAILLE MOYENNE
C INTEGER  DIME               : DIMENSION DE L'ESPACE
C    
C VARIABLE DE SORTIE
C REAL*8  TANG(DIME,DIME-1,*) : TANGENTE[S]
C                              ( T1.1X,T1.1Y,[T1.1Z,T1.2X,T1.2Y,T1.2Z],
C                               [T2.1X, ... ] )
C                                T1.1 [T1.2] TANGENTE[S] CONSTANTE[S] 
C                                OU T*.1 [T*.2] TANGENTE[S] AU NOEUD *
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      INTEGER DIME,NNR,L1,L2,I,J,K,K1,K2,L,Q,NT,NOECOQ(2,9)
      REAL*8  TANG(DIME,DIME-1,*),NO(DIME,*),R,COETAN(162)

      NT = DIME-1
      L = L1
      Q = 0

      CALL COTANG(NNR,DIME,L,COETAN)

C --- TANGENTE VARIABLE

      IF (L.EQ.0) THEN

        DO 10 I = 1, NNR
          DO 10 J = 1, NT
            DO 10 K = 1, DIME
              TANG(K,J,I) = 0.D0
 10     CONTINUE

C ----- NOEUDS DE LA MAILLE MOYENNE

        IF (L2.EQ.0) THEN

          DO 20 I = 1, NNR
            DO 20 J = 1, NT
              DO 20 K = 1, NNR

                Q = Q + 1
                R = COETAN(Q)
                
                IF (R.NE.0.D0) THEN

                  DO 30 L = 1, DIME
                    TANG(L,J,I) = TANG(L,J,I) + R*NO(L,K)
 30               CONTINUE

                ENDIF

 20       CONTINUE 
                    
C ----- NOEUDS DE LA MAILLE SOLIDE

        ELSE

          CALL NOCOQU(DIME,NNR,NOECOQ)

          DO 40 I = 1, NNR
            DO 40 J = 1, NT
              DO 40 K = 1, NNR

                K1 = NOECOQ(1,K)
                K2 = NOECOQ(2,K)

                Q = Q + 1
                R = COETAN(Q)

                IF (R.NE.0.D0) THEN

                  DO 50 L = 1, DIME
                    TANG(L,J,I) = TANG(L,J,I) + R*(NO(L,K1)+NO(L,K2))
 50               CONTINUE

                ENDIF

 40       CONTINUE 
 
        ENDIF

C --- TANGENTE CONSTANTE

      ELSE

        DO 60 I = 1, NT
          DO 60 J = 1, DIME
            TANG(J,I,1) = 0.D0
 60     CONTINUE

C ----- NOEUDS DE LA MAILLE MOYENNE

        IF (L2.EQ.0) THEN

          DO 70 I = 1, NT
            DO 70 J = 1, NNR

              Q = Q + 1
              R = COETAN(Q)

              IF (R.NE.0.D0) THEN

                DO 80 K = 1, DIME
                  TANG(K,I,1) = TANG(K,I,1) + R*NO(K,J)
 80             CONTINUE

              ENDIF

 70       CONTINUE 
        
C ----- NOEUDS DE LA MAILLE SOLIDE
            
        ELSE

          CALL NOCOQU(DIME,NNR,NOECOQ)

          DO 90 I = 1, NT
            DO 90 J = 1, NNR

              K1 = NOECOQ(1,J)
              K2 = NOECOQ(2,J)

              Q = Q + 1
              R = COETAN(Q)

              IF (R.NE.0.D0) THEN

                DO 100 K = 1, DIME
                  TANG(K,I,1) = TANG(K,I,1) + R*(NO(K,K1)+NO(K,K2))
 100            CONTINUE

                ENDIF

 90       CONTINUE 
 
        ENDIF

C ----- TANGENTE CONSTANTE --> TANGENTE AUX NOEUDS

        IF (L1.EQ.0) THEN

          DO 110 I = 2, NNR
            DO 110 J = 1, NT
              CALL R8COPY(NNR,TANG(1,J,1),1,TANG(1,J,I),1)
 110      CONTINUE

        ENDIF

      ENDIF
      
      END
