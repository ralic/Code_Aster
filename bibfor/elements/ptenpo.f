      SUBROUTINE PTENPO(N,X,MAT,EP,ITYPE,IFORM)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           N,         ITYPE,IFORM
      REAL*8              X(*),MAT(N,N),EP(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
C     CALCUL ENERGIE DE DEFORMATION POUR
C         - ELEMENT DE POUTRE (POU_D_T, POU_D_E, POU_C_T)
C         - ELEMENT DISCRET
C         - ELEMENT BARRE
C     ------------------------------------------------------------------
C IN  N      :  DIMENSION DE LA MATRICE MAT
C IN  X      :  VECTEUR DE DEPLACEMENT
C IN  MAT    :  MATRICE DE RAIDEUR
C OUT EP     :  ENERGIE DE DEFORMATION
C IN  ITYPE  :  TYPE DE LA SECTION
C IN  IFORM  :
C     ------------------------------------------------------------------
      INTEGER JCFT(8), ICOU(6,2), NCFT(3), ICFT(6,3), NA(4), IA(4,4)
      REAL*8   X2(12), MAT2(144)
C-DEL LOGICAL  LTEST
C
C             COUPLAGE FLEXION-TORSION
      DATA JCFT/  2 ,  3 ,  5 ,  6 ,  8 ,  9 , 11 , 12 /
      DATA NCFT/  2 ,  6 ,  6 /
      DATA ICFT/  1 ,  7 ,  0 ,  0 ,  0 ,  0 ,
     +            2 ,  4 ,  6 ,  8 , 10 , 12 ,
     +            3 ,  4 ,  5 ,  9 , 10 , 11 /
C
C             ELEMENT COURBE
      DATA ICOU/  1 ,  2 ,  6 ,  7 ,  8 , 12 ,
     +            3 ,  4 ,  5 ,  9 , 10 , 11 /
C
C            ELEMENT DROIT CLASSIQUE
      DATA NA/  2 ,  2 ,  4 ,  4  /
      DATA IA/  1 ,  7 ,  0 ,  0 ,
     +          4 , 10 ,  0 ,  0 ,
     +          2 ,  6 ,  8 , 12 ,
     +          3 ,  5 ,  9 , 11  /
C
C-DEL LTEST = .FALSE.
      ZERO  = 0.D0
      DEUX  = 2.D0
C
C     --- ENERGIE POTENTIELLE GLOBALE ---
      CALL VTMV ( N , X , MAT , R )
      EP(1) = R / DEUX
      IF ( IFORM .EQ. 0 ) GOTO 900
      IF (ABS(EP(1)).LT.1.D-06) GOTO 900
C-DEL IFORM = 0
C
C                    -----------------------------
C                    --- REPARTITION D'ENERGIE ---
C                    -----------------------------
      IF ( ITYPE.EQ.0 .OR. ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
C       --- ELEMENT DROIT DE SECTION CONSTANTE OU VARIABLE ---
         NN    = 4
         DO 10 KK = 1,8
            IF ( MAT( 4,JCFT(KK)).NE.ZERO .OR.
     +           MAT(10,JCFT(KK)).NE.ZERO      )THEN
C             --- COUPLAGE FLEXION-TORSION ---
              DO  5 L = 1 , 3
                DO 4 I = 1 , NCFT(L)
                  X2(I) = X(ICFT(I,L))
                  DO 3 J = 1 , NCFT(L)
                     MAT2(NCFT(L)*(J-1)+I) = MAT(ICFT(I,L),ICFT(J,L) )
    3             CONTINUE
    4           CONTINUE
                CALL VTMV ( NCFT(L) , X2 , MAT2 , R )
                EP(1+L) = R / DEUX
    5         CONTINUE
              IFORM= 101
              GOTO 900
           ENDIF
   10    CONTINUE
C       --- ELEMENT DROIT CLASSIQUE ---
C-DEL    IFORM = 100
         DO 15 L = 1 , 4
            DO 14 I = 1 , NA(L)
               X2(I) = X(IA(I,L))
               DO 13 J = 1 , NA(L)
                  MAT2(NA(L)*(J-1)+I) = MAT ( IA(I,L) , IA(J,L) )
   13          CONTINUE
   14       CONTINUE
            CALL VTMV ( NA(L) , X2 , MAT2 , R )
            EP(1+L) = R / DEUX
   15    CONTINUE
C
      ELSE IF ( ITYPE .EQ. 10 ) THEN
C       --- ELEMENT COURBE DE SECTION CONSTANTE ---
C-DEL    IFORM = 110
         NN    = 2
         DO 25 L = 1 , 2
            DO 24 I = 1 , 6
               X2(I) = X(ICOU(I,L))
               DO 23 J = 1 , 6
                  MAT2(6*(J-1)+I) = MAT ( ICOU(I,L) , ICOU(J,L) )
   23          CONTINUE
   24       CONTINUE
            CALL VTMV ( 6, X2 , MAT2 , R )
            EP(1+L) = R / DEUX
   25    CONTINUE
C
      ELSE IF ( ITYPE .EQ. 20 .OR. ITYPE.EQ.21 ) THEN
C        --- ELEMENT DISCRET TYPE NODALE ..._N ---
C-DEL    IFORM = 10
         NN    = N
         DO 202 I = 2,N
            DO 201 J = 1,I-1
               IF ( MAT(I,J) .NE. ZERO ) GOTO 900
  201       CONTINUE
  202    CONTINUE
         DO 203 I = 1,N
            EP(1+I) = X(I) * MAT(I,I) * X(I) / DEUX
  203    CONTINUE
C
      ELSE IF ( ITYPE .EQ. 40 .OR. ITYPE.EQ.41 ) THEN
C        --- ELEMENT DISCRET TYPE LIAISON ..._L ---
C-DEL    IFORM = 10
         NN    = N / 2
         DO 402 I = 2,NN
            DO 401 J = 1,I-1
               IF ( MAT(I,J).NE.ZERO .OR. MAT(I,J+NN).NE.ZERO .OR.
     +              MAT(I+NN,J+NN) .NE. ZERO                ) GOTO 900
  401       CONTINUE
  402    CONTINUE
         DO 403 I = 1,NN
            EP(1+I) = ( X(I) * MAT(I,I) * X(I)
     +                 + 2 * X(I) * MAT(I,I+NN) * X(I+NN)
     +                 + X(I+NN) * MAT(I+NN,I+NN) * X(I+NN) ) / DEUX
  403    CONTINUE
      ENDIF
C
C     --- POURCENTAGE ----
      DO 410 I = 2,NN+1
         EP(I) = EP(I)/EP(1)
  410 CONTINUE
C
  900 CONTINUE
C-DEL IF ( LTEST ) THEN
C-DEL    WRITE(6,*)'--------ENERGIE POTENTIELLE--------'
C-DEL    WRITE(6,*)'ENERGIE POTENTIELLE GLOBALE :',EP(1)
C-DEL    IF ( IFORM.EQ.100 ) THEN
C-DEL       WRITE(6,*)'ELEMENT DROIT "ORDINAIRE"'
C-DEL       WRITE(6,*)'FRACTION EN TRACTION-COMPRESSION :',EP(2)
C-DEL       WRITE(6,*)'FRACTION EN TORSION              :',EP(3)
C-DEL       WRITE(6,*)'FRACTION EN FLEXION Y            :',EP(4)
C-DEL       WRITE(6,*)'FRACTION EN FLEXION Z            :',EP(5)
C-DEL    ELSEIF ( IFORM.EQ.101 ) THEN
C-DEL       WRITE(6,*)'ELEMENT DROIT AVEC COUPLAGE FLEXION-TORSION'
C-DEL       WRITE(6,*)'FRACTION EN TRACTION-COMPRESSION :',EP(2)
C-DEL       WRITE(6,*)'FRACTION EN FLEXION-TORSION Y    :',EP(3)
C-DEL       WRITE(6,*)'FRACTION EN FLEXION-TORSION Z    :',EP(4)
C-DEL    ELSEIF ( IFORM.EQ.110 ) THEN
C-DEL       WRITE(6,*)'ELEMENT COURBE'
C-DEL       WRITE(6,*)'FRACTION EN FLEXION DANS LE PLAN :',EP(2)
C-DEL       WRITE(6,*)'FRACTION EN FLEXION HORS DU PLAN :',EP(3)
C-DEL    ELSE IF ( IFORM .EQ. 10 ) THEN
C-DEL       DO 905 K = 1 , NN
C-DEL          WRITE(6,*)'FRACTION SUR LE D.D.L. ',K,' :',EP(1+K)
C-DEL 905   CONTINUE
C-DEL    ENDIF
C-DEL ENDIF
C
      END
