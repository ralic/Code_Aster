      SUBROUTINE PTENCI(NEQ,X,MAT,OMEG,EN,ITYPE,KANL,IDIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NEQ,            ITYPE,KANL,IDIS
      REAL*8              X(*),MAT(NEQ,NEQ),OMEG,EN(*)
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
C     CALCUL ENERGIE CINETIQUE POUR
C         - ELEMENT DE POUTRE (POU_D_T, POU_D_E, POU_C_T)
C         - ELEMENT DISCRET
C         - ELEMENT BARRE
C     ------------------------------------------------------------------
C IN  : NEQ    : DIMENSION DE LA MATRICE MAT
C IN  : X      : VECTEUR DE DEPLACEMENT
C IN  : MAT    : MATRICE DE MASSE
C IN  : OMEG   : PULSATION AU CARREE
C OUT : EN     : ENERGIE CINETIQUE
C IN  : ITYPE  : TYPE DE LA SECTION
C IN  : KANL   : TYPE DE LA MATRICE DE MASSE
C IN  : IDIS   : = 0 , PAS DE CALCUL DE LA REPARTITON DE L'ENERGIE
C                = 1 , CALCUL DE LA REPARTITON DE L'ENERGIE
C     ------------------------------------------------------------------
      INTEGER JCFT(8), ICOU(6,2), NCFT(3), ICFT(6,3), NA(4), IA(4,4)
      REAL*8   X2(12), MAT2(144)
      LOGICAL  LTEST
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
C ----------------------------------------------------------------------
      CONST = OMEG / 2.D0
      ZERO  = 0.D0
      LTEST = .FALSE.
C
C     --- ENERGIE CINETIQUE GLOBALE ---
      CALL VTMV ( NEQ , X , MAT , R )
      EN(1) = R * CONST
      IF ( IDIS .EQ. 0 ) GOTO 910
      IF (ABS(EN(1)).LT.1.D-06) GOTO 910
      IFORM = 0
C
C                    -----------------------------
C                    --- REPARTITION D'ENERGIE ---
C                    -----------------------------
C
      IF ( ITYPE .EQ. 10 ) THEN
C       --- ELEMENT COURBE DE SECTION CONSTANTE ---
         IFORM = 110
         NN = 3
         DO 325 L = 1 , 2
            DO 324 I = 1 , 6
               X2(I) = X(ICOU(I,L))
               DO 323 J = 1 , 6
                  MAT2(6*(J-1)+I) = MAT ( ICOU(I,L) , ICOU(J,L) )
  323          CONTINUE
  324       CONTINUE
            CALL VTMV ( 6, X2 , MAT2 , R )
            EN(1+L) = R * CONST
  325    CONTINUE
      ELSEIF ( KANL.EQ.0 ) THEN
C
C        NEQ  : NOMBRE D'EQUATION DE LA MATRICE ( 12, 6, 3 )
C        NDDL : NOMBRE DE DDL ( 6, 3 )
C
C        ITYPE : 20 , MAILLE POI1 DE TRANSLATION
C        ITYPE : 21 , MAILLE POI1 DE TRANSLATION ET ROTATION
C        ITYPE : 40 , MAILLE SEG2 DE TRANSLATION
C        ITYPE : 41 , MAILLE SEG2 DE TRANSLATION ET ROTATION
C        ITYPE : 0, 1, 2, ELEMENT DE POUTRE
C
         NDDL = NEQ / 2
         IF ( ITYPE.EQ.20 .OR. ITYPE.EQ.21 ) NDDL = NEQ
         NN = 1 + NDDL
C
C        --- ON N'A QUE LA DIAGONALE ( PAS DE TERMES D'INERTIE ) ---
         DO 10 I = 1 , NEQ-1
            DO 12 J = I+1 , NEQ
               IF ( MAT(I,J) .NE. ZERO ) GOTO 500
 12         CONTINUE
 10      CONTINUE
C
         IF ( ITYPE.EQ.20 .OR. ITYPE.EQ.21 ) THEN
            DO 14 I = 1 , NDDL
               EN(I+1) = ( X(I) * MAT(I,I) * X(I) ) * CONST
 14         CONTINUE
         ELSE
            DO 16 I = 1 , NDDL
               EN(I+1) = ( X(I+NDDL) * MAT(I+NDDL,I+NDDL) * X(I+NDDL)
     +                     + X(I) * MAT(I,I) * X(I) ) * CONST
 16         CONTINUE
         ENDIF
         IFORM = 10
         IF ( ITYPE.EQ.40 .OR. ITYPE.EQ.20 ) IFORM = 11
C
         GOTO 900
C
C        --- ON TIENT COMPTE DES TERMES D'INERTIE ---
 500     CONTINUE
C
         IF ( NDDL .GE. 6 ) THEN
            DO 20 I = 1, 3
              DO 22 J = I+1, 6
                 IF ( MAT(I,J) .NE. ZERO ) GOTO 600
 22           CONTINUE
 20         CONTINUE
         ELSE
            GOTO 600
         ENDIF
C            --- MASSE CONCENTREE + INERTIES --
         IF ( ITYPE .EQ. 21 ) THEN
            DO 24 I = 1 , NDDL
               EN(I+1) = ( X(I) * MAT(I,I) * X(I) ) * CONST
 24         CONTINUE
         ELSE
            DO 26 I = 1 , NDDL
               EN(I+1) = ( X(I+NDDL) * MAT(I+NDDL,I+NDDL) * X(I+NDDL)
     +                     + X(I) * MAT(I,I) * X(I) ) * CONST
 26         CONTINUE
         ENDIF
         IF ( NDDL .EQ. 6 ) THEN
            DO 203 I = 1 , 3
               X2(I) = X(I+3)
               DO 204 J = 1 , 3
                  MAT2(3*(J-1)+I) = MAT(I+3,J+3)
  204          CONTINUE
  203       CONTINUE
         ELSE
            DO 205 I = 1 , 3
               X2(I)    = X(I+3)
               X2(I+3) =  X(I+9)
               DO 206 J = 1 , 3
                  MAT2(6*(J-1)+I  ) = MAT(I+3,J+3)
                  MAT2(6*(J+2)+I+3) = MAT(I+9,J+9)
  206          CONTINUE
  205       CONTINUE
         ENDIF
         CALL VTMV ( NDDL , X2 , MAT2 , R )
         EN(5) = R * CONST
         IFORM = 20
         NN = 5
         GOTO 900
C
 600     CONTINUE
C
C
      ELSEIF ( ITYPE.EQ.0 .OR. ITYPE.EQ.1 .OR. ITYPE.EQ.2 ) THEN
C       -------------------- MASSES EQUIVALENTES -----------------------
C       --- ELEMENT DROIT DE SECTION CONSTANTE OU VARIABLE ---
C
         DO 310 KK= 1,8
           IF ( MAT( 4,JCFT(KK)).NE.ZERO  .OR.
     +          MAT(10,JCFT(KK)).NE.ZERO)THEN
C             --- COUPLAGE FLEXION-TORSION ---
              DO 305 L = 1 , 3
                DO 304 I = 1 , NCFT(L)
                  X2(I) = X(ICFT(I,L))
                  DO 303 J = 1 , NCFT(L)
                    MAT2(NCFT(L)*(J-1)+I) = MAT(ICFT(I,L),ICFT(J,L) )
  303             CONTINUE
  304           CONTINUE
                CALL VTMV ( NCFT(L) , X2 , MAT2 , R )
                EN(1+L) = R * CONST
  305         CONTINUE
              IFORM = 101
              NN = 4
              GOTO 900
           ENDIF
  310    CONTINUE
C
C       --- ELEMENT DROIT CLASSIQUE ---
         IFORM = 101
         NN = 5
         DO 315 L = 1 , 4
            DO 314 I = 1 , NA(L)
               X2(I) = X(IA(I,L))
               DO 313 J = 1 , NA(L)
                  MAT2(NA(L)*(J-1)+I) = MAT ( IA(I,L) , IA(J,L) )
  313          CONTINUE
  314       CONTINUE
            CALL VTMV ( NA(L) , X2 , MAT2 , R )
            EN(1+L) = R * CONST
  315    CONTINUE
      ENDIF
  900 CONTINUE
C
C     --- POURCENTAGE ----
      DO 410 I = 2,NN
         EN(I) = EN(I)/EN(1)
  410 CONTINUE
C
C     -- SORTIE --------------------------------------------------------
  910 CONTINUE
      IF ( LTEST ) THEN
          WRITE(6,*)'--->> PTENCI     ITYPE = ',ITYPE
          WRITE(6,*)'                  KANL = ',KANL
          WRITE(6,*)'                  OMEG = ',OMEG
          WRITE(6,*)'---------ENERGIE CINETIQUE--------'
          WRITE(6,*)'ENERGIE CINETIQUE GLOBALE ',EN(1)
          IF ( IFORM.EQ.10  ) THEN
             WRITE(6,*)'MASSE CONCENTREE STRICTEMENT DIAGONALE'
             WRITE(6,*)'TRANSLATION X  ',EN(2)
             WRITE(6,*)'TRANSLATION Y  ',EN(3)
             WRITE(6,*)'TRANSLATION Z  ',EN(4)
             WRITE(6,*)'ROTATION /X    ',EN(5)
             WRITE(6,*)'ROTATION /Y    ',EN(6)
             WRITE(6,*)'ROTATION /Z    ',EN(7)
          ELSEIF ( IFORM.EQ.11  ) THEN
             WRITE(6,*)'MASSE CONCENTREE STRICTEMENT DIAGONALE'
             WRITE(6,*)'TRANSLATION X  ',EN(2)
             WRITE(6,*)'TRANSLATION Y  ',EN(3)
             WRITE(6,*)'TRANSLATION Z  ',EN(4)
          ELSEIF ( IFORM.EQ. 20 ) THEN
             WRITE(6,*)'MASSE DIAGONALE + INERTIE '
             WRITE(6,*)'TRANSLATION X  ',EN(2)
             WRITE(6,*)'TRANSLATION Y  ',EN(3)
             WRITE(6,*)'TRANSLATION Z  ',EN(4)
             WRITE(6,*)'ROTATION       ',EN(5)
          ELSEIF ( IFORM.EQ.100 ) THEN
             WRITE(6,*)'ELEMENT DROIT "ORDINAIRE"'
             WRITE(6,*)'DE TRACTION-COMPRESSION ',EN(2)
             WRITE(6,*)'DE TORSION              ',EN(3)
             WRITE(6,*)'DE FLEXION Y            ',EN(4)
             WRITE(6,*)'DE FLEXION Z            ',EN(5)
          ELSEIF ( IFORM.EQ.101 ) THEN
             WRITE(6,*)'ELEMENT DROIT AVEC COUPLAGE FLEXION-TORSION'
             WRITE(6,*)'DE TRACTION-COMPRESSION ',EN(2)
             WRITE(6,*)'DE FLEXION-TORSION Y    ',EN(3)
             WRITE(6,*)'DE FLEXION-TORSION Z    ',EN(4)
          ELSEIF ( IFORM.EQ.110 ) THEN
             WRITE(6,*)'ELEMENT COURBE'
             WRITE(6,*)'DE FLEXION DANS LE PLAN ',EN(2)
             WRITE(6,*)'DE FLEXION HORS DU PLAN ',EN(3)
          ENDIF
      ENDIF
C
      END
