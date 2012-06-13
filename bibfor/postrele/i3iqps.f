      SUBROUTINE I3IQPS ( EPSI, K, F, DESC, DESCTM, CONEXK, COORDO,
     &                    SGT, NBPT, LSTPT, FINK)
      IMPLICIT REAL*8 (A-H,O-Z)
      INCLUDE 'jeveux.h'
      INTEGER K,DESC(*),DESCTM(*),CONEXK(*),NBPT,LSTPT(*),F
      REAL*8  EPSI,SGT(*),COORDO(*)
      LOGICAL FINK
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     INTERSECTION FACE QUADANGLE PLANE F SGT (AB)
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  K      : I : -
C IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
C IN  DESCTM : I : -
C IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
C IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
C IN  COORDO : R : TABLE GLOBALE DES COORDONEES
C IN  SGT    : R : COORDONNEES DES POINTS A ET B -
C VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU MAILLE 3D
C OUT NBPT   : I : NOMBRE DE POINT TROUVE
C            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
C            :   : DANS CE CAS OUT = EXTREMITES
C OUT LSTPT  : I : OBJ LISTE_POINT
C     ------------------------------------------------------------------
C
      CHARACTER*4 TYPSL
      INTEGER     I,J,DS1,DECF,ADESCM,ARETE,NBS,IRET,IPOS,VALI(3)
      REAL*8      A(3,3),B(3),X(3),C,ZERO,UN,R,S,T,NORMAB
      REAL*8      E1(3),E2(3),E3(3),CS(3,4),LCARA,TOLE
      REAL*8      E1I(3),E2I(3)
      LOGICAL     DEDANS,DJALA1,DJALA2
C======================================================================
C
      ZERO   = 0.0D0
      UN     = 1.0D0
      NORMAB = ZERO
C
C --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
C
      NBS    = 4
      DECF   = 8 + F
      ADESCM = DESCTM(DESC(K))
      DO 10, I = 1, 4, 1
         DS1   = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
         DO 11, J = 1, 3, 1
            CS(J,I) = COORDO(3*(DS1-1) + J)
11       CONTINUE
10    CONTINUE
C
C --- LCARA : LONGUEUR DE LA PLUS PETITE ARETE
C     AFIN DE DEFINIR UNE PRECISION RELATIVE
C
      LCARA = 1.D+50
      DO 20, J = 1, 3, 1
         C = ZERO
         DO 22, I = 1, 3, 1
            S = CS(I,J+1) - CS(I,J)
            C = C + S*S
 22      CONTINUE
         C = SQRT( C )
         LCARA = MIN ( C, LCARA )
 20   CONTINUE
      C = ZERO
      DO 24, I = 1, 3, 1
         S = CS(I,4) - CS(I,1)
         C = C + S*S
 24   CONTINUE
      C = SQRT( C )
      LCARA = MIN ( C, LCARA )
      TOLE = LCARA * EPSI
C
C --- DEFINITION DU REPERE LOCAL DE LA FACE
C     E1 : DEFINIT PAR L'ARETE N1 N2
C     E2 : DEFINIT PAR L'ARETE N1 N4
C     E3 : PERPENDICULAIRE A E1 E2
C
      C = ZERO
      T = ZERO
      DO 15, I = 1, 3, 1
         S     = CS(I,2) - CS(I,1)
         R     = CS(I,4) - CS(I,1)
         E1(I) = S
         E2(I) = R
         C     = C + S*S
         T     = T + R*R
15    CONTINUE
      C = SQRT(C)
      T = SQRT(T)
      IF ( (C.LE.EPSI) .OR. (T.LE.EPSI) ) THEN
         VALI(1) = K
         VALI(2) = F
         CALL U2MESI('F','INTEMAIL_24',2,VALI)
      ENDIF
      DO 16, I = 1, 3, 1
         E1(I) = E1(I)/C
         E2(I) = E2(I)/T
16    CONTINUE
      E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
      E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
      E3(3) = E1(1)*E2(2) - E1(2)*E2(1)
      C = ZERO
      DO 17, I = 1, 3, 1
         C = C + E3(I)*E3(I)
17    CONTINUE
      C = SQRT(C)
      IF ( C .LE. EPSI ) THEN
         VALI(1) = K
         VALI(2) = F
         CALL U2MESI('F','INTEMAIL_24',2,VALI)
      ENDIF
      E3(1) =  E3(1)/C
      E3(2) =  E3(2)/C
      E3(3) =  E3(3)/C
      E2(1) = -E1(2)*E3(3) + E1(3)*E3(2)
      E2(2) = -E1(3)*E3(1) + E1(1)*E3(3)
      E2(3) = -E1(1)*E3(2) + E1(2)*E3(1)
      C     =  SQRT(E2(1)*E2(1)+E2(2)*E2(2)+E2(3)*E2(3))
      E2(1) =  E2(1)/C
      E2(2) =  E2(2)/C
      E2(3) =  E2(3)/C
C
C --- UN TOUR DE PASSE-PASSE POUR AVOIR DES VECTEURS AVEC DES 1 ET 0
C     AFIN DE SUPPRIMER LES ERREURS NUMERIQUES
C
      IF ( ABS(ABS(E3(1))-UN) .LE. TOLE) THEN
         E3(1) = SIGN(UN,E3(1))
         E3(2) = ZERO
         E3(3) = ZERO
      ELSE IF ( ABS(ABS(E3(2))-UN) .LE. TOLE) THEN
         E3(2) = SIGN(UN,E3(2))
         E3(1) = ZERO
         E3(3) = ZERO
      ELSE IF ( ABS(ABS(E3(3))-UN) .LE. TOLE) THEN
         E3(3) = SIGN(UN,E3(3))
         E3(1) = ZERO
         E3(2) = ZERO
      ENDIF
C
      IF ( (ABS(ABS(E1(1))-UN) .LE. TOLE) .AND.
     &         ((E1(2) .NE. ZERO) .OR. (E1(3) .NE. ZERO)) ) THEN
         IF (ABS(E1(1)) .NE. UN) THEN
            E1I(1) = SIGN(UN,E1(1))
            E1I(2) = ZERO
            E1I(3) = ZERO
            CALL I3SL3R ( E1, E1I, E3, CS )
            E1(1) = SIGN(UN,E1(1))
            E1(3) = ZERO
            E1(2) = ZERO
         ENDIF
       ELSE IF ( (ABS(ABS(E1(2))-UN) .LE. TOLE) .AND.
     &         ((E1(1) .NE. ZERO) .OR. (E1(3) .NE. ZERO)) ) THEN
         IF (ABS(E1(2)) .NE. UN) THEN
            E1I(2) = SIGN(UN,E1(2))
            E1I(1) = ZERO
            E1I(3) = ZERO
            CALL I3SL3R ( E1, E1I, E3, CS )
            E1(2) = SIGN(UN,E1(2))
            E1(1) = ZERO
            E1(3) = ZERO
         ENDIF
      ELSE IF ( (ABS(ABS(E1(3))-UN) .LE. TOLE) .AND.
     &         ((E1(2) .NE. ZERO) .OR. (E1(1) .NE. ZERO)) ) THEN
         IF (ABS(E1(3)) .NE. UN) THEN
            E1I(3) = SIGN(UN,E1(3))
            E1I(1) = ZERO
            E1I(2) = ZERO
            CALL I3SL3R ( E1, E1I, E3, CS )
            E1(3) = SIGN(UN,E1(3))
            E1(1) = ZERO
            E1(2) = ZERO
         ENDIF
      ENDIF
C
      IF ( (ABS(ABS(E2(1))-UN) .LE. TOLE) .AND.
     &         ((E2(2) .NE. ZERO) .OR. (E2(3) .NE. ZERO)) ) THEN
         IF (ABS(E2(1)) .NE. UN) THEN
            E2I(1) = SIGN(UN,E2(1))
            E2I(3) = ZERO
            E2I(2) = ZERO
            CALL I3SL3R ( E2, E2I, E3, CS )
            E2(1) = SIGN(UN,E2(1))
            E2(3) = ZERO
            E2(2) = ZERO
         ENDIF
      ELSE IF ( (ABS(ABS(E2(2))-UN) .LE. TOLE) .AND.
     &         ((E2(1) .NE. ZERO) .OR. (E2(3) .NE. ZERO)) ) THEN
         IF (ABS(E2(2)) .NE. UN) THEN
            E2I(2) = SIGN(UN,E2(2))
            E2I(1) = ZERO
            E2I(3) = ZERO
            CALL I3SL3R ( E2, E2I, E3, CS )
            E2(2) = SIGN(UN,E2(2))
            E2(1) = ZERO
            E2(3) = ZERO
         ENDIF
      ELSE IF ( (ABS(ABS(E2(3))-UN) .LE. TOLE) .AND.
     &         ((E2(1) .NE. ZERO) .OR. (E2(2) .NE. ZERO)) ) THEN
         IF (ABS(E2(3)) .NE. UN) THEN
            E2I(3) = SIGN(UN,E2(3))
            E2I(1) = ZERO
            E2I(2) = ZERO
            CALL I3SL3R ( E2, E2I, E3, CS )
            E2(3) = SIGN(UN,E2(3))
            E2(1) = ZERO
            E2(2) = ZERO
         ENDIF
      ENDIF
C
C --- UN DEUXIEME TOUR DE PASSE-PASSE POUR METTRE LE POINT 3 DANS LE
C     PLAN DE LA FACE
C
      C = ZERO
      DO 80, I = 1, 3, 1
        C = C - ( (CS(I,3)-CS(I,1)) * E3(I) )
 80   CONTINUE
      DO 82, J = 1, 3, 1
         CS(J,3) = CS(J,3) + ( C * E3(J) )
 82   CONTINUE
C
C --- RECHERCHE DE L'INTERSECTION FACE SEGMENT AB
C     MATRICE A : VECTEUR DE LA FACE
C     VECTEUR B : COORDONNEES DU NOEUD 1 DE LA FACE
C     VECTEUR C : SEGMENT AB
C
      DO 25, I = 1, 3, 1
         A(I,1) =  E1(I)
         A(I,2) =  E2(I)
         C      =  SGT(I+3) - SGT(I)
         NORMAB =  NORMAB + C*C
         B(I)   = -CS(I,1)
25    CONTINUE
      NORMAB = SQRT(NORMAB)
      A(1,3) =  ZERO
      A(2,3) =  ZERO
      A(3,3) = -NORMAB
      DO 30, I = 1, 3, 1
         C = ZERO
         DO 31, J = 1, 3, 1
            C = MAX(C,ABS(A(I,J)))
31       CONTINUE
         IF ( C .GT. EPSI ) THEN
            DO 32, J = 1, 3, 1
               A(I,J) = A(I,J)/C
32          CONTINUE
            B(I) = B(I)/C
         ENDIF
30    CONTINUE
C
C --- RESOLUTION DU SYSTEME
C
      CALL I3SL33 ( TOLE, A, B, X, TYPSL )
C
      IF ( TYPSL .EQ. 'INCO' ) THEN
C          -----------------
C        PLAN INTER DROITE = VIDE ==> FACE INTER SGT = VIDE
C        DONC ACTION = VIDE
C
      ELSEIF ( TYPSL .EQ. 'DETE' ) THEN
C              -----------------
         R = X(1)
         S = X(2)
         T = X(3)
         IF ( ABS(T) .LE. TOLE ) THEN
            T = ZERO
         ELSEIF ( ABS(T-UN) .LE. TOLE ) THEN
            T = UN
         ENDIF
         IF ( ABS(R+UN) .LT. TOLE ) THEN
            R = -UN
         ELSEIF ( ABS(R-UN) .LT. TOLE ) THEN
            R =  UN
         ENDIF
         IF ( ABS(UN-S) .LT. TOLE ) THEN
            S =  UN
         ELSEIF ( ABS(UN+S) .LT. TOLE ) THEN
            S = -UN
         ENDIF
         CALL I3PTRV ( TOLE, LSTPT, NBPT, T, DJALA1, IPOS )
         IF ((T.GE.ZERO).AND.(T.LE.UN).AND.(.NOT.DJALA1) ) THEN
            X(1) =  ZERO
            X(2) =  ZERO
            X(3) =  T*NORMAB
            CALL I3PDM2(EPSI,E3,CS,NBS,X,DEDANS)
            IF ( DEDANS ) THEN
               DO 50, I = 1, 3, 1
                  X(I)    = CS(I,1)
                  CS(I,1) = ZERO
50             CONTINUE
               CALL I3RPQP ( X, E1, E2, E3, CS(1,2), NBS-1 )
               CALL I3CRQP ( EPSI, EPSI, CS, R, S, X, IRET )
               IF ( IRET .EQ. -1 ) THEN
                  VALI(1) = K
                  VALI(2) = F
                  CALL U2MESI('F','INTEMAIL_24',2,VALI)
               ELSE
                  R = X(1)
                  S = X(2)
                  IF ( ABS(R+UN) .LT. TOLE ) THEN
                     ARETE =  4
                     R     = -UN
                  ELSE IF ( ABS(R-UN) .LT. TOLE ) THEN
                     ARETE =  2
                     R     =  UN
                  ELSEIF ( ABS(UN-S) .LT. TOLE ) THEN
                     ARETE =  3
                     S     =  UN
                  ELSE IF ( ABS(UN+S) .LT. TOLE ) THEN
                     ARETE =  1
                     S     = -UN
                  ELSE
                     ARETE = 0
                  ENDIF
                  ZR(LSTPT(1) +   NBPT)     = T
                  ZI(LSTPT(2) +   NBPT)     = F
                  ZI(LSTPT(3) +   NBPT)     = ARETE
                  ZI(LSTPT(4) +   NBPT)     = 0
                  ZR(LSTPT(5) + 2*NBPT+1-1) = R
                  ZR(LSTPT(5) + 2*NBPT+2-1) = S
                  ZI(LSTPT(6) +   NBPT)     = NBPT + 1
                  NBPT                      = NBPT + 1
               ENDIF
            ENDIF
         ENDIF
C
      ELSEIF ( TYPSL .EQ. 'INDE' ) THEN
C              -----------------
C        ---> NIVEAU DIRECTEMENT INFERRIEUR
         CALL I3IDFS ( TOLE, K, F, NBS, SGT, CS, NBPT, LSTPT, FINK)
         FINK = .TRUE.
         IF ( NBPT .EQ. 0 ) THEN
            CALL I3PDM2(EPSI,E3,CS,NBS,SGT,   DJALA1)
            CALL I3PDM2(EPSI,E3,CS,NBS,SGT(4),DJALA2)
            IF ( DJALA1 .AND. DJALA2 ) THEN
               DO 60, I = 1, 3, 1
                  X(I)    = CS(I,1)
                  CS(I,1) = ZERO
                  A (I,1) = SGT(I)
                  A (I,2) = SGT(I+3)
60             CONTINUE
               CALL I3RPQP(X,E1,E2,E3,CS(1,2),NBS-1)
               CALL I3RPQP(X,E1,E2,E3,A,2)
               R = A(1,1)
               S = A(2,1)
               CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)),IRET)
               IF ( IRET .EQ. -1 ) THEN
                  VALI(1) = K
                  VALI(2) = F
                  CALL U2MESI('F','INTEMAIL_24',2,VALI)
               ENDIF
               ZR(LSTPT(1)) = ZERO
               ZI(LSTPT(2)) = F
               ZI(LSTPT(3)) = 0
               ZI(LSTPT(4)) = 0
               ZI(LSTPT(6)) = 1
               R = A(1,2)
               S = A(2,2)
               CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)+2),IRET)
               IF ( IRET .EQ. -1 ) THEN
                  VALI(1) = K
                  VALI(2) = F
                  CALL U2MESI('F','INTEMAIL_24',2,VALI)
               ENDIF
               ZR(LSTPT(1)+1) = UN
               ZI(LSTPT(2)+1) = F
               ZI(LSTPT(3)+1) = 0
               ZI(LSTPT(4)+1) = 0
               ZI(LSTPT(6)+1) = 1
               NBPT = -2
            ENDIF
         ELSE IF ( NBPT .EQ. 1) THEN
            T = ZR(LSTPT(1))
            IF ((ABS(T).LE.TOLE).OR.(ABS(T-UN).LE.TOLE)) THEN
               IF ( ABS(T-UN) .LE.TOLE ) THEN
                  I = 1
                  J = 4
                  T = ZERO
               ELSE
                  J = 1
                  I = 4
                  T = UN
               ENDIF
               CALL I3PDM2(EPSI,E3,CS,NBS,SGT(I),DJALA1)
               IF ( DJALA1 ) THEN
                  DO 61, DS1 = 1, 3, 1
                     X (DS1)   = CS(DS1,1)
                     CS(DS1,1) = ZERO
                     A (DS1,1) = SGT(I-1+DS1)
                     A (DS1,2) = SGT(J-1+DS1)
61                CONTINUE
                  CALL I3RPQP(X,E1,E2,E3,CS(1,2),NBS-1)
                  CALL I3RPQP(X,E1,E2,E3,A,2)
                  R = A(1,1)
                  S = A(2,1)
                  CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)+2),IRET)
                  IF ( IRET .EQ. -1 ) THEN
                     VALI(1) = K
                     VALI(2) = F
                     CALL U2MESI('F','INTEMAIL_24',2,VALI)
                  ENDIF
                  R = A(1,2)
                  S = A(2,2)
                  CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)),IRET)
                  IF ( IRET .EQ. -1 ) THEN
                     VALI(1) = K
                     VALI(2) = F
                     CALL U2MESI('F','INTEMAIL_24',2,VALI)
                  ENDIF
                  ZR(LSTPT(1)+1) =  T
                  ZI(LSTPT(2)+1) =  F
                  ZI(LSTPT(3)+1) =  0
                  ZI(LSTPT(4)+1) =  0
                  ZI(LSTPT(6)+1) =  2
                  NBPT           = -2
               ENDIF
            ELSE
               CALL I3PDM2(EPSI,E3,CS,NBS,SGT,DJALA1)
               IF ( DJALA1 ) THEN
                  DO 62, I = 1, 3, 1
                     X (I)   = CS(I,1)
                     CS(I,1) = ZERO
                     A (I,1) = SGT(I)
62                CONTINUE
                  CALL I3RPQP(X,E1,E2,E3,CS(1,2),NBS-1)
                  CALL I3RPQP(X,E1,E2,E3,A,1)
                  R = A(1,1)
                  S = A(2,1)
                  CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)+2),IRET)
                  IF ( IRET .EQ. -1 ) THEN
                     VALI(1) = K
                     VALI(2) = F
                     CALL U2MESI('F','INTEMAIL_24',2,VALI)
                  ENDIF
                  ZR(LSTPT(1)+1) =  ZERO
                  ZI(LSTPT(2)+1) =  F
                  ZI(LSTPT(3)+1) =  0
                  ZI(LSTPT(4)+1) =  0
                  ZI(LSTPT(6)+1) =  2
                  NBPT           = -2
               ELSE
                  CALL I3PDM2(EPSI,E3,CS,NBS,SGT(4),DJALA1)
                  IF ( DJALA1 ) THEN
                     DO 63, I = 1, 3, 1
                  X (I)   = CS(I,1)
                  CS(I,1) = ZERO
                  A (I,1) = SGT(I+3)
63                   CONTINUE
                     CALL I3RPQP(X,E1,E2,E3,CS(1,2),NBS-1)
                     CALL I3RPQP(X,E1,E2,E3,A,1)
                     R = A(1,1)
                     S = A(2,1)
                     CALL I3CRQP(EPSI,EPSI,CS,R,S,ZR(LSTPT(5)+2),
     &                     IRET)
                     IF ( IRET .EQ. -1 ) THEN
                        VALI(1) = K
                        VALI(2) = F
                        CALL U2MESI('F','INTEMAIL_24',2,VALI)
                     ENDIF
                     ZR(LSTPT(1)+1) =  UN
                     ZI(LSTPT(2)+1) =  F
                     ZI(LSTPT(3)+1) =  0
                     ZI(LSTPT(4)+1) =  0
                     ZI(LSTPT(6)+1) =  2
                     NBPT           = -2
                  ENDIF
               ENDIF
            ENDIF
            IF ( NBPT .NE. -2 ) THEN
               NBPT = 0
            ENDIF
         ELSE IF ( NBPT .EQ. 2) THEN
            NBPT = -2
         ELSE IF ( NBPT .GT. 2 ) THEN
            VALI(1) = K
            VALI(2) = F
            VALI(3) = NBPT
            CALL U2MESI('F','INTEMAIL_26',3,VALI)
         ENDIF
C
      ELSE
         CALL U2MESK('F','INTEMAIL_8',1,TYPSL)
      ENDIF
C
      END
