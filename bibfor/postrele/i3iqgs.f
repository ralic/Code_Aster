      SUBROUTINE I3IQGS ( EPSI, K, F, DESC, DESCTM, CONEXK, COORDO,
     +                    SGT, ATRV, BTRV, NBPT, LSTPT, FINK, FIND )
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER K,DESC(*),DESCTM(*),CONEXK(*),NBPT,LSTPT(*),F
      REAL*8  EPSI,SGT(*),COORDO(*)
      LOGICAL FIND,ATRV,BTRV,FINK
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 17/12/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     ------------------------------------------------------------------
C     INTERSECTION FACE QUADANGLE GAUCHE F SGT (AB)
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  K      : I : -
C IN  DESC   : I :  !--> OBJ MAILLE POINTEE (ET CE QU' ELLE POINTE)
C IN  DESCTM : I : -
C IN  F      : I : NUMERO LOCALE DE LA FACE TRAITEE
C IN  CONEXK : I : CONNECTIVITE DE LA MAILLE POINTEE
C IN  COORDO : R : TABLE GLOBALE DES COORDONEES
C IN  SGT    : R : COORDONNEES DES POINTS A ET B -
C VAR ATRV   : L : INDICATEUR DE RENCONTRE DE A   !--> IE DESC_SGT
C VAR BTRV   : L : INDICATEUR DE RENCONTRE DE B  -
C VAR FIND   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU OMEGA
C VAR FINK   : L : INDICATEUR DE FIN DE REPERAGE NIVEAU MAILLE 3D
C OUT NBPT   : I : NOMBRE DE POINT TROUVE
C            :   : CONVENTION NBPT = -2 <=> CARD(INTER) = INFINI
C            :   : DANS CE CAS OUT = EXTREMITES
C OUT LSTPT  : I : OBJ LISTE_POINT
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER I,J,DS1,DECF,ADESCM,IRET,NPT,TF,IPOS1,IPOS2
      REAL*8  ZERO,UNSUR2,UN,SEUIL,LCARA,TOLE
      REAL*8  A(3,3),FK(4,3),C,R1,S1,T1,R2,S2,T2,NORMAB,X1,X2,SGTF(6)
      REAL*8  E1(3),E2(3),E3(3),CS(3,4),SGTP(6),X(3),R(3),S(3),T(3)
      REAL*8  E1I(3),E2I(3)
      LOGICAL PB,DJALA1,DJALA2
C
C======================================================================
C
      PB     = .FALSE.
      TF     =  0
      DECF   =  8 + F
      ADESCM =  DESCTM(DESC(K))
      ZERO   =  0.0D0
      UNSUR2 =  0.5D0
      SEUIL  =  0.2D0
      UN     =  1.0D0
      NORMAB =  ZERO
C
C --- RECUPERATION DES NOEUDS SOMMET DE LA FACE ET DE SES COORDONNEES
C
      DO 10, I = 1, 4, 1
         DS1     = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
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
            S1 = CS(I,J+1) - CS(I,J)
            C = C + S1*S1
 22      CONTINUE
         C = SQRT( C ) 
         LCARA = MIN ( C, LCARA ) 
 20   CONTINUE
      C = ZERO
      DO 24, I = 1, 3, 1
         S1 = CS(I,4) - CS(I,1)
         C = C + S1*S1
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
      C     = ZERO
      T1    = ZERO
      DO 15, I = 1, 3, 1
         S1    = CS(I,2) - CS(I,1)
         R1    = CS(I,4) - CS(I,1)
         E1(I) = S1
         E2(I) = R1
         C     = C  + S1*S1
         T1    = T1 + R1*R1
15    CONTINUE
      C  = SQRT(C)
      T1 = SQRT(T1)
      IF ( (C.LE.EPSI) .OR. (T1.LE.EPSI) ) THEN
         PB = .TRUE.
      ELSE
         C  = UN/C
         T1 = UN/T1
         DO 16, I = 1, 3, 1
            E1(I) = E1(I)*C
            E2(I) = E2(I)*T1
16       CONTINUE
         E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
         E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
         E3(3) = E1(1)*E2(2) - E1(2)*E2(1)
         C     = ZERO
         DO 17, I = 1, 3, 1
            C      = C + E3(I)*E3(I)
            X(I)   = CS(I,1)
            A(I,1) = SGT(I)
            A(I,2) = SGT(I+3)
17       CONTINUE
         C = SQRT(C)
         IF ( C.LE.EPSI ) THEN
            PB = .TRUE.
         ELSE
            C     =  UN/C
            E3(1) =  E3(1)*C
            E3(2) =  E3(2)*C
            E3(3) =  E3(3)*C
            E2(1) = -E1(2)*E3(3) + E1(3)*E3(2)
            E2(2) = -E1(3)*E3(1) + E1(1)*E3(3)
            E2(3) = -E1(1)*E3(2) + E1(2)*E3(1)
            C     =  UN/SQRT(E2(1)*E2(1)+E2(2)*E2(2)+E2(3)*E2(3))
            E2(1) =  E2(1)*C
            E2(2) =  E2(2)*C
            E2(3) =  E2(3)*C
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
     +         ((E1(2) .NE. ZERO) .OR. (E1(3) .NE. ZERO)) ) THEN 
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
     +         ((E1(1) .NE. ZERO) .OR. (E1(3) .NE. ZERO)) ) THEN 
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
     +         ((E1(2) .NE. ZERO) .OR. (E1(1) .NE. ZERO)) ) THEN 
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
     +         ((E2(2) .NE. ZERO) .OR. (E2(3) .NE. ZERO)) ) THEN 
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
     +         ((E2(1) .NE. ZERO) .OR. (E2(3) .NE. ZERO)) ) THEN 
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
     +         ((E2(1) .NE. ZERO) .OR. (E2(2) .NE. ZERO)) ) THEN 
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
C
            CALL I3RPQP(X,E1,E2,E3,A,2)
            CALL I3RPQP(X,E1,E2,E3,CS(1,2),3)
            DO 18, I = 1, 2, 1
               C         = A(I,2) - A(I,1)
               SGTP(I)   = A(I,1)
               SGTP(I+3) = A(I,2)
               SGTF(I)   = A(I,1)
               SGTF(I+3) = A(I,2)
               CS(I,1)   = ZERO
               NORMAB    = NORMAB + C*C
18          CONTINUE
            SGTP(3)  = ZERO
            SGTF(3)  = A(3,1)
            SGTP(6)  = ZERO
            SGTF(6)  = A(3,2)
            CS(3,1) = ZERO
            NORMAB = SQRT(NORMAB)
            CALL I3AFK2(EPSI,CS,FK,IRET)
            PB = ( IRET .EQ. -1 )
            IF ( .NOT. PB ) THEN
               IF ( NORMAB .GT. EPSI*SGT(6) ) THEN
C              /* SGT PROJETE NON REDUIT A UN POINT */
                  CALL I3QPSP(TOLE,K,F,DESC,DESCTM,SGTP,CS,A,NPT)
                  IF ( NPT .EQ. 2 ) THEN
                     R1 = A(1,1)
                     R2 = A(1,2)
                     S1 = A(2,1)
                     S2 = A(2,2)
                     T1 = A(3,1)
                     T2 = A(3,2)
                     DO 42, I = 1,2, 1
                        R(I) = A(1,I)
                        S(I) = A(2,I)
 42                  CONTINUE
                     CALL I3EFK2(FK,2,R,S,A)
                     DO 40, I = 1, 3, 1
                        A(I,3) = (A(I,1)+A(I,2))*UNSUR2
40                   CONTINUE
                     R(1) = ZERO
                     R(2) = A(3,1)
                     S(1) = ABS(T2-T1)*NORMAB
                     S(2) = A(3,2)
                     IF ( (ABS(R1-R2) .LE. EPSI)  .OR.
     +                    (ABS(S1-S2) .LE. EPSI) ) THEN
C                    /* COUPE DE LA FACE = SGT */
                        CALL I3ICFS(TOLE,A,SGTF,R,S,NPT,IRET)
                     ELSE
C                    /* COUPE DE LA FACE = ARC PARABOLE */
                        X1 = A(1,3)
                        X2 = A(2,3)
                        CALL I3CRQP(EPSI,SEUIL,CS,X1,X2,X,IRET)
                        PB = ( IRET .EQ. -1 )
                        IF ( .NOT. PB ) THEN
                           T(1) = X(2)
                           CALL I3EFK2(FK,1,X,T,A(1,3))
                           T(1) = S(1)*UNSUR2
                           T(2) = A(3,3)
                           CALL I3ICFP(EPSI,A,SGTF,R,S,T,TF,NPT,IRET)
                        ELSE
                           NPT = 0
                        ENDIF
                     ENDIF
                     IF ( IRET .EQ. -1 ) THEN
                        PB  = .TRUE.
                        NPT = 0
                     ENDIF
                     IF ( NPT .EQ. -2 ) THEN
                        FINK = .TRUE.
                        NPT  = -NPT
                     ENDIF
                     IF ( (NPT .GE. 1) .AND. (.NOT. PB)) THEN
                        C  = R(1)
                        T1 = R(2)
                        CALL I3PTRV(TOLE,LSTPT,NBPT,T1,DJALA1,IPOS1)
                        X1 =   C* (A(1,2)-A(1,1))*UNSUR2+A(1,3)
                        X1 = C*C*((A(1,2)+A(1,1))*UNSUR2-A(1,3))+X1
                        X2 =   C* (A(2,2)-A(2,1))*UNSUR2+A(2,3)
                        X2 = C*C*((A(2,2)+A(2,1))*UNSUR2-A(2,3))+X2
                        CALL I3CRQP(EPSI,SEUIL,CS,X1,X2,X,IRET)
                        R1 = X(1)
                        S1 = X(2)
                        PB = ( IRET .EQ. -1 )
                     ENDIF
                     IF ( (NPT .GE. 2) .AND. (.NOT. PB)) THEN
                        C  = S(1)
                        T2 = S(2)
                        CALL I3PTRV(TOLE,LSTPT,NBPT,T2,DJALA2,IPOS2)
                        X1 =   C* (A(1,2)-A(1,1))*UNSUR2+A(1,3)
                        X1 = C*C*((A(1,2)+A(1,1))*UNSUR2-A(1,3))+X1
                        X2 =   C* (A(2,2)-A(2,1))*UNSUR2+A(2,3)
                        X2 = C*C*((A(2,2)+A(2,1))*UNSUR2-A(2,3))+X2
                        CALL I3CRQP(EPSI,SEUIL,CS,X1,X2,X,IRET)
                        R2 = X(1)
                        S2 = X(2)
                        PB = ( IRET .EQ. -1 )
                     ENDIF
                  ELSE
                     NPT = 0
                  ENDIF
               ELSE
C              /* SGT PROJETE REDUIT A UN POINT */
                  X1 = (SGTP(4)+SGTP(1))*UNSUR2
                  X2 = (SGTP(5)+SGTP(2))*UNSUR2
                  CALL I3CRQP(EPSI,SEUIL,CS,X1,X2,X,IRET)
                  R1 = X(1)
                  S1 = X(2)
                  PB = ( IRET .EQ. -1 )
                  IF ( .NOT. PB ) THEN
                     T(1) = X(2)
                     CALL I3EFK2(FK,1,X,T,A)
                     T1 = ZERO
                     T2 = ZERO
                     DO 30, I = 1, 3, 1
                        C  = SGTF(3+I)-SGTF(I)
                        T2 = T2 + C* C
                        T1 = T1 + C*(A(I,1)-SGTF(I))
30                   CONTINUE
                     T2  = UN/SQRT(T2)
                     T1  = T1*T2
                     CALL I3PTRV(TOLE,LSTPT,NBPT,T1,DJALA1,IPOS1)
                     NPT = 1
                     TF  = 0
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
      IF ( PB ) THEN
         CALL UTDEBM('F','I3IQGS','FACE DEGENEREE')
         CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
         CALL UTIMPI('S',' FACE : ',1,F)
         CALL UTFINM()
      ENDIF
C
      IF ( FINK )  NBPT = 0
      IF ( NPT .GE. 1 ) THEN
         IF ( .NOT. DJALA1 ) THEN
            ZR(LSTPT(1) +   NBPT)     = T1
            ZI(LSTPT(2) +   NBPT)     = F
            ZI(LSTPT(3) +   NBPT)     = 0
            ZI(LSTPT(4) +   NBPT)     = TF
            ZR(LSTPT(5) + 2*NBPT+1-1) = R1
            ZR(LSTPT(5) + 2*NBPT+2-1) = S1
            ZI(LSTPT(6) +   NBPT)     = NBPT + 1
            NBPT                      = NBPT + 1
         ELSE
            ZR(LSTPT(1) +   IPOS1-1      ) = T1
            ZI(LSTPT(2) +   IPOS1-1      ) = F
            ZI(LSTPT(3) +   IPOS1-1      ) = 0
            ZI(LSTPT(4) +   IPOS1-1      ) = TF
            ZR(LSTPT(5) + 2*(IPOS1-1)+1-1) = R1
            ZR(LSTPT(5) + 2*(IPOS1-1)+2-1) = S1
         ENDIF
      ENDIF
      IF ( NPT .GE. 2 ) THEN
         IF ( .NOT. DJALA2 ) THEN
            ZR(LSTPT(1) +   NBPT)     = T2
            ZI(LSTPT(2) +   NBPT)     = F
            ZI(LSTPT(3) +   NBPT)     = 0
            ZI(LSTPT(4) +   NBPT)     = TF
            ZR(LSTPT(5) + 2*NBPT+1-1) = R2
            ZR(LSTPT(5) + 2*NBPT+2-1) = S2
            ZI(LSTPT(6) +   NBPT)     = NBPT + 1
            NBPT                      = NBPT + 1
         ELSE
            ZR(LSTPT(1) +    IPOS2-1     ) = T2
            ZI(LSTPT(2) +    IPOS2-1     ) = F
            ZI(LSTPT(3) +    IPOS2-1     ) = 0
            ZI(LSTPT(4) +    IPOS2-1     ) = TF
            ZR(LSTPT(5) + 2*(IPOS2-1)+1-1) = R2
            ZR(LSTPT(5) + 2*(IPOS2-1)+2-1) = S2
         ENDIF
      ENDIF
      IF ( FINK )  NBPT = -2
C
      END
