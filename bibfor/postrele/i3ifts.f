      SUBROUTINE I3IFTS(EPSI,K,F,DESC,DESCTM,CONEXK,COORDO,
     &                  SGT,NBPT,LSTPT,FINK)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER K,DESC(*),DESCTM(*),CONEXK(*),NBPT,LSTPT(*),F
      REAL*8  EPSI,SGT(*),COORDO(*)
      LOGICAL FINK
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     INTERSECTION FACE TRIANGLE F SGT (AB)
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
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
      CHARACTER*32 JEXNUM,JEXNOM
C
      CHARACTER*4 TYPSL
      INTEGER     SM(3),I,J,DS1,DECF,ADESCM,ARETE,NBA,IRET,IPOS
      REAL*8      C,ZERO,UN,R,S,T,LCARA,UNSUR2,EPS
      REAL*8      A(3,3),B(3),X(3),CS(3,3),E1(3),E2(3),E3(3)
      LOGICAL     DJALA1,DJALA2
C
C======================================================================
C
      NBA    = 3
      DECF   = 8 + F
      ADESCM = DESCTM(DESC(K))
      ZERO   = 0.0D0
      UN     = 1.0D0
      UNSUR2 = 0.5D0
      C = ZERO
      DO 10, I = 1, 3, 1
         DS1   = CONEXK(ZI(ADESCM-1 + DECF + (I-1)*6))
         SM(I) = DS1
         DO 11, J = 1, 3, 1
            CS(J,I) = COORDO(3*(DS1-1) + J)
11       CONTINUE
         C = C + (SGT(I+3)-SGT(I))*(SGT(I+3)-SGT(I))
10    CONTINUE
      A(1,3) =  ZERO
      A(2,3) =  ZERO
      A(3,3) = -SQRT(C)
      C      = ZERO
      T      = ZERO
      DO 15, I = 1, 3, 1
         S     = CS(I,2) - CS(I,1)
         R     = CS(I,3) - CS(I,1)
         E1(I) = S
         E2(I) = R
         C     = C + S*S
         T     = T + R*R
15    CONTINUE
      C     = SQRT(C)
      T     = SQRT(T)
      LCARA = UNSUR2*(C + T)
      EPS   = MAX(EPSI/LCARA,EPSI)
      IF ((C.LE.ABS(CS(1,1))*EPSI) .OR. (T.LE.ABS(CS(1,1))*EPSI)) THEN
         CALL UTDEBM('F','I3IQPS','FACE DEGENEREE')
         CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
         CALL UTIMPI('S',' FACE : ',1,F)
         CALL UTFINM()
      ELSE
         C = UN/C
         T = UN/T
         DO 16, I = 1, 3, 1
            E1(I) = E1(I)*C
            E2(I) = E2(I)*T
16       CONTINUE
         E3(1) = E1(2)*E2(3) - E1(3)*E2(2)
         E3(2) = E1(3)*E2(1) - E1(1)*E2(3)
         E3(3) = E1(1)*E2(2) - E1(2)*E2(1)
         C     = ZERO
         DO 17, I = 1, 3, 1
            C = C + E3(I)*E3(I)
17       CONTINUE
         C = SQRT(C)
         IF ( C .LE. ABS(E1(1))*EPSI ) THEN
            CALL UTDEBM('F','I3IQPS','FACE DEGENEREE')
            CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
            CALL UTIMPI('S',' FACE : ',1,F)
            CALL UTFINM()
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
         ENDIF
      ENDIF
      DS1 =  3*(SM(1) - 1)
      DO 20, I = 1, 3, 1
         C    =  COORDO(DS1 + I)
         B(I) = -C
         DO 21, J = 1, 2, 1
            A(I,J) = COORDO(3*(SM(J+1)-1)+I) - C
21       CONTINUE
20    CONTINUE
      DO 30, I = 1, 3, 1
         C = ZERO
         DO 31, J = 1, 3, 1
            C = MAX(C,ABS(A(I,J)))
31       CONTINUE
         IF ( ABS(C) .GT. EPSI*COORDO(DS1+I) ) THEN
            C = UN/C
            DO 32, J = 1, 3, 1
               A(I,J) = A(I,J)*C
32          CONTINUE
            B(I) = B(I)*C
         ENDIF
30    CONTINUE
      CALL I3SL33(EPS,A,B,X,TYPSL)
      IF ( TYPSL .EQ. 'INCO' ) THEN
C        PLAN INTER DROITE = VIDE ==> FACE INTER SGT = VIDE
C        DONC ACTION = VIDE
      ELSE IF ( TYPSL .EQ. 'DETE' ) THEN
         R = X(1)
         S = X(2)
         T = X(3)
         IF ( ABS(T-UN) .LE. EPSI ) THEN
            T = UN
         ENDIF
         IF ( ABS(T) .LE. EPSI ) THEN
            T = ZERO
         ENDIF
         IF ( ABS(R) .LE. EPSI ) THEN
            R = ZERO
         ENDIF
         IF ( ABS(R-UN) .LE. EPSI ) THEN
            R = UN
         ENDIF
         IF ( ABS(S) .LE. EPSI ) THEN
            S = ZERO
         ENDIF
         IF ( ABS(S-UN) .LE. EPSI ) THEN
            S = UN
         ENDIF
         CALL I3PTRV(EPS,LSTPT,NBPT,T,DJALA1,IPOS)
         IF (  (R .LE. (UN+EPSI)) .AND. (R .GE. -EPSI) .AND.
     &         (S .LE. (UN+EPSI)) .AND. (S .GE. -EPSI) .AND.
     &         ((R+S-UN) .LE.  EPSI                   ).AND.
     &         (T .LE. (UN+EPSI)) .AND. (T .GE. -EPSI) .AND.
     &         ( .NOT. DJALA1 )                       ) THEN
            IF ( ABS(S) .LT. EPSI ) THEN
               ARETE = 1
               S     = ZERO
            ELSE IF ( ABS(R) .LT. EPSI ) THEN
               ARETE = 3
               R     = ZERO
            ELSE IF ( ABS(UN-R-S) .LT. EPSI ) THEN
               ARETE = 2
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
      ELSE IF ( TYPSL .EQ. 'INDE' ) THEN
         CALL I3IDFS(EPSI,K,F,NBA,SGT,CS,NBPT,LSTPT,FINK)
         FINK = .TRUE.
         IF ( NBPT .EQ. 0 ) THEN
            CALL I3PDM2(EPSI,E3,CS,NBA,SGT,   DJALA1)
            CALL I3PDM2(EPSI,E3,CS,NBA,SGT(4),DJALA2)
            IF ( DJALA1 .AND. DJALA2 ) THEN
               CALL I3CRTP(EPSI,CS,SGT,ZR(LSTPT(5)),IRET)
               IF ( IRET .NE. 0 ) THEN
                  CALL UTDEBM('F','I3IFTS',
     &                        'UNE FACE DEGENEREE EST DETECTEE')
                  CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
                  CALL UTIMPI('S',' FACE NUMERO : ',1,F)
                  CALL UTFINM()
               ENDIF
               ZR(LSTPT(1)) = ZERO
               ZI(LSTPT(2)) = F
               ZI(LSTPT(3)) = 0
               ZI(LSTPT(4)) = 0
               ZI(LSTPT(6)) = 1
               CALL I3CRTP(EPSI,CS,SGT(4),ZR(LSTPT(5)+2),IRET)
               IF ( IRET .NE. 0 ) THEN
                  CALL UTDEBM('F','I3IFTS',
     &                        'UNE FACE DEGENEREE EST DETECTEE')
                  CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
                  CALL UTIMPI('S',' FACE NUMERO : ',1,F)
                  CALL UTFINM()
               ENDIF
               ZR(LSTPT(1)+1) = UN
               ZI(LSTPT(2)+1) = F
               ZI(LSTPT(3)+1) = 0
               ZI(LSTPT(4)+1) = 0
               ZI(LSTPT(6)+1) = 2
               NBPT           = -2
            ENDIF
         ELSE IF ( NBPT .EQ. 1 ) THEN
            T = ZR(LSTPT(1))
            IF ( (ABS(T-UN) .LT. EPSI) .OR. (ABS(T) .LT. EPSI)  ) THEN
               IF ( ABS(T-UN) .LT. EPSI ) THEN
                  I = 1
                  J = 4
                  T = ZERO
               ELSE
                  I = 4
                  J = 1
                  T = UN
               ENDIF
               CALL I3PDM2(EPSI,E3,CS,NBA,SGT(I),DJALA1)
               IF ( DJALA1 ) THEN
                  CALL I3CRTP(EPSI,CS,SGT(J),ZR(LSTPT(5)),IRET)
                  CALL I3CRTP(EPSI,CS,SGT(I),ZR(LSTPT(5)+2),IRET)
                  IF ( IRET .EQ. -1 ) THEN
                      CALL UTDEBM('F','I3IQPS',
     &                            'UNE FACE DEGENEREE EST DETECTEE')
                      CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
                      CALL UTIMPI('S',' FACE NUMERO : ',1,F)
                      CALL UTFINM()
                  ENDIF
                  ZR(LSTPT(1)+1) = T
                  ZI(LSTPT(2)+1) = F
                  ZI(LSTPT(3)+1) = 0
                  ZI(LSTPT(4)+1) = 0
                  ZI(LSTPT(6)+1) = 2
                  NBPT           = -2
               ENDIF
            ELSE
               CALL I3PDM2(EPSI,E3,CS,NBA,SGT,DJALA1)
               IF ( DJALA1 ) THEN
                  CALL I3CRTP(EPSI,CS,SGT,ZR(LSTPT(5)+2),IRET)
                  IF ( IRET .EQ. -1 ) THEN
                     CALL UTDEBM('F','I3IQPS',
     &                               'UNE FACE DEGENEREE EST DETECTEE')
                     CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
                     CALL UTIMPI('S',' FACE NUMERO : ',1,F)
                     CALL UTFINM()
                  ENDIF
                  ZR(LSTPT(1)+1) = ZERO
                  ZI(LSTPT(2)+1) = F
                  ZI(LSTPT(3)+1) = 0
                  ZI(LSTPT(4)+1) = 0
                  ZI(LSTPT(6)+1) = 2
                  NBPT           = -2
               ELSE
                  CALL I3PDM2(EPSI,E3,CS,NBA,SGT(4),DJALA1)
                  IF ( DJALA1 ) THEN
                     CALL I3CRTP(EPSI,CS,SGT(4),ZR(LSTPT(5)+2),
     &                           IRET)
                     IF ( IRET .EQ. -1 ) THEN
                         CALL UTDEBM('F','I3IQPS',
     &                               'UNE FACE DEGENEREE EST DETECTEE')
                         CALL UTIMPI('L','MAILLE NUMERO : ',1,K)
                         CALL UTIMPI('S',' FACE NUMERO : ',1,F)
                        CALL UTFINM()
                     ENDIF
                     ZR(LSTPT(1)+1) = UN
                     ZI(LSTPT(2)+1) = F
                     ZI(LSTPT(3)+1) = 0
                     ZI(LSTPT(4)+1) = 0
                     ZI(LSTPT(6)+1) = 2
                     NBPT           = -2
                  ENDIF
               ENDIF
            ENDIF
            IF ( NBPT .NE. -2 ) THEN
               NBPT = 0
            ELSE
               R    =  ZR(LSTPT(1))
               S    =  ZR(LSTPT(1)+1)
            ENDIF
         ELSE IF ( NBPT .EQ. 2 ) THEN
            NBPT = -2
            R    =  ZR(LSTPT(1))
            S    =  ZR(LSTPT(1)+1)
         ELSE IF ( NBPT .GT. 2 ) THEN
                  CALL UTDEBM('F','I3IFTS',
     &                        'SEGMENT ET FACE COPLANAIRE, '//
     &                        'INTERSECTION : TROP DE POINT')
                  CALL UTIMPI('L','MAILLE : ',1,K)
                  CALL UTIMPI('S','FACE : ',1,F)
                  CALL UTIMPI('S','NOMBRE DE POINT : ',1,NBPT)
                  CALL UTFINM()
         ELSE
         ENDIF
      ELSE
         CALL U2MESS('F','POSTRELE_20')
      ENDIF
      END
