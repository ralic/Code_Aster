      SUBROUTINE I3TRIP(LSTPT,NBPT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER LSTPT(*),NBPT
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     TRI DANS  OBJ DE TYPE LISTE_POINT
C     ------------------------------------------------------------------
C IN  LSTPT  : I : POINTEUR SUR LE TYPE LISTE_POINT
C IN  NBPT   : I : NOMBRE DE POINTS DANS OBJ POINTE
C     ------------------------------------------------------------------
C     STRUCT LISTE_POINT
C             ( REEL          ABSC      (NMAXPT)
C               ENTIER        FACE      (NMAXPT)
C               ENTIER        ARETE     (NMAXPT)
C              (PLANE,GAUCHE) TYPE_FACE (NMAXPT)
C               REEL          COORDO_REF(NMAXPT)
C               ENTIER        ORDRE     (NMAXPT)
C             );
C     STRUCT LISTE_POINT LSTPT;
C     ------------------------------------------------------------------
C     INVARIANT : ABSC(ORDRE(J)) < ABSC(ORDRE(J+1)) J := 2,I-1,1
C     ------------------------------------------------------------------
C
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
C
      INTEGER I,J,K,AA,AO
      REAL*8  AUX
      LOGICAL INSERT
C
C======================================================================
C
      AA = LSTPT(1)
      AO = LSTPT(6)
      DO 10, I = 1, NBPT, 1
         ZI(AO + I-1) = I
10    CONTINUE
      DO 100, I = 1, NBPT, 1
         INSERT = .FALSE.
         AUX    =  ZR(AA + I-1)
         J      =  1
110      CONTINUE
         IF ( (J .LT. I) .AND. (.NOT. INSERT) ) THEN
            IF ( ZR(AA + ZI(AO + J-1)-1) .GT. AUX ) THEN
               INSERT = .TRUE.
               DO 120, K = I, J+1, -1
                  ZI(AO + K-1) = ZI(AO + K-2)
120            CONTINUE
               ZI(AO + J-1) = I
            ENDIF
            J = J + 1
            GOTO 110
         ENDIF
         IF ( .NOT. INSERT ) THEN
            ZI(AO + I-1) = I
         ENDIF
100   CONTINUE
      END
