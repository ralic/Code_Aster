      SUBROUTINE I3TRIP(LSTPT,NBPT)
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INCLUDE 'jeveux.h'
      INTEGER LSTPT(*),NBPT
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
