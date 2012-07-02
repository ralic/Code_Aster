      SUBROUTINE I3PTRV(EPSI,LSTPT,NBPT,T,TROUVE,IPOS)
      IMPLICIT NONE
C
      INCLUDE 'jeveux.h'
      INTEGER NBPT,LSTPT(*),IPOS
      REAL*8  EPSI,T
      LOGICAL TROUVE
C
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C     TEST DE PRESENCE POINT DANS OBJ LISTE_POINT
C     ------------------------------------------------------------------
C IN  EPSI   : R : PRECISION
C IN  LSTPT  : I : OBJ LISTE_POINT
C IN  NBPT   : I : NOMBRE DE POINT TCONTENU DANS LSTPT
C IN  T      : R : VALEUR A CHERCHEE DANS LE CHAMP ABSC
C OUT TROUVE : L : REPONSE
C OUT IPOS   : L : POSITION
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
C
C
C
      INTEGER I
C
C======================================================================
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      I      = 0
      TROUVE = .FALSE.
40    CONTINUE
      IF ( (I .LT. NBPT) .AND. (.NOT. TROUVE) ) THEN
         I      = I+1
         TROUVE = ( ABS(T-ZR(LSTPT(1)-1+I)) .LE. EPSI )
         GOTO 40
      ENDIF
      IPOS = I
      END
