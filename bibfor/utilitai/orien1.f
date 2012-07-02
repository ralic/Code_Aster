      SUBROUTINE ORIEN1 ( XP, XQ, ANGL )
      IMPLICIT NONE
      REAL*8              XP(*), XQ(*) , ANGL(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C ----------------------------------------------------------------------
C     ORIENTATION D'UN AXE(XQ,XP) DEFINI PAR DEUX POINTS
C ----------------------------------------------------------------------
C IN  : XP     : EXTREMITE INITIALE DE L'AXE
C IN  : XQ     : EXTREMITE FINALE DE L'AXE
C OUT : A B G  : ANGLES D'ORIENTATION DE L'AXE
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      REAL*8 D ,PI ,R ,R8PI ,S ,T ,ZERO 

C-----------------------------------------------------------------------
      PI = R8PI()
      ZERO = 0.D0
C
      R = XQ(1) - XP(1)
      S = XQ(2) - XP(2)
      T = XQ(3) - XP(3)
      D = SQRT( R*R + S*S )
C
      ANGL(3) = ZERO
      IF ( D .NE. ZERO ) THEN
         ANGL(1) =  ATAN2(S,R)
         ANGL(2) = -ATAN2(T,D)
      ELSE
         ANGL(1) =  ZERO
         IF ( T .EQ. ZERO ) THEN
            CALL U2MESS('F','UTILITAI3_39')
         ELSEIF ( T .LT. ZERO ) THEN
           ANGL(2) =  PI / 2.D0
         ELSE
           ANGL(2) = -PI / 2.D0
        ENDIF
      ENDIF
C
      END
