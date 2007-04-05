      SUBROUTINE USVECT ( COEF,ALPHAD,ALPHAM,ALPHAF,PROF,NDIM,VECT )
      IMPLICIT   NONE
      INTEGER             NDIM
      REAL*8              COEF,VECT(*), ALPHAD, ALPHAM, ALPHAF, PROF
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 06/04/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
      INTEGER      I, IND
      REAL*8       THET0, THETA, AI1, AI2, BI1, BI2, P, PM
C-----------------------------------------------------------------------
C
      IND = 0
      DO 10 I = 1 , NDIM
         IF ( VECT(2*I-1) .GE. ALPHAD ) THEN
            IND = I
            GOTO 12
         ENDIF
 10   CONTINUE
      CALL U2MESS('F','PREPOST_58')
 12   CONTINUE
C
      AI1 = PROF / ( ALPHAM - ALPHAD )
      BI1 = AI1*ALPHAD
      AI2 = PROF / ( ALPHAF - ALPHAM )
      BI2 = AI2*ALPHAM
      PM  = 0.9D0 * ( AI1*ALPHAM - BI1 )
C
      DO 20 I = IND , NDIM
C
         THETA = VECT(2*I-1)
C
         IF ( THETA .GT. ALPHAF ) GOTO 9999
C
         IF ( THETA .LT. ALPHAM ) THEN
            P = AI1*THETA - BI1
         ELSE
            P = PROF + BI2 - AI2*THETA
         ENDIF
         P = MIN ( P , PM )
         VECT(2*I) = VECT(2*I) + COEF*P
C
 20   CONTINUE
C
      DO 30 I = 1 , NDIM
C
         THET0 = VECT(2*I-1)
         THETA = 360.D0 + THET0
C
         IF ( THETA .GT. ALPHAF ) GOTO 9999
C
         IF ( THETA .LT. ALPHAM ) THEN
            P = AI1*THETA - BI1
         ELSE
            P = PROF + BI2 - AI2*THETA
         ENDIF
         P = MIN ( P , PM )
         VECT(2*I) = VECT(2*I) + COEF*P
C
 30   CONTINUE
C
 9999 CONTINUE
C
      END
