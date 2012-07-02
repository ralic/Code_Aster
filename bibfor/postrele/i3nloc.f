      SUBROUTINE I3NLOC(T1,T2,N1,N2,T3)
      IMPLICIT NONE
C
      INTEGER T1(*),T2(*),T3(*)
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
C     NUMEROTATION LOCALE DE NOEUD DANS UNE FACE
C     ------------------------------------------------------------------
C IN  T1     : I : TABLE(1..N) D' ENTIERS
C IN  T2     : I : TABLE(1..N) D' ENTIERS
C IN  N1     : I : DIMENSION DE T1
C IN  N2     : I : DIMENSION DE T2
C OUT T3     : I : TABLE(1..N1) D' ENTIERS
C            :   : T3(I) = J  J>0 => T1(I) = T2(J)
C            :   :            J=0 => T1(I) NON DANS T2
C     ------------------------------------------------------------------
      INTEGER I,J,I1
      LOGICAL TROUVE
      INTEGER N1 ,N2
C-----------------------------------------------------------------------
C
      DO 100, I = 1, N1, 1
         I1     =  T1(I)
         J      =  1
         TROUVE = .FALSE.
10       CONTINUE
         IF ( (.NOT. TROUVE) .AND. (J .LE. N2) ) THEN
            IF ( T2(J) .EQ. I1 ) THEN
               TROUVE = .TRUE.
            ELSE
               J = J + 1
            ENDIF
            GOTO 10
         ENDIF
         IF ( .NOT. TROUVE ) THEN
            J = 0
         ENDIF
         T3(I) = J
100   CONTINUE
      END
