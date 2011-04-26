        SUBROUTINE LCOPIL ( TYP, MOD, MATER, KOOH)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C     ----------------------------------------------------------------
C     OPERATEUR DE SOUPLESSE POUR UN COMPORTEMENT ELASTIQUE LINEAIRE
C     IN  TYP    :  TYPE OPERATEUR
C                   'ISOTROPE'
C                   'ORTHOTRO'
C                   'ANISOTRO'
C         MOD    :  MODELISATION
C         MATER  :  COEFFICIENTS MATERIAU ELASTIQUE
C     OUT KOOH   :  OPERATEUR DE SOUPLESSE ELASTIQUE LINEAIRE
C     ----------------------------------------------------------------
C
      INTEGER         NDT , NDI, I, J
      REAL*8          UN  ,  ZERO
      PARAMETER       ( UN   = 1.D0   )
      PARAMETER       ( ZERO = 0.D0   )
C
      REAL*8          KOOH(6,6)
      REAL*8          MATER(*) , E ,   NU , UNPNUE , UNSURE , MNUSE
C
      CHARACTER*8     MOD , TYP
C     ----------------------------------------------------------------
      COMMON /TDIM/   NDT  , NDI
C     ----------------------------------------------------------------

      CALL LCINMA ( ZERO , KOOH )
C
      IF ( TYP .EQ. 'ISOTROPE' ) THEN
         E  = MATER(1)
         NU = MATER(2)
         UNPNUE = ( UN + NU ) / E
         UNSURE =   UN / E
         MNUSE  = - NU / E
C
C - 3D/DP/AX/CP
C
          IF ( MOD(1:2) .EQ. '3D'     .OR.
     1         MOD(1:6) .EQ. 'D_PLAN' .OR.
     2         MOD(1:6) .EQ. 'C_PLAN' .OR.
     3         MOD(1:4) .EQ. 'AXIS'        )THEN
              DO 40 I = 1,NDI
              DO 40 J = 1,NDI
                      IF(I.EQ.J) KOOH(I,J) = UNSURE
                      IF(I.NE.J) KOOH(I,J) = MNUSE
 40           CONTINUE
              DO 45 I = NDI+1 , NDT
              DO 45 J = NDI+1 , NDT
                      IF(I.EQ.J) KOOH(I,J) = UNPNUE
 45           CONTINUE
C
C - 1D
C
          ELSE IF ( MOD(1:2) .EQ. '1D' )THEN
              KOOH(1,1) = UNSURE
          ENDIF

      ELSEIF ( TYP .EQ. 'ORTHOTRO' ) THEN

         DO 55 I=1,6
            DO 56 J=1,6
               KOOH(I,J)=MATER(36+6*(J-1)+I)
56          CONTINUE
55       CONTINUE

      ENDIF
      END
