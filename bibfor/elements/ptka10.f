      SUBROUTINE PTKA10(K,   E,AREA,IY,IZ,IX,G,KY,KZ,R,A,IST)
      IMPLICIT NONE
      REAL*8            K(*),E,AREA,IY,IZ,IX,G,KY,KZ,R,A
      INTEGER                                            IST
C    -------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C              MATRICE DE RAIDEUR DES ELEMENTS COURBES
C    -------------------------------------------------------------------
C    TYPE  !   NOM  !  TABLEAU  !             SIGNIFICATION
C    -------------------------------------------------------------------
C OUT      !    K   !   (78)    ! MATRICE DE RAIDEUR ELEMENT
C IN       !    E   !    -      ! MODULE D ELASTICITE MATERIAU
C IN       !   AREA !    -      ! AIRE DE LA SECTION DROITE ELEMENT
C IN       !    IY  !    -      ! MOMENT D INERTIE / Y PRINCIPAL
C IN       !    IZ  !    -      ! MOMENT D INERTIE / Z PRINCIPAL
C IN       !    IX  !    -      ! CONSTANTE DE TORSION
C IN       !    G   !    -      ! MODULE DE CISAILLEMENT DU MATERIAU
C IN       !    KY  !    -      ! COEFFICIENT DE CISAILLEMENT AXE Y
C IN       !    KZ  !    -      ! COEFFICIENT DE CISAILLEMENT AXE Z
C IN       !    R   !    -      ! RAYON DE COURBURE ELEMENT
C IN  R8   !    A   !    -      ! ANGLE AU CENTRE DE L'ARC EN RADIANS
C IN   I   !   IST  !    -      ! TYPE DE STRUCTURE
C    -------------------------------------------------------------------
C LOC R*8  !   C    !    6      ! COEFFICIENTS DE FLEXIBILITE
      REAL*8 C(6)
C
C LOC  I   !   IP   !    12     ! TABLEAU DESCRIPTEUR DE LA MATRICE
      INTEGER IP(12)
      REAL*8     ZERO
      REAL*8  CA
      REAL*8  EA,EIZ,EIY,GA,GIX, RCA1,R2,R3,S,S2,DET
C
C
C-----------------------------------------------------------------------
      INTEGER I 
C-----------------------------------------------------------------------
      PARAMETER (ZERO=0.D0)
      DATA   IP/0,1,3,6,10,15,21,28,36,45,55,66/
C     ------------------------------------------------------------------
      DO 10 I=1,78
        K(I) = ZERO
10    CONTINUE
C
C     RAPPEL :  A = ANGLE AU CENTRE
      R2=R*R
      R3=R2*R
      EA=E*AREA
      EIZ=E*IZ
      EIY=E*IY
      GIX=G*IX
      GA=G*AREA
C     ------------------------------------------------------------------
C     FLEXION DANS LE PLAN DE L ELEMENT
      S    = SIN(A)
      CA   = COS(A)
      RCA1 = R*(CA-1.D0)
      S2   = SIN(2.D0*A)
C
      C (1) = R3/(4.0D0*EIZ)*(6.0D0*A-8.0D0*S+S2)+R/(4.0D0*EA)*(2.00D0
     &                     *A+S2)+R/(4.0D0*GA)*KZ*(2.0D0*A-S2)
      C (2) = R3/(4.0D0*EIZ)*(2.0D0*A-S2)+R/(4.0D0*EA)*(2.0D0*A-S2)
     &                      +R*KZ/(4.0D0*GA)*(2.0D0*A+S2)
      C (3) = R*A/EIZ
      C (4) = R3/(2.0D0*EIZ)*(2.0D0-2.0D0*CA-S*S)-R/(2.0D0*EA)*S*S
     &              +R*KZ/(2.0D0*GA)*S*S
      C (5) = - R2/EIZ*(A-S)
      C (6) = - R2/EIZ*(1.0D0-CA)
C
C
      DET = 2.D0*C(4)*C(5)*C(6) + C(1)*C(2)*C(3) - C(1)*C(6)*C(6)
     1      - C(3)*C(4)*C(4) - C(2)*C(5)*C(5)
C
      K(IP( 1)+ 1) =   (C(2)*C(3)-C(6)*C(6)) / DET
      K(IP( 2)+ 1) =   (C(5)*C(6)-C(3)*C(4)) / DET
      K(IP( 6)+ 1) =   (C(4)*C(6)-C(2)*C(5)) / DET
      K(IP( 2)+ 2) =   (C(1)*C(3)-C(5)*C(5)) / DET
      K(IP( 6)+ 2) =   (C(5)*C(4)-C(1)*C(6)) / DET
      K(IP( 6)+ 6) =   (C(1)*C(2)-C(4)*C(4)) / DET
      K(IP( 7)+ 1) =  K(IP( 2)+ 1)*S - K(IP( 1)+ 1)*CA
      K(IP( 7)+ 2) =  K(IP( 2)+ 2)*S - K(IP( 2)+ 1)*CA
      K(IP( 7)+ 6) =  K(IP( 6)+ 2)*S - K(IP( 6)+ 1)*CA
      K(IP( 8)+ 1) = -K(IP( 1)+ 1)*S - K(IP( 2)+ 1)*CA
      K(IP( 8)+ 2) = -K(IP( 2)+ 1)*S - K(IP( 2)+ 2)*CA
      K(IP( 8)+ 6) = -K(IP( 6)+ 1)*S - K(IP( 6)+ 2)*CA
      K(IP(12)+ 1) = -K(IP(1)+1)*RCA1 + K(IP(2)+1)*R*S - K(IP(6)+ 1)
      K(IP(12)+ 2) = -K(IP(2)+1)*RCA1 + K(IP(2)+2)*R*S - K(IP(6)+ 2)
      K(IP(12)+ 6) = -K(IP(6)+1)*RCA1 + K(IP(6)+2)*R*S - K(IP(6)+ 6)
      K(IP( 7)+ 7) =  K(IP( 1)+ 1)
      K(IP( 8)+ 7) = -K(IP( 2)+ 1)
      K(IP( 8)+ 8) =  K(IP( 2)+ 2)
      K(IP(12)+ 7) =  K(IP( 6)+ 1)
      K(IP(12)+ 8) = -K(IP( 6)+ 2)
      K(IP(12)+12) =  K(IP( 6)+ 6)
C
      IF(IST.EQ.3) GOTO 99999
C
C     FLEXION PERPENDICULAIRE AU PLAN DE L ELEMENT
      C(1) =  R*KY*A/GA + R3/EIY*(A/2.0D0-S2/4.0D0)+R3/GIX
     1           *(1.5D0*A-2.0D0*S+S2/4.0D0)
      C(2) =  R/EIY*(A/2.0D0+S2/4.0D0)+R/GIX*(A/2.0D0-S2/4.0D0)
      C(3) =  R/EIY*(A/2.0D0-S2/4.0D0)+R/GIX*(A/2.0D0+S2/4.0D0)
      C(4) =  R2*S*S/(2.0D0*EIY)     +R2/GIX*(1.0D0-CA-S*S/2.0D0)
      C(5) =  R2/EIY*(A/2.0D0-S2/4.0D0)-R2/GIX*(S-A/2.0D0-S2/4.0D0)
      C(6) =  R*S*S/(2.0D0*EIY)-R*S*S/(2.0D0*GIX)
C
C
      DET = C(1)*C(2)*C(3)+ 2.D0*C(4)*C(5)*C(6) - C(1)*C(6)*C(6)
     1     -C(3)*C(4)*C(4) - C(2)*C(5)*C(5)
C
      K(IP( 3)+ 3) = (C(2)*C(3)-C(6)*C(6))/DET
      K(IP( 4)+ 3) = (C(4)*C(6)-C(2)*C(5))/DET
      K(IP( 4)+ 4) = (C(1)*C(2)-C(4)*C(4))/DET
      K(IP( 5)+ 3) = (C(5)*C(6)-C(3)*C(4))/DET
      K(IP( 5)+ 4) = (C(5)*C(4)-C(1)*C(6))/DET
      K(IP( 5)+ 5) = (C(1)*C(3)-C(5)*C(5))/DET
      K(IP( 9)+ 3) = -K(IP( 3)+ 3)
      K(IP( 9)+ 4) = -K(IP( 4)+ 3)
      K(IP( 9)+ 5) = -K(IP( 5)+ 3)
      K(IP(10)+ 3) = -K(IP(3)+3)*RCA1 - K(IP(4)+3)*CA + K(IP(5)+3)*S
      K(IP(10)+ 4) = -K(IP(4)+3)*RCA1 - K(IP(4)+4)*CA + K(IP(5)+4)*S
      K(IP(10)+ 5) = -K(IP(5)+3)*RCA1 - K(IP(5)+4)*CA + K(IP(5)+5)*S
      K(IP(11)+ 3) = -K(IP(3)+3)*R*S - K(IP(4)+3)*S - K(IP(5)+3)*CA
      K(IP(11)+ 4) = -K(IP(4)+3)*R*S - K(IP(4)+4)*S - K(IP(5)+4)*CA
      K(IP(11)+ 5) = -K(IP(5)+3)*R*S - K(IP(5)+4)*S - K(IP(5)+5)*CA
      K(IP( 9)+ 9) =  K(IP( 3)+ 3)
      K(IP(10)+ 9) = -K(IP(10)+ 3)
      K(IP(11)+ 9) = -K(IP(11)+ 3)
      K(IP(10)+10) = -K(IP(10)+3)*RCA1 + K(IP(10)+5)*S - K(IP(10)+4)*CA
      K(IP(11)+10) = -K(IP(10)+3)*R*S  - K(IP(10)+4)*S - K(IP(10)+5)*CA
      K(IP(11)+11) = -K(IP(11)+3)*R*S  - K(IP(11)+4)*S - K(IP(11)+5)*CA
C
99999 CONTINUE
      END
