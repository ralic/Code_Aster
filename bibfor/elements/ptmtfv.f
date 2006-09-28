      SUBROUTINE PTMTFV(M,RHO,E,ROF,CE,A1,A2,AI1,AI2,XL,XIY1,XIY2,
     &                  XIZ1,XIZ2,G,ALFAY1,ALFAY2,ALFAZ1,ALFAZ2,EY,EZ,
     &                  ITYPE,ISECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER ITYPE, ISECT
      REAL*8 M(*)
      REAL*8 RHO,E,ROF,CE,A1,AI1,A2,AI2,XL,XIY1,XIY2,XIZ1,XIZ2
      REAL*8 G,ALFAY1,ALFAY2,ALFAZ1,ALFAZ2,EY,EZ
C    -------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C TOLE CRP_21
C    * CE SOUS PROGRAMME CALCULERA LA MATRICE DE MASSE DES ELEMENTS
C      DE TUYAU DROIT A SECTION VARIABLE .
C
C    * DESCRIPTION DE L'ELEMENT:
C      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
C      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
C      DEPLACEMENTS).
C      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
C    -------------------------------------------------------------------
C IN TYPE ! NOM    !          SIGNIFICATION
C IN -------------------------------------------------------------------
C IN I    ! ITYPE  ! TYPE DE VARIATION SECTION DROITE
C IN I    ! ISECT  ! TYPE DE SECTION : 0 : SECTION QUELCONQUE
C         !        !                   1 : SECTION RECTANGULAIRE
C         !        !                   2 : SECTION CIRCULAIRE
C IN R*8  ! E      ! MODULE D ELASTICITE DU MATERIAU
C IN R*8  ! RHO    ! MASSE VOLUMIQUE DU MATERIAU DU TUYAU
C IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE DANS LE TUYAU
C IN R*8  ! CE     ! CELERITE DU SON DANS LE FLUIDE
C IN R*8  ! A1     ! AIRE DE LA SECTION DROITE DE TUYAU INITIALE
C IN R*8  ! A2     ! AIRE DE LA SECTION DROITE DE TUYAU IFINALEE
C IN R*8  ! AI1    ! AIRE DE LA SECTION DE FLUIDE INITIALE
C IN R*8  ! AI2    ! AIRE DE LA SECTION DE FLUIDE FINALE
C IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
C IN R*8  ! XIY1   ! MOMENT D INERTIE / Y PRINCIPAL  SECTION INITIALE
C IN R*8  ! XIY2   ! MOMENT D INERTIE / Y PRINCIPAL  SECTION FINALE
C IN R*8  ! XIZ1   ! MOMENT D INERTIE / Z PRINCIPAL  SECTION INITIALE
C IN R*8  ! XIZ2   ! MOMENT D INERTIE / Z PRINCIPAL  SECTION FINALE
C IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
C IN R*8  ! ALFAY1 ! COEFFICIENT DE CISAILLEMENT AXE Y SECTION INITIALE
C IN R*8  ! ALFAY2 ! COEFFICIENT DE CISAILLEMENT AXE Y SECTION FINALE
C IN R*8  ! ALFAZ1 ! COEFFICIENT DE CISAILLEMENT AXE Z SECTION INITIALE
C IN R*8  ! ALFAZ2 ! COEFFICIENT DE CISAILLEMENT AXE Z SECTION FINALE
C IN R*8  ! EY     ! COMPOSANTE TG SUR Y PRINCIPAL
C IN R*8  ! EZ     ! COMPOSANTE TG SUR Z PRINCIPAL
C
C REMARQUE :
C ALFAY OU ALFAZ EST NUL ALORS ON CONSIDERE L'ELEMENT DE TYPE
C EULER-BERNOULLI (I.E.  SANS EFFORT TRANCHANT)
C SEUL LES TUYAUX A SECTION CIRCULAIRE SONT PROGRAMMES
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8  !  M    ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
C
C SOUS - PROGRAMMES APPELES
C
C     FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
C     FUN2     -
C-----------------------------------------------------------------------
C
      INTEGER IP(16)
      REAL*8  AS, SE, R1, R2, ZIAL, YIAL, XL2
      REAL*8  ASY,ASY1,ASY2,ASZ,ASZ1,ASZ2, PHIY,PHIZ, PHIY2,PHIZ2
      REAL*8  XIY,XIZ,CY,CZ,VS,VF,ROSF
      REAL*8 ZERO,R8GAEM
      REAL*8 C1 ,C2  ,C3  ,C4  ,C5  ,C6  ,C7  ,C8 ,C9 ,C10
      REAL*8        C11,C12 ,C13 ,C15 ,C24 ,C30 ,C35 ,C40,C48,C60
      REAL*8        C70,C105,C120,C140,C210,C420,PI
C
C     INITIALISATION
      ZERO =   0.D0
      PI   = R8PI()
      C1   =   1.D0
      C2   =   2.D0
      C3   =   3.D0
      C4   =   4.D0
      C5   =   5.D0
      C6   =   6.D0
      C7   =   7.D0
      C8   =   8.D0
      C9   =   9.D0
      C10  =  10.D0
      C11  =  11.D0
      C12  =  12.D0
      C13  =  13.D0
      C15  =  15.D0
      C24  =  24.D0
      C30  =  30.D0
      C35  =  35.D0
      C40  =  40.D0
      C60  =  60.D0
      C70  =  70.D0
      C105 = 105.D0
      C120 = 120.D0
      C140 = 140.D0
      C210 = 210.D0
      C420 = 420.D0
      DATA IP/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
C ---------------------------------------------------------------------
      DO 1,I=1,136
         M(I) =ZERO
    1 CONTINUE
C
C     -- SI G  ET E SONT NULS : ON FAIT G=1.
      IF (ABS(G).LT.1.D0/R8GAEM()) THEN
        IF (ABS(E).LT.1.D0/R8GAEM())  THEN
          G=1.D0
        ELSE
          CALL U2MESS('F','ELEMENTS2_54')
        END IF
      END IF
C
      IF ( ISECT .NE. 2 ) THEN
         CALL U2MESS('F','ELEMENTS2_55')
      ENDIF
      IF ( ITYPE .EQ. 2 ) THEN
         AS = ( A1 + A2 + SQRT(A1*A2) ) / C3
         SE = ( AI1 + AI2 + 2 * SQRT(AI1*AI2) ) / C4
      ELSE
         AS = ( A1 + A2) / C2
         SE = ( AI1 + AI2) / C2
      ENDIF
      R1 = SQRT (AI1 / PI)
      R2 = SQRT (AI2 / PI)
      VS = XL * (A1 + A2 + SQRT((A1+AI1)*(A2+AI2)) - SQRT(AI1*AI2)) / C3
      VF = XL * (AI1 + AI2 +SQRT(AI1*AI2)) / C3
      ROSF = RHO + ROF * VF / VS
C
      M(IP( 1)+ 1) = RHO *AS * XL / C3
      M(IP( 9)+ 1) = M(IP(1)+ 1) / C2
      M(IP( 9)+ 9) = M(IP(1)+ 1)
C
      XL2 = XL * XL
      XIY = ( XIY1 + XIY2 ) / C2
      XIZ = ( XIZ1 + XIZ2 ) / C2
C
C     CALCUL DES COEFFICIENTS INDUIT PAR LE CISAILLEMENT
      IF ( ALFAZ1 .NE. ZERO .AND. ALFAZ2 .NE. ZERO   .AND.
     &     ALFAY1 .NE. ZERO .AND. ALFAY2 .NE. ZERO )  THEN
C        1/ AIRE REDUITE EN Y
         ASY1 = A1 / ALFAZ1
         ASY2 = A2 / ALFAZ2
         CALL FUN1 ( ASY , ASY1 , ASY2 , ITYPE )
         PHIZ  = ( C12 * E * XIY ) / (G * ASY * XL2 )
C        2/ AIRE REDUITE EN Z
         ASZ1 = A1 / ALFAY1
         ASZ2 = A2 / ALFAY2
         CALL FUN1 ( ASZ , ASZ1 , ASZ2 , ITYPE )
         PHIY  = ( C12 * E * XIZ ) / (G * ASZ * XL2 )
      ELSE
C        EULER SANS INERTIE DE ROTATION
         PHIY  = ZERO
         PHIZ  = ZERO
         XIY   = ZERO
         XIZ   = ZERO
      ENDIF
      PHIY2 = PHIY * PHIY
      PHIZ2 = PHIZ * PHIZ
C
      CY = ROSF * AS *XL / ( C1 + PHIY )**2
      CZ = ROSF * AS *XL / ( C1 + PHIZ )**2
      YIAL  = XIY / ( AS * XL2 )
      ZIAL  = XIZ / ( AS * XL2 )
C
      M(IP( 2)+ 2) = CY * (C13/C35 + PHIY*C7/C10 + PHIY2/C3
     &               + ZIAL*C6/C5)
      M(IP( 3)+ 3) = CZ * (C13/C35 + PHIZ*C7/C10 + PHIZ2/C3
     &               + YIAL*C6/C5)
      M(IP( 4)+ 4) = ROSF * XL * (XIY + XIZ) / C3
      M(IP( 6)+ 2) = CY * XL * (C11/C210 + PHIY*C11/C120
     &               + PHIY2/C24 + ZIAL*(C1/C10 -PHIY/C2))
      M(IP( 5)+ 3) = -CZ * XL * (C11/C210 + PHIZ*C11/C120
     &               + PHIZ2/C24 + YIAL*(C1/C10 -PHIZ/C2))
      M(IP( 6)+ 6) = CY * XL2 * (C1/C105 + PHIY/C60 + PHIY2/C120
     &               + ZIAL*(C2/C15 + PHIY/C6 + PHIY2/C3))
      M(IP( 5)+ 5) = CZ * XL2 * (C1/C105 + PHIZ/C60 + PHIZ2/C120
     &               + YIAL*(C2/C15 + PHIZ/C6 + PHIZ2/C3))
      M(IP(10)+ 2) = CY * (C9/C70 + C3*PHIY/C10 + PHIY2/C6
     &               - ZIAL*C6/C5)
      M(IP(11)+ 3) = CZ * (C9/C70 + C3*PHIZ/C10 + PHIZ2/C6
     &               - YIAL*C6/C5)
      M(IP(10)+ 6) = CY * XL *(C13/C420 + C3*PHIY/C40 + PHIY2/C24
     &               - ZIAL*(C1/C10 -PHIY/C2))
      M(IP(13)+ 3) = CZ * XL *(C13/C420 + C3*PHIZ/C40 + PHIZ2/C24
     &               - YIAL*(C1/C10 -PHIZ/C2))
      M(IP(14)+ 6) = -CY * XL2 * (C1/C140 + PHIY/C60 + PHIY2/C120
     &               + ZIAL*(C1/C30 + PHIY/C6 - PHIY2/C6))
      M(IP(13)+ 5) = -CZ * XL2 * (C1/C140 + PHIZ/C60 + PHIZ2/C120
     &               + YIAL*(C1/C30 + PHIZ/C6 - PHIZ2/C6))
      M(IP(12)+ 4) =   M(IP( 4)+ 4) / C2
      M(IP(14)+ 2) = - M(IP(10)+ 6)
      M(IP(14)+10) = - M(IP( 6)+ 2)
      M(IP(14)+14) =   M(IP( 6)+ 6)
      M(IP(10)+10) =   M(IP( 2)+ 2)
      M(IP(11)+11) =   M(IP( 3)+ 3)
      M(IP(11)+ 5) = - M(IP(13)+ 3)
      M(IP(13)+11) = - M(IP( 5)+ 3)
      M(IP(12)+12) =   M(IP( 4)+ 4)
      M(IP(13)+13) =   M(IP( 5)+ 5)
C
      IF (EZ.NE.ZERO .OR. EY.NE.ZERO ) THEN
         M(IP( 4)+ 2) = - EZ * M(IP( 2)+ 2)
         M(IP(12)+ 2) = - EZ * M(IP(10)+ 2)
         M(IP( 4)+ 3) =   EY * M(IP( 3)+ 3)
         M(IP(12)+ 3) =   EY * M(IP(11)+ 3)
         M(IP( 4)+ 4) =   M(IP( 4)+ 4) + EZ * EZ * M(IP( 2)+ 2) +
     &                    EY * EY * M(IP( 3)+ 3)
         M(IP( 5)+ 4) =   EY * M(IP( 5)+ 3)
         M(IP( 6)+ 4) = - EZ * M(IP( 6)+ 2)
         M(IP(10)+ 4) =   M(IP(12)+ 2)
         M(IP(11)+ 4) =   M(IP(12)+ 3)
         M(IP(12)+ 4) =   M(IP(12)+ 4) + EZ * EZ * M(IP(10)+ 3) +
     &                    EY * EY * M(IP(11)+ 3)
         M(IP(13)+ 4) =   EY * M(IP(13)+ 3)
         M(IP(14)+ 4) = - EZ * M(IP(14)+ 2)
         M(IP(12)+ 5) = - M(IP(14)+ 4)
         M(IP(12)+ 6) = - M(IP(12)+ 4)
         M(IP(12)+10) =   M(IP( 4)+ 2)
         M(IP(12)+11) =   M(IP( 4)+ 3)
         M(IP(12)+12) =   M(IP( 4)+ 4)
         M(IP(13)+12) = - M(IP( 5)+ 4)
         M(IP(14)+12) = - M(IP( 6)+ 4)
      ENDIF
C
C     CONTRIBUTION DU FLUIDE
C
      M(IP( 8)+ 8) = - ROF * ( AI1 + AI2 + C4 * SE ) / ( XL * C6 )
      M(IP(16)+ 8) = - M(IP( 8)+ 8)
      M(IP(16)+16) =   M(IP( 8)+ 8)
      M(IP( 8)+ 7) =   XL * ( C9*AI1 -AI2 + C12*SE ) / ( CE*CE*C60 )
      M(IP(16)+15) =   XL * ( -AI1 + C9*AI2 + C12*SE ) / ( CE*CE*C60 )
      M(IP(16)+ 7) =   XL * ( AI1 + AI2 + C8*SE ) / ( CE*CE*C60 )
      M(IP(15)+ 8) =   M(IP(16)+ 7)
      M(IP( 8)+ 1) =   ROF* (-AI1 + PI * (R2 - R1) * (C3*R1 + R2) / C6)
      M(IP(16)+ 9) =   ROF* ( AI2 + PI * (R2 - R1) * (R1 + C3*R2) / C6)
      M(IP(16)+ 1) =   ROF* PI * ( R2 - R1 ) * ( R1 + R2) / C6
      M(IP( 9)+ 8) =   M(IP(16)+ 1)
9999  CONTINUE
      END
