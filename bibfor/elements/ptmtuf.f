      SUBROUTINE PTMTUF(M,RHO,E,ROF,CE,A,AI,XL,XIY,XIZ,G,ALFAY,ALFAZ,
     &                  EY,EZ)
      IMPLICIT NONE
      REAL*8  M(*)
      REAL*8  RHO,E,ROF,CE,A,AI,XL,XIY,XIZ,G,ALFAY,ALFAZ,EY,EZ
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
C    * CE SOUS PROGRAMME CALCULE LA MATRICE DE MASSE DES ELEMENTS DE
C      TUYAU DROIT A SECTION CONSTANTE.
C
C    * DESCRIPTION DE L'ELEMENT:
C      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
C      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
C      DEPLACEMENTS).
C      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
C
C    * REMARQUE :
C      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
C      UNICOLONNE
C    -------------------------------------------------------------------
C  DONNEES NON MODIFIEES
C
C IN TYPE ! NOM    !       SIGNIFICATION
C IN -------------------------------------------------------------------
C IN INT  ! ITYPE  ! TYPE DE LA SECTION= 0: SECTION CONSTANTE
C         !                             = 1: VARIATION LINEAIRE
C         !                             = 2: SECTION HOMOTETIQUE
C IN R*8  ! RHO    ! MASSE VOLUMIQUE DU TUYAU
C IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
C IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
C IN R*8  ! CE     ! CELERITE DU SON DANS LE FLUIDE
C IN R*8  ! A      ! AIRE DE LA SECTION DE STRUCTURE
C IN R*8  ! AI     ! AIRE DE LA SECTION DE FLUIDE
C IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
C IN R*8  ! XIY    ! MOMENT D INERTIE / Y PRINCIPAL
C IN R*8  ! XIZ    ! MOMENT D INERTIE / Z PRINCIPAL
C IN R*8  ! XJX    ! CONSTANTE DE TORSION
C IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
C IN R*8  ! ALFAY  ! COEFFICIENT DE CISAILLEMENT AXE Y (+)
C IN R*8  ! ALFAZ  ! COEFFICIENT DE CISAILLEMENT AXE Z (+)
C IN R*8  ! EY     ! TORSION /Y
C IN R*8  ! EZ     ! TORSION /Z
C
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 ! M      ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
C
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
C LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
C LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C     ------------------------------------------------------------------
      INTEGER IP(16),I
      REAL*8  ASY,ASZ
      REAL*8  XL2,PHIY,PHIZ,ZIAL,YIAL,CY,CZ
      REAL*8 ZERO,R8GAEM,PHIY2,PHIZ2
      REAL*8 C1 ,C2 ,C3  ,C5  ,C6  ,C7  ,C9 ,C10
      REAL*8       C11,C12,C13 ,C15 ,C24 ,C30 ,C35
      REAL*8       C40,C60,C70,C105,C120,C140,C210,C420
C-----------------------------------------------------------------------
      DATA IP/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
C ---------------------------------------------------------------------
C
      ZERO =   0.D0
      C1   =   1.D0
      C2   =   2.D0
      C3   =   3.D0
      C5   =   5.D0
      C6   =   6.D0
      C7   =   7.D0
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
C     1/
      M(IP(1)+1)= RHO * A * XL / C3
      M(IP(9)+1)= M(IP(1)+1) / C2
      M(IP(9)+9)= M(IP(1)+1)
C
C     2.1) CALCUL DES CONSTANTES
      XL2 = XL * XL
      IF ( ALFAY.NE.ZERO ) THEN
         ASZ  = A  / ALFAY
         PHIY = (C12 * E * XIZ) / (G * ASZ * XL2)
      ELSE
         PHIY = ZERO
      ENDIF
      IF ( ALFAZ.NE.ZERO ) THEN
         ASY  = A  / ALFAZ
         PHIZ = (C12 * E * XIY) / (G * ASY * XL2)
      ELSE
         PHIZ = ZERO
      ENDIF
      PHIY2 = PHIY * PHIY
      PHIZ2 = PHIZ * PHIZ
      CY    = (RHO * A + ROF * AI) * XL / (C1 + PHIY)**2
      CZ    = (RHO * A + ROF * AI) * XL / (C1 + PHIZ)**2
      YIAL  = XIY / ( A * XL2 )
      ZIAL  = XIZ / ( A * XL2 )
C
C     2.2) REMPLISSAGE DE LA MATRICE
C
      M(IP( 2)+ 2) = CY * (C13/C35 + PHIY*C7/C10 + PHIY2/C3
     &               + ZIAL*C6/C5)
      M(IP( 3)+ 3) = CZ * (C13/C35 + PHIZ*C7/C10 + PHIZ2/C3
     &               + YIAL*C6/C5)
      M(IP( 4)+ 4) = RHO * XL * (XIY+XIZ) /C3
      M(IP( 6)+ 2) = CY * XL * (C11/C210 + PHIY*C11/C120
     &               + PHIY2/C24 + ZIAL*(C1/C10 - PHIY/C2))
      M(IP( 5)+ 3) = -CZ * XL * (C11/C210 + PHIZ*C11/C120
     &               + PHIZ2/C24 + YIAL*(C1/C10 - PHIZ/C2))
      M(IP( 6)+ 6) = CY * XL2 * (C1/C105 + PHIY/C60 + PHIY2/C120
     &               + ZIAL*(C2/C15 + PHIY/C6 + PHIY2/C3))
      M(IP( 5)+ 5) = CZ * XL2 * (C1/C105 + PHIZ/C60 + PHIZ2/C120
     &               + YIAL*(C2/C15 + PHIZ/C6 + PHIZ2/C3))
      M(IP(10)+ 2) = CY * (C9/C70 + C3*PHIY/C10 + PHIY2/C6
     &               - ZIAL*C6/C5)
      M(IP(11)+ 3) = CZ * (C9/C70 + C3*PHIZ/C10 + PHIZ2/C6
     &               - YIAL*C6/C5)
      M(IP(10)+ 6) = CY * XL *(C13/C420 + C3*PHIY/C40 + PHIY2/C24
     &              - ZIAL*(C1/C10 - PHIY/C2))
      M(IP(13)+ 3) = CZ * XL *(C13/C420 + C3*PHIZ/C40 + PHIZ2/C24
     &               - YIAL*(C1/C10 - PHIZ/C2))
      M(IP(14)+ 6) = -CY * XL2 *(C1/C140 + PHIY/C60 + PHIY2/C120
     &               + ZIAL*(C1/C30 + PHIY/C6 - PHIY2/C6))
      M(IP(13)+ 5) = -CZ * XL2 *(C1/C140 + PHIZ/C60 + PHIZ2/C120
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
      IF (EZ.NE.ZERO .OR. EY.NE.ZERO) THEN
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
C     2/CONTRIBUTION DU FLUIDE
      M(IP( 8)+ 8) = - AI * ROF / XL
      M(IP(16)+ 8) = - M(IP( 8)+ 8)
      M(IP(16)+16) =   M(IP( 8)+ 8)
      M(IP( 8)+ 7) =   XL * AI / (CE * CE * C3)
      M(IP(16)+ 7) =   M(IP( 8)+ 7) / C2
      M(IP(15)+ 8) =   M(IP( 8)+ 7) / C2
      M(IP(16)+15) =   M(IP( 8)+ 7)
      M(IP( 8)+ 1) = - AI * ROF
      M(IP(16)+ 9) =   AI * ROF
      END
