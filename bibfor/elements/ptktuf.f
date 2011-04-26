      SUBROUTINE PTKTUF(SK,E,ROF,C,A,AI,XL,XIY,XIZ,XJX,G,ALFAY,ALFAZ,
     &                  EY,EZ)
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 SK(*)
      REAL*8 E,ROF,C,A,AI,XL,XIY,XIZ,XJX,G,ALFAY,ALFAZ,EY,EZ
C    -------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    * CE SOUS PROGRAMME CALCULE LA MATRICE DE RAIDEUR DES ELEMENTS DE
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
C IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
C IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
C IN R*8  ! C      ! CELERITE DU SON DANS LE FLUIDE
C IN R*8  ! A      ! AIRE DE LA SECTION DE STRUCTURE
C IN R*8  ! AI     ! AIRE DE LA SECTION DE FLUIDE
C IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
C IN R*8  ! XIY    ! MOMENT D INERTIE / Y PRINCIPAL
C IN R*8  ! XIZ    ! MOMENT D INERTIE / Z PRINCIPAL
C IN R*8  ! XJX    ! CONSTANTE DE TORSION
C IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
C IN R*8  ! ALFAY  ! COEFFICIENT DE CISAILLEMENT AXE Y (+)
C IN R*8  ! ALFAZ  ! COEFFICIENT DE CISAILLEMENT AXE Z (+)
C IN R*8  ! EY     ! TORSION / Y
C IN R*8  ! EZ     ! TORSION / Z
C
C (+) REMARQUES :
C  -  LE COEFFICIENT DE CISAILLEMENT EST L'INVERSE DU COEFFICIENT DE
C     FORME ( IL EST DONC SUPERIEUR A 1)
C  -  SI ALFAY OU ALFAZ EST NUL ALORS ON CONSIDERE L'ELEMENT DE TYPE
C     EULER-BERNOULLI (I.E.  SANS EFFORT TRANCHANT)
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 !   SK   ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
C
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
C LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
C LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C     ------------------------------------------------------------------
      INTEGER IP(16)
      REAL*8 ZERO,R8GAEM
      REAL*8  C1,C2,C3,C4,C6,C12
      REAL*8  XL2,XL3, PHIY,PHIZ, EIY,EIZ, ASY,ASZ
C
      ZERO = 0.D0
      C1   = 1.D0
      C2   = 2.D0
      C3   = 3.D0
      C4   = 4.D0
      C6   = 6.D0
      C12  =12.D0
      DATA IP/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
C ---------------------------------------------------------------------
      DO 1,I=1,136
         SK(I) =ZERO
    1 CONTINUE
C
C     -- SI G  ET E SONT NULS : K=0
      IF (ABS(G).LT.1.D0/R8GAEM()) THEN
        IF (ABS(E).LT.1.D0/R8GAEM())   GO TO 9999
        CALL U2MESS('F','ELEMENTS2_54')
      END IF
C
C     1/ TRACTION - COMPRESSION
      SK(IP( 1)+ 1) =  E * A / XL
      SK(IP( 9)+ 1) = -SK(IP( 1)+ 1)
      SK(IP( 9)+ 9) =  SK(IP( 1)+ 1)
C
C     2/ FLEXION
C     2.1) CALCUL DES CONSTANTES
      XL2 = XL * XL
      XL3 = XL * XL2
      EIY = E * XIY
      EIZ = E * XIZ
      IF ( ALFAY.NE.ZERO ) THEN
         ASZ  = A / ALFAY
         PHIY = (C12 * EIZ) / (G * ASZ * XL2)
      ELSE
         PHIY = ZERO
      ENDIF
      IF ( ALFAZ .NE. ZERO) THEN
         ASY  = A / ALFAZ
         PHIZ = (C12 * EIY) / (G * ASY * XL2)
      ELSE
         PHIZ = ZERO
      ENDIF
C
C     2.2) REMPLISSAGE DE LA MATRICE
C     2/ FLEXION
C
C     FLEXION DANS LE PLAN XOY
      SK(IP( 2)+ 2) =  C12 * EIZ / ((C1 + PHIY) * XL3)
      SK(IP( 6)+ 2) =  C6 * EIZ / ((C1 + PHIY) * XL2)
      SK(IP(10)+ 2) = -SK(IP( 2)+ 2)
      SK(IP(14)+ 2) =  SK(IP( 6)+ 2)
      SK(IP( 6)+ 6) =  (C4 + PHIY) * EIZ / ((C1 + PHIY) * XL)
      SK(IP(10)+ 6) = -SK(IP( 6)+ 2)
      SK(IP(14)+ 6) =  (C2 - PHIY) * EIZ / ((C1 + PHIY) * XL)
      SK(IP(10)+10) =  SK(IP( 2)+ 2)
      SK(IP(14)+10) = -SK(IP( 6)+ 2)
      SK(IP(14)+14) =  SK(IP( 6)+ 6)
C
C     FLEXION DANS LE PLAN XOZ
      SK(IP( 3)+ 3) =  C12 * EIY / ((C1 + PHIZ) * XL3)
      SK(IP( 5)+ 3) = -C6 * EIY / ((C1 + PHIZ) * XL2)
      SK(IP(11)+ 3) = -SK(IP( 3)+ 3)
      SK(IP(13)+ 3) =  SK(IP( 5)+ 3)
      SK(IP( 5)+ 5) =  (C4 + PHIZ) * EIY / ((C1 + PHIZ) * XL)
      SK(IP(11)+ 5) = -SK(IP( 5)+ 3)
      SK(IP(13)+ 5) =  (C2 - PHIZ) * EIY / ((C1 + PHIZ) * XL)
      SK(IP(11)+11) =  SK(IP( 3)+ 3)
      SK(IP(13)+11) = -SK(IP( 5)+ 3)
      SK(IP(13)+13) =  SK(IP( 5)+ 5)
C
C  3/ TORSION
      SK(IP(4) + 4)= G * XJX / XL
      SK(IP(12)+ 4)=-SK(IP( 4)+ 4)
      SK(IP(12)+12)= SK(IP( 4)+ 4)
C
C  4/ CAS OU IL EXISTE UNE EXCENTRICITE
      IF (EZ.NE.ZERO . OR .EY.NE.ZERO) THEN
C        RECTIFICATION DES TERMES DE TORSION
         SK(IP(4) + 4) =  SK(IP(4)+4) + EZ * EZ * SK(IP( 2)+ 2)
     &                     + EY * EY * SK(IP( 3)+ 3)
         SK(IP(12)+ 4) = -SK(IP(4)+ 4)
         SK(IP(12)+12) =  SK(IP(4)+ 4)
C
C        TERMES INDUITS PAR L'EXCENTRICITE
         SK(IP( 4)+ 2) = -EZ * SK(IP(2)+ 2)
         SK(IP(12)+ 2) = -SK(IP(4)+ 2)
         SK(IP( 4)+ 3) =  EY * SK(IP(3)+ 3)
         SK(IP(12)+ 3) = -SK(IP( 4)+ 3)
         SK(IP( 5)+ 4) =  EY * SK(IP( 5)+ 3)
         SK(IP( 6)+ 4) = -EZ * SK(IP( 6)+ 2)
         SK(IP(10)+ 4) =  SK(IP(12)+ 2)
         SK(IP(11)+ 4) =  SK(IP(12)+ 3)
         SK(IP(13)+ 4) =  SK(IP( 5)+ 4)
         SK(IP(14)+ 4) =  SK(IP( 6)+ 4)
         SK(IP(12)+ 5) = -SK(IP( 5)+ 4)
         SK(IP(12)+ 6) = -SK(IP( 6)+ 4)
         SK(IP(12)+10) =  SK(IP( 4)+ 2)
         SK(IP(12)+11) =  SK(IP( 4)+ 3)
         SK(IP(13)+12) =  SK(IP(12)+ 5)
         SK(IP(14)+12) =  SK(IP(12)+ 6)
      ENDIF
C
C     5/ CONTRIBUTION DU FLUIDE
      SK(IP( 7)+ 7) = XL * AI / (ROF * C * C * C3)
      SK(IP(15)+15) = SK(IP(7)+ 7)
      SK(IP(15)+ 7) = SK(IP(7)+ 7) / C2
9999  CONTINUE
      END
