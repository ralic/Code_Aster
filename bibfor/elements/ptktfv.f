      SUBROUTINE PTKTFV(ITYPE,SK,E,ROF,CE,A1,AI1,A2,AI2,XL,XIY1,XIY2,
     &                  XIZ1,XIZ2,XJX1,XJX2,G,ALFAY1,ALFAY2,
     &                  ALFAZ1,ALFAZ2,EY,EZ)
      IMPLICIT NONE
      INTEGER ITYPE
      REAL*8 SK(*)
      REAL*8 E,ROF,CE,A1,AI1,A2,AI2,XL,XIY1,XIY2,XIZ1,XIZ2,XJX1,XJX2
      REAL*8 G,ALFAY1,ALFAY2,ALFAZ1,ALFAZ2,EY,EZ
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
C TOLE CRP_21
C    * CE SOUS PROGRAMME CALCULERA LA MATRICE DE RAIDEUR DES ELEMENTS
C      DE TUYAU DROIT A SECTION VARIABLE .
C
C    * DESCRIPTION DE L'ELEMENT:
C      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
C      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
C      DEPLACEMENTS).
C      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
C    -------------------------------------------------------------------
C IN TYPE ! NOM    !        SIGNIFICATION
C IN -------------------------------------------------------------------
C IN  I   ! ITYPE  ! TYPE DE VARIATION SECTION DROITE
C IN      !        !    ITYPE = 1 : SECTIONS AFFINES
C IN      !        !    ITYPE = 2 : SECTIONS HOMOTHETIQUES
C IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
C IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
C IN R*8  ! C      ! CELERITE DU SON DANS LE FLUIDE
C IN R*8  ! A1     ! AIRE DE LA SECTION DROITE DE TUYAU INITIALE
C IN R*8  ! AI1    ! AIRE DE LA SECTION DE FLUIDE INITIALE
C IN R*8  ! A2     ! AIRE DE LA SECTION DROITE DE TUYAU IFINALEE
C IN R*8  ! AI2    ! AIRE DE LA SECTION DE FLUIDE FINALE
C IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
C IN R*8  ! XIY1   ! MOMENT D INERTIE / Y PRINCIPAL SECTION INITIALE
C IN R*8  ! XIY2   ! MOMENT D INERTIE / Y PRINCIPAL SECTION FINALE
C IN R*8  ! XIZ1   ! MOMENT D INERTIE / Z PRINCIPAL SECTION INITIALE
C IN R*8  ! XIZ2   ! MOMENT D INERTIE / Z PRINCIPAL SECTION FINALE
C IN R*8  ! XJX1   ! CONSTANTE DE TORSION SECTION INITIALE
C IN R*8  ! XJX2   ! CONSTANTE DE TORSION SECTION FINALE
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
C
C OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
C OUT ------------------------------------------------------------------
C OUT R*8 ! SK     ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
C
C LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
C LOC ------------------------------------------------------------------
C LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
C LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
C LOC R*8 ! PHIY   !   -     ! COEFFICIENT DU A L'EFFORT TRANCHANT SUR Z
C LOC R*8 ! PHIZ   !   -     ! COEFFICIENT DU A L'EFFORT TRANCHANT SUR Y
C LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
C     ------------------------------------------------------------------
C
C     SOUS - PROGRAMMES UTILISES
C FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
C FUN2     - MOMENTS D INERTIE EQUIVALENTS
C     ------------------------------------------------------------------
      INTEGER IP(16),I,J,K
      REAL*8 ZERO,R8GAEM
      REAL*8  C2,C4,C8,C9,C12,C60
      REAL*8  EXL,XL2,XL3, PHIY,PHIZ, ASY,ASZ
      REAL*8  AA,ASY1,ASY2,ASZ1,ASZ2, XJX, VT, Q, XKK
      REAL*8  SE, CE2
C
      ZERO =  0.D0
      C2   =  2.D0
      C4   =  4.D0
      C8   =  8.D0
      C9   =  9.D0
      C12  = 12.D0
      C60  = 60.D0
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      DATA IP/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
C ---------------------------------------------------------------------
      DO 1,I=1,136
         SK(I) = ZERO
    1 CONTINUE
C
C     -- SI G  ET E SONT NULS : K=0
      IF (ABS(G).LT.1.D0/R8GAEM()) THEN
        IF (ABS(E).LT.1.D0/R8GAEM())   GO TO 9999
        CALL U2MESS('F','ELEMENTS2_54')
      END IF
C
C     1/ TRACTION-COMPRESSION
      CALL FUN1(AA,A1,A2,ITYPE)
C
      SK(IP( 1)+ 1) =   E * AA / XL
      SK(IP( 9)+ 1) = - SK(IP(1)+1)
      SK(IP( 9)+ 9) =   SK(IP(1)+1)
C
C     2/ FLEXION
C     2.1) CALCUL DES CONSTANTES
      XL2 = XL * XL
      XL3 = XL * XL2
      EXL = E / XL
      IF ( ALFAZ1.NE.ZERO )  THEN
         ASY1 = A1 / ALFAZ1
      ELSE
         ASY1 = A1
      ENDIF
      IF ( ALFAZ2.NE.ZERO )  THEN
         ASY2 = A2 / ALFAZ2
      ELSE
         ASY2 = A2
      ENDIF
      IF ( ALFAZ1 .EQ. ZERO .AND. ALFAZ2 .EQ. ZERO ) THEN
         PHIZ = ZERO
      ELSE
         CALL FUN1(ASY,ASY1,ASY2,ITYPE)
         PHIZ = E / (G * ASY * XL2)
      ENDIF
C
      IF ( ALFAY1.NE.ZERO ) THEN
         ASZ1 = A1 / ALFAY1
      ELSE
         ASZ1 = A1
      ENDIF
      IF ( ALFAY2.NE.ZERO ) THEN
         ASZ2 = A2 / ALFAY2
      ELSE
         ASZ2 = A2
      ENDIF
      IF ( ALFAY1 .EQ. ZERO .AND. ALFAY2 .EQ. ZERO ) THEN
         PHIY = ZERO
      ELSE
         CALL FUN1(ASZ,ASZ1,ASZ2,ITYPE)
         PHIY = E / (G * ASZ * XL2)
      ENDIF
C
C     2.1) REMPLISSAGE DE LA MATRICE
C
C     2/  FLEXION DANS LE PLAN X0Y
      K = ITYPE + 2
      CALL FUN2(XIZ1,XIZ2,PHIY,XKK,Q,VT,K)
      SK(IP( 2)+ 2) =   E * XKK / XL3
      SK(IP( 6)+ 2) =   XL * Q * SK(IP( 2)+ 2)
      SK(IP(10)+ 2) = - SK(IP( 2)+ 2)
      SK(IP(14)+ 2) =   (1.D0/Q - 1.D0) * SK(IP( 6)+ 2)
      SK(IP( 6)+ 6) =   EXL * (VT + Q * Q * XKK)
      SK(IP(10)+ 6) = - SK(IP( 6)+ 2)
      SK(IP(14)+ 6) =   EXL * (XKK * Q * (1.D0- Q) - VT)
C
      CALL FUN2(XIZ2,XIZ1,PHIY,XKK,Q,VT,K)
      SK(IP(10)+10) =   SK(IP( 2)+ 2)
      SK(IP(14)+10) = - SK(IP(14)+ 2)
      SK(IP(14)+14) =   EXL * (VT + Q * Q * XKK)
C
C     3/  FLEXION DANS LE PLAN X0Z
      IF ( ITYPE .EQ. 2 ) THEN
         K = 4
      ELSE
         K = 1
      ENDIF
C
      CALL FUN2(XIY1,XIY2,PHIZ,XKK,Q,VT,K)
      SK(IP( 3)+ 3) =   E * XKK / XL3
      SK(IP( 5)+ 3) = - XL * Q * SK(IP( 3)+ 3)
      SK(IP(11)+ 3) = - SK(IP( 3)+ 3)
      SK(IP(13)+ 3) =   (1.D0/Q - 1.D0) * SK(IP( 5)+ 3)
      SK(IP( 5)+ 5) =   EXL * (VT + Q * Q * XKK)
      SK(IP(11)+ 5) = - SK(IP( 5)+ 3)
      SK(IP(13)+ 5) =   EXL * (XKK * Q * (1.D0 - Q) - VT)
C
      CALL FUN2(XIY2,XIY1,PHIZ,XKK,Q,VT,K)
      SK(IP(11)+11) =   SK(IP( 3)+ 3)
      SK(IP(13)+11) = - SK(IP(13)+ 3)
      SK(IP(13)+13) =   EXL * (VT + Q * Q * XKK)
C
C     4/  TORSION
      CALL FUN1(XJX,XJX1,XJX2,ITYPE+2)
      SK(IP( 4)+ 4) =   G * XJX / XL
      SK(IP(12)+ 4) = - SK(IP( 4)+ 4)
      SK(IP(12)+12) =   SK(IP( 4)+ 4)
C
C     5/  CAS OU IL EXISTE UNE EXCENTICITE
C
      IF ( EZ.EQ.ZERO .AND. EY.EQ. ZERO ) GOTO  10
C
C        CORRECTION DES TERMES DE TORSION
C
         SK(IP( 4)+ 4) =   SK(IP( 4)+ 4)
     &                     + EZ*EZ*SK(IP( 2)+ 2) + EY*EY*SK(IP( 3)+ 3)
         SK(IP(12)+ 4) = - SK(IP( 4)+ 4)
         SK(IP(12)+12) =   SK(IP( 4)+ 4)
         SK(IP( 4)+ 2) = - EZ * SK(IP( 2)+ 2)
         SK(IP(12)+2)  = - SK(IP( 4)+ 2)
         SK(IP( 4)+ 3) =   EY * SK(IP(3)+ 3)
         SK(IP(12)+ 3) = - SK(IP( 4)+ 3)
         SK(IP( 5)+ 4) =   EY * SK(IP( 5)+ 3)
         SK(IP( 6)+ 4) = - EZ * SK(IP( 6)+ 2)
         SK(IP(10)+ 4) =   SK(IP(12)+ 2)
         SK(IP(11)+ 4) =   SK(IP(12)+ 3)
         SK(IP(13)+ 4) =   EY * SK(IP(13)+ 3)
         SK(IP(14)+ 4) = - EZ * SK(IP(14)+ 2)
         SK(IP(12)+ 5) = - SK(IP( 5)+ 4)
         SK(IP(12)+ 6) = - SK(IP( 6)+ 4)
         SK(IP(12)+10) =   SK(IP( 4)+ 2)
         SK(IP(12)+11) =   SK(IP( 4)+ 3)
         SK(IP(13)+12) = - SK(IP(13)+ 4)
         SK(IP(14)+12) = - SK(IP(14)+ 4)
   10 CONTINUE
C
C     CONTRIBUTION DU FLUIDE
C
      CE2 = CE * CE
      IF (ITYPE . EQ . 2) THEN
         SE = (AI1 + AI2 + C2 * SQRT(AI1 * AI2)) / C4
      ELSE
         SE = (AI1 + AI2) / C2
      ENDIF
      SK(IP( 7)+ 7) =  XL * (C9*AI1 -AI2 + C12*SE) / (ROF * CE2 * C60)
      SK(IP(15)+ 7) =  XL * (AI1 +AI2 + C8*SE) / (ROF * CE2 * C60)
      SK(IP(15)+15) =  XL * (-AI1 +C9*AI2 + C12*SE) / (ROF * CE2 * C60)
9999  CONTINUE
      END
