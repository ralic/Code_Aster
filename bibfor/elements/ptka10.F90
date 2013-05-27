subroutine ptka10(k, e, area, iy, iz,&
                  ix, g, ky, kz, r,&
                  a, ist)
    implicit    none
    real(kind=8) :: k(*), e, area, iy, iz, ix, g, ky, kz, r, a
    integer :: ist
!    -------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!              MATRICE DE RAIDEUR DES ELEMENTS COURBES
!    -------------------------------------------------------------------
!    TYPE  !   NOM  !  TABLEAU  !             SIGNIFICATION
!    -------------------------------------------------------------------
! OUT      !    K   !   (78)    ! MATRICE DE RAIDEUR ELEMENT
! IN       !    E   !    -      ! MODULE D ELASTICITE MATERIAU
! IN       !   AREA !    -      ! AIRE DE LA SECTION DROITE ELEMENT
! IN       !    IY  !    -      ! MOMENT D INERTIE / Y PRINCIPAL
! IN       !    IZ  !    -      ! MOMENT D INERTIE / Z PRINCIPAL
! IN       !    IX  !    -      ! CONSTANTE DE TORSION
! IN       !    G   !    -      ! MODULE DE CISAILLEMENT DU MATERIAU
! IN       !    KY  !    -      ! COEFFICIENT DE CISAILLEMENT AXE Y
! IN       !    KZ  !    -      ! COEFFICIENT DE CISAILLEMENT AXE Z
! IN       !    R   !    -      ! RAYON DE COURBURE ELEMENT
! IN  R8   !    A   !    -      ! ANGLE AU CENTRE DE L'ARC EN RADIANS
! IN   I   !   IST  !    -      ! TYPE DE STRUCTURE
!    -------------------------------------------------------------------
! LOC R*8  !   C    !    6      ! COEFFICIENTS DE FLEXIBILITE
    real(kind=8) :: c(6)
!
! LOC  I   !   IP   !    12     ! TABLEAU DESCRIPTEUR DE LA MATRICE
    integer :: ip(12), i
    real(kind=8) :: zero, ca
    real(kind=8) :: ea, eiz, eiy, ga, gix, rca1, r2, r3, s, s2, det
!---- ------------------------------------------------------------------
    parameter  (zero=0.d0)
    data        ip/0,1,3,6,10,15,21,28,36,45,55,66/
! --- ------------------------------------------------------------------
    do 10 i = 1, 78
        k(i) = zero
10  end do
!
!     RAPPEL :  A = ANGLE AU CENTRE
    r2 = r*r
    r3 = r2*r
    ea = e*area
    eiz= e*iz
    eiy= e*iy
    gix= g*ix
    ga = g*area
! --- ------------------------------------------------------------------
!     FLEXION DANS LE PLAN DE L ELEMENT
    s = sin(a)
    ca = cos(a)
    rca1 = r*(ca-1.d0)
    s2 = sin(2.d0*a)
!
    c(1) = r3*(6.0d0*a-8.0d0*s+s2)/(4.0d0*eiz) + r*(2.0d0*a+s2)/(4.0d0*ea) + r*(2.0d0*a-s2)*kz/(4&
           &.0d0*ga)
    c(2) = r3*(2.0d0*a-s2)/(4.0d0*eiz) + r*(2.0d0*a-s2)/(4.0d0*ea) + r*(2.0d0*a+s2)*kz/(4.0d0*ga)
    c(3) = r*a/eiz
    c(4) = r3*(2.0d0-2.0d0*ca-s*s)/(2.0d0*eiz) - r*s*s/(2.0d0*ea)+r*s*s*kz/(2.0d0*ga)
    c(5) = -r2*(a-s)/eiz
    c(6) = -r2*(1.0d0-ca)/eiz
!
    det = 2.d0*c(4)*c(5)*c(6) + c(1)*c(2)*c(3) - c(1)*c(6)*c(6) - c(3)*c(4)*c(4) - c(2)*c(5)*c(5)
!
    k(ip( 1)+ 1) = (c(2)*c(3)-c(6)*c(6))/det
    k(ip( 2)+ 1) = (c(5)*c(6)-c(3)*c(4))/det
    k(ip( 6)+ 1) = (c(4)*c(6)-c(2)*c(5))/det
    k(ip( 2)+ 2) = (c(1)*c(3)-c(5)*c(5))/det
    k(ip( 6)+ 2) = (c(5)*c(4)-c(1)*c(6))/det
    k(ip( 6)+ 6) = (c(1)*c(2)-c(4)*c(4))/det
    k(ip( 7)+ 1) = k(ip(2)+ 1)*s - k(ip(1)+ 1)*ca
    k(ip( 7)+ 2) = k(ip(2)+ 2)*s - k(ip(2)+ 1)*ca
    k(ip( 7)+ 6) = k(ip(6)+ 2)*s - k(ip(6)+ 1)*ca
    k(ip( 8)+ 1) = -k(ip(1)+ 1)*s - k(ip(2)+ 1)*ca
    k(ip( 8)+ 2) = -k(ip(2)+ 1)*s - k(ip(2)+ 2)*ca
    k(ip( 8)+ 6) = -k(ip(6)+ 1)*s - k(ip(6)+ 2)*ca
    k(ip(12)+ 1) = -k(ip(1)+1)*rca1 + k(ip(2)+1)*r*s - k(ip(6)+ 1)
    k(ip(12)+ 2) = -k(ip(2)+1)*rca1 + k(ip(2)+2)*r*s - k(ip(6)+ 2)
    k(ip(12)+ 6) = -k(ip(6)+1)*rca1 + k(ip(6)+2)*r*s - k(ip(6)+ 6)
    k(ip( 7)+ 7) = k(ip(1)+ 1)
    k(ip( 8)+ 7) = -k(ip(2)+ 1)
    k(ip( 8)+ 8) = k(ip(2)+ 2)
    k(ip(12)+ 7) = k(ip(6)+ 1)
    k(ip(12)+ 8) = -k(ip(6)+ 2)
    k(ip(12)+12) = k(ip(6)+ 6)
!
    if (ist .eq. 3) goto 99999
!
!     FLEXION PERPENDICULAIRE AU PLAN DE L ELEMENT
    c(1) = r*ky*a/ga + r3*(a/2.0d0-s2/4.0d0)/eiy + r3*(1.5d0*a-2.0d0*s+s2/4.0d0)/gix
    c(2) = r*(a/2.0d0+s2/4.0d0)/eiy+r*(a/2.0d0-s2/4.0d0)/gix
    c(3) = r*(a/2.0d0-s2/4.0d0)/eiy+r*(a/2.0d0+s2/4.0d0)/gix
    c(4) = r2*s*s/(2.0d0*eiy) +r2*(1.0d0-ca-s*s/2.0d0)/gix
    c(5) = r2*(a/2.0d0-s2/4.0d0)/eiy-r2*(s-a/2.0d0-s2/4.0d0)/gix
    c(6) = r*s*s/(2.0d0*eiy)-r*s*s/(2.0d0*gix)
!
    det = c(1)*c(2)*c(3)+ 2.0d0*c(4)*c(5)*c(6) - c(1)*c(6)*c(6) - c(3)*c(4)*c(4) - c(2)*c(5)*c(5)
!
    k(ip( 3)+ 3) = (c(2)*c(3)-c(6)*c(6))/det
    k(ip( 4)+ 3) = (c(4)*c(6)-c(2)*c(5))/det
    k(ip( 4)+ 4) = (c(1)*c(2)-c(4)*c(4))/det
    k(ip( 5)+ 3) = (c(5)*c(6)-c(3)*c(4))/det
    k(ip( 5)+ 4) = (c(5)*c(4)-c(1)*c(6))/det
    k(ip( 5)+ 5) = (c(1)*c(3)-c(5)*c(5))/det
    k(ip( 9)+ 3) = -k(ip(3)+ 3)
    k(ip( 9)+ 4) = -k(ip(4)+ 3)
    k(ip( 9)+ 5) = -k(ip(5)+ 3)
    k(ip(10)+ 3) = -k(ip(3)+3)*rca1 - k(ip(4)+3)*ca + k(ip(5)+3)*s
    k(ip(10)+ 4) = -k(ip(4)+3)*rca1 - k(ip(4)+4)*ca + k(ip(5)+4)*s
    k(ip(10)+ 5) = -k(ip(5)+3)*rca1 - k(ip(5)+4)*ca + k(ip(5)+5)*s
    k(ip(11)+ 3) = -k(ip(3)+3)*r*s - k(ip(4)+3)*s - k(ip(5)+3)*ca
    k(ip(11)+ 4) = -k(ip(4)+3)*r*s - k(ip(4)+4)*s - k(ip(5)+4)*ca
    k(ip(11)+ 5) = -k(ip(5)+3)*r*s - k(ip(5)+4)*s - k(ip(5)+5)*ca
    k(ip( 9)+ 9) = k(ip( 3)+ 3)
    k(ip(10)+ 9) = -k(ip(10)+ 3)
    k(ip(11)+ 9) = -k(ip(11)+ 3)
    k(ip(10)+10) = -k(ip(10)+3)*rca1 + k(ip(10)+5)*s - k(ip(10)+4)*ca
    k(ip(11)+10) = -k(ip(10)+3)*r*s - k(ip(10)+4)*s - k(ip(10)+5)*ca
    k(ip(11)+11) = -k(ip(11)+3)*r*s - k(ip(11)+4)*s - k(ip(11)+5)*ca
!
99999  continue
end subroutine
