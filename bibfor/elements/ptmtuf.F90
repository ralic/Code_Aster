subroutine ptmtuf(m, rho, e, rof, ce,&
                  a, ai, xl, xiy, xiz,&
                  g, alfay, alfaz, ey, ez)
    implicit none
    include 'asterc/r8gaem.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: m(*)
    real(kind=8) :: rho, e, rof, ce, a, ai, xl, xiy, xiz, g, alfay, alfaz, ey
    real(kind=8) :: ez
!    -------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    * CE SOUS PROGRAMME CALCULE LA MATRICE DE MASSE DES ELEMENTS DE
!      TUYAU DROIT A SECTION CONSTANTE.
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
!      DEPLACEMENTS).
!      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
!      UNICOLONNE
!    -------------------------------------------------------------------
!  DONNEES NON MODIFIEES
!
! IN TYPE ! NOM    !       SIGNIFICATION
! IN -------------------------------------------------------------------
! IN INT  ! ITYPE  ! TYPE DE LA SECTION= 0: SECTION CONSTANTE
!         !                             = 1: VARIATION LINEAIRE
!         !                             = 2: SECTION HOMOTETIQUE
! IN R*8  ! RHO    ! MASSE VOLUMIQUE DU TUYAU
! IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
! IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
! IN R*8  ! CE     ! CELERITE DU SON DANS LE FLUIDE
! IN R*8  ! A      ! AIRE DE LA SECTION DE STRUCTURE
! IN R*8  ! AI     ! AIRE DE LA SECTION DE FLUIDE
! IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
! IN R*8  ! XIY    ! MOMENT D INERTIE / Y PRINCIPAL
! IN R*8  ! XIZ    ! MOMENT D INERTIE / Z PRINCIPAL
! IN R*8  ! XJX    ! CONSTANTE DE TORSION
! IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
! IN R*8  ! ALFAY  ! COEFFICIENT DE CISAILLEMENT AXE Y (+)
! IN R*8  ! ALFAZ  ! COEFFICIENT DE CISAILLEMENT AXE Z (+)
! IN R*8  ! EY     ! TORSION /Y
! IN R*8  ! EZ     ! TORSION /Z
!
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 ! M      ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
! LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
! LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!     ------------------------------------------------------------------
    integer :: ip(16), i
    real(kind=8) :: asy, asz
    real(kind=8) :: xl2, phiy, phiz, zial, yial, cy, cz
    real(kind=8) :: zero, phiy2, phiz2
    real(kind=8) :: c1, c2, c3, c5, c6, c7, c9, c10
    real(kind=8) :: c11, c12, c13, c15, c24, c30, c35
    real(kind=8) :: c40, c60, c70, c105, c120, c140, c210, c420
!-----------------------------------------------------------------------
    data ip/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
! ---------------------------------------------------------------------
!
    zero = 0.d0
    c1 = 1.d0
    c2 = 2.d0
    c3 = 3.d0
    c5 = 5.d0
    c6 = 6.d0
    c7 = 7.d0
    c9 = 9.d0
    c10 = 10.d0
    c11 = 11.d0
    c12 = 12.d0
    c13 = 13.d0
    c15 = 15.d0
    c24 = 24.d0
    c30 = 30.d0
    c35 = 35.d0
    c40 = 40.d0
    c60 = 60.d0
    c70 = 70.d0
    c105 = 105.d0
    c120 = 120.d0
    c140 = 140.d0
    c210 = 210.d0
    c420 = 420.d0
    do 1,i=1,136
    m(i) =zero
    1 end do
!
!     -- SI G  ET E SONT NULS : ON FAIT G=1.
    if (abs(g) .lt. 1.d0/r8gaem()) then
        if (abs(e) .lt. 1.d0/r8gaem()) then
            g=1.d0
        else
            call u2mess('F', 'ELEMENTS2_54')
        endif
    endif
!
!     1/
    m(ip(1)+1)= rho * a * xl / c3
    m(ip(9)+1)= m(ip(1)+1) / c2
    m(ip(9)+9)= m(ip(1)+1)
!
!     2.1) CALCUL DES CONSTANTES
    xl2 = xl * xl
    if (alfay .ne. zero) then
        asz = a / alfay
        phiy = (c12 * e * xiz) / (g * asz * xl2)
    else
        phiy = zero
    endif
    if (alfaz .ne. zero) then
        asy = a / alfaz
        phiz = (c12 * e * xiy) / (g * asy * xl2)
    else
        phiz = zero
    endif
    phiy2 = phiy * phiy
    phiz2 = phiz * phiz
    cy = (rho * a + rof * ai) * xl / (c1 + phiy)**2
    cz = (rho * a + rof * ai) * xl / (c1 + phiz)**2
    yial = xiy / ( a * xl2 )
    zial = xiz / ( a * xl2 )
!
!     2.2) REMPLISSAGE DE LA MATRICE
!
    m(ip( 2)+ 2) = cy * (c13/c35 + phiy*c7/c10 + phiy2/c3 + zial*c6/c5)
    m(ip( 3)+ 3) = cz * (c13/c35 + phiz*c7/c10 + phiz2/c3 + yial*c6/c5)
    m(ip( 4)+ 4) = rho * xl * (xiy+xiz) /c3
    m(ip( 6)+ 2) = cy * xl * ( c11/c210 + phiy*c11/c120 + phiy2/c24 + zial*( c1/c10 - phiy/c2) )
    m(ip( 5)+ 3) = -cz * xl * ( c11/c210 + phiz*c11/c120 + phiz2/c24 + yial*( c1/c10 - phiz/c2) )
    m(ip( 6)+ 6) = cy * xl2 * (&
                   c1/c105 + phiy/c60 + phiy2/c120 + zial*( c2/c15 + phiy/c6 + phiy2/c3))
    m(ip( 5)+ 5) = cz * xl2 * (&
                   c1/c105 + phiz/c60 + phiz2/c120 + yial*( c2/c15 + phiz/c6 + phiz2/c3))
    m(ip(10)+ 2) = cy * (c9/c70 + c3*phiy/c10 + phiy2/c6 - zial*c6/c5)
    m(ip(11)+ 3) = cz * (c9/c70 + c3*phiz/c10 + phiz2/c6 - yial*c6/c5)
    m(ip(10)+ 6) = cy * xl *( c13/c420 + c3*phiy/c40 + phiy2/c24 - zial*(c1/c10 - phiy/c2) )
    m(ip(13)+ 3) = cz * xl *( c13/c420 + c3*phiz/c40 + phiz2/c24 - yial*(c1/c10 - phiz/c2) )
    m(ip(14)+ 6) = -cy * xl2 *(&
                   c1/c140 + phiy/c60 + phiy2/c120 + zial*( c1/c30 + phiy/c6 - phiy2/c6))
    m(ip(13)+ 5) = -cz * xl2 *(&
                   c1/c140 + phiz/c60 + phiz2/c120 + yial*( c1/c30 + phiz/c6 - phiz2/c6))
    m(ip(12)+ 4) = m(ip( 4)+ 4) / c2
    m(ip(14)+ 2) = - m(ip(10)+ 6)
    m(ip(14)+10) = - m(ip( 6)+ 2)
    m(ip(14)+14) = m(ip( 6)+ 6)
    m(ip(10)+10) = m(ip( 2)+ 2)
    m(ip(11)+11) = m(ip( 3)+ 3)
    m(ip(11)+ 5) = - m(ip(13)+ 3)
    m(ip(13)+11) = - m(ip( 5)+ 3)
    m(ip(12)+12) = m(ip( 4)+ 4)
    m(ip(13)+13) = m(ip( 5)+ 5)
!
    if (ez .ne. zero .or. ey .ne. zero) then
        m(ip( 4)+ 2) = - ez * m(ip( 2)+ 2)
        m(ip(12)+ 2) = - ez * m(ip(10)+ 2)
        m(ip( 4)+ 3) = ey * m(ip( 3)+ 3)
        m(ip(12)+ 3) = ey * m(ip(11)+ 3)
        m(ip( 4)+ 4) = m( ip( 4)+ 4) + ez * ez * m(ip( 2)+ 2) + ey * ey * m(ip( 3)+ 3 )
        m(ip( 5)+ 4) = ey * m(ip( 5)+ 3)
        m(ip( 6)+ 4) = - ez * m(ip( 6)+ 2)
        m(ip(10)+ 4) = m(ip(12)+ 2)
        m(ip(11)+ 4) = m(ip(12)+ 3)
        m(ip(12)+ 4) = m( ip(12)+ 4) + ez * ez * m(ip(10)+ 3) + ey * ey * m(ip(11)+ 3 )
        m(ip(13)+ 4) = ey * m(ip(13)+ 3)
        m(ip(14)+ 4) = - ez * m(ip(14)+ 2)
        m(ip(12)+ 5) = - m(ip(14)+ 4)
        m(ip(12)+ 6) = - m(ip(12)+ 4)
        m(ip(12)+10) = m(ip( 4)+ 2)
        m(ip(12)+11) = m(ip( 4)+ 3)
        m(ip(12)+12) = m(ip( 4)+ 4)
        m(ip(13)+12) = - m(ip( 5)+ 4)
        m(ip(14)+12) = - m(ip( 6)+ 4)
    endif
!     2/CONTRIBUTION DU FLUIDE
    m(ip( 8)+ 8) = - ai * rof / xl
    m(ip(16)+ 8) = - m(ip( 8)+ 8)
    m(ip(16)+16) = m(ip( 8)+ 8)
    m(ip( 8)+ 7) = xl * ai / (ce * ce * c3)
    m(ip(16)+ 7) = m(ip( 8)+ 7) / c2
    m(ip(15)+ 8) = m(ip( 8)+ 7) / c2
    m(ip(16)+15) = m(ip( 8)+ 7)
    m(ip( 8)+ 1) = - ai * rof
    m(ip(16)+ 9) = ai * rof
end subroutine
