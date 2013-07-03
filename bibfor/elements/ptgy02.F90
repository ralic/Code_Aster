subroutine ptgy02(sk, nl, xnu, rho, a,&
                  xl, xiy, xiz, xjx, alfinv,&
                  ey, ez, ist)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
    real(kind=8) :: sk(*)
    real(kind=8) :: xnu, rho, a, xl, xiy, xiz, xjx, ey, ez
    integer :: nl, ist
!    -------------------------------------------------------------------
!    * CE SOUS PROGRAMME CALCULE LA MATRICE D'AMORITSSEMENT GYROSCOPIQUE
!      DE L'ELEMENT DE POUTRE DROITE A SECTION CONSTANTE.
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A SIX DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS ET 3 ROTATIONS).
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE PLEINE (ANTISYMETRIQUE)
!      UNICOLONNE
!    -------------------------------------------------------------------
!
! IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
! IN -------------------------------------------------------------------
! IN R*8  ! E      !     -   ! MODULE D'ELASTICITE DU MATERIAU
! IN  I   ! NL     !     -   ! TAILLE MATRICE DECLAREE DANS te0006.f
! IN R*8  ! RHO    !     -   ! MASSE VOLUMIQUE DU MATERIAU
! IN R*8  ! A      !     -   ! AIRE DE LA SECTION DROITE DE L'ELEMENT
! IN R*8  ! XL     !     -   ! LONGUEUR DE L ELEMENT
! IN R*8  ! XIY    !     -   ! MOMENT D INERTIE / Y PRINCIPAL
! IN R*8  ! XIZ    !     -   ! MOMENT D INERTIE / Z PRINCIPAL
! IN R*8  ! XJX    !     -   ! CONSTANTE DE TORSION
! IN R*8  ! G      !     -   ! MODULE DE CISAILLEMENT DU MATERIAU
! IN R*8  ! ALFINV  !     ! INVERSE DU COEFFICIENT DE CISAILLEMENT
! IN R*8  ! EY     !     -   ! COMPOSANTE GT SUR Y PRINCIPAL
! IN R*8  ! EZ     !     -   ! COMPOSANTE GT SUR Z PRINCIPAL
! IN  I   ! IST    !    -    ! TYPE DE STRUCTURE DE LA POUTRE
! IN
! IN (+) REMARQUES :
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 !   SK   ! (144)    ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
!
    integer :: iadzi, iazk24
    character(len=8) :: nomail
    real(kind=8) :: zero
    real(kind=8) :: phi, com
    real(kind=8) :: ip, alfinv
    integer :: i, j, ipoint, nc
!
    parameter (zero=0.d0,nc=6)
!
! ---------------------------------------------------------------------
    do 1,i = 1,nl
    sk(i) = zero
    1 end do
!
    call assert(nl.eq.144)
!
!
    if (abs(xl) .lt. r8prem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
    ip = (xiy+xiz)
    phi = 12.d0*ip*alfinv*(1.d0+xnu)/(a*xl*xl)
    com = rho * ip / (30.d0 * xl*(1.d0+phi)*(1.d0+phi))
!
!
!     I : LIGNE ; J : COLONNE
    i = 3
    j = 2
    ipoint = nc*(j) + i
    sk(ipoint) = -36.d0 * com
    i = 5
    j = 2
    ipoint = nc*(j) + i
    sk(ipoint) = (3.d0 - 15.d0 * phi) * com *xl
    i = 9
    j = 2
    ipoint = nc*(j) + i
    sk(ipoint) = 36.d0 * com
    i = 11
    j = 2
    ipoint = nc*(j) + i
    sk(ipoint) = (3.d0 - 15.d0 * phi) * com * xl
    i = 3
    j = 6
    ipoint = nc*(j) + i
    sk(ipoint) = -(3.d0 - 15.d0 * phi) * com *xl
    i = 5
    j = 6
    ipoint = nc*(j) + i
    sk(ipoint) = (4.d0+5.d0*phi+10.d0*phi*phi)*com*xl*xl
    i = 9
    j = 6
    ipoint = nc*(j) + i
    sk(ipoint) = (3.d0 - 15.d0 * phi) * com * xl
    i = 11
    j = 6
    ipoint = nc*(j) + i
    sk(ipoint) = -(1.d0+5.d0*phi-5.d0*phi*phi)*com*xl*xl
    i = 3
    j = 8
    ipoint = nc*(j) + i
    sk(ipoint) = 36.d0 * com
    i = 5
    j = 8
    ipoint = nc*(j) + i
    sk(ipoint) = -(3.d0 - 15.d0 * phi) * com * xl
    i = 9
    j = 8
    ipoint = nc*(j) + i
    sk(ipoint) = -36.d0 * com
    i = 11
    j = 8
    ipoint = nc*(j) + i
    sk(ipoint) = -(3.d0 - 15.d0 * phi) * com * xl
    i = 3
    j = 12
    ipoint = nc*(j) + i
    sk(ipoint) = -(3.d0 - 15.d0 * phi) * com * xl
    i = 5
    j = 12
    ipoint = nc*(j) + i
    sk(ipoint) = -(1.d0+5.d0*phi-5.d0*phi*phi)*com*xl*xl
    i = 9
    j = 12
    ipoint = nc*(j) + i
    sk(ipoint) = (3.d0 - 15.d0 * phi) * com * xl
    i = 11
    j = 12
    ipoint = nc*(j) + i
    sk(ipoint) = (4.d0+5.d0*phi+10.d0*phi*phi)*com*xl*xl
!
!
end subroutine
