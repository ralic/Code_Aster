subroutine pipeba(ndim, mate, sup, sud, vim,&
                  dtau, copilo)
!
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
!
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/rcvalb.h"
#include "asterfort/zerop2.h"
#include "blas/ddot.h"
    integer, intent(in) :: ndim
    integer, intent(in) :: mate
    real(kind=8), intent(in) :: sup(ndim)
    real(kind=8), intent(in) :: sud(ndim)
    real(kind=8), intent(in) :: vim
    real(kind=8), intent(in) :: dtau
    real(kind=8), intent(out) :: copilo(5)
!
!-----------------------------------------------------------------------
!
! PILOTAGE PRED_ELAS POUR LES LOIS COHESIVES CZM_LIN_REG ET CZM_EXP_REG
! DE L'ELEMENT DE JOINT (2D ET 3D)
!
!-----------------------------------------------------------------------
!
    integer :: i, j, nrac, ok(4), nsol
    real(kind=8) :: p0, p1, p2, rac(2), eta(4), a0(4), a1(4), tmp
    real(kind=8) :: lc, k0, ka, kref, c, val(3), etasol(4), xn
    integer :: cod(3), kpg, spt
    character(len=16) :: nom(3) 
    character(len=8) :: fami, poum
!
!-----------------------------------------------------------------------
!
!
! INITIALISATION
!
    nom(1) = 'GC'
    nom(2) = 'SIGM_C'
    nom(3) = 'PENA_ADHERENCE'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'RUPT_FRAG', 0, ' ', [0.d0],&
                3, nom, val, cod, 2)
    lc = val(1)/val(2)
    k0 = val(1)/val(2)*val(3)
    ka = max(vim,k0)
    kref = max(lc,ka)
!
    c = dtau*kref + ka
!
    ok(1) = 0
    ok(2) = 0
    ok(3) = 0
    ok(4) = 0
!
!    RESOLUTION FEL(ETA) = DTAU
!    OU FEL(ETA) = ( SQRT(P0 + 2 P1 ETA + P2 ETA**2) - KA) / KREF
!    PORTION EN COMPRESSION : FEL = (ABS(SU(2)) - KA ) / KREF
!    ON INCLUT EGALEMENT UN SAFE-GUARD SU_N > -KREF CAR AU-DELA CE SONT
!    DES SOLUTIONS TRES FORTEMENT EN COMPRESSION QUI FONT EXPLOSER LA
!    PENALISATION
!
    p0=0.d0
    p1=0.d0
    p2=0.d0
    do i = 2, ndim
        p2 = p2 + sud(i)*sud(i)
        p1 = p1 + sud(i)*sup(i)
        p0 = p0 + sup(i)*sup(i)
    end do
!
!    PAS DE SOLUTION
    if (p2 .lt. (1.d0/r8gaem()**0.5d0)) goto 100
!
!    RECHERCHE DES SOLUTIONS
    call zerop2(2*p1/p2, (p0-c**2)/p2, rac, nrac)
    if (nrac .le. 1) goto 100
!
    xn = sup(1)+rac(2)*sud(1)
    if (xn .le. 0 .and. xn .ge. -kref) then
        ok(1) = 1
        eta(1) = rac(2)
        a1(1) = (p1+p2*eta(1))/(kref*c)
        a0(1) = dtau-eta(1)*a1(1)
    endif
!
    xn = sup(1)+rac(1)*sud(1)
    if (xn .le. 0 .and. xn .ge. -kref) then
        ok(2) = 1
        eta(2) = rac(1)
        a1(2) = (p1+p2*eta(2))/(kref*c)
        a0(2) = dtau-eta(2)*a1(2)
    endif
!
100 continue
!
!
!    PORTION EN TRACTION : FEL = (SQR(SU(1)**2 + SU(2)**2) - KA) / KREF
!
    p2 = ddot(ndim,sud,1,sud,1)
    p1 = ddot(ndim,sud,1,sup,1)
    p0 = ddot(ndim,sup,1,sup,1)
!
!    PAS DE SOLUTION
    if (p2 .lt. (1.d0/r8gaem()**0.5d0)) goto 200
!
!    RECHERCHE DES SOLUTIONS
    call zerop2(2*p1/p2, (p0-c**2)/p2, rac, nrac)
    if (nrac .le. 1) goto 200
!
    if (sup(1)+rac(2)*sud(1) .gt. 0) then
        ok(3) = 1
        eta(3) = rac(2)
        a1(3) = (p1+p2*eta(3))/(kref*c)
        a0(3) = dtau-eta(3)*a1(3)
    endif
!
    if (sup(1)+rac(1)*sud(1) .gt. 0) then
        ok(4) = 1
        eta(4) = rac(1)
        a1(4) = (p1+p2*eta(4))/(kref*c)
        a0(4) = dtau-eta(4)*a1(4)
    endif
!
200 continue
!
!
! -- CLASSEMENT DES SOLUTIONS
!
    nsol = ok(1)+ok(2)+ok(3)+ok(4)
    ASSERT(nsol.le.2)
!
    j = 0
    do i = 1, 4
        if (ok(i) .eq. 1) then
            j = j+1
            etasol(j) = eta(i)
            copilo(1 + 2*(j-1)) = a0(i)
            copilo(2 + 2*(j-1)) = a1(i)
        endif
    end do
!
!    ON RANGE LES SOLUTIONS DANS L'ORDRE CROISSANT (SI NECESSAIRE)
    if (nsol .eq. 2) then
        if (etasol(2) .lt. etasol(1)) then
            tmp = etasol(2)
            etasol(2) = etasol(1)
            etasol(1) = tmp
!
            tmp = copilo(1)
            copilo(1) = copilo(3)
            copilo(3) = tmp
!
            tmp = copilo(2)
            copilo(2) = copilo(4)
            copilo(4) = tmp
        endif
    endif
!
!
!    TRAITEMENT EN L'ABSENCE DE SOLUTION
    if (nsol .eq. 0) then
!    SI DEPLACEMENT PILOTE NUL ET SAUT EQ INFERIEUR A DTAU
!    ON IGNORE LE POINT POUR LA RESOLUTION GLOBALE
        if (p2 .le. (1.d0/r8gaem()**0.5d0) .and. (sqrt(p0)) .le. dtau) then
            copilo(1) = 0.d0
            copilo(2) = 0.d0
            copilo(3) = 0.d0
            copilo(4) = 0.d0
! DANS LES AUTRE CAS COURBE TJS SUPERIEURE A DTAU : ON PLANTE
        else
            copilo(1) = 1.d0
            copilo(5) = 1.d0
        endif
    endif
!
end subroutine
