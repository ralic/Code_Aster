subroutine vfimat(maxdim, ndim, a, am1)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/vfcdet.h'
    integer :: maxdim, ndim
    real(kind=8) :: a(maxdim, maxdim)
    real(kind=8) :: am1(maxdim, maxdim)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! VARIABES IN MAXDIM:DIMENSION MAXIMALE
!         NDIM DIMENSION REELLE
!
!         A MATRICE A INVERSER DE DIMENSION MAXDIM*MAXDIM
! VARIABLE OUT LA MATRICE INVERSE DE A
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: deta
!
! ----------------------------------------------------------------------
!
    call assert(ndim.eq.2.or.ndim.eq.3)
!
    call vfcdet(maxdim, ndim, a, deta)
!
    if (ndim .eq. 2) then
        am1(1,1)=(1.d0/deta)* a(2,2)
        am1(1,2)=(1.d0/deta)*(-a(1,2))
        am1(2,1)=(1.d0/deta)*(-a(2,1))
        am1(2,2)=(1.d0/deta)* a(1,1)
    else if (ndim.eq.3) then
        am1(1,1)=(1.d0/deta)*( a(2,2)*a(3,3)-a(2,3)*a(3,2))
        am1(2,1)=(1.d0/deta)*(-(a(2,1)*a(3,3)-a(2,3)*a(3,1)))
        am1(3,1)=(1.d0/deta)*( a(2,1)*a(3,2)-a(2,2)*a(3,1))
        am1(1,2)=(1.d0/deta)*(-(a(1,2)*a(3,3)-a(1,3)*a(3,2)))
        am1(2,2)=(1.d0/deta)*( a(1,1)*a(3,3)-a(1,3)*a(3,1))
        am1(3,2)=(1.d0/deta)*(-(a(1,1)*a(3,2)-a(1,2)*a(3,1)))
        am1(1,3)=(1.d0/deta)*( a(1,2)*a(2,3)-a(1,3)*a(2,2))
        am1(2,3)=(1.d0/deta)*(-(a(1,1)*a(2,3)-a(1,3)*a(2,1)))
        am1(3,3)=(1.d0/deta)*( a(1,1)*a(2,2)-a(1,2)*a(2,1))
    endif
!
end subroutine
