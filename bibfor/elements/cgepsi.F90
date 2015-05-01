subroutine cgepsi(ndim, nno1, nno2, npg, wref,&
                  vff1, dffr1,geom, tang, ddl, iu,iuc,eps)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterfort/cgcine.h"
    integer :: ndim, nno1, nno2, npg, iu(3, 3), iuc(3)
    real(kind=8) :: vff1(nno1, npg),dffr1(nno1, npg)
    real(kind=8) :: geom(ndim, nno1), wref(npg)
    real(kind=8) :: tang(*), eps(npg)
    real(kind=8) :: ddl(nno1*(ndim+1) + nno2)
! ----------------------------------------------------------------------
!
!   EPSI POUR L'ELEMENT CABLE/GAINE
!
! ----------------------------------------------------------------------

    integer :: nddl,g,n,j
    real(kind=8) :: wg, l(3), b(4, 3), courb

    nddl = nno1*(ndim+1) + nno2

    do g = 1, npg
!
!      CALCUL DES ELEMENTS GEOM DE L'EF AU POINT DE GAUSS CONSIDERE
!
        call cgcine(ndim, nno1, vff1(1, g), wref(g), dffr1(1, g),&
                    geom, tang, wg, l, b,&
                    courb)
        eps(g) = 0.d0

        do n = 1, nno1
            do j = 1, ndim
                eps(g) = eps(g) + b(j,n)*ddl(iu(j,n))
            end do
            eps(g) = eps(g) + b(ndim+1,n)*ddl(iuc(n))
        end do
    end do
!
end subroutine
