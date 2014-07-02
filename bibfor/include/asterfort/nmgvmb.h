!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
interface
    subroutine nmgvmb(ndim, nno1, nno2, npg, axi,&
                      geom, vff1, vff2, idfde1, idfde2,&
                      iw, nddl, neps, b, w,&
                      ni2ldc)
        integer :: npg
        integer :: nno2
        integer :: nno1
        integer :: ndim
        aster_logical :: axi
        real(kind=8) :: geom(ndim, nno1)
        real(kind=8) :: vff1(nno1, npg)
        real(kind=8) :: vff2(nno2, npg)
        integer :: idfde1
        integer :: idfde2
        integer :: iw
        integer :: nddl
        integer :: neps
        real(kind=8) :: b(3*ndim+2, npg, *)
        real(kind=8) :: w(npg)
        real(kind=8) :: ni2ldc(3*ndim+2)
    end subroutine nmgvmb
end interface
