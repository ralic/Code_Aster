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
    subroutine acyel1(nmcolz, nomobz, nobl, nobc, okpart,&
                      lilig, nblig, licol, nbcol, cmat,&
                      ndim, ideb, jdeb, x)
        integer :: ndim
        integer :: nbcol
        integer :: nblig
        character(len=*) :: nmcolz
        character(len=*) :: nomobz
        integer :: nobl
        integer :: nobc
        aster_logical :: okpart
        integer :: lilig(nblig)
        integer :: licol(nbcol)
        complex(kind=8) :: cmat(ndim, ndim)
        integer :: ideb
        integer :: jdeb
        real(kind=8) :: x
    end subroutine acyel1
end interface
