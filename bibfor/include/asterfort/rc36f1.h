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
    subroutine rc36f1(nbsigr, nocc, saltij, isk, isl,&
                      nk, nl, n0, nbp12, nbp23,&
                      nbp13, sigr, yapass, typass, nsitup)
        integer :: nbsigr
        integer :: nocc(*)
        real(kind=8) :: saltij(*)
        integer :: isk
        integer :: isl
        integer :: nk
        integer :: nl
        integer :: n0
        integer :: nbp12
        integer :: nbp23
        integer :: nbp13
        integer :: sigr(*)
        aster_logical :: yapass
        character(len=3) :: typass
        integer :: nsitup
    end subroutine rc36f1
end interface
