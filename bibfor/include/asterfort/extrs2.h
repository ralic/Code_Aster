!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine extrs2(resu0, resu1, typcon, lrest, mailla,&
                      modele, cara, chmat, nbordr, nuordr, nbacc, nomacc,&
                      nbarch, nuarch, nbexcl, chexcl, nbnosy)
        character(len=*) :: resu0
        character(len=*) :: resu1
        character(len=16) :: typcon
        aster_logical :: lrest
        character(len=8), intent(in) :: mailla
        character(len=8), intent(in) :: modele
        character(len=8), intent(in) :: cara
        character(len=8), intent(in) :: chmat
        integer :: nbordr
        integer :: nuordr(*)
        integer :: nbacc
        character(len=16) :: nomacc(*)
        integer :: nbarch
        integer :: nuarch(*)
        integer :: nbexcl
        character(len=16) :: chexcl(*)
        integer :: nbnosy
    end subroutine extrs2
end interface
