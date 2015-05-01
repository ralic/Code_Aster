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
! aslint: disable=W1504
!
#include "asterf_types.h"
!
interface
    subroutine nttcmv(model , mate  , cara_elem, list_load, nume_dof,&
                      solver, time  , tpsthe   , tpsnp1   , reasvt  ,&
                      reasmt, creas , vtemp    , vtempm   , vec2nd  ,&
                      matass, maprec, cndirp   , cnchci   , cnchtp)
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: nume_dof
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: time
        real(kind=8) :: tpsthe(6)
        real(kind=8) :: tpsnp1
        aster_logical :: reasvt
        aster_logical :: reasmt
        character(len=1) :: creas
        character(len=24) :: vtemp
        character(len=24) :: vtempm
        character(len=24) :: vec2nd
        character(len=24) :: matass
        character(len=19) :: maprec
        character(len=24) :: cndirp
        character(len=24) :: cnchci
        character(len=24) :: cnchtp
    end subroutine nttcmv
end interface
