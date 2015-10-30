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
#include "intdef.h"
interface
    subroutine intget(sd_int_, ip, iocc, lonvec, savejv,&
                      iscal, rscal, cscal, kscal, ivect,&
                      rvect, cvect, kvect, vi, vr,&
                      vc, vk8, vk16, vk24, address,&
                      buffer)
        character(len=*)          , intent(in) :: sd_int_
        integer                   , intent(in) :: ip
        integer,          optional, intent(in) :: iocc
        character(len=24),optional, intent(out):: savejv
        integer,          optional, intent(out):: lonvec
        integer,          optional, intent(out):: iscal
        real(kind=8),     optional, intent(out):: rscal
        complex(kind=8),  optional, intent(out):: cscal   
        character(len=*), optional, intent(out):: kscal
        integer,          optional, intent(out):: ivect(*)
        real(kind=8),     optional, intent(out):: rvect(*)
        complex(kind=8),  optional, intent(out):: cvect(*)
        character(len=*), optional, intent(out):: kvect(*)
        integer          , pointer, optional, intent(out) :: vi(:)
        real(kind=8)     , pointer, optional, intent(out) :: vr(:)
        complex(kind=8)  , pointer, optional, intent(out) :: vc(:)
        character(len=8) , pointer, optional, intent(out) :: vk8(:)
        character(len=16), pointer, optional, intent(out) :: vk16(:)
        character(len=24), pointer, optional, intent(out) :: vk24(:)
        integer                   , optional, intent(out) :: address
        integer          , pointer, optional, intent(in)  :: buffer(:)
    end subroutine intget
end interface

