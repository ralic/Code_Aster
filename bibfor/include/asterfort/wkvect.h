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
          interface
            subroutine wkvect(nom,carac,dim,jadr,vl,vi,vi4,vr,vc,vk8,   &
     &vk16,vk24,vk32,vk80)
#include "asterf_types.h"
    character(len=*), intent(in) :: nom
    character(len=*), intent(in) :: carac
    integer, intent(in) :: dim
    integer, intent(out), optional :: jadr

    aster_logical,           pointer, optional, intent(out) :: vl(:)
    integer,           pointer, optional, intent(out) :: vi(:)
    integer(kind=4),   pointer, optional, intent(out) :: vi4(:)
    real(kind=8),      pointer, optional, intent(out) :: vr(:)
    complex(kind=8),   pointer, optional, intent(out) :: vc(:)
    character(len=8),  pointer, optional, intent(out) :: vk8(:)
    character(len=16), pointer, optional, intent(out) :: vk16(:)
    character(len=24), pointer, optional, intent(out) :: vk24(:)
    character(len=32), pointer, optional, intent(out) :: vk32(:)
    character(len=80), pointer, optional, intent(out) :: vk80(:)

            end subroutine wkvect
          end interface
