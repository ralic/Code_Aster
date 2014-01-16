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
! To have a syntax similar to the standard DEALLOCATE
#define AS_DEALLOCATE call as_deallocate
!
interface
    subroutine as_deallocate(vl, vi, vi4, vr, vc, vk8,&
                             vk16, vk24, vk32, vk80)
        logical, pointer, optional :: vl(:)
        integer, optional, pointer :: vi(:)
        integer(kind=4), optional, pointer :: vi4(:)
        real(kind=8), optional, pointer :: vr(:)
        complex(kind=8), optional, pointer :: vc(:)
        character(len=8), optional, pointer :: vk8(:)
        character(len=16), optional, pointer :: vk16(:)
        character(len=24), optional, pointer :: vk24(:)
        character(len=32), optional, pointer :: vk32(:)
        character(len=80), optional, pointer :: vk80(:)
    end subroutine as_deallocate
end interface
