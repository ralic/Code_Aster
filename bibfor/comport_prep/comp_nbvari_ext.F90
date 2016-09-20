subroutine comp_nbvari_ext(l_umat        , nb_vari_umat ,&
                           l_mfront_proto, l_mfront_offi,&
                           libr_name     , subr_name    ,&
                           model_dim     , model_mfront ,&
                           nb_vari_exte)
!
implicit none
!
#include "asterf_types.h"
#include "asterc/mfront_get_nbvari.h"
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    aster_logical, intent(in) :: l_umat
    integer, intent(in) :: nb_vari_umat
    aster_logical, intent(in) :: l_mfront_proto
    aster_logical, intent(in) :: l_mfront_offi
    character(len=255), intent(in) :: libr_name
    character(len=255), intent(in) :: subr_name
    integer, intent(in) :: model_dim
    character(len=16), intent(in) :: model_mfront
    integer, intent(out) :: nb_vari_exte
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Count number of internal variables for external constitutive laws (MFront and UMAT)
!
! --------------------------------------------------------------------------------------------------
!
! In  l_umat           : .true. if UMAT
! In  nb_vari_umat     : number of internal variables for UMAT
! In  l_mfront_proto   : .true. if MFront prototype
! In  l_mfront_offi    : .true. if MFront official
! In  libr_name        : name of library if UMAT or MFront
! In  subr_name        : name of comportement in library if UMAT or MFront
! In  model_dim        : dimension of modelisation (2D or 3D)
! In  model_mfront     : type of modelisation MFront
! Out nb_vari_exte     : number of internal variable if external computing for comportment
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_vari_mfront
!
! --------------------------------------------------------------------------------------------------
!
    nb_vari_exte = 0
!
! - Number of internal variables for UMAT
!
    if (l_umat) then
        nb_vari_exte = nb_vari_umat
    endif
!
! - Number of internal variables for MFront
!
    if ((l_mfront_offi .or. l_mfront_proto) .and. libr_name.ne.' ') then
        call mfront_get_nbvari(libr_name, subr_name, model_mfront, model_dim, nb_vari_mfront)
        if ( nb_vari_mfront .eq. 0 ) then
            nb_vari_mfront = 1
        endif
        nb_vari_exte = nb_vari_mfront
    endif
!
end subroutine
