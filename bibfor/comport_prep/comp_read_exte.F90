subroutine comp_read_exte(rela_comp   , kit_comp      ,&
                          l_umat      , l_mfront_proto, l_mfront_offi,&
                          libr_name   , subr_name     ,&
                          keywordfact_, i_comp_       , nb_vari_umat_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/mfront_get_libname.h"
#include "asterfort/mfront_get_function.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(in) :: rela_comp
    character(len=16), intent(in) :: kit_comp(4)
    aster_logical, intent(out) :: l_umat
    aster_logical, intent(out) :: l_mfront_proto
    aster_logical, intent(out) :: l_mfront_offi
    character(len=255), intent(out) :: libr_name
    character(len=255), intent(out) :: subr_name
    character(len=16), optional, intent(in) :: keywordfact_
    integer, optional, intent(in) :: i_comp_
    integer, optional, intent(out) :: nb_vari_umat_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get parameters for external programs (MFRONT/UMAT)
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp        : RELATION comportment
! In  kit_comp         : KIT comportment
! Out l_umat           : .true. if UMAT
! Out l_mfront_proto   : .true. if MFront prototyp
! Out l_mfront_offi    : .true. if MFront official
! Out libr_name        : name of library if UMAT or MFront
! Out subr_name        : name of comportement in library if UMAT or MFront
! In  keywordfact      : factor keyword to read (COMPORTEMENT)
! In  i_comp           : factor keyword index
! Out nb_vari_umat     : number of internal variables for UMAT
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_comp_ask
    aster_logical :: l_kit_thm = .false._1
    integer :: nb_vari_umat
!
! --------------------------------------------------------------------------------------------------
!
    l_umat         = .false._1
    l_mfront_proto = .false._1
    l_mfront_offi  = .false._1
    libr_name      = ' '
    subr_name      = ' '
    nb_vari_umat   = 0
!
! - Select comportement
!
    call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
    if (l_kit_thm) then
        rela_comp_ask = kit_comp(4)
    else
        rela_comp_ask = rela_comp
    endif
!
! - Detect type
!
    call comp_meca_l(rela_comp_ask, 'UMAT'        , l_umat)
    call comp_meca_l(rela_comp_ask, 'MFRONT_OFFI' , l_mfront_offi)
    call comp_meca_l(rela_comp_ask, 'MFRONT_PROTO', l_mfront_proto)
!
! - Get parameters
!
    if (l_mfront_offi) then
        ASSERT(.not.l_kit_thm)
        call mfront_get_libname(libr_name)
        call mfront_get_function(rela_comp_ask, subr_name)
    elseif (l_mfront_proto) then
        if (present(keywordfact_)) then
            call getvtx(keywordfact_, 'LIBRAIRIE'  , iocc = i_comp_, scal = libr_name)
            call getvtx(keywordfact_, 'NOM_ROUTINE', iocc = i_comp_, scal = subr_name)
        endif
    elseif (l_umat) then
        if (present(keywordfact_)) then
            call getvtx(keywordfact_, 'LIBRAIRIE'  , iocc = i_comp_, scal = libr_name)
            call getvtx(keywordfact_, 'NOM_ROUTINE', iocc = i_comp_, scal = subr_name)
            call getvis(keywordfact_, 'NB_VARI'    , iocc = i_comp_, scal = nb_vari_umat)
        endif
    endif
!
! - Save
!
    if (present(nb_vari_umat_)) then
        nb_vari_umat_ = nb_vari_umat
    endif
!
end subroutine
