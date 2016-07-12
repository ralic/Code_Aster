subroutine comp_read_exte(rela_comp_  , kit_comp ,&
                          l_umat      , l_mfront , l_mfront_offi,&
                          libr_name   , subr_name,&
                          keywordfact_, iocc_    )
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=16), intent(in) :: rela_comp_
    character(len=16), intent(in) :: kit_comp(4)
    aster_logical, intent(out) :: l_umat
    aster_logical, intent(out) :: l_mfront
    aster_logical, intent(out) :: l_mfront_offi
    character(len=255), intent(out) :: libr_name
    character(len=255), intent(out) :: subr_name
    character(len=16), optional, intent(in) :: keywordfact_
    integer, optional, intent(in) :: iocc_
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
! Out l_mfront         : .true. if MFront
! Out l_mfront_offi    : .true. if MFront official
! Out libr_name        : name of library if UMAT or MFront
! Out subr_name        : name of comportement in library if UMAT or MFront
! In  keywordfact      : factor keyword to read (COMPORTEMENT)
! In  iocc             : factor keyword index
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: rela_comp
    aster_logical :: l_kit_thm = .false._1
!
! --------------------------------------------------------------------------------------------------
!
    l_umat        = .false._1
    l_mfront      = .false._1
    l_mfront_offi = .false._1
    libr_name     = ' '
    subr_name     = ' '
!
! - Select comportement
!
    call comp_meca_l(rela_comp_, 'KIT_THM', l_kit_thm)
    if (l_kit_thm) then
        rela_comp = kit_comp(4)
    else
        rela_comp = rela_comp_
    endif
!
! - Detect type
!
    call comp_meca_l(rela_comp, 'UMAT'       , l_umat)
    call comp_meca_l(rela_comp, 'MFRONT_OFFI', l_mfront_offi)
    l_mfront = l_mfront_offi
    if (.not. l_mfront) then
        call comp_meca_l(rela_comp, 'MFRONT', l_mfront)
    endif
!
! - Get parameters
!
    if ( l_mfront ) then
        if (l_mfront_offi) then
            ASSERT(.not.l_kit_thm)
            call mfront_get_libname(libr_name)
            call mfront_get_function(rela_comp, subr_name)
        else
            if (present(keywordfact_)) then
                call getvtx(keywordfact_, 'LIBRAIRIE'  , iocc = iocc_, scal = libr_name)
                call getvtx(keywordfact_, 'NOM_ROUTINE', iocc = iocc_, scal = subr_name)
            endif
        endif
    elseif ( l_umat ) then
        if (present(keywordfact_)) then
            call getvtx(keywordfact_, 'LIBRAIRIE'  , iocc = iocc_, scal = libr_name)
            call getvtx(keywordfact_, 'NOM_ROUTINE', iocc = iocc_, scal = subr_name)
        endif
    endif
!
end subroutine
