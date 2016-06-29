subroutine comp_read_exte(keywordfact, iocc    , rela_comp    ,&
                          l_umat     , l_mfront, l_mfront_offi,&
                          libr_name  , subr_name)
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
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=16), intent(in) :: rela_comp
    aster_logical, intent(out) :: l_umat
    aster_logical, intent(out) :: l_mfront
    aster_logical, intent(out) :: l_mfront_offi
    character(len=255), intent(out) :: libr_name
    character(len=255), intent(out) :: subr_name
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Get parameters for external programs (MFRONT/UMAT)
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact      : factor keyword to read (COMPORTEMENT)
! In  iocc             : factor keyword index
! In  rela_comp        : RELATION comportment
! Out l_umat           : .true. if UMAT
! Out l_mfront         : .true. if MFront
! Out l_mfront_offi    : .true. if MFront official
! Out libr_name        : name of library if UMAT or MFront
! Out subr_name        : name of comportement in library if UMAT or MFront
!
! --------------------------------------------------------------------------------------------------
!
    l_umat        = .false._1
    l_mfront      = .false._1
    l_mfront_offi = .false._1
    libr_name     = ' '
    subr_name     = ' '
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
            call mfront_get_libname(libr_name)
            call mfront_get_function(rela_comp, subr_name)
        else
            call getvtx(keywordfact, 'LIBRAIRIE'  , iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
        endif
    elseif ( l_umat ) then
        call getvtx(keywordfact, 'LIBRAIRIE'  , iocc = iocc, scal = libr_name)
        call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
    endif
!
end subroutine
