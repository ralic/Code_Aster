recursive subroutine comp_meca_l(rela_comp, whatz, l_detec, type_matg, post_iter)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterc/lccree.h"
#include "asterc/lctype.h"
!
! ======================================================================
! COPYRIGHT (C) 2091 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=16), intent(in) :: rela_comp
    character(len=*), intent(in) :: whatz
    aster_logical, intent(out) :: l_detec
    character(len=16), optional, intent(in) :: type_matg
    character(len=16), optional, intent(in) :: post_iter
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Detection of specific cases
!
! --------------------------------------------------------------------------------------------------
!
! In  rela_comp    : RELATION comportment
! In  what         : what to detect
! In  type_matg    : type of tangent matrix
! In  post_iter    : type of post_treatment
! Out l_detec      : .true. if specific case is detected
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: what, rela_comp_py, ldctyp
!
! --------------------------------------------------------------------------------------------------
!
    what = whatz
    l_detec = .false.
    if (what .eq. 'CRISTAL') then
        l_detec = (rela_comp .eq. 'MONOCRISTAL') .or. (rela_comp .eq.'POLYCRISTAL')
    else if (what .eq. 'KIT_META') then
        l_detec = (rela_comp(1:4).eq.'META').and.(rela_comp.ne.'META_LEMA_ANI')
    else if (what .eq. 'KIT_THM') then
        l_detec = ((rela_comp(1:5).eq.'KIT_H') .or. (rela_comp(1:6).eq.'KIT_TH'))
    else if (what .eq. 'KIT_DDI') then
        l_detec = (rela_comp.eq.'KIT_DDI')
    else if (what .eq. 'KIT_CG') then
        l_detec = (rela_comp.eq.'KIT_CG')
    else if (what .eq. 'KIT') then
        l_detec = (rela_comp(1:4).eq.'KIT_').or.(rela_comp(1:4).eq.'META')
    else if (what .eq. 'UMAT') then
        l_detec = (rela_comp .eq. 'UMAT')
    else if (what .eq. 'MFRONT_OFFI') then
        call lccree(1, rela_comp, rela_comp_py)
        call lctype(rela_comp_py, ldctyp)
        l_detec = ldctyp == 'mfront'
    else if (what .eq. 'MFRONT') then
        l_detec = (rela_comp .eq. 'MFRONT')
        if (.not. l_detec) then
            call comp_meca_l(rela_comp, 'MFRONT_OFFI', l_detec)
        endif
    else if (what .eq. 'EXTE_COMP') then
        l_detec = (rela_comp .eq. 'MFRONT').or.(rela_comp .eq. 'UMAT')
        if (.not. l_detec) then
            call comp_meca_l(rela_comp, 'MFRONT_OFFI', l_detec)
        endif
    else if (what .eq. 'PMF') then
        l_detec = (rela_comp .eq. 'MULTIFIBRE')
    else if (what .eq. 'MATR_TGSC') then
        ASSERT(present(type_matg))
        l_detec = type_matg .eq. 'TANGENTE_SECANTE'
    else if (what .eq. 'CRIT_RUPT') then
        ASSERT(present(post_iter))
        l_detec = post_iter .eq. 'CRIT_RUPT'
    else
        write(6,*) 'What: ',rela_comp,what,whatz
        ASSERT(.false.)
    endif
!
!
end subroutine
