subroutine comp_meca_chck(model  , mesh , full_elem_s, l_etat_init, info_comp_valk,&
                          l_auto_elas, l_auto_deborst, l_comp_erre)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/lccree.h"
#include "asterc/lctest.h"
#include "asterc/lcdiscard.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_full.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/getvtx.h"
#include "asterfort/jeveuo.h"
#include "asterfort/dismoi.h"
#include "asterfort/nmdovd.h"
#include "asterfort/nmdovm.h"
#include "asterfort/thm_kit_chck.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
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
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: full_elem_s
    aster_logical, intent(in) :: l_etat_init
    character(len=16), intent(inout) :: info_comp_valk(:)
    aster_logical, intent(out) :: l_auto_elas
    aster_logical, intent(out) :: l_auto_deborst
    aster_logical, intent(out) :: l_comp_erre
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE> - MECHANICS
!
! Check with Comportement.py
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  full_elem_s      : <CHELEM_S> of FULL_MECA option
! In  l_etat_init      : .true. if initial state is defined
! IO  info_comp_valk   : comportment informations (character)
! Out l_auto_elas      : .true. if at least one element use ELAS by default
! Out l_auto_deborst   : .true. if at least one element swap to Deborst algorithm
! Out l_comp_erre      : .true. if at least one element use comportment on element 
!                        doesn't support it
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    aster_logical :: l_affe_all
    integer :: nb_elem_affe
    character(len=16) :: texte(2)
    character(len=16) :: defo_comp, rela_comp, rela_thmc, type_cpla
    character(len=16) :: rela_comp_py, defo_comp_py
    character(len=16) :: kit_comp(4)
    integer :: iret
    character(len=16) :: keywordfact
    integer :: iocc, nbocc
    character(len=8) :: typmcl(2), repons
    character(len=16) :: motcle(2)
    integer :: nt
    aster_logical :: l_kit_thm, l_one_elem, l_elem_bound
!
! --------------------------------------------------------------------------------------------------
!
    nbocc = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
    list_elem_affe = '&&COMPMECASAVE.LIST'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Get mesh
!
        call getvtx(keywordfact, 'TOUT', iocc = iocc, nbret = nt)
        if (nt .ne. 0) then
            l_affe_all = .true.
        else
            l_affe_all = .false.
            call reliem(' ', mesh, 'NU_MAILLE', keywordfact, iocc,&
                        2, motcle, typmcl, list_elem_affe, nb_elem_affe)
            if (nb_elem_affe .eq. 0) l_affe_all = .true.
        endif
!
! ----- Get infos
!
        rela_comp   = info_comp_valk(16*(iocc-1) + 1)
        defo_comp   = info_comp_valk(16*(iocc-1) + 2)
        kit_comp(1) = info_comp_valk(16*(iocc-1) + 5)
        rela_thmc   = kit_comp(1)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
!
! ----- Warning if ELASTIC comportment and initial state
!
        if (l_etat_init .and. rela_comp(1:10).eq.'ELAS_VMIS_') then
            call utmess('A', 'COMPOR1_61')
        endif
!
! ----- Coding comportment (Python)
!
        call lccree(1, rela_comp, rela_comp_py)
        call lccree(1, defo_comp, defo_comp_py)
!
! ----- Check comportment/model with Comportement.py
!
        type_cpla = 'VIDE'
        call nmdovm(model       , l_affe_all  , list_elem_affe, nb_elem_affe  , full_elem_s,&
                    rela_comp_py, type_cpla   , l_auto_elas   , l_auto_deborst, l_comp_erre,&
                    l_one_elem  , l_elem_bound)
        if (.not. l_one_elem) then
            if (l_elem_bound) then
                call utmess('F', 'COMPOR1_60', si=iocc)
            else
                call utmess('F', 'COMPOR1_59', si=iocc)
            endif
        endif
        info_comp_valk(16*(iocc-1) + 4) = type_cpla
!
! ----- Check comportment/deformation with Comportement.py
!
        call lctest(rela_comp_py, 'DEFORMATION', defo_comp, iret)
        if (iret .eq. 0) then
            texte(1) = defo_comp
            texte(2) = rela_comp
            call utmess('F', 'COMPOR1_44', nk = 2, valk = texte)
        endif
!
! ----- Check comportment/model for THM (cannot use Comportement.py)
!
        if (l_kit_thm) then
            call thm_kit_chck(model, l_affe_all, list_elem_affe, nb_elem_affe, rela_thmc)
        endif
!
! ----- Check deformation with Comportement.py
!
        call nmdovd(model    , l_affe_all  , l_auto_deborst ,&
                    list_elem_affe,  nb_elem_affe, full_elem_s,&
                    defo_comp, defo_comp_py)

!
! ----- Check if COQUE_3D+GROT_GDEP is activated
!
        call dismoi('EXI_COQ3D', model, 'MODELE', repk=repons)
        if ( (repons .eq. 'OUI') .and. (defo_comp .eq. 'GROT_GDEP') ) then
            texte(1) = defo_comp
            texte(2) = 'COQUE_3D'
            call utmess('A', 'COMPOR1_47', nk = 2, valk = texte)
        endif
!
        call lcdiscard(rela_comp_py)
        call lcdiscard(defo_comp_py)
    end do
!
end subroutine
