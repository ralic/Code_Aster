subroutine medime(base, cumul, model, list_load, matr_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/exisd.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/load_list_info.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=1), intent(in) :: base
    character(len=4), intent(in) :: cumul
    character(len=24), intent(in) :: model
    character(len=19), intent(in) :: list_load
    character(len=19), intent(in) :: matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Load
!
! Elementary matrix for Dirichlet BC
!
! --------------------------------------------------------------------------------------------------
!
! In  base             : JEVEUX base to create vect_elem
! In  cumul            : option to add/erase matr_elem
!                      'ZERO' - Erase old matr_elem
!                      'CUMU' - Add matr_elem to old ones
! In  model            : name of model
! In  list_load        : name of datastructure for list of loads
! In  matr_elem        : elementary matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=1)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: load_name
    character(len=16) :: option
    character(len=19) :: ligrch, resu_elem
    integer :: iret, ilires, load_nume
    integer :: nluti, nb_load, i_load
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
    aster_logical :: load_empty
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    option    = 'MECA_DDLM_R'
    resu_elem = matr_elem(1:8)//'.???????'
!
! - Init fields
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Loads
!
    call load_list_info(load_empty, nb_load    , v_load_name, v_load_info,&
                        list_load_ = list_load)
    if (load_empty) then
        goto 99
    endif
!
! - Output field
!
    lpaout(1) = 'PMATUUR'
!
! - Allocate result
!
    if (cumul .eq. 'ZERO') then
        call jedetr(matr_elem//'.RELR')
        call memare(base, matr_elem, model, ' ', ' ',&
                    'RIGI_MECA')
        call reajre(matr_elem, ' ', base)
    endif
    if (cumul .eq. 'ZERO') then
        nluti = 1
    else if (cumul.eq.'CUMU') then
        call jelira(matr_elem//'.RELR', 'LONUTI', nluti)
        nluti = nluti+1
        call codent(nluti+1, 'D0', lchout(1) (12:14))
    else
        ASSERT(.false.)
    endif
!3
! - Computation
!
    ilires = nluti-1
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(i_load+1) 
        if (load_nume .ne. 0) then
            ligrch = load_name(1:8)//'.CHME.LIGRE'
            call jeexin(ligrch(1:19)//'.LIEL', iret)
            if (iret .le. 0) cycle
            call exisd('CHAMP_GD', load_name(1:8)//'.CHME.CMULT', iret)
            if (iret .le. 0) cycle
!
! --------- Input field
!
            lpain(1) = 'PDDLMUR'
            lchin(1) = load_name//'.CHME.CMULT'
!
! --------- Generate new RESU_ELEM name
!
            ASSERT(ilires+1.le.9999999)
            call codent(ilires+1, 'D0', resu_elem(10:16))
            lchout(1) = resu_elem
!
! --------- Computation
!
            call calcul('S'  , option, ligrch, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, base,&
                        'OUI')
!
! --------- Copying resu_elem
!
            call reajre(matr_elem, lchout(1), base)
            ilires = ilires + 1
        endif
    end do
 99 continue
!
    call jedema()
end subroutine
