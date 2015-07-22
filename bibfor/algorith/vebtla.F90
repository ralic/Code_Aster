subroutine vebtla(base     , model_    , mate, cara_elem, disp_,&
                  list_load, vect_elemz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/gcnco2.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/load_list_info.h"
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
    character(len=*), intent(in) :: model_
    character(len=24), intent(in) :: mate
    character(len=24), intent(in) :: cara_elem
    character(len=*), intent(in) :: disp_
    character(len=19), intent(in) :: list_load
    character(len=*), intent(in) :: vect_elemz
!
! --------------------------------------------------------------------------------------------------
!
! Compute Dirichlet loads
!
! For Lagrange elements (AFFE_CHAR_MECA) - BT . LAMBDA (reaction loads)
!
! --------------------------------------------------------------------------------------------------
!
! In  base             : JEVEUX base to create vect_elem
! In  model            : name of model
! In  list_load        : name of datastructure for list of loads
! In  mate             : name of material characteristics (field)
! In  cara_elem        : name of elementary characteristics (field)
! In  disp             : displacements
! In  vect_elem        : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=2)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: load_name, newnom
    character(len=16) :: option
    character(len=24) :: ligrch
    character(len=8) :: model
    integer :: iret, load_nume, nb_load, i_load
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
    aster_logical :: load_empty
    character(len=19) :: disp, vect_elem, resu_elem
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    model     = model_
    disp      = disp_
    vect_elem = vect_elemz
    newnom    = '.0000000'
    resu_elem = vect_elem(1:8)//'.???????'
    option    = 'MECA_BTLA_R'
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
! - Allocate result
!
    call jeexin(vect_elem//'.RELR', iret)
    if (iret .eq. 0) then
        call memare(base, vect_elem, model, mate, cara_elem,&
                    'CHAR_MECA')
    else
        call jedetr(vect_elem//'.RELR')
    endif
    call reajre(vect_elem, ' ', base)
!
! - Input fields
!
    lpain(1) = 'PLAGRAR'
    lchin(1) = disp
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(i_load+1)  
        if (load_nume.gt.0) then
            ligrch = load_name // '.CHME.LIGRE'
!
! --------- Input field
!
            lpain(2) = 'PDDLMUR'
            lchin(2) = load_name(1:8)//'.CHME.CMULT'
!
! --------- Generate new RESU_ELEM name
!
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
!
! --------- Output field
!
            lpaout(1) = 'PVECTUR'
            lchout(1) = resu_elem
!
! --------- Computation
!
            call calcul('S'  , option, ligrch, nbin  , lchin,&
                        lpain, nbout , lchout, lpaout, base ,&
                        'OUI')
!
! --------- Copying output field
!
            call reajre(vect_elem, lchout(1), 'V')
        endif
    end do
!
99  continue
    call jedema()
end subroutine
