subroutine vebume(model_, matass_, disp_, list_load, vect_elemz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calcul.h"
#include "asterfort/conlag.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/load_list_info.h"
#include "asterfort/ischar_iden.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
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
    character(len=*), intent(in) :: model_
    character(len=*), intent(in) :: matass_
    character(len=*), intent(in) :: disp_
    character(len=19), intent(in) :: list_load
    character(len=*), intent(in) :: vect_elemz
!
! --------------------------------------------------------------------------------------------------
!
! Compute Dirichlet loads
!
! For Lagrange elements (AFFE_CHAR_MECA) - B . U 
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of model
! In  list_load        : name of datastructure for list of loads
! In  disp             : displacements
! In  matass           : matrix
! In  vect_elem        : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: iret, ibid, nb_load, i_load
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
    aster_logical :: load_empty
    character(len=8) :: load_name, newnom, model
    character(len=16) :: option
    character(len=19) :: disp, vect_elem, matass, resu_elem
    character(len=24) :: ligrch, chalph
    real(kind=8) :: alpha
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    vect_elem = vect_elemz
    matass    = matass_
    model     = model_
    disp      = disp_
    newnom    = '.0000000'
    chalph    = '&&VEBUME.CH_NEUT_R'
    resu_elem = '&&VEBUME.???????'
    option    = 'MECA_BU_R'
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
! - Cart for Lagrange conditionner
!
    call conlag(matass, alpha)  
    call mecact('V', chalph, 'MODELE', model, 'NEUT_R  ',&
                ncmp=1, nomcmp='X1', sr=alpha)
!
! - Allocate result
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare('V', vect_elem, model, ' ', ' ',&
                'CHAR_MECA')
    call reajre(vect_elem, ' ', 'V')
!
! - Input fields
! 
    lpain(1) = 'PDDLIMR'
    lchin(1) = disp
    lpain(2) = 'PALPHAR'
    lchin(2) = chalph(1:19)
!
! - Output fields
! 
    lpaout(1) = 'PVECTUR'
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8) 
        if (      ischar_iden(v_load_info, i_load, nb_load, 'DIRI', 'DUAL') .and.&
            .not. ischar_iden(v_load_info, i_load, nb_load, 'DIRI', 'SUIV')) then
            ligrch = load_name//'.CHME.LIGRE'
            call jeexin(load_name//'.CHME.LIGRE.LIEL', iret)
            if (iret .le. 0) cycle
            call exisd('CHAMP_GD', load_name//'.CHME.CMULT', iret)
            if (iret .le. 0) cycle
!
! --------- Input field
!
            lpain(3) = 'PDDLMUR'
            lchin(3) = load_name(1:8)//'.CHME.CMULT'
!
! --------- Generate new RESU_ELEM name
!
            call gcnco2(newnom)
            resu_elem(10:16) = newnom(2:8)
            call corich('E', resu_elem, i_load, ibid)
            lchout(1) = resu_elem
!
! --------- Computation
!
            call calcul('S', option, ligrch, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, 'V',&
                        'OUI')
!
! --------- Copying output field
!
            call reajre(vect_elem, lchout(1), 'V')
        endif
    end do
!
 99 continue
!
    call jedema()
end subroutine
