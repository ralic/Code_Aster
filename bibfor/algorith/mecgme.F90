subroutine mecgme(modelz   , cara_elemz    , matez    , list_load, inst_curr,&
                  disp_prev, disp_cumu_inst, inst_prev, compor   , carcri,&
                  matr_elem)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxliis.h"
#include "asterfort/load_list_info.h"
#include "asterfort/load_neum_prep.h"
#include "asterfort/load_neum_matr.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: cara_elemz
    character(len=*), intent(in) :: matez
    character(len=19), intent(in) :: list_load
    real(kind=8), intent(in) :: inst_prev
    real(kind=8), intent(in) :: inst_curr
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: carcri
    character(len=19), intent(in) :: matr_elem
!
! --------------------------------------------------------------------------------------------------
!
! Compute Neumann loads
! 
! Undead loads - Depending on geometry or speed - Matrix
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  inst_prev      : previous time
! In  inst_curr      : current time
! In  list_load      : list of loads
! In  disp_prev      : displacement at beginning of current time
! In  disp_cumu_inst : displacement increment from beginning of current time
! In  compor         : name of comportment definition (field)
! In  carcri         : name of comportment parameters (field)
! In  matr_elem      : name of matr_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 42, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    character(len=24) :: model, cara_elem, mate
    character(len=24), pointer :: p_matr_elem_relr(:) => null()
    character(len=24), pointer :: v_load_name(:) => null()
    integer, pointer :: v_load_info(:) => null()
    character(len=8) :: load_name
    integer :: load_nume
    real(kind=8) :: inst_theta
    character(len=19) :: ligrel_model
    integer :: iret, ier, ichme, i_load, idx_matr
    aster_logical :: l_first_matr, load_empty
    integer :: nb_load, nbchme, nb_in_prep
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    model        = modelz
    cara_elem    = cara_elemz
    mate         = matez
    ligrel_model = model(1:8)//'.MODELE'
    nb_load      = 0
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Loads
!
    call load_list_info(load_empty, nb_load  , v_load_name, v_load_info,&
                        list_load = list_load)
    if (load_empty) then
        goto 99
    endif
!
! - Allocate result
!
    call jeexin(matr_elem//'.RELR', iret)
    if (iret .eq. 0) then
        l_first_matr = .true.
        call memare('V', matr_elem, model(1:8), mate, cara_elem,&
                    'CHAR_MECA')
        call reajre(matr_elem, ' ', 'V')
    else
        l_first_matr = .false.
        call jelira(matr_elem//'.RELR', 'LONUTI', nbchme)
        if (nbchme .gt. 0) then
            call jeveuo(matr_elem//'.RELR', 'L', vk24 = p_matr_elem_relr)
        endif
    endif
!
! - Preparing input fields
!
    call load_neum_prep(model    , cara_elem , mate      , 'Suiv'    , inst_prev,&
                        inst_curr, inst_theta, nb_in_maxi, nb_in_prep, lchin    ,&
                        lpain    , disp_prev = disp_prev , disp_cumu_inst = disp_cumu_inst,&
                        compor = compor, carcri = carcri)
!
! - Computation
!
    if (l_first_matr) then
        do i_load = 1, nb_load
            idx_matr  = 0
            load_name = v_load_name(i_load)(1:8)
            load_nume = v_load_info(nb_load+i_load+1)  
            if (load_nume .eq. 4) then
                call load_neum_matr(i_load      , idx_matr  , load_name , load_nume, 'Suiv',&
                                    ligrel_model, nb_in_maxi, nb_in_prep, lpain    , lchin ,&
                                    matr_elem  )
            endif
        end do
    else
        do ichme = 1, nbchme
            if (p_matr_elem_relr(ichme)(10:10) .eq. 'G') then
                call lxliis(p_matr_elem_relr(ichme)(7:8), i_load, ier)
                idx_matr  = -ichme
                load_name = v_load_name(i_load)(1:8)
                load_nume = v_load_info(nb_load+i_load+1)
                if (load_nume .eq. 4) then
                    call load_neum_matr(i_load      , idx_matr  , load_name , load_nume, 'Suiv',&
                                        ligrel_model, nb_in_maxi, nb_in_prep, lpain    , lchin ,&
                                        matr_elem   )
                endif
            endif
        end do
    endif
!
 99 continue
!
    call jedema()
end subroutine

