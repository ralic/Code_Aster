subroutine vecgme(model    , cara_elem , matez          , lload_namez, lload_infoz,&
                  inst_curr, disp_prevz, disp_cumu_instz, vect_elemz , inst_prev  ,&
                  compor   , carcri    , ligrel_calcz   , vite_currz , strx_prevz)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/detrsd.h"
#include "asterfort/load_list_info.h"
#include "asterfort/load_neum_prep.h"
#include "asterfort/load_neum_comp.h"
#include "asterfort/dismoi.h"
#include "asterfort/inical.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/nmvgme.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=*), intent(in) :: matez
    real(kind=8), intent(in) :: inst_curr
    character(len=*), intent(in) :: disp_prevz
    character(len=*), intent(in) :: disp_cumu_instz
    character(len=*), intent(in) :: lload_namez
    character(len=*), intent(in) :: lload_infoz
    character(len=*), intent(inout) :: vect_elemz
    real(kind=8), intent(in) :: inst_prev
    character(len=24), intent(in) :: compor
    character(len=24), intent(in) :: carcri
    character(len=*), intent(in) :: ligrel_calcz
    character(len=*), intent(in) :: vite_currz
    character(len=*), intent(in) :: strx_prevz
!
! --------------------------------------------------------------------------------------------------
!
! Compute Neumann loads
! 
! Undead loads - Depending on geometry or speed - Vector
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  inst_prev      : previous time
! In  inst_curr      : current time
! In  lload_name     : name of object for list of loads name
! In  lload_info     : name of object for list of loads info
! In  ligrel_calc    : LIGREL to compute 
! In  vite_curr      : speed at current of current time
! In  disp_prev      : displacement at beginning of current time
! In  strx_prev      : fibers information at beginning of current time
! In  disp_cumu_inst : displacement increment from beginning of current time
! In  compor         : name of comportment definition (field)
! In  carcri         : name of comportment parameters (field)
! IO  vect_elem      : name of vect_elem result
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 42, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    character(len=1) :: stop
    character(len=8) :: newnom, load_name
    integer :: nb_load, i_load
    integer :: load_nume
    integer :: nb_in_prep
    real(kind=8) :: inst_theta 
    character(len=24) :: ligrel_calc, mate
    character(len=19) :: vect_elem, resu_elem
    character(len=19) :: disp_prev, disp_cumu_inst, vite_curr, strx_prev
    character(len=24) :: lload_name
    character(len=24), pointer :: v_load_name(:) => null()
    character(len=24) :: lload_info
    integer, pointer :: v_load_info(:) => null()
    aster_logical :: load_empty
    character(len=1) :: base
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    stop           = 'S'
    newnom         = '.0000000'
    resu_elem      = '&&VECGME.0000000'
    mate           = matez
    lload_name     = lload_namez
    lload_info     = lload_infoz
    disp_prev      = disp_prevz
    strx_prev      = strx_prevz
    disp_cumu_inst = disp_cumu_instz
    vite_curr      = vite_currz
    ligrel_calc    = ligrel_calcz
    inst_theta     = 0.d0
    if (ligrel_calc .eq. ' ') then
        ligrel_calc = model(1:8)//'.MODELE'
    endif
    base           = 'V'
!
! - Init fields
!
    call inical(nb_in_maxi, lpain, lchin, nbout, lpaout,&
                lchout)
!
! - Result name for vect_elem
!
    vect_elem = vect_elemz
    if (vect_elem .eq. ' ') then
        vect_elem = '&&VECGME'
    endif
!
! - Loads
!
    call load_list_info(load_empty, nb_load   , v_load_name, v_load_info,&
                        lload_name, lload_info)
!
! - Allocate result
!
    call detrsd('VECT_ELEM', vect_elem)
    call memare(base, vect_elem, model, mate, cara_elem,&
                'CHAR_MECA')
    call reajre(vect_elem, ' ', base)
    if (load_empty) then
        goto 99
    endif
!
! - Preparing input fields
!
    call load_neum_prep(model    , cara_elem , mate      , 'Suiv'      , inst_prev,&
                        inst_curr, inst_theta, nb_in_maxi, nb_in_prep  , lchin    ,&
                        lpain    , disp_prev = disp_prev, disp_cumu_inst = disp_cumu_inst,&
                        compor = compor, carcri = carcri)
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(nb_load+i_load+1)  
        if (load_nume .eq. 4) then  
            call load_neum_comp(stop       , i_load    , load_name , load_nume, 'Suiv',&
                                ligrel_calc, nb_in_maxi, nb_in_prep, lpain    , lchin ,&
                                base       , resu_elem , vect_elem )
        endif
!
! ----- TRAITEMENT DE AFFE_CHAR_MECA/EVOL_CHAR
!
        newnom = resu_elem(10:16)
        call gcnco2(newnom)
        resu_elem(10:16) = newnom(2:8)
        call nmvgme(model    , ligrel_calc, cara_elem, lload_name    , i_load   ,&
                    inst_curr, resu_elem  , disp_prev, disp_cumu_inst, vite_curr,&
                    strx_prev)
        call reajre(vect_elem, resu_elem, base)
    end do
!
 99 continue
!
    vect_elemz = vect_elem//'.RELR'
!
    call jedema()
end subroutine
