subroutine vechme(stop     , modelz, lload_namez, lload_infoz, inst        ,&
                  cara_elem, mate  , vect_elemz , varc_currz , ligrel_calcz,&
                  nharm)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/load_list_info.h"
#include "asterfort/load_neum_prep.h"
#include "asterfort/load_neum_comp.h"
#include "asterfort/load_neum_evcd.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=1), intent(in) :: stop
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: lload_namez
    character(len=*), intent(in) :: lload_infoz
    real(kind=8), intent(in) :: inst(3)
    character(len=*), intent(in) :: cara_elem
    character(len=*), intent(in) :: mate
    character(len=*), intent(inout) :: vect_elemz
    character(len=*), optional, intent(in) :: varc_currz
    character(len=*), optional, intent(in) :: ligrel_calcz
    integer, optional, intent(in) :: nharm
!
! --------------------------------------------------------------------------------------------------
!
! Compute Neumann loads
! 
! Dead and fixed loads
!
! --------------------------------------------------------------------------------------------------
!
! In  stop           : continue or stop computation if no loads on elements
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  lload_name     : name of object for list of loads name
! In  lload_info     : name of object for list of loads info
! In  inst           : times informations
! In  ligrel_calc    : LIGREL to compute 
! In  varc_curr      : command variable for current time
! IO  vect_elem      : name of vect_elem result
! In  nharm          : Fourier mode
!
! ATTENTION :
!   LE VECT_ELEM (VECELZ) RESULTAT A 1 PARTICULARITE :
!   CERTAINS RESUELEM NE SONT PAS DES RESUELEM MAIS DES CHAM_NO (.VEASS)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_in_maxi, nbout
    parameter (nb_in_maxi = 42, nbout = 1)
    character(len=8) :: lpain(nb_in_maxi), lpaout(nbout)
    character(len=19) :: lchin(nb_in_maxi), lchout(nbout)
!
    integer :: nb_load, i_load
    integer :: load_nume
    integer :: nb_in_prep
    real(kind=8) :: inst_prev, inst_curr, inst_theta 
    character(len=8) :: load_name
    character(len=24) :: ligrel_calc, model
    character(len=19) :: vect_elem, varc_curr, resu_elem
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
    resu_elem   = '&&VECHME.0000000'
    model       = modelz
    lload_name  = lload_namez
    lload_info  = lload_infoz
    varc_curr   = ' '
    if (present(varc_currz)) then
        varc_curr   = varc_currz
    endif
    ligrel_calc = model(1:8)//'.MODELE'
    if (present(ligrel_calcz)) then
        ligrel_calc = ligrel_calcz
    endif
    inst_prev   = inst(1)
    inst_curr   = inst(1)+inst(2)
    inst_theta  = inst(3)
    base        = 'V'
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
        vect_elem = '&&VECHME'
    endif
!
! - Loads
!
    call load_list_info(load_empty, nb_load  , v_load_name, v_load_info,&
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
    if (present(nharm)) then
        call load_neum_prep(model    , cara_elem , mate      , 'Dead'      , inst_prev,&
                            inst_curr, inst_theta, nb_in_maxi, nb_in_prep  , lchin    ,&
                            lpain    , varc_curr = varc_curr, nharm = nharm)
    else
        call load_neum_prep(model    , cara_elem , mate      , 'Dead'      , inst_prev,&
                            inst_curr, inst_theta, nb_in_maxi, nb_in_prep  , lchin    ,&
                            lpain    , varc_curr = varc_curr)
    endif
!
! - Computation
!
    do i_load = 1, nb_load
        load_name = v_load_name(i_load)(1:8)
        load_nume = v_load_info(nb_load+i_load+1)  
!
! ----- Standard dead Neumann loads
!
        if ((load_nume .gt. 0 .and. load_nume .lt. 4).or.(load_nume.eq.55)) then
            call load_neum_comp(stop       , i_load    , load_name , load_nume, 'Dead'   ,&
                                ligrel_calc, nb_in_maxi, nb_in_prep, lpain    , lchin    ,& 
                                base       , resu_elem , vect_elem  )
        endif
!
! ----- Composite dead Neumann loads (EVOL_CHAR)
!
        call load_neum_evcd(stop      , inst_prev , load_name, i_load, ligrel_calc,&
                            nb_in_maxi, nb_in_prep, lpain    , lchin , base       ,&
                            resu_elem , vect_elem)
    end do
!
 99 continue
!
    vect_elemz = vect_elem//'.RELR'
!
    call jedema()
end subroutine
