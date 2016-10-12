subroutine load_neum_evcu(model    , ligrel_calc, cara_elem, load_name     , i_load,&
                          inst_curr, disp_prev  , strx_prev, disp_cumu_inst, vite_curr,&
                          base     , resu_elem  , vect_elem)
!
implicit none
!
#include "asterfort/gettco.h"
#include "asterfort/assert.h"
#include "asterfort/barych.h"
#include "asterfort/calcul.h"
#include "asterfort/chpnua.h"
#include "asterfort/cnocre.h"
#include "asterfort/copisd.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/nuachp.h"
#include "asterfort/pronua.h"
#include "asterfort/rsinch.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
#include "asterfort/vtgpld.h"
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
! person_in_charge: jean-luc.flejou at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in)  :: ligrel_calc
    real(kind=8), intent(in) :: inst_curr
    character(len=8), intent(in) :: load_name
    character(len=19), intent(in) :: disp_prev
    character(len=19), intent(in) :: strx_prev
    character(len=19), intent(in) :: disp_cumu_inst
    character(len=19), intent(in) :: vite_curr
    integer, intent(in) :: i_load
    character(len=19), intent(inout) :: resu_elem
    character(len=19), intent(in) :: vect_elem
    character(len=1), intent(in) :: base
!
! --------------------------------------------------------------------------------------------------
!
! Compute Neumann loads
! 
! EVOL_CHAR - Undead loads
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  cara_elem      : name of elementary characteristics (field)
! In  ligrel_calc    : LIGREL to compute 
! In  vite_curr      : speed at current of current time
! In  disp_prev      : displacement at beginning of current time
! In  strx_prev      : fibers information at beginning of current time
! In  disp_cumu_inst : displacement increment from beginning of current time
! In  inst_curr      : current time
! In  i_load         : index of current load
! In  load_name      : name of current load
! IO  resu_elem      : name of resu_elem
! In  vect_elem      : name of vect_elem
! In  base           : JEVEUX base to create vect_elem
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: newnom
    integer :: ibid, ier, nb_cham
    character(len=8) :: evol_char
    character(len=16) :: type_sd, option, repk
    character(len=19) :: load_name_evol
    character(len=24) :: chgeom, chcara(18)
    character(len=8) :: lpain(8), lpaout
    character(len=19) :: lchin(8)
    character(len=8) :: mesh_1, mesh_2, mesh_defo
    character(len=19) :: field_no_refe, nuage1, nuage2, method, field_no_refe1
    integer :: nbequa, nbno, dime, ndim
    integer, pointer :: p_mesh1_dime(:) => null()
    character(len=24) :: object
    character(len=8), pointer :: p_object(:) => null()
    character(len=24), pointer :: p_field_refe(:) => null()
!
    integer :: nblic
    character(len=8) :: licmp(3)
    data licmp/'DX','DY','DZ'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    field_no_refe  = '&&MNVGME.RESU_PROJE'
    load_name_evol = '&&NMVGME.FNOE_CALC'
    mesh_defo      = '.0000000'
    field_no_refe1 = '.0000000'
    newnom         = '.0000000'
!
! - Get evol_char
!
    object = load_name//'.CHME.EVOL.CHAR'
    call jeexin(object, ier)
    if (ier .eq. 0) then
        goto 99
    endif
    call jeveuo(object, 'L', vk8 = p_object)
    evol_char = p_object(1)
!
! - Check
!
    call dismoi('NB_CHAMP_UTI', evol_char, 'RESULTAT', repi=nb_cham)
    ASSERT(nb_cham.gt.0)
    call gettco(evol_char, type_sd)
    ASSERT(type_sd .eq. 'EVOL_CHAR')
!
! - Get lineic forces (CHAR_MECA_SR1D1D)
!
    option = ' '
    call rsinch(evol_char, 'VITE_VENT', 'INST', inst_curr, load_name_evol,&
                'EXCLU', 'EXCLU', 0, 'V', ier)
    if (ier .le. 2) then
        option = 'CHAR_MECA_SR1D1D'
        goto 10
    else if (ier.eq.11 .or. ier.eq.12 .or. ier.eq.20) then
        call utmess('F', 'CHARGES3_9', sk=evol_char, sr=inst_curr)
    endif
10 continue
!
! - Compute lineic forces (CHAR_MECA_SR1D1D)
!
    if (option .eq. 'CHAR_MECA_SR1D1D') then
!
! ----- Mesh information
!
        call jelira(load_name_evol//'.VALE', 'LONMAX', ival=nbequa)
        call dismoi('NOM_MAILLA', load_name_evol, 'CHAMP', repk=mesh_1)
        call dismoi('Z_CST', mesh_1, 'MAILLAGE', repk=repk)
        ndim = 3
        if (repk .eq. 'OUI') then
            ndim=2
        endif
!
! ----- Check
!
        call jeveuo(mesh_1//'.DIME', 'E', vi = p_mesh1_dime)
        nbno = p_mesh1_dime(1)
        dime = p_mesh1_dime(6)
        if (nbno * dime .ne. nbequa) then
            call utmess('F', 'CHARGES3_10', sk=load_name_evol)
        endif
!
! ----- Mesh deformation
!
        call gcncon('.', mesh_defo)
        call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh_2)
        call copisd('MAILLAGE', 'V', mesh_2, mesh_defo)
        call vtgpld('CUMU', mesh_2//'.COORDO', 1.d0, disp_prev, 'V',&
                    mesh_defo//'.COORDO1')
        call vtgpld('CUMU', mesh_defo//'.COORDO1', 1.d0, disp_cumu_inst, 'V',&
                    mesh_defo//'.COORDO')
        call detrsd('CHAMP_GD', mesh_defo//'.COORDO1')
!
! ----- Create reference field
!
        nblic = 3
        call cnocre(mesh_defo, 'DEPL_R', 0, [ibid], nblic,&
                    licmp, [ibid], 'V', ' ', field_no_refe)
!
! ----- Create SD NUAGE
!
        nuage1 = '&&NUAGE1'
        nuage2 = '&&NUAGE2'
        call chpnua(ndim, load_name_evol, ' ', nuage1)
        call chpnua(ndim, field_no_refe , ' ', nuage2)
!
! ----- Projection on deformed mesg
!
        method = 'NUAGE_DEG_1'
        call pronua(method, nuage1, nuage2)
        call nuachp(nuage2, ' ', field_no_refe)
!
! ----- Set right mesh
!
        call jeveuo(field_no_refe//'.REFE', 'E', vk24 = p_field_refe)
        p_field_refe(1) = mesh_2
!
! ----- Relative speed field
! 
        call jeexin(vite_curr(1:19)//'.VALE', ier)
        if (ier .gt. 0) then
            call gcncon('.', field_no_refe1)
            call copisd('CHAMP_GD', 'V', field_no_refe, field_no_refe1)
            call barych(field_no_refe1, vite_curr(1:19), 1.0d0, -1.0d0, field_no_refe,&
                        'V')  
        endif
!
! ----- Input fields
!
        call megeom(model, chgeom)
        call mecara(cara_elem, chcara)
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom(1:19)
        lpain(2) = 'PVITER'
        lchin(2) = field_no_refe
        lpain(3) = 'PVENTCX'
        lchin(3) = chcara(14)(1:19)
        lpain(4) = 'PDEPLMR'
        lchin(4) = disp_prev(1:19)
        lpain(5) = 'PDEPLPR'
        lchin(5) = disp_cumu_inst(1:19)
        lpain(6) = 'PCAGNPO'
        lchin(6) = chcara(6)(1:19)
        lpain(7) = 'PCAORIE'
        lchin(7) = chcara(1)(1:19)
        lpain(8) = 'PSTRXMR'
        lchin(8) = strx_prev(1:19)
!
! ----- Output fields
!
        lpaout = 'PVECTUR'
!
! ----- Generate new RESU_ELEM name
!
        newnom = resu_elem(10:16)
        call gcnco2(newnom)
        resu_elem(10:16) = newnom(2:8)
        call corich('E', resu_elem, i_load, ibid)
!
! ----- Compute 
!
        call calcul('S', option, ligrel_calc, 8, lchin,&
                    lpain, 1, resu_elem, lpaout, base,&
                    'OUI')
        call reajre(vect_elem, resu_elem, base)
!
! ----- Clean
!
        call detrsd('NUAGE', nuage1)
        call detrsd('NUAGE', nuage2)
        call jedetc('V', mesh_defo, 1)
        call detrsd('CHAMP_GD', field_no_refe1)
    endif
!
 99 continue
!
    call jedema()
end subroutine
