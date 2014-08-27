subroutine load_neum_prep(model    , cara_elem , mate      , load_type     , inst_prev,&
                          inst_curr, inst_theta, nb_in_maxi, nb_in_prep    , lchin    ,&
                          lpain    , varc_curr , disp_prev , disp_cumu_inst, compor   ,&
                          carcri   , nharm)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exixfe.h"
#include "asterfort/xajcin.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/meharm.h"
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    character(len=24), intent(in) :: mate
    character(len=4), intent(in) :: load_type
    real(kind=8), intent(in) :: inst_prev 
    real(kind=8), intent(in) :: inst_curr
    real(kind=8), intent(in) :: inst_theta 
    integer, intent(in) :: nb_in_maxi
    character(len=8), intent(inout) :: lpain(nb_in_maxi)
    character(len=19), intent(inout) :: lchin(nb_in_maxi)
    integer, intent(out) :: nb_in_prep
    character(len=19), optional, intent(in) :: varc_curr
    character(len=19), optional, intent(in) :: disp_prev
    character(len=19), optional, intent(in) :: disp_cumu_inst
    character(len=24), optional, intent(in) :: compor
    character(len=24), optional, intent(in) :: carcri
    integer, optional, intent(in) :: nharm
!
! --------------------------------------------------------------------------------------------------
!
! Neumann loads computation
!
! Preparing input fields - Common for all Neumann loads
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  mate           : name of material characteristics (field)
! In  cara_elem      : name of elementary characteristics (field)
! In  load_type      : load type to compute
!                        'Dead' - Dead loads (not dependent on displacements)
!                        'Pilo' - Loads for continuation (not dependent on displacements)
!                        'Suiv' - Undead loads (dependent on displacements)
! In  inst_prev      : previous time
! In  inst_curr      : current time
! In  inst_theta     : parameter theta
! In  nb_in_maxi     : maximum number of input fields
! IO  lpain          : list of input parameters
! IO  lchin          : list of input fields
! Out nb_in_prep     : number of input fields before specific ones
! In  varc_curr      : command variable for current time
! In  disp_prev      : displacement at beginning of current time
! In  disp_cumu_inst : displacement increment from beginning of current time
! In  compor         : name of comportment definition (field)
! In  carcri         : name of comportment parameters (field)
! In  nharm          : Fourier mode
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: chgeom, chcara(18), chharm, chtime, chinst_curr, chinst_prev
    character(len=19) :: ligrel_model
    character(len=8) :: nomcmp(3), mesh
    real(kind=8) :: time(3)
    integer :: ier
    logical :: l_xfem
!
! --------------------------------------------------------------------------------------------------
!
    ligrel_model = model(1:8)//'.MODELE'
    chtime       = '&&VECHME.CHTIME'
    chharm       = '&&VECHME.CHHARM'
    call exixfe(model, ier)
    l_xfem = ier.ne.0
    call dismoi('NOM_MAILLA', model, 'MODELE', repk = mesh)
!
! - Geometry field
!
    call megeom(model, chgeom)
!
! - Elementary characteristics fields
!
    call mecara(cara_elem, chcara)
!
! - Times field
! 
    if (load_type.ne.'Dead') then
        nomcmp(1) = 'INST'
        chinst_curr = '&&VECHME.CH_INST_R'
        call mecact('V', chinst_curr, 'LIGREL', ligrel_model, 'INST_R  ',&
                    ncmp=1, nomcmp=nomcmp(1), sr=inst_curr)
        chinst_prev = '&&VECHME.CH_INST_M'
        call mecact('V', chinst_prev, 'LIGREL', ligrel_model, 'INST_R  ',&
                    ncmp=1, nomcmp=nomcmp(1), sr=inst_prev)
        call mecact('V', chtime     , 'LIGREL', ligrel_model, 'INST_R  ',&
                    ncmp=1, nomcmp=nomcmp(1), sr=inst_curr)
    else
        nomcmp(1) = 'INST'
        nomcmp(2) = 'DELTAT'
        nomcmp(3) = 'THETA'
        time(1) = inst_prev
        time(2) = inst_curr-inst_prev
        time(3) = inst_theta
        call mecact('V', chtime, 'LIGREL', ligrel_model, 'INST_R  ',&
                    ncmp=3, lnomcmp=nomcmp, vr=time)
    endif
!
! - Fourier field
!
    if (present(nharm)) then
        call meharm(model, nharm, chharm)
    endif
!
! - Input fields
!
    lpain(1)  = 'PGEOMER'
    lchin(1)  = chgeom(1:19)
    lpain(2)  = 'PTEMPSR'
    lchin(2)  = chtime(1:19)
    lpain(3)  = 'PMATERC'
    lchin(3)  = mate(1:19)
    lpain(4)  = 'PCACOQU'
    lchin(4)  = chcara(7)(1:19)
    lpain(5)  = 'PCAGNPO'
    lchin(5)  = chcara(6)(1:19)
    lpain(6)  = 'PCADISM'
    lchin(6)  = chcara(3)(1:19)
    lpain(7)  = 'PCAORIE'
    lchin(7)  = chcara(1)(1:19)
    lpain(8)  = 'PCACABL'
    lchin(8)  = chcara(10)(1:19)
    lpain(9)  = 'PCAARPO'
    lchin(9)  = chcara(9)(1:19)
    lpain(10) = 'PCAGNBA'
    lchin(10) = chcara(11)(1:19)
    lpain(11) = 'PCAMASS'
    lchin(11) = chcara(12)(1:19)
    lpain(12) = 'PCAGEPO'
    lchin(12) = chcara(5)(1:19)
    lpain(13) = 'PNBSP_I'
    lchin(13) = chcara(16)(1:19)
    lpain(14) = 'PFIBRES'
    lchin(14) = chcara(17)(1:19)
    lpain(15) = 'PCINFDI'
    lchin(15) = chcara(15)(1:19)
    nb_in_prep = 15
    if (present(nharm)) then
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PHARMON'
        lchin(nb_in_prep) = chharm(1:19)
    endif
!
! - Specific
!
    if (load_type.eq.'Dead'.or.load_type.eq.'Pilo') then
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PVARCPR'
        lchin(nb_in_prep) = varc_curr(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PCOMPOR'
        lchin(nb_in_prep) = mate(1:8)//'.COMPOR'
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PABSCUR'
        lchin(nb_in_prep) = mesh(1:8)//'.ABSC_CURV'
    elseif (load_type.eq.'Suiv') then
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PDEPLMR'
        lchin(nb_in_prep) = disp_prev(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PDEPLPR'
        lchin(nb_in_prep) = disp_cumu_inst(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PCARCRI'
        lchin(nb_in_prep) = carcri(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PINSTMR'
        lchin(nb_in_prep) = chinst_prev(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PINSTPR'
        lchin(nb_in_prep) = chinst_curr(1:19)
        nb_in_prep = nb_in_prep + 1
        lpain(nb_in_prep) = 'PCOMPOR'
        lchin(nb_in_prep) = compor(1:19)
    else
        ASSERT(.false.)
    endif
!
! - XFEM fields
!
    if (l_xfem) then
        call xajcin(model, 'CHAR_MECA_NEUM', nb_in_maxi, lchin, lpain,&
                    nb_in_prep)
    endif

end subroutine
