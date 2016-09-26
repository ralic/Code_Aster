subroutine pmdorc(compor, carcri, nb_vari, incela, mult_comp)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/carc_info.h"
#include "asterfort/carc_read.h"
#include "asterfort/comp_meca_cvar.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_info.h"
#include "asterfort/comp_meca_pvar.h"
#include "asterfort/comp_meca_read.h"
#include "asterfort/imvari.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdocv.h"
#include "asterfort/utlcal.h"
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
! aslint: disable=W1003
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(out) :: compor(20)
    real(kind=8), intent(out) :: carcri(21)
    integer, intent(out) :: nb_vari
    integer, intent(out) :: incela
    character(len=16), intent(out) :: mult_comp
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics) for SIMU_POINT_MAT
!
! Prepare objects COMPOR <CARTE> and CARCRI <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! OUT COMPOR  : OBJET COMPOR DECRIVANT LE TYPE DE COMPORTEMENT
! OUT CARCRI  : OBJET CARCRI CRITERES DE CONVERGENCE LOCAUX
! OUT NBVARI  : NOMBRE DE VARIABLE INTERNES
! OUT incela  : =1 si COMP_INCR, =2 si COMP_ELAS
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: compor_info
    integer :: i_comp, nume_comp(4), nb_vari_comp(4)
    integer :: nbocc1, nbocc2, nbocc3
    character(len=16) :: keywordfact
    character(len=16) :: rela_comp, algo_inte, defo_comp, type_comp
    character(len=16) :: kit_comp(4), type_cpla, type_matg, post_iter
    aster_logical :: l_kit_thm, l_etat_init
    real(kind=8) :: algo_inte_r, iter_inte_maxi, resi_inte_rela
    type(NL_DS_ComporPrep) :: ds_compor_prep
    type(NL_DS_ComporParaPrep) :: ds_compor_para
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    compor_info  = '&&PMDORC.LIST_VARI'
    keywordfact  = 'COMPORTEMENT'
    compor(1:20) = 'VIDE'
!
! - Initial state
!
    call getfac('SIGM_INIT', nbocc1)
    call getfac('EPSI_INIT', nbocc2)
    call getfac('VARI_INIT', nbocc3)
    l_etat_init = (nbocc1+nbocc2+nbocc3) > 0
!
! - Create datastructure to prepare comportement
!
    call comp_meca_info(ds_compor_prep)
    if (ds_compor_prep%nb_comp .eq. 0) then
        call utmess('F', 'COMPOR4_63')
    endif
!
! - Read informations from command file
!
    call comp_meca_read(l_etat_init, ds_compor_prep)
!
! - Count internal variables
!
    call comp_meca_cvar(ds_compor_prep)
!
! - Save it
!
    i_comp = 1
    nb_vari         = ds_compor_prep%v_comp(i_comp)%nb_vari
    nb_vari_comp(:) = ds_compor_prep%v_comp(i_comp)%nb_vari_comp(:)
    nume_comp(:)    = ds_compor_prep%v_comp(i_comp)%nume_comp(:)
    rela_comp       = ds_compor_prep%v_comp(i_comp)%rela_comp
    defo_comp       = ds_compor_prep%v_comp(i_comp)%defo_comp
    type_comp       = ds_compor_prep%v_comp(i_comp)%type_comp
    type_cpla       = ds_compor_prep%v_comp(i_comp)%type_cpla
    kit_comp(:)     = ds_compor_prep%v_comp(i_comp)%kit_comp(:)
    mult_comp       = ds_compor_prep%v_comp(i_comp)%mult_comp
    type_matg       = ds_compor_prep%v_comp(i_comp)%type_matg
    post_iter       = ds_compor_prep%v_comp(i_comp)%post_iter
    call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
    if (l_kit_thm) then
        call utmess('F', 'COMPOR2_7')
    endif
    if (type_comp .eq. 'COMP_ELAS') then
        incela = 2
    else if (type_comp.eq.'COMP_INCR') then
        incela = 1
    else
        ASSERT(.false.)
    endif
!  
! - Save in list
!
    compor(1)  = rela_comp
    write (compor(2),'(I16)') nb_vari
    compor(3)  = defo_comp
    compor(4)  = type_comp
    write (compor(6),'(I16)') nume_comp(1)
    compor(7)  = ' '
    compor(8)  = kit_comp(1)
    compor(9)  = kit_comp(2)
    compor(10) = kit_comp(3)
    compor(11) = kit_comp(4)
    write (compor(12),'(I16)') i_comp
    compor(13) = type_matg
    compor(14) = post_iter
    write (compor(15),'(I16)') nume_comp(2)
    write (compor(16),'(I16)') nume_comp(3)
    write (compor(17),'(I16)') nb_vari_comp(1)
    write (compor(18),'(I16)') nb_vari_comp(2)
    write (compor(19),'(I16)') nb_vari_comp(3)
    write (compor(20),'(I16)') nb_vari_comp(4)
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(compor_list_ = compor, compor_info = compor_info)
!
! - Print informations about internal variables
!
    call imvari(compor_info)
!
! - Create carcri informations objects
!
    call carc_info(ds_compor_para)
!
! - Read informations from command file
!
    call carc_read(ds_compor_para)
!  
! - Save in list
!
    i_comp = 1
    algo_inte = ds_compor_para%v_para(i_comp)%algo_inte
    if (rela_comp.eq.'MFRONT') then
        call nmdocv(keywordfact, i_comp, algo_inte, 'RESI_INTE_MAXI', resi_inte_rela)
    else
        call nmdocv(keywordfact, i_comp, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
    endif
    call nmdocv(keywordfact, i_comp, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
    call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
    carcri(1)  = iter_inte_maxi
    carcri(2)  = ds_compor_para%v_para(i_comp)%type_matr_t
    carcri(3)  = resi_inte_rela
    carcri(4)  = ds_compor_para%v_para(i_comp)%parm_theta
    carcri(5)  = ds_compor_para%v_para(i_comp)%iter_inte_pas
    carcri(6)  = algo_inte_r
    carcri(7)  = ds_compor_para%v_para(i_comp)%vale_pert_rela
    carcri(8)  = ds_compor_para%v_para(i_comp)%resi_deborst_max
    carcri(9)  = ds_compor_para%v_para(i_comp)%iter_deborst_max
    carcri(10) = ds_compor_para%v_para(i_comp)%seuil
    carcri(11) = ds_compor_para%v_para(i_comp)%amplitude
    carcri(12) = ds_compor_para%v_para(i_comp)%taux_retour
    carcri(13) = ds_compor_para%v_para(i_comp)%post_iter
    carcri(14) = ds_compor_para%v_para(i_comp)%c_pointer%nbvarext
    carcri(15) = ds_compor_para%v_para(i_comp)%c_pointer%namevarext
    carcri(16) = ds_compor_para%v_para(i_comp)%c_pointer%fct_ldc
    carcri(17) = 1
    carcri(18) = 0
    carcri(19) = ds_compor_para%v_para(i_comp)%c_pointer%matprop
    carcri(20) = ds_compor_para%v_para(i_comp)%c_pointer%nbprop
    carcri(21) = ds_compor_para%v_para(i_comp)%post_incr
!
! - Cleaning
!
    deallocate(ds_compor_prep%v_comp)
    deallocate(ds_compor_para%v_para)
!
    call jedema()
!
end subroutine
