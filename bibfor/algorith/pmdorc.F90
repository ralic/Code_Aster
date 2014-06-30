subroutine pmdorc(compor, carcri, nb_vari, incela)
!
    implicit none
!
#include "asterfort/as_deallocate.h"
#include "asterc/zaswri.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-michel.proix at edf.fr
!
    character(len=16), intent(out) :: compor(20)
    real(kind=8), intent(out) :: carcri(13)
    integer, intent(out) :: nb_vari
    integer, intent(out) :: incela
!
! --------------------------------------------------------------------------------------------------
!
!           OPERATEUR    CALC_POINT_MAT : LECTURE COMPOR ET CARCRI
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
    character(len=19) :: list_vari_name
    integer :: iocc, unit_comp, i, nume_comp, nbocc_compor, nbocc_carcri
    integer :: nbocc1, nbocc2, nbocc3
    character(len=16) :: keywordfact
    character(len=16) :: rela_comp, algo_inte, type_matg, post_iter, defo_comp, type_comp, mult_comp
    logical(kind=1) :: l_cristal, l_zmat, l_exte_comp, l_matr_tgsc, l_crit_rupt, l_kit_thm, l_etat_init
    real(kind=8) :: algo_inte_r, iter_inte_maxi, resi_inte_rela
    character(len=16), pointer :: p_info_comp_valk(:) => null()
    integer          , pointer :: p_info_comp_vali(:) => null()
    integer          , pointer :: p_info_comp_nvar(:) => null()
    character(len=16), pointer :: p_info_carc_valk(:) => null()
    real(kind=8)     , pointer :: p_info_carc_valr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    list_vari_name = '&&PMDORC.LIST_VARI'
    keywordfact    = 'COMPORTEMENT'
    do i = 1,20
        compor(i) = 'VIDE'
    end do
!
! - Initial state
!
    call getfac('SIGM_INIT', nbocc1)
    call getfac('EPSI_INIT', nbocc2)
    call getfac('VARI_INIT', nbocc3)
    l_etat_init = (nbocc1+nbocc2+nbocc3) > 0
!
! - Create comportment informations objects
!
    call comp_meca_info(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar, nbocc_compor)
    if (nbocc_compor.eq.0) then
        call utmess('F', 'COMPOR4_63')
    endif
!
! - Read informations from command file
!
    call comp_meca_read(l_etat_init, p_info_comp_valk, p_info_comp_vali)
!
! - Count internal variables
!
    call comp_meca_cvar(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar)
!
! - Save it
!
    iocc = 1
    nb_vari      = p_info_comp_nvar(10*(iocc-1) + 2)
    nume_comp    = p_info_comp_nvar(10*(iocc-1) + 1)
    unit_comp    = p_info_comp_vali(2*(iocc-1) + 2)
    rela_comp    = p_info_comp_valk(16*(iocc-1) + 1)
    defo_comp    = p_info_comp_valk(16*(iocc-1) + 2)
    type_comp    = p_info_comp_valk(16*(iocc-1) + 3)
    mult_comp    = p_info_comp_valk(16*(iocc-1) + 14)
    type_matg    = p_info_comp_valk(16*(iocc-1) + 15)
    post_iter    = p_info_comp_valk(16*(iocc-1) + 16)
    call comp_meca_l(rela_comp, 'MATR_TGSC', l_matr_tgsc, type_matg = type_matg)
    call comp_meca_l(rela_comp, 'CRIT_RUPT', l_crit_rupt, post_iter = post_iter)
    call comp_meca_l(rela_comp, 'ZMAT'     , l_zmat)
    call comp_meca_l(rela_comp, 'CRISTAL'  , l_cristal)
    call comp_meca_l(rela_comp, 'EXTE_COMP', l_exte_comp)
    call comp_meca_l(rela_comp, 'KIT_THM'  , l_kit_thm)
    if (l_kit_thm) then
        call utmess('F', 'COMPOR2_7')
    endif
!
    if (type_comp.eq.'COMP_ELAS') then
        incela = 2
    elseif (type_comp.eq.'COMP_INCR') then
        incela = 1
    else
        ASSERT(.false.)
    endif
!
    compor(1)  = rela_comp
    write (compor(2),'(I16)') nb_vari
    compor(3)  = defo_comp
    compor(4)  = type_comp    
    write (compor(6),'(I16)') nume_comp
    if (l_cristal) then
        compor(7) = mult_comp
    else
         write (compor(7),'(I16)') unit_comp
    endif
    compor(8)  = p_info_comp_valk(16*(iocc-1) + 5)
    compor(9)  = p_info_comp_valk(16*(iocc-1) + 6)
    compor(10) = p_info_comp_valk(16*(iocc-1) + 7)
    compor(11) = p_info_comp_valk(16*(iocc-1) + 8)
    if (l_exte_comp) then
        compor(12) = p_info_comp_valk(16*(iocc-1) + 9)
    else
        write (compor(12),'(I16)') iocc
    endif
    if (l_exte_comp) then
        if (l_matr_tgsc) call utmess('F','COMPOR4_59')
        if (l_crit_rupt) call utmess('F','COMPOR4_60')
        compor(13) = p_info_comp_valk(16*(iocc-1) + 10)
        compor(14) = p_info_comp_valk(16*(iocc-1) + 11)
    else
        compor(13) = p_info_comp_valk(16*(iocc-1) + 15)
        compor(14) = p_info_comp_valk(16*(iocc-1) + 16)
    endif
    compor(15) = p_info_comp_valk(16*(iocc-1) + 12)
    compor(16) = p_info_comp_valk(16*(iocc-1) + 13)
!
! - No THM
!
    write (compor(17),'(I16)') 0
    write (compor(18),'(I16)') 0
    write (compor(19),'(I16)') 0
    write (compor(20),'(I16)') 0
!
! - For LC0050.F90
!
    compor(17) = 'POINT'
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(list_vari_name, compor_list = compor)
!
! - Print informations about internal variables
!
    call imvari(list_vari_name, compor_list = compor)
!
! - Create carcri informations objects
!
    call carc_info(p_info_carc_valk, p_info_carc_valr, nbocc_carcri)
    ASSERT(nbocc_carcri.ne.0)
!
! - Read informations from command file
!
    call carc_read(p_info_carc_valk, p_info_carc_valr)
    iocc = 1
!
    algo_inte  = p_info_carc_valk(2*(iocc-1) + 2)
    call nmdocv(keywordfact   , iocc, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
    call nmdocv(keywordfact   , iocc, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
    call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
    carcri(1)  =  iter_inte_maxi
    carcri(2)  =  p_info_carc_valr(13*(iocc-1) + 2) 
    carcri(3)  =  resi_inte_rela
    carcri(4)  =  p_info_carc_valr(13*(iocc-1) + 4) 
    carcri(5)  =  p_info_carc_valr(13*(iocc-1) + 5) 
    carcri(6)  =  algo_inte_r
    carcri(7)  =  p_info_carc_valr(13*(iocc-1) + 7) 
    carcri(8)  =  p_info_carc_valr(13*(iocc-1) + 8) 
    carcri(9)  =  p_info_carc_valr(13*(iocc-1) + 9) 
    carcri(10) =  p_info_carc_valr(13*(iocc-1) + 10)
    carcri(11) =  p_info_carc_valr(13*(iocc-1) + 11)
    carcri(12) =  p_info_carc_valr(13*(iocc-1) + 12)
    carcri(13) =  p_info_carc_valr(13*(iocc-1) + 13)
!
    AS_DEALLOCATE(vk16 = p_info_comp_valk)
    AS_DEALLOCATE(vi   = p_info_comp_vali)
    AS_DEALLOCATE(vi   = p_info_comp_nvar)
    AS_DEALLOCATE(vk16 = p_info_carc_valk)
    AS_DEALLOCATE(vr   = p_info_carc_valr)
!
! - Init ZASTER_HANDLER
!
    if (l_zmat) call zaswri()
!
    call jedema()
!
end subroutine
