subroutine pmdorc(compor, carcri, nb_vari, incela)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_deallocate.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=16), intent(out) :: compor(20)
    real(kind=8), intent(out) :: carcri(21)
    integer, intent(out) :: nb_vari
    integer, intent(out) :: incela
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
    integer, parameter :: carsiz=20
    character(len=19) :: compor_info
    integer :: iocc, nume_comp(4), nbocc_compor, nbocc_carcri, nb_vari_comp(4)
    integer :: nbocc1, nbocc2, nbocc3
    character(len=16) :: keywordfact
    character(len=16) :: rela_comp, algo_inte, defo_comp, type_comp
    character(len=16) :: mult_comp, kit_comp(4), type_cpla, type_matg, post_iter
    aster_logical :: l_kit_thm, l_etat_init
    real(kind=8) :: algo_inte_r, iter_inte_maxi, resi_inte_rela
    character(len=16), pointer :: p_info_comp_valk(:) => null()
    integer, pointer :: p_info_comp_vali(:) => null()
    integer, pointer :: p_info_comp_nvar(:) => null()
    character(len=16), pointer :: p_info_carc_valk(:) => null()
    real(kind=8), pointer :: p_info_carc_valr(:) => null()
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
! - Create comportment informations objects
!
    call comp_meca_info(p_info_comp_valk, p_info_comp_vali, p_info_comp_nvar, nbocc_compor)
    if (nbocc_compor .eq. 0) then
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
    nb_vari         = p_info_comp_nvar(10*(iocc-1) + 1)
    nb_vari_comp(1) = p_info_comp_nvar(10*(iocc-1) + 2)
    nb_vari_comp(2) = p_info_comp_nvar(10*(iocc-1) + 3)
    nb_vari_comp(3) = p_info_comp_nvar(10*(iocc-1) + 4)
    nb_vari_comp(4) = p_info_comp_nvar(10*(iocc-1) + 5)
    nume_comp(1)    = p_info_comp_nvar(10*(iocc-1) + 6)
    nume_comp(2)    = p_info_comp_nvar(10*(iocc-1) + 7)
    nume_comp(3)    = p_info_comp_nvar(10*(iocc-1) + 8)
    nume_comp(4)    = p_info_comp_nvar(10*(iocc-1) + 9)
    rela_comp       = p_info_comp_valk(16*(iocc-1) + 1)
    defo_comp       = p_info_comp_valk(16*(iocc-1) + 2)
    type_comp       = p_info_comp_valk(16*(iocc-1) + 3)
    type_cpla       = p_info_comp_valk(16*(iocc-1) + 4)
    kit_comp(1)     = p_info_comp_valk(16*(iocc-1) + 5)
    kit_comp(2)     = p_info_comp_valk(16*(iocc-1) + 6)
    kit_comp(3)     = p_info_comp_valk(16*(iocc-1) + 7)
    kit_comp(4)     = p_info_comp_valk(16*(iocc-1) + 8)
    mult_comp       = p_info_comp_valk(16*(iocc-1) + 14)
    type_matg       = p_info_comp_valk(16*(iocc-1) + 15)
    post_iter       = p_info_comp_valk(16*(iocc-1) + 16)
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
    compor(1)  = rela_comp
    write (compor(2),'(I16)') nb_vari
    compor(3)  = defo_comp
    compor(4)  = type_comp
    write (compor(6),'(I16)') nume_comp(1)
    compor(7)  = mult_comp
    compor(8)  = kit_comp(1)
    compor(9)  = kit_comp(2)
    compor(10) = kit_comp(3)
    compor(11) = kit_comp(4)
    write (compor(12),'(I16)') iocc
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
    call comp_meca_pvar(compor_list_ = compor, compor_info = compor_info,&
                        l_list_elem_ = .true._1, l_info_full_ = .true._1)
!
! - Print informations about internal variables
!
    call imvari(compor_info)
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
    algo_inte = p_info_carc_valk(2*(iocc-1) + 2)
    if (rela_comp.eq.'MFRONT') then
        call nmdocv(keywordfact, iocc, algo_inte, 'RESI_INTE_MAXI', resi_inte_rela)
    else
        call nmdocv(keywordfact, iocc, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
    endif
    call nmdocv(keywordfact, iocc, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
    call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
    carcri(1) = iter_inte_maxi
    carcri(2) = p_info_carc_valr(carsiz*(iocc-1) + 2)
    carcri(3) = resi_inte_rela
    carcri(4) = p_info_carc_valr(carsiz*(iocc-1) + 4)
    carcri(5) = p_info_carc_valr(carsiz*(iocc-1) + 5)
    carcri(6) = algo_inte_r
    carcri(7) = p_info_carc_valr(carsiz*(iocc-1) + 7)
    carcri(8) = p_info_carc_valr(carsiz*(iocc-1) + 8)
    carcri(9) = p_info_carc_valr(carsiz*(iocc-1) + 9)
    carcri(10) = p_info_carc_valr(carsiz*(iocc-1) + 10)
    carcri(11) = p_info_carc_valr(carsiz*(iocc-1) + 11)
    carcri(12) = p_info_carc_valr(carsiz*(iocc-1) + 12)
    carcri(13) = p_info_carc_valr(carsiz*(iocc-1) + 13)
    carcri(14) = p_info_carc_valr(carsiz*(iocc-1) + 14)
    carcri(15) = p_info_carc_valr(carsiz*(iocc-1) + 15)
    carcri(16) = p_info_carc_valr(carsiz*(iocc-1) + 16)
    carcri(17) = 1
    carcri(18) = 0
    carcri(19) = p_info_carc_valr(carsiz*(iocc-1) + 19)
    carcri(20) = p_info_carc_valr(carsiz*(iocc-1) + 20)
    carcri(21) = p_info_carc_valr(carsiz*(iocc-1) + 21)
!
    AS_DEALLOCATE(vk16 = p_info_comp_valk)
    AS_DEALLOCATE(vi   = p_info_comp_vali)
    AS_DEALLOCATE(vi   = p_info_comp_nvar)
    AS_DEALLOCATE(vk16 = p_info_carc_valk)
    AS_DEALLOCATE(vr   = p_info_carc_valr)
!
    call jedema()
!
end subroutine
