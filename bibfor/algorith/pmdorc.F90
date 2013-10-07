subroutine pmdorc(compor, carcri, nb_vari, incela)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/zaswri.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/carc_read.h"
#include "asterfort/comp_meca_cvar.h"
#include "asterfort/comp_meca_l.h"
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
    character(len=24) :: list_vale
    integer :: j_lvalk, j_lvali, j_lnvar, j_lvalr
    integer :: iocc, nbocc, unit_comp, i, nume_comp
    integer :: nbocc1, nbocc2, nbocc3
    character(len=16) :: keywordfact
    character(len=16) :: rela_comp, algo_inte, type_matg, post_iter, defo_comp, type_comp, mult_comp
    logical :: l_cristal, l_zmat, l_exte_comp, l_matr_tgsc, l_crit_rupt, l_kit_thm, l_etat_init
    real(kind=8) :: algo_inte_r, iter_inte_maxi, resi_inte_rela
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    list_vari_name = '&&PMDORC.LIST_VARI'
    list_vale      = '&&PMDORC.LIST_VALE'
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
! - Read informations from command file
!
    call comp_meca_read(list_vale, l_etat_init, nbocc)
    ASSERT(nbocc.eq.1)
!
! - Count internal variables
!
    call comp_meca_cvar(list_vale)
!
! - Save it
!
    iocc = 1
    call jeveuo(list_vale(1:19)//'.NVAR', 'L', j_lnvar)
    call jeveuo(list_vale(1:19)//'.VALK', 'L', j_lvalk)
    call jeveuo(list_vale(1:19)//'.VALI', 'L', j_lvali)
    nb_vari      = zi(j_lnvar+10*(iocc-1) -1 + 2)
    nume_comp    = zi(j_lnvar+10*(iocc-1) -1 + 1)
    unit_comp    = zi(j_lvali+2*(iocc-1) -1 + 2)
    rela_comp    = zk24(j_lvalk+16*(iocc-1) -1 + 1)(1:16)
    defo_comp    = zk24(j_lvalk+16*(iocc-1) -1 + 2)(1:16)
    type_comp    = zk24(j_lvalk+16*(iocc-1) -1 + 3)(1:16)
    mult_comp    = zk24(j_lvalk+16*(iocc-1) -1 + 14)(1:16)
    type_matg    = zk24(j_lvalk+16*(iocc-1) -1 + 15)(1:16)
    post_iter    = zk24(j_lvalk+16*(iocc-1) -1 + 16)(1:16)
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
    compor(8)  = zk24(j_lvalk+16*(iocc-1) -1 + 5)(1:16)
    compor(9)  = zk24(j_lvalk+16*(iocc-1) -1 + 6)(1:16)
    compor(10) = zk24(j_lvalk+16*(iocc-1) -1 + 7)(1:16)
    compor(11) = zk24(j_lvalk+16*(iocc-1) -1 + 8)(1:16)
    if (l_exte_comp) then
        compor(12) = zk24(j_lvalk+16*(iocc-1) -1 + 9)(1:16)
    else
        write (compor(12),'(I16)') iocc
    endif
    if (l_exte_comp) then
        if (l_matr_tgsc) call utmess('F','COMPOR4_59')
        if (l_crit_rupt) call utmess('F','COMPOR4_60')
        compor(13) = zk24(j_lvalk+16*(iocc-1) -1 + 10)(1:16)
        compor(14) = zk24(j_lvalk+16*(iocc-1) -1 + 11)(1:16)
    else
        compor(13) = zk24(j_lvalk+16*(iocc-1) -1 + 15)(1:16)
        compor(14) = zk24(j_lvalk+16*(iocc-1) -1 + 16)(1:16)
    endif
    compor(15) = zk24(j_lvalk+16*(iocc-1) -1 + 12)(1:16)
    compor(16) = zk24(j_lvalk+16*(iocc-1) -1 + 13)(1:16)
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
    call jedetr(list_vale(1:19)//'.NVAR')
    call jedetr(list_vale(1:19)//'.VALK')
    call jedetr(list_vale(1:19)//'.VALI')
!
! - Prepare informations about internal variables
!
    call comp_meca_pvar(list_vari_name, compor_list = compor)
!
! - Print informations about internal variables
!
    call imvari(list_vari_name, compor_list = compor)
!
! - Read informations from command file
!
    call carc_read(list_vale, nbocc)
    ASSERT(nbocc.eq.1)
!
! - Save it
!
    call jeveuo(list_vale(1:19)//'.VALR', 'L', j_lvalr)
    call jeveuo(list_vale(1:19)//'.VALK', 'L', j_lvalk)
    iocc = 1
!
    algo_inte  = zk24(j_lvalk+2*(iocc-1) -1 + 2)(1:16)
    call nmdocv(keywordfact, iocc, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
    call nmdocv(keywordfact, iocc, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
    call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
    carcri(1)  =  iter_inte_maxi
    carcri(2)  =  zr(j_lvalr+13*(iocc-1) -1 + 2) 
    carcri(3)  =  resi_inte_rela
    carcri(4)  =  zr(j_lvalr+13*(iocc-1) -1 + 4) 
    carcri(5)  =  zr(j_lvalr+13*(iocc-1) -1 + 5) 
    carcri(6)  =  algo_inte_r
    carcri(7)  =  zr(j_lvalr+13*(iocc-1) -1 + 7) 
    carcri(8)  =  zr(j_lvalr+13*(iocc-1) -1 + 8) 
    carcri(9)  =  zr(j_lvalr+13*(iocc-1) -1 + 9) 
    carcri(10) =  zr(j_lvalr+13*(iocc-1) -1 + 10)
    carcri(11) =  zr(j_lvalr+13*(iocc-1) -1 + 11)
    carcri(12) =  zr(j_lvalr+13*(iocc-1) -1 + 12)
    carcri(13) =  zr(j_lvalr+13*(iocc-1) -1 + 13)
!
    call jedetr(list_vale(1:19)//'.VALR')
    call jedetr(list_vale(1:19)//'.VALK')
!
! - Init ZASTER_HANDLER
!
    if (l_zmat) call zaswri()
!
    call jedema()
!
end subroutine
