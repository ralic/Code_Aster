subroutine romMultiParaROM2mbrCreate(ds_empi       , ds_multipara, i_coef,&
                                     syst_2mbr_type, syst_2mbr   , syst_2mbrROM)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "blas/zdotc.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(ROM_DS_Empi), intent(in) :: ds_empi
    type(ROM_DS_MultiPara), intent(inout) :: ds_multipara
    integer, intent(in) :: i_coef
    character(len=1), intent(in) :: syst_2mbr_type
    character(len=19), intent(in) :: syst_2mbr
    character(len=19), intent(in) :: syst_2mbrROM
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Create second member for multiparametric problems (reduced problem)
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_empi          : datastructure for empiric modes
! IO  ds_multipara     : datastructure for multiparametric problems
! In  i_coef           : index of coefficient
! In  syst_2mbr_type   : type of second member (R or C)
! In  syst_2mbr        : second member
! In  syst_2mbrROM     : second member on reduced model
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nb_mode, nb_equa
    integer :: i_mode, i_equa, iret
    character(len=24) :: field_iden
    character(len=8) :: base
    complex(kind=8) :: term
    aster_logical :: l_coef_cplx, l_coef_real
    real(kind=8) :: coef_r
    complex(kind=8) :: coef_c, coef_cplx
    character(len=19) :: mode
    complex(kind=8), pointer :: v_mode(:) => null()
    complex(kind=8), pointer :: v_syst_2mbc(:) => null()
    real(kind=8), pointer :: v_syst_2mbr(:) => null()
    complex(kind=8), pointer :: v_syst_2mbp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        call utmess('I', 'ROM5_66')
    endif
!
! - Initializations
!
    base           = ds_empi%base
    nb_mode        = ds_empi%nb_mode
    nb_equa        = ds_empi%nb_equa
!
! - Get second member
!
    if (syst_2mbr_type .eq. 'C') then
        call jeveuo(syst_2mbr(1:19)//'.VALE', 'L', vc = v_syst_2mbc)
    elseif (syst_2mbr_type .eq. 'R') then
        call jeveuo(syst_2mbr(1:19)//'.VALE', 'L', vr = v_syst_2mbr)
    else
        ASSERT(.false.)
    endif
    call jeveuo(syst_2mbrROM, 'E', vc = v_syst_2mbp)
!
! - Apply coefficient on second member
!
    l_coef_cplx = ds_multipara%vect_coef%l_cplx
    l_coef_real = ds_multipara%vect_coef%l_real 

    do i_mode = 1, nb_mode
        if (l_coef_cplx) then
            coef_c    = ds_multipara%vect_coef%coef_cplx(i_coef)
            coef_cplx = coef_c
        else
            coef_r    = ds_multipara%vect_coef%coef_real(i_coef)
            coef_cplx = dcmplx(coef_r)
        endif
        field_iden = 'DEPL'
        call rsexch(' ', base, field_iden, i_mode, mode, iret)
        call jeveuo(mode(1:19)//'.VALE', 'L', vc = v_mode)
        term = dcmplx(0.d0, 0.d0)
        do i_equa = 1, nb_equa
            term = term + coef_cplx*dcmplx(v_syst_2mbc(i_equa))*dconjg(v_mode(i_equa))
        end do
        v_syst_2mbp(i_mode) = term
        !WRITE(6,*) 'Secred: ', i_mode, coef_cplx, term
    end do
!
end subroutine
