subroutine nmetcc(field_type, algo_name, init_name,&
                  compor    , sddyna   , sdpost   , ds_contact,&
                  hydr      , temp_init, hydr_init)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/ndynkk.h"
#include "asterfort/nmlesd.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: field_type
    character(len=24), intent(out) :: algo_name
    character(len=24), intent(out) :: init_name
    type(NL_DS_Contact), optional, intent(in) :: ds_contact
    character(len=19), optional, intent(in) :: compor
    character(len=19), optional, intent(in) :: sddyna
    character(len=19), optional, intent(in) :: sdpost
    character(len=24), optional, intent(in) :: hydr
    character(len=24), optional, intent(in) :: hydr_init
    character(len=24), optional, intent(in) :: temp_init
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Get name of field during non-linear algorithm and initial state
!
! --------------------------------------------------------------------------------------------------
!
! In  field_type       : type of field (symbolic name in result datastructure)
! Out algo_name        : name of field during non-linear algorithm
!                       If 'XXXXXXXXXXXXXXXX' => already defined during DS creation
! Out init_name        : name of field for initial state
!                       If 'XXXXXXXXXXXXXXXX' => already defined during DS creation
! In  compor           : name of <CARTE> COMPOR
! In  ds_contact       : datastructure for contact management
! In  sddyna           : name of dynamic parameters datastructure
! In  sdpost           : name of post-treatment for stability analysis parameters datastructure
! In  hydr             : name of field for hydratation (HYDR_ELNO)
! In  hydr_init        : name of field for initial hydratation
! In  temp_init        : name of field for initial temperature
!
!     if algo_name = #H#TYPCHA#
!       => field name is the TYPCHA "hat" variable datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: xindco, xcohes, xseuco
    character(len=24) :: nochco, sdcont_solv
    character(len=24), pointer :: cont_sdname(:) => null()
    character(len=19) :: vecfla, vecvib, vecsta
    character(len=19) :: depabs, vitabs, accabs
    real(kind=8) :: r8bid
    integer :: ibid, iret
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_solv = ' '
    if (present(ds_contact)) then
        sdcont_solv = ds_contact%sdcont_solv
    endif
!
! - Special fields
!
    if (present(ds_contact)) then
        xindco = sdcont_solv(1:14)//'.XFIN'
        xcohes = sdcont_solv(1:14)//'.XCOP'
        xseuco = sdcont_solv(1:14)//'.XFSE'
    endif
    if (present(sdpost)) then
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_FLAM', ibid, r8bid,&
                    vecfla)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_STAB', ibid, r8bid,&
                    vecsta)
        call nmlesd('POST_TRAITEMENT', sdpost, 'SOLU_MODE_VIBR', ibid, r8bid,&
                    vecvib)
    endif
    if (present(sddyna)) then
        call ndynkk(sddyna, 'DEPABS', depabs)
        call ndynkk(sddyna, 'VITABS', vitabs)
        call ndynkk(sddyna, 'ACCABS', accabs)
    endif
!
! - Standard fields
!
    if (field_type.eq.'COMPORTEMENT') then
        algo_name = compor
        init_name = ' '
    else if (field_type.eq.'CONT_NOEU') then
        nochco = sdcont_solv(1:14)//'.NOCHCO'
        call jeexin(nochco, iret)
        if (iret.ne.0) then
            call jeveuo(nochco, 'L', vk24 = cont_sdname)
            algo_name = cont_sdname(2)(1:19)
        endif
        init_name = ' '
    else if (field_type.eq.'INDC_ELEM') then
        algo_name = xindco
        init_name = sdcont_solv(1:14)//'.XFI0'
    else if (field_type.eq.'SECO_ELEM') then
        algo_name = xseuco
        init_name = sdcont_solv(1:14)//'.XFS0'
    else if (field_type.eq.'COHE_ELEM') then
        algo_name = xcohes
        init_name = sdcont_solv(1:14)//'.XCO0'
    else if (field_type.eq.'MODE_FLAMB') then
        algo_name = vecfla
        init_name = ' '
    else if (field_type.eq.'MODE_STAB') then
        algo_name = vecsta
        init_name = ' '
    else if (field_type.eq.'DEPL_VIBR') then
        algo_name = vecvib
        init_name = ' '
    else if (field_type.eq.'DEPL_ABSOLU') then
        algo_name = depabs
        init_name = '&&CNPART.ZERO'
    else if (field_type.eq.'VITE_ABSOLU') then
        algo_name = vitabs
        init_name = '&&CNPART.ZERO'
    else if (field_type.eq.'ACCE_ABSOLU') then
        algo_name = accabs
        init_name = '&&CNPART.ZERO'
    else if (field_type.eq.'TEMP') then
        algo_name = 'XXXXXXXXXXXXXXXX'
        init_name = temp_init
    else if (field_type.eq.'HYDR_ELNO') then
        algo_name = hydr
        init_name = hydr_init
    else if (field_type.eq.'COMPORTHER') then
        algo_name = compor
        init_name = ' '
    else
        algo_name = 'XXXXXXXXXXXXXXXX'
        init_name = 'XXXXXXXXXXXXXXXX'
    endif
!
end subroutine
