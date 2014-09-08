subroutine nmetcc(field_type     , field_name_algo, field_name_init,&
                  compor         , sddyna         , sdpost         , sdcont_algo,&
                  hydr           , temp_init      , hydr_init)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/ndynkk.h"
#include "asterfort/nmlesd.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: field_type
    character(len=24), intent(out) :: field_name_algo
    character(len=24), intent(out) :: field_name_init
    character(len=24), optional, intent(in) :: sdcont_algo
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
! Out field_name_algo  : name of field during non-linear algorithm
! Out field_name_init  : name of field for initial state
! In  compor           : name of <CARTE> COMPOR
! In  sdcont_algo      : name of contact algorithm datastructure
! In  sddyna           : name of dynamic parameters datastructure
! In  sdpost           : name of post-treatment for stability analysis parameters datastructure
! In  hydr             : name of field for hydratation (HYDR_ELNO)
! In  hydr_init        : name of field for initial hydratation
! In  temp_init        : name of field for initial temperature
!
!     if field_name_algo = CHAP#TYPCHA#
!       => field name is the TYPCHA "hat" variable datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: xindco, xcohes, xseuco
    character(len=24) :: nochco
    character(len=24), pointer :: cont_sdname(:) => null()
    character(len=19) :: vecfla, vecvib, vecsta
    character(len=19) :: depabs, vitabs, accabs
    real(kind=8) :: r8bid
    integer :: ibid
!
! --------------------------------------------------------------------------------------------------
!
!
! - Special fields
!
    if (present(sdcont_algo)) then
        xindco = sdcont_algo(1:14)//'.XFIN'
        xcohes = sdcont_algo(1:14)//'.XCOH'
        xseuco = sdcont_algo(1:14)//'.XFSE'
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
    if (field_type .eq. 'DEPL') then
        field_name_algo = 'CHAP#VALINC#DEPMOI'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'VITE') then
        field_name_algo = 'CHAP#VALINC#VITMOI'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'ACCE') then
        field_name_algo = 'CHAP#VALINC#ACCMOI'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'SIEF_ELGA') then
        field_name_algo = 'CHAP#VALINC#SIGMOI'
        field_name_init = '&&NMETCR.SIGMO0'
    else if (field_type.eq.'VARI_ELGA') then
        field_name_algo = 'CHAP#VALINC#VARMOI'
        field_name_init = '&&NMETCR.VARMO0'
    else if (field_type.eq.'STRX_ELGA') then
        field_name_algo = 'CHAP#VALINC#STRMOI'
        field_name_init = '&&NMETCR.STRMO0'
    else if (field_type.eq.'COMPORTEMENT') then
        field_name_algo = compor
        field_name_init = ' '
    else if (field_type.eq.'VALE_CONT') then
        nochco = sdcont_algo(1:14)//'.NOCHCO'
        call jeveuo(nochco, 'L', vk24 = cont_sdname)
        field_name_algo = cont_sdname(2)(1:19)
        field_name_init = ' '
    else if (field_type.eq.'INDC_ELEM') then
        field_name_algo = xindco
        field_name_init = sdcont_algo(1:14)//'.XFI0'
    else if (field_type.eq.'SECO_ELEM') then
        field_name_algo = xseuco
        field_name_init = sdcont_algo(1:14)//'.XFS0'
    else if (field_type.eq.'COHE_ELEM') then
        field_name_algo = xcohes
        field_name_init = sdcont_algo(1:14)//'.XCO0'
    else if (field_type.eq.'MODE_FLAMB') then
        field_name_algo = vecfla
        field_name_init = ' '
    else if (field_type.eq.'MODE_STAB') then
        field_name_algo = vecsta
        field_name_init = ' '
    else if (field_type.eq.'DEPL_VIBR') then
        field_name_algo = vecvib
        field_name_init = ' '
    else if (field_type.eq.'DEPL_ABSOLU') then
        field_name_algo = depabs
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'VITE_ABSOLU') then
        field_name_algo = vitabs
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'ACCE_ABSOLU') then
        field_name_algo = accabs
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'FORC_NODA') then
        field_name_algo = 'CHAP#VEASSE#CNFINT'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'FORC_AMOR') then
        field_name_algo = 'CHAP#VALINC#FAMMOI'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'FORC_LIAI') then
        field_name_algo = 'CHAP#VALINC#FLIMOI'
        field_name_init = '&&CNPART.ZERO'
    else if (field_type.eq.'EPSI_ELGA') then
        field_name_algo = '&&NMETCR.EPSI'
        field_name_init = '&&NMETCR.EPSI'
!
    else if (field_type.eq.'TEMP') then
        field_name_algo = 'CHAP#VALINC#TEMP'
        field_name_init = temp_init
    else if (field_type.eq.'HYDR_ELNO') then
        field_name_algo = hydr
        field_name_init = hydr_init
    else if (field_type.eq.'COMPORTHER') then
        field_name_algo = compor
        field_name_init = ' '
!
    else
        ASSERT(.false.)
    endif
!
end subroutine
