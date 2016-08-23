subroutine dfllpe(keywf    , i_fail        , event_type,&
                  vale_ref , nom_cham      , nom_cmp   , crit_cmp,&
                  pene_maxi, resi_glob_maxi)
!
implicit none
!
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
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
!
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: i_fail
    character(len=16), intent(in) :: event_type
    real(kind=8), intent(out) :: vale_ref
    character(len=16), intent(out) :: nom_cham
    character(len=16), intent(out) :: nom_cmp
    character(len=16), intent(out) :: crit_cmp
    real(kind=8), intent(out) :: pene_maxi
    real(kind=8), intent(out) :: resi_glob_maxi
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_LIST_INST
!
! Get parameters of EVENEMENT for current failure keyword
!
! --------------------------------------------------------------------------------------------------
!
! In  keywf            : factor keyword to read failures
! In  i_fail           : index of current factor keyword to read failure
! In  event_type       : type of event
! Out vale_ref         : value of VALE_REF for EVENEMENT=DELTA_GRANDEUR
! Out nom_cham         : value of NOM_CHAM for EVENEMENT=DELTA_GRANDEUR
! Out nom_cmp          : value of NOM_CMP for EVENEMENT=DELTA_GRANDEUR
! Out crit_cmp         : value of CRIT_CMP for EVENEMENT=DELTA_GRANDEUR
! Out pene_maxi        : value of PENE_MAXI for EVENEMENT=INTERPENETRATION
! Out resi_glob_maxi   : value of RESI_GLOB_MAXI for EVENEMENT=RESI_MAXI
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc
!
! --------------------------------------------------------------------------------------------------
!
    pene_maxi      = 0.d0
    vale_ref       = 0.d0
    resi_glob_maxi = 0.d0
    nom_cham       = ' '
    nom_cmp        = ' '
    crit_cmp       = ' '
!
! - Read parameters
!
    if (event_type .eq. 'DELTA_GRANDEUR') then
        call getvr8(keywf, 'VALE_REF', iocc=i_fail, scal=vale_ref, nbret=nocc)
        ASSERT(nocc .gt. 0)
        call getvtx(keywf, 'NOM_CHAM', iocc=i_fail, scal=nom_cham, nbret=nocc)
        ASSERT(nocc .gt. 0)
        call getvtx(keywf, 'NOM_CMP', iocc=i_fail, scal=nom_cmp, nbret=nocc)
        ASSERT(nocc .gt. 0)
        crit_cmp = 'GT'
    else if (event_type.eq.'INTERPENETRATION') then
        call getvr8(keywf, 'PENE_MAXI', iocc=i_fail, scal=pene_maxi, nbret=nocc)
        ASSERT(nocc .gt. 0)
    else if (event_type.eq.'RESI_MAXI') then
        call getvr8(keywf, 'RESI_GLOB_MAXI', iocc=i_fail, scal=resi_glob_maxi, nbret=nocc)
        ASSERT(nocc .gt. 0)
    endif
!
end subroutine
