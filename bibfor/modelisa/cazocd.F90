subroutine cazocd(sdcont, keywf, i_zone, nb_cont_zone)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cazouu.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
    character(len=8), intent(in) :: sdcont
    integer, intent(in) :: i_zone
    character(len=16), intent(in) :: keywf
    integer, intent(in) :: nb_cont_zone
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Discrete methods - Get parameters of contact zone
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  keywf            : factor keyword to read
! In  i_zone           : index of contact zone
! In  nb_cont_zone     : number of zones of contact
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zcmdf
    character(len=24) :: sdcont_defi
    integer :: noc
    character(len=16) :: s_glis
    real(kind=8) :: glis_alarm
    real(kind=8) :: coef_pena_frot, coef_pena_cont, coef_frot, coef_matr_frot
    aster_logical :: l_cont_acti, l_frot, l_cont_pena, l_frot_pena
    real(kind=8), pointer :: v_sdcont_caradf(:) => null()
    character(len=24) :: sdcont_caradf
!
! --------------------------------------------------------------------------------------------------
!
    coef_pena_frot = 0.d0
    coef_pena_cont = 0.d0
    coef_frot      = 0.d0
    coef_matr_frot = 0.d0
    glis_alarm     = 0.d0
    s_glis         = ' '
!
! - Datastructure for contact definition
!
    sdcont_defi   = sdcont(1:8)//'.CONTACT'
    sdcont_caradf = sdcont_defi(1:16)//'.CARADF'
    call jeveuo(sdcont_caradf, 'E', vr = v_sdcont_caradf)
    zcmdf = cfmmvd('ZCMDF')
!
! - Parameters
!
    l_cont_acti = cfdisl(sdcont_defi,'CONT_ACTI')
    l_frot      = cfdisl(sdcont_defi,'FROTTEMENT')
    l_frot_pena = cfdisl(sdcont_defi,'FROT_PENA')
    l_cont_pena = cfdisl(sdcont_defi,'CONT_PENA')
!
! - Friction
!
    if (l_frot) then
        call getvr8(keywf, 'COULOMB', iocc=i_zone, scal=coef_frot)
        v_sdcont_caradf(zcmdf*(i_zone-1)+4) = coef_frot
        call getvr8(keywf, 'COEF_MATR_FROT', iocc=i_zone, scal=coef_matr_frot)
        v_sdcont_caradf(zcmdf*(i_zone-1)+1) = coef_matr_frot
    endif
!
! - Parameters of penalization
!
    if (l_cont_pena) then
        call getvr8(keywf, 'E_N', iocc=i_zone, scal=coef_pena_cont, nbret=noc)
        ASSERT(noc.gt.0)
        v_sdcont_caradf(zcmdf*(i_zone-1)+2) = coef_pena_cont
    endif
!
    if (l_frot_pena) then
        call getvr8(keywf, 'E_T', iocc=i_zone, scal=coef_pena_frot, nbret=noc)
        v_sdcont_caradf(zcmdf*(i_zone-1)+3) = coef_pena_frot
    endif
!
! - Bilateral contact: on all zones
!
    if (l_cont_acti) then
        call cazouu(keywf, nb_cont_zone, 'GLISSIERE')
        call getvtx(keywf, 'GLISSIERE', iocc=1, scal=s_glis)
        if (s_glis .eq. 'OUI') then
            v_sdcont_caradf(zcmdf*(i_zone-1)+6) = 1.d0
            call cazouu(keywf, nb_cont_zone, 'ALARME_JEU')
            call getvr8(keywf, 'ALARME_JEU', iocc=1, scal=glis_alarm)
            v_sdcont_caradf(zcmdf*(i_zone-1)+5) = glis_alarm
        endif
    endif
!
end subroutine
