subroutine nxnoli(modele, mate, carele, lostat, lnonl   ,&
                  levol , para, sddisc, sdcrit, ds_inout)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infniv.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ntarch.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsrusd.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_InOut), intent(inout) :: ds_inout
    real(kind=8) :: para(*)
    aster_logical :: lnonl, lostat, levol
    character(len=19) :: sddisc, sdcrit
    character(len=24) :: modele, mate, carele
!
! --------------------------------------------------------------------------------------------------
!
! THER_* - Init
!
! Prepare storing
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdarch
    character(len=24) :: sdarch_ainf
    integer, pointer :: v_sdarch_ainf(:) => null()
    integer :: numarc, numins
    integer :: ifm, niv
    aster_logical :: force, lreuse
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    call infniv(ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<THERMIQUE> PREPARATION DE LA SD EVOL_THER'
    endif
!
! - INSTANT INITIAL
!
    numins = 0
    force = .true.
!
! - Get parameters in input/ouput management datastructure
!
    result = ds_inout%result
    lreuse = ds_inout%l_reuse
!
! --- ACCES SD ARCHIVAGE
!
    sdarch      = sddisc(1:14)//'.ARCH'
    sdarch_ainf = sdarch(1:19)//'.AINF'
!
! - Current storing index
!
    call jeveuo(sdarch_ainf, 'L', vi = v_sdarch_ainf)
    numarc = v_sdarch_ainf(1)
!
! --- CREATION DE LA SD EVOL_THER OU NETTOYAGE DES ANCIENS NUMEROS
!
    if (lreuse) then
        ASSERT(numarc.ne.0)
        call rsrusd(result, numarc)
    else
        ASSERT(numarc.eq.0)
        call rscrsd('G', result, 'EVOL_THER', 100)
    endif
!
! --- ARCHIVAGE ETAT INITIAL
!
    if ((.not.lreuse) .and. (.not.lostat) .and. levol) then
        call utmess('I', 'ARCHIVAGE_4')
        call ntarch(numins, modele, mate  , carele  , lnonl,&
                    para  , sddisc, sdcrit, ds_inout, force)
    endif
!
end subroutine
