subroutine nmnoli(sddisc, sderro, ds_constitutive, ds_print , sdcrit  ,&
                  fonact, sddyna, sdpost         , modele   , mate    ,&
                  carele, sdpilo, ds_measure     , ds_energy, ds_inout,&
                  sdcriq)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmarch.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsrusd.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19) :: sddisc, sdcrit, sddyna, sdpost, sdpilo
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    type(NL_DS_Energy), intent(in) :: ds_energy
    character(len=24) :: sderro
    character(len=24) :: modele, mate, carele
    character(len=24) :: sdcriq
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_InOut), intent(inout) :: ds_inout
    integer :: fonact(*)
    type(NL_DS_Print), intent(in) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Prepare storing
!
! --------------------------------------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! In  ds_print         : datastructure for printing parameters
! In  ds_constitutive  : datastructure for constitutive laws management
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  SDDYNA : SD DYNAMIQUE
! IO  ds_inout         : datastructure for input/output management
! IN  SDCRIT : INFORMATIONS RELATIVES A LA CONVERGENCE
! IN  SDPILO : SD PILOTAGE
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDERRO : SD ERREUR
! In  ds_energy        : datastructure for energy management
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdarch
    character(len=24) :: sdarch_ainf
    integer, pointer :: v_sdarch_ainf(:) => null()
    integer :: numarc, numins
    integer :: ifm, niv
    aster_logical :: lreuse
    character(len=8) :: result
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PREPARATION DE LA SD EVOL_NOLI'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lreuse = isfonc(fonact,'REUSE')
!
! --- INSTANT INITIAL
!
    numins = 0
!
! - Get name of result's datastructure
!
    result = ds_inout%result
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
! --- CREATION DE LA SD EVOL_NOLI OU NETTOYAGE DES ANCIENS NUMEROS
!
    if (lreuse) then
        ASSERT(numarc.ne.0)
        call rsrusd(result, numarc)
    else
        ASSERT(numarc.eq.0)
        call rscrsd('G', result, 'EVOL_NOLI', 100)
    endif
!
! --- ARCHIVAGE ETAT INITIAL
!
    if (.not.lreuse) then
        call utmess('I', 'ARCHIVAGE_4')
        call nmarch(numins         , modele  , mate  , carele, fonact   ,&
                    ds_constitutive, ds_print, sddisc, sdpost, sdcrit   ,&
                    ds_measure     , sderro  , sddyna, sdpilo, ds_energy,&
                    ds_inout       , sdcriq  )
    endif
!
end subroutine
