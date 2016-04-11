subroutine nmarch(numins    , modele  , mate  , carele, fonact   ,&
                  carcri    , ds_print, sddisc, sdpost, sdcrit   ,&
                  ds_measure, sderro  , sddyna, sdpilo, ds_energy,&
                  ds_inout  , sdcriq  )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/diinst.h"
#include "asterfort/dinuar.h"
#include "asterfort/nmarc0.h"
#include "asterfort/nmarce.h"
#include "asterfort/nmarpc.h"
#include "asterfort/nmfinp.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpg.h"
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
    integer :: fonact(*)
    integer :: numins
    type(NL_DS_Print), intent(in) :: ds_print
    type(NL_DS_InOut), intent(in) :: ds_inout
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(NL_DS_Energy), intent(in) :: ds_energy
    character(len=19) :: sddisc, sdcrit, sddyna, sdpost, sdpilo
    character(len=24) :: carcri
    character(len=24) :: sderro, sdcriq
    character(len=24) :: modele, mate, carele
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Algorithm
!
! Storing results
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_inout         : datastructure for input/output management
! In  ds_print         : datastructure for printing parameters
! IN  NUMINS : NUMERO DE L'INSTANT
! IN  MODELE : NOM DU MODELEE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  SDCRIT : VALEUR DES CRITERES DE CONVERGENCE
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  SDERRO : SD ERREUR
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  SDPILO : SD PILOTAGE
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_energy        : datastructure for energy management
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, nume_store
    real(kind=8) :: instan
    character(len=8) :: result
    aster_logical :: force, lprint
    character(len=19) :: k19bid, list_load_resu
    character(len=4) :: etcalc
    integer :: nume_reuse
!
! --------------------------------------------------------------------------------------------------
!
    result         = ds_inout%result
    list_load_resu = ds_inout%list_load_resu
!
! - Loop state.
!
    call nmleeb(sderro, 'CALC', etcalc)
!
! - Last step => storing
!
    force = .false.
    call nmfinp(sddisc, numins, force)
!
! - Storing
!
    if (etcalc .eq. 'CONV') then 
        force = .true.
    endif
    if (etcalc .eq. 'STOP') then
        force = .true.
    endif
!
! - Print timer
!
    call uttcpg('IMPR', 'INCR')
!
! - Get index for storing
!
    call dinuar(result    , sddisc    , numins, force,&
                nume_store, nume_reuse)
!
! - Current time
!
    instan = diinst(sddisc,numins)
!
! - Save energy parameters in output table
!
    call nmarpc(ds_energy, nume_reuse, instan)
!
! - Print or not ?
!
    lprint = ds_print%l_print
!
! - Storing
!
    if (nume_store .ge. 0) then
!
! ----- Begin timer
!
        call nmtime(ds_measure, 'Launch', 'Store')
!
! ----- Print head
!
        if (lprint) then
            call utmess('I', 'ARCHIVAGE_5')
        endif
!
! ----- Increased result datastructure if necessary
!
        call rsexch(' ', result, 'DEPL', nume_store, k19bid,&
                    iret)
        if (iret .eq. 110) then
            call rsagsd(result, 0)
        endif
!
! ----- Storing parameters
!
        call nmarc0(result, modele        , mate  , carele, fonact,&
                    sdcrit, sddyna        , sdpost, carcri, sdcriq,&
                    sdpilo, list_load_resu, nume_store, instan)
!
! ----- Stroring fields
!
        call nmarce(ds_inout, result  , sddisc, instan, nume_store,&
                    force   , ds_print)
!
! ----- End timer
!
        call nmtime(ds_measure, 'Stop', 'Store')
        call nmrinc(ds_measure, 'Store')
    endif
!
end subroutine
