subroutine nmevac(sdimpr      , sddisc   , sderro, sdcont_defi, sdcont_solv,&
                  i_echec_acti, nume_inst, iterat, retact)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmadcp.h"
#include "asterfort/nmdeco.h"
#include "asterfort/nmecev.h"
#include "asterfort/nmeraz.h"
#include "asterfort/nmerge.h"
#include "asterfort/nmevdp.h"
#include "asterfort/nmitsp.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sderro
    character(len=24) :: sdcont_defi, sdcont_solv
    character(len=19) :: sddisc
    integer :: i_echec_acti
    integer :: iterat, nume_inst
    integer :: retact
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ACTIONS SUITE A UN EVENEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! In  sddisc           : datastructure for time discretization
! IN  SDERRO : SD ERREUR
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACTreac
! IN  IEVDAC : INDICE DE L'EVENEMENT ACTIF
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! OUT RETACT : CODE RETOUR
!     0 - ON NE FAIT RIEN
!     1 - ON REFAIT LE PAS DE TEMPS
!     2 - ON CONTINUE LA BOUCLE DE NEWTON (ITERATIONS EN PLUS)
!     3 - L'ACTION A ECHOUE
!
! ----------------------------------------------------------------------
!
    character(len=16) :: action, event_name
    integer :: retsup, retswa, retpen, retdec
    aster_logical :: trydec, litmax
!
! ----------------------------------------------------------------------
!
    retact = 3
    action = 'ARRET'
    trydec = .false.
    ASSERT(i_echec_acti.ne.0)
!
! --- RECUPERATION ERREURS PARTICULIERES
!
    litmax = .false.
    if (sderro .ne. ' ') then
        call nmerge(sderro, 'ITER_MAXI', litmax)
    endif
!
! - Event and action
!
    call utdidt('L', sddisc, 'ECHE', 'NOM_EVEN', index_ = i_echec_acti,&
                valk_ = event_name)
    call utdidt('L', sddisc, 'ECHE', 'ACTION'  , index_ = i_echec_acti,&
                valk_ = action)
!
! --- REALISATION DE L'ACTION
!
    if (action .eq. 'ARRET') then
        call utmess('I', 'MECANONLINE10_30')
        retact = 3
        trydec = .false.
    else if (action.eq.'ITER_SUPPL') then
        ASSERT(iterat.ge.0)
        if (litmax) then
            call utmess('I', 'MECANONLINE10_32')
            call nmitsp(sdimpr, sddisc, iterat, retsup)
        else
            retsup = 0
        endif
        if (retsup .eq. 0) then
            trydec = .true.
        else if (retsup.eq.1) then
            retact = 2
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'DECOUPE') then
        trydec = .true.
    else if (action.eq.'AUTRE_PILOTAGE') then
        if (litmax) then
            call utmess('I', 'MECANONLINE10_34')
            call nmevdp(sddisc, retswa)
        else
            retswa = 0
        endif
        if (retswa .eq. 0) then
            trydec = .true.
        else if (retswa.eq.1) then
            retact = 1
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'ADAPT_COEF_PENA') then
        call utmess('I', 'MECANONLINE10_35')
        call nmadcp(sddisc, sdcont_defi, sdcont_solv, i_echec_acti, retpen)
        trydec = .false.
        if (retpen .eq. 0) then
            retact = 3
        else if (retpen.eq.1) then
            retact = 1
        else
            ASSERT(.false.)
        endif
    else if (action.eq.'CONTINUE') then
        retact = 0
    else
        ASSERT(.false.)
    endif
!
! --- CAS DE LA DECOUPE
!
    if (trydec) then
        call utmess('I', 'MECANONLINE10_33')
        call nmdeco(sddisc, nume_inst, iterat, i_echec_acti, retdec)
        if (retdec .eq. 0) then
            retact = 3
        else if (retdec.eq.1) then
            retact = 1
        else if (retdec.eq.2) then
            retact = 0
        else
            ASSERT(.false.)
        endif
    endif
!
! --- ECHEC DE L'ACTION -> EVENEMENT ERREUR FATALE
!
    if (retact .eq. 3) then
        call nmecev(sderro, 'E', event_name, action)
    endif
!
! --- ON DESACTIVE LES EVENEMENTS
!
    call nmeraz(sderro, 'EVEN')
!
end subroutine
