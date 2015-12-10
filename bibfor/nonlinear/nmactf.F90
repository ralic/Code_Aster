subroutine nmactf(ds_print, sddisc, sderro, ds_contact,&
                  ds_conv , iterat, numins)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevac.h"
#include "asterfort/nmleeb.h"
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
    type(NL_DS_Print), intent(in) :: ds_print
    character(len=24), intent(in) :: sderro
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19), intent(in) :: sddisc
    type(NL_DS_Conv), intent(in) :: ds_conv
    integer, intent(in) :: iterat
    integer, intent(in) :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DES ACTIONS A LA FIN D'UNE BOUCLE DE POINT FIXE
!
! BOUCLE POINT FIXE -> BOUCLE TEMPS
!
! ----------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for time discretization
! IN  SDERRO : SD GESTION DES ERREURS
! In  ds_contact       : datastructure for contact management
! In  ds_conv          : datastructure for convergence management
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
!
! ----------------------------------------------------------------------
!
    integer :: retact, i_event_acti
    character(len=4) :: etfixe
    integer :: actfix
!
! ----------------------------------------------------------------------
!
    retact = 4
    actfix = 3
!
! --- ETAT DU POINT FIXE ?
!
    call nmleeb(sderro, 'FIXE', etfixe)
!
! --- ACTIONS SUITE A UN EVENEMENT
!
    if (etfixe .eq. 'CONV') then
        retact = 0
    else if (etfixe.eq.'EVEN') then
        call nmacto(sddisc, i_event_acti)
        call nmevac(sddisc, sderro  , i_event_acti, numins, iterat,&
                    retact, ds_print, ds_contact)
! ----- ON NE PEUT PAS CONTINUER LES ITERATIONS DE NEWTON ICI
        ASSERT(retact.ne.2)
    else if (etfixe.eq.'CONT') then
        retact = 2
    else if (etfixe.eq.'ERRE') then
        retact = 1
    else if (etfixe.eq.'STOP') then
        retact = 4
    else
        ASSERT(.false.)
    endif
!
! --- TRAITEMENT DE L'ACTION
!
    if (retact .eq. 0) then
!
! ----- TOUT EST OK -> ON PASSE A LA SUITE
!
        actfix = 0
    else if (retact.eq.1) then
!
! ----- ON REFAIT LE PAS DE TEMPS
!
        actfix = 1
    else if (retact.eq.2) then
!
! ----- ON CONTINUE LES ITERATIONS DE POINT FIXE
!
        actfix = 2
    else if (retact.eq.3) then
!
! ----- ECHEC DE L'ACTION
!
        if (.not.ds_conv%l_stop) then
!
! ------- CONVERGENCE FORCEE -> ON PASSE A LA SUITE
!
            call utmess('A', 'MECANONLINE2_37')
            actfix = 0
        else
!
! ------- ARRET DU CALCUL
!
            actfix = 3
        endif
    else if (retact.eq.4) then
!
! ----- ARRET DU CALCUL
!
        actfix = 3
    else
        ASSERT(.false.)
    endif
!
! --- CHANGEMENT DE STATUT DE LA BOUCLE
!
    if (actfix .eq. 0) then
        call nmeceb(sderro, 'FIXE', 'CONV')
    else if (actfix.eq.1) then
        call nmeceb(sderro, 'FIXE', 'ERRE')
    else if (actfix.eq.2) then
        call nmeceb(sderro, 'FIXE', 'CONT')
    else if (actfix.eq.3) then
        call nmeceb(sderro, 'FIXE', 'STOP')
    else
        ASSERT(.false.)
    endif
!
end subroutine
