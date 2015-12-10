subroutine nmactp(ds_print, sddisc, sderro, ds_contact,&
                  ds_conv , nbiter, numins)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/isacti.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevac.h"
#include "asterfort/nmleeb.h"
#include "asterfort/utdidt.h"
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
    integer, intent(in) :: nbiter
    integer, intent(in) :: numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DES ACTIONS A LA FIN D'UN PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
! In  sddisc           : datastructure for time discretization
! IN  SDERRO : SD GESTION DES ERREURS
! In  ds_contact       : datastructure for contact management
! In  ds_conv          : datastructure for convergence management
! IN  NBITER : NOMBRE D'ITERATIONS DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
!
! ----------------------------------------------------------------------
!
    integer :: retact, i_echec_acti, actpas, iterat, i_action
    character(len=4) :: etinst
    integer :: piless
    character(len=16) :: pilcho
!
! ----------------------------------------------------------------------
!
    retact = 4
    actpas = 3
    iterat = nbiter - 1
!
! --- ETAT DE LA BOUCLE EN TEMPS ?
!
    call nmleeb(sderro, 'INST', etinst)
!
! --- ACTIONS SUITE A UN EVENEMENT
!
    if (etinst .eq. 'CONV') then
        retact = 0
    else if (etinst.eq.'EVEN') then
        call nmacto(sddisc, i_echec_acti)
        call nmevac(sddisc, sderro  , i_echec_acti  , numins, iterat, &
                    retact, ds_print, ds_contact)
    else if (etinst.eq.'ERRE') then
        retact = 1
    else if (etinst.eq.'STOP') then
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
        actpas = 0
!
    else if (retact.eq.1) then
!
! ----- ON REFAIT LE PAS DE TEMPS
!
        actpas = 1
!
    else if (retact.eq.2) then
!
! ----- PAS D'ITERATION EN PLUS ICI
!
        ASSERT(.false.)
!
    else if (retact.eq.3) then
!
! ----- ECHEC DE L'ACTION
!
        if (.not.ds_conv%l_stop) then
!
! ------- CONVERGENCE FORCEE -> ON PASSE A LA SUITE
!
            call utmess('A', 'MECANONLINE2_37')
            actpas = 0
        else
!
! ------- ARRET DU CALCUL
!
            actpas = 3
        endif
!
    else if (retact.eq.4) then
!
! ----- ARRET DU CALCUL
!
        actpas = 3
    else
        ASSERT(.false.)
    endif
!
! --- CHANGEMENT DE STATUT DE LA BOUCLE
!
    if (actpas .eq. 0) then
        call nmeceb(sderro, 'INST', 'CONV')
    else if (actpas.eq.1) then
        call nmeceb(sderro, 'INST', 'ERRE')
    else if (actpas.eq.3) then
        call nmeceb(sderro, 'INST', 'STOP')
    else
        ASSERT(.false.)
    endif
!
! --- PROCHAIN INSTANT: ON REINITIALISE
!
    if (actpas .eq. 0) then

!
! ----- REMISE A ZERO ESSAI_ITER_PILO
!
        call isacti(sddisc, 'AUTRE_PILOTAGE', i_action)
        if (i_action .ne. 0) then
            piless = 1
            pilcho = 'NATUREL'
            call utdidt('E', sddisc, 'ECHE', 'ESSAI_ITER_PILO',&
                        vali_ = piless)
            call utdidt('E', sddisc, 'ECHE', 'CHOIX_SOLU_PILO',&
                        valk_ = pilcho)
        endif
    endif
!
end subroutine
