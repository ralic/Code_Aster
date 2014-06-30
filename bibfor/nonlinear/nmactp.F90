subroutine nmactp(sdimpr, sddisc, sderro, defico, resoco,&
                  solveu, parcri, nbiter, numins)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/isacti.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevac.h"
#include "asterfort/nmleeb.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sderro
    character(len=24) :: defico, resoco
    character(len=19) :: sddisc, solveu
    real(kind=8) :: parcri(*)
    integer :: nbiter, numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DES ACTIONS A LA FIN D'UN PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION
! IN  SDERRO : SD GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  PARCRI : CRITERES DE CONVERGENCE
! IN  NBITER : NOMBRE D'ITERATIONS DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
!
!
!
!
    integer :: retact, ievdac, actpas, iterat, ibid
    character(len=4) :: etinst
    logical(kind=1) :: arret
    integer :: piless, ireapc
    character(len=16) :: pilcho
    real(kind=8) :: r8bid
    character(len=8) :: k8bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    arret = (nint(parcri(4)).eq.0)
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
        call nmacto(sddisc, ievdac)
        call nmevac(sdimpr, sddisc, sderro, defico, resoco,&
                    solveu, ievdac, numins, iterat, retact)
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
        if (.not.arret) then
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
! ----- REMISE A ZERO ESSAI_REAC_PRECOND
!
        call isacti(sddisc, 'REAC_PRECOND', ievdac)
        if (ievdac .ne. 0) then
            ireapc = 0
            call utdidt('E', sddisc, 'ECHE', ievdac, 'ESSAI_REAC_PRECOND',&
                        r8bid, ireapc, k8bid)
        endif
!
! ----- REMISE A ZERO ESSAI_ITER_PILO
!
        call isacti(sddisc, 'AUTRE_PILOTAGE', ievdac)
        if (ievdac .ne. 0) then
            piless = 1
            pilcho = 'NATUREL'
            call utdidt('E', sddisc, 'ECHE', ievdac, 'ESSAI_ITER_PILO',&
                        r8bid, piless, k8bid)
            call utdidt('E', sddisc, 'ECHE', ievdac, 'CHOIX_SOLU_PILO',&
                        r8bid, ibid, pilcho)
        endif
    endif
!
    call jedema()
end subroutine
