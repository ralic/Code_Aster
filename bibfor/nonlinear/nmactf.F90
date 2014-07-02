subroutine nmactf(sdimpr, sddisc, sderro, defico, resoco,&
                  solveu, parcri, iterat, numins)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevac.h"
#include "asterfort/nmleeb.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdimpr, sderro
    character(len=24) :: defico, resoco
    character(len=19) :: sddisc, solveu
    real(kind=8) :: parcri(*)
    integer :: iterat, numins
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
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION
! IN  SDERRO : SD GESTION DES ERREURS
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SOLVEU : SD SOLVEUR
! IN  PARCRI : CRITERES DE CONVERGENCE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
!
!
!
!
    integer :: retact, ievdac
    aster_logical :: arret
    character(len=4) :: etfixe
    integer :: actfix
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    arret = (nint(parcri(4)).eq.0)
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
        call nmacto(sddisc, ievdac)
        call nmevac(sdimpr, sddisc, sderro, defico, resoco,&
                    solveu, ievdac, numins, iterat, retact)
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
        if (.not.arret) then
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
    call jedema()
!
end subroutine
