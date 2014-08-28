subroutine ndxdec(sdimpr, sddisc, sderro, numins)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmacto.h"
#include "asterfort/nmeceb.h"
#include "asterfort/nmevac.h"
#include "asterfort/nmleeb.h"
    integer :: numins
    character(len=24) :: sdimpr, sderro
    character(len=19) :: sddisc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DE LA DECOUPE DU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
! IN  SDIMPR : SD AFFICHAGE
! IN  SDERRO : SD GESTION DES ERREURS
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
!
!
!
!
    integer :: iterat, retact, ievdac, actnew
    character(len=24) :: k24bla
    character(len=4) :: etnewt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    retact = 4
    actnew = 3
    iterat = 0
    k24bla = ' '
!
! --- ETAT DE NEWTON ?
!
    call nmleeb(sderro, 'NEWT', etnewt)
!
! --- ACTIONS SUITE A UN EVENEMENT
!
    if (etnewt .eq. 'CONV') then
        retact = 0
    else if (etnewt.eq.'EVEN') then
        call nmacto(sddisc, ievdac)
        call nmevac(sdimpr, sddisc, sderro, k24bla, k24bla,&
                    ievdac, numins, iterat, retact)
    else if (etnewt.eq.'CONT') then
! ----- CONTINUER LA BOUCLE DE NEWTON EST IMPOSSIBLE EN EXPLICITE
        ASSERT(.false.)
    else if (etnewt.eq.'ERRE') then
! ----- ERRREUR NON TRAITEE DANS NDXCVG
        retact = 4
    else if (etnewt.eq.'STOP') then
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
        actnew = 0
    else if (retact.eq.1) then
!
! ----- ON REFAIT LE PAS DE TEMPS
!
        actnew = 1
    else if (retact.eq.2) then
!
        ASSERT(.false.)
    else if (retact.eq.3) then
!
! ----- ECHEC DE L'ACTION -> ARRET DU CALCUL
!
        actnew = 3
    else if (retact.eq.4) then
!
! ----- ARRET DU CALCUL
!
        actnew = 3
    else
        ASSERT(.false.)
    endif
!
! --- CHANGEMENT DE STATUT DE LA BOUCLE
!
    if (actnew .eq. 0) then
        call nmeceb(sderro, 'NEWT', 'CONV')
    else if (actnew.eq.1) then
        call nmeceb(sderro, 'NEWT', 'ERRE')
    else if (actnew.eq.2) then
        ASSERT(.false.)
    else if (actnew.eq.3) then
        call nmeceb(sderro, 'NEWT', 'STOP')
    else
        ASSERT(.false.)
    endif
!
! --- TRANSFERT ETAT DE LA BOUCLE
!
    call nmleeb(sderro, 'NEWT', etnewt)
    call nmeceb(sderro, 'FIXE', etnewt)
!
    call jedema()
end subroutine
