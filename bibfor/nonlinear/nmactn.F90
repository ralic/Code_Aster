subroutine nmactn(sdimpr, sddisc, sderro, defico, resoco,&
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
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmacto.h'
    include 'asterfort/nmeceb.h'
    include 'asterfort/nmevac.h'
    include 'asterfort/nmleeb.h'
    include 'asterfort/u2mess.h'
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
! GESTION DES ACTIONS A LA FIN D'UNE BOUCLE DE NEWTON
!
! BOUCLE NEWTON -> BOUCLE POINT FIXE
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
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  NUMINS : NUMERO D'INSTANT
!
!
!
!
    integer :: retact, ievdac, actnew
    logical :: arret
    character(len=4) :: etnewt
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    arret = (nint(parcri(4)).eq.0)
    retact = 4
    actnew = 3
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
        call nmevac(sdimpr, sddisc, sderro, defico, resoco,&
                    solveu, ievdac, numins, iterat, retact)
    else if (etnewt.eq.'CONT') then
! ----- TROP TARD POUR CONTINUE NEWTON -> IMPOSSIBLE
        call assert(.false.)
    else if (etnewt.eq.'ERRE') then
! ----- ERRREUR NON TRAITE DANS NMCVGN -> IMPOSSIBLE
        call assert(.false.)
    else if (etnewt.eq.'STOP') then
        retact = 4
    else
        call assert(.false.)
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
! ----- ON CONTINUE LES ITERATIONS DE NEWTON
!
        actnew = 2
    else if (retact.eq.3) then
!
! ----- ECHEC DE L'ACTION
!
        if (.not.arret) then
!
! ------- CONVERGENCE FORCEE -> ON PASSE A LA SUITE
!
            call u2mess('A', 'MECANONLINE2_37')
            actnew = 0
            call nmeceb(sderro, 'RESI', 'CONV')
        else
!
! ------- ARRET DU CALCUL
!
            actnew = 3
        endif
    else if (retact.eq.4) then
!
! ----- ARRET DU CALCUL
!
        actnew = 3
    else
        call assert(.false.)
    endif
!
! --- CHANGEMENT DE STATUT DE LA BOUCLE
!
    if (actnew .eq. 0) then
        call nmeceb(sderro, 'NEWT', 'CONV')
    else if (actnew.eq.1) then
        call nmeceb(sderro, 'NEWT', 'ERRE')
    else if (actnew.eq.2) then
        call nmeceb(sderro, 'NEWT', 'CONT')
    else if (actnew.eq.3) then
        call nmeceb(sderro, 'NEWT', 'STOP')
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
