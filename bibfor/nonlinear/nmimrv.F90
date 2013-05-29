subroutine nmimrv(sdimpr, fonact, iterat, fetite, relcoe,&
                  relite, eta)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/isfonc.h'
    include 'asterfort/nmimci.h'
    include 'asterfort/nmimck.h'
    include 'asterfort/nmimcr.h'
    integer :: fonact(*), iterat
    character(len=24) :: sdimpr
    real(kind=8) :: relcoe, eta
    integer :: relite, fetite
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
!
! ENREGISTRE LES DONNEES DANS LA SDIMPR
! LES DONNES POUR LES RESIDUS SONT ECRITS DANS NMIMRE
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  FETITE : NB ITER. FETI
! IN  RELCOE : VALEUR COEF. RECHERCHE LINEAIRE
! IN  RELITE : NB ITER. RECHERCHE LINEAIRE
! IN  ETA    : VALEUR COEF. PILOTAGE
!
! ----------------------------------------------------------------------
!
    logical :: lreli, lfeti, lpilo, lborst
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    lborst = isfonc(fonact,'DEBORST')
    lpilo = isfonc(fonact,'PILOTAGE')
    lreli = isfonc(fonact,'RECH_LINE')
    lfeti = isfonc(fonact,'FETI')
!
! --- ECRITURE CRITERES RECHERCHE LINEAIRE
!
    if (lreli .and. (iterat.ne.0)) then
        call nmimci(sdimpr, 'RELI_NBIT', relite, .true.)
        call nmimcr(sdimpr, 'RELI_COEF', relcoe, .true.)
    endif
!
! --- ECRITURE CRITERES PILOTAGE
!
    if (lpilo) then
        call nmimcr(sdimpr, 'PILO_COEF', eta, .true.)
    endif
!
! --- ECRITURE NUMERO ITERATION FETI
!
    if (lfeti) then
        call nmimci(sdimpr, 'FETI_NBIT', fetite, .true.)
    endif
!
! --- ECRITURE DE BORST
!
    if (lborst) then
        call nmimck(sdimpr, 'DEBORST  ', 'DE BORST...', .true.)
    endif
!
end subroutine
