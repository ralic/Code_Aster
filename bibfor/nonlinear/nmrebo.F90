subroutine nmrebo(f, mem, sens, rho, rhoopt,&
                  ldcopt, ldccvg, fopt, fcvg, opt,&
                  act, rhomin, rhomax, rhoexm, rhoexp,&
                  stite, echec)
! ----------------------------------------------------------------------
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
    implicit none
    include 'asterfort/zbinte.h'
    include 'asterfort/zbiter.h'
    real(kind=8) :: mem(2, *), sens
    real(kind=8) :: rho, rhoopt
    logical :: echec, stite
    integer :: ldcopt, ldccvg
    real(kind=8) :: f, fopt, fcvg
    integer :: opt, act
    real(kind=8) :: rhomin, rhomax, rhoexm, rhoexp
!
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
!
! RECHERCHE LINEAIRE AVEC LA METHODE MIXTE: BORNES + UNIDIRECTIONNEL
!
! ----------------------------------------------------------------------
!
!
!  IN  RHO    : SOLUTION COURANTE
!  IN  F      : VALEUR DE LA FONCTION EN RHO
!  I/O MEM    : COUPLES (RHO,F) ARCHIVES - GESTION INTERNE PAR ZEITER
!  I/O RHOOPT : VALEUR OPTIMALE DU RHO
!  I/O FOPT   : VALEUR OPTIMALE DE LA FONCTIONNELLE
!  OUT RHONEW : NOUVEL ITERE
!  OUT ECHEC  : .TRUE. SI LA RECHERCHE A ECHOUE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhoneg, rhopos
    real(kind=8) :: parmul, fneg, fpos
    integer :: dimcpl, nbcpl
    logical :: bpos, lopti
    common /zbpar/ rhoneg,rhopos,&
     &               parmul,fneg  ,fpos  ,&
     &               dimcpl,nbcpl ,bpos  ,lopti
!
    real(kind=8) :: rhonew
!
! ----------------------------------------------------------------------
!
    stite = .false.
    echec = .false.
    call zbiter(sens*rho, sens*f, rhoopt, fopt, mem,&
                rhonew, echec)
!
! --- GESTION DES BORNES
!
    call zbinte(rhonew, rhomin, rhomax, rhoexm, rhoexp)
    call zbinte(rhoopt, rhomin, rhomax, rhoexm, rhoexp)
!
! --- PRISE EN COMPTE D'UN RESIDU OPTIMAL SI NECESSAIRE
!
    if (lopti) then
        ldcopt = ldccvg
        opt = act
        act = 3 - act
        if (abs(fopt) .lt. fcvg) then
            stite = .true.
        endif
    endif
!
    rho = rhonew * sens
!
end subroutine
