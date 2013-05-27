subroutine ftest1(np3, rc, theta, typch, nbseg,&
                  xloc, ic, itestc, toln)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DE LA FONCTION TEST CHOC SUR BUTEE IC
! -----------
!               APPELANTS : MDCHOE, MDCHOF
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/disbut.h'
    integer :: np3
    real(kind=8) :: rc(np3, *), theta(np3, *)
    integer :: typch(*), nbseg(*)
    real(kind=8) :: xloc(*)
    integer :: ic, itestc
    real(kind=8) :: toln
!
! VARIABLES LOCALES
! -----------------
!
    integer :: typobs, nbs
    real(kind=8) :: xjeu, dnorm, cost, sint
    real(kind=8) :: ftest, tolch
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL   DISBUT
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    itestc = 0
    tolch = 10.0d0 * toln
!
! --- RECUPERATION DES VALEURS RELATIVES A LA BUTEE IC
!
    typobs = typch(ic)
    nbs = nbseg(ic)
    if ((typobs.eq.0) .or. (typobs.eq.1) .or. (typobs.eq.2)) xjeu = rc(1,ic)
!
! --- CALCUL DE LA DISTANCE NORMALE A LA BUTEE
!
    call disbut(np3, ic, xloc, typobs, xjeu,&
                rc, theta, nbs, cost, sint,&
                dnorm)
!
! --- CALCUL DE LA FONCTION TEST CHOC SUR BUTEE IC
!
    ftest = abs(dnorm)
    if (ftest .lt. tolch) then
        dnorm = 0.0d0
        itestc = -1
    endif
    if (dnorm .lt. 0.0d0) itestc = 1
!
! --- FIN DE FTEST1.
end subroutine
