subroutine pipere(npg, a, tau, nsol, eta)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterc/r8maem.h'
    integer :: npg, nsol
    real(kind=8) :: a(0:1, npg), tau
    real(kind=8) :: eta(2)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! RESOLUTION P(ETA) = TAU POUR LE PILOTAGE PAR PREDICTION ELASTIQUE OU
! DEFORMATION
!
! ----------------------------------------------------------------------
!
!
! IN  NPG    : NOMBRE DE POINTS DE GAUSS CONSIDERES
! IN  A      : COEFFICIENT DE LA DROITE TAU = A(1,G)*ETA + A(0,G)
! IN  TAU    : SECOND MEMBRE DE L'EQUATION SCALAIRE
! OUT NSOL   : NOMBRE DE SOLUTIONS (0, 1 OU 2)
! OUT ETA    : SOLUTIONS OU ESTIMATION SI NSOL=0
!
! ----------------------------------------------------------------------
!
    integer :: g
    real(kind=8) :: x, infini
!
! ----------------------------------------------------------------------
!
!
! -- INITIALISATION DE I0
!
    infini = r8maem()
    eta(1) = -infini
    eta(2) = infini
!
!
! -- CONSTRUCTION DES INTERVALLES IG PAR RECURRENCE
!
    do 10 g = 1, npg
!
!      LA PENTE DE LA DROITE CONSIDEREE EST NULLE
        if (abs(a(1,g)) .le. abs(tau - a(0,g))/infini) then
!
!      SON ORDONNEE EST SUPERIEURE AU SECOND MEMBRE -> PAS DE SOL.
            if (a(0,g) .gt. tau) goto 1000
!
!
!      LA PENTE DE LA DROITE CONSIDEREE EST NEGATIVE
        else if (a(1,g) .lt. 0) then
            x = (tau - a(0,g)) / a(1,g)
            if (x .gt. eta(2)) then
                eta(1) = eta(2)
                goto 1000
            else if (x .gt. eta(1)) then
                eta(1) = x
            endif
!
!
!      LA PENTE DE LA DROITE CONSIDEREE EST POSITIVE
        else
            x = (tau - a(0,g)) / a(1,g)
            if (x .lt. eta(1)) then
                eta(2) = eta(1)
                goto 1000
            else if (x .lt. eta(2)) then
                eta(2) = x
            endif
!
        endif
10  end do
!
!
! -- TRAITEMENT DU NOMBRE DE SOLUTION
!
    if (eta(1) .eq. -infini .and. eta(2) .eq. infini) then
        goto 1000
    else if (eta(1).eq.-infini) then
        nsol = 1
        eta(1) = eta(2)
    else if (eta(2).eq.infini) then
        nsol = 1
        eta(2) = eta(1)
    else
        nsol = 2
    endif
    goto 9999
!
!
! -- CONSTRUCTION D'UNE ESTIMATION QUAND L'INTERVALLE EST VIDE
!
1000  continue
    nsol = 0
    if (eta(1) .eq. -infini) eta(1) = eta(2)
    if (eta(1) .eq. infini) eta(1) = 0.d0
!
!
9999  continue
end subroutine
