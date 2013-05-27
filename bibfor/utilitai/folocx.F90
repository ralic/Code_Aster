subroutine folocx(vale, n, x, prolgd, i,&
                  epsi, coli, ier)
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mesr.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: vale(n)
    real(kind=8) :: valr(2)
    character(len=*) :: prolgd
    character(len=1) :: coli
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mathieu.courtois at edf.fr
!     RECHERCHE DE LA PLACE DE X DANS LE VECTEUR VALE ORDONNE CROISSANT
!     ON VERIFIE SI X EST DANS L'INTERVALLE (V(1),V(N))
!                SINON, SUIVANT PROLGD, ON AGIT...
!     ------------------------------------------------------------------
! IN  : VALE   : VECTEUR DES VALEURS DES ABSCISSES
! IN  : N      : NOMBRE DE POINTS DE VALE
! IN  : X      : VALEUR DE L'ABSCISSE
! IN  : PROLGD : PROLONGEMENTS A DROITE ET A GAUCHE
! VAR : I      : NUMERO DU POINT TEL QUE VALE(I) <= X
! IN  : EPSI   : PRECISION A LAQUELLE ON RECHERCHE LA VALEUR
! OUT : COLI   : = 'C',   X = VALE(I)
!                = 'I',   INTERPOLATION VALE(I) < X < VALE(I+1)
!                = 'E',   EXTRAPOLATION PERMISE
!                = 'T',   FONCTION INTERPRETEE
!                = '?',   IER > 0
! OUT : IER    : CODE RETOUR
!                IER = 10 --->  MOINS DE 1 POINT
!                IER = 20 --->  EXTRAPOLATION INCONNUE
!                IER = 30 --->  ON DEBORDE A GAUCHE
!                IER = 40 --->  ON DEBORDE A DROITE
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, id, ie, ier, ind, j, n
!
    real(kind=8) :: epsi, tole, x
!-----------------------------------------------------------------------
    ier = 0
    coli = '?'
    if (n .lt. 1) then
        ier = 10
        call u2mess('E', 'FONCT0_18')
        goto 9999
    else if (n.eq.1) then
!             ON A X = VALE(1) + EPSILON
!             ON A : X < VALE(1) ET PROL GAUCHE AUTORISE
!             ON A : X > VALE(1) ET PROL DROITE AUTORISE
        if ((x.eq.0.d0 .and. abs(vale(n)).le.epsi ) .or.&
            ( x.lt.vale( n) .and. prolgd(1:1).ne.'E' ) .or.&
            ( x.gt.vale(n) .and. prolgd(2:2).ne.'E' )) then
            i = n
            coli = 'C'
        else if (abs((vale(n)-x)/x).le.epsi) then
            i = n
            coli = 'C'
        else
            ier = 30
            call u2mess('E', 'FONCT0_23')
        endif
        goto 9999
    endif
!
!     --- PROLONGEMENT A GAUCHE ---
    if (x .le. vale(1)) then
        i = 1
        if (prolgd(1:1) .eq. 'E') then
            tole = epsi * abs( vale(1) - vale(2) )
            if (abs(vale(1)-x) .le. tole) then
                coli = 'C'
                goto 9999
            endif
            ier = 30
            valr (1) = x
            valr (2) = vale(1)
            call u2mesr('E', 'FONCT0_19', 2, valr)
            goto 9999
        else if (prolgd(1:1) .eq. 'L') then
            coli = 'E'
        else if (prolgd(1:1) .eq. 'C') then
            coli = 'C'
        else if (prolgd(1:1) .eq. 'I') then
            coli = 'T'
        else
            ier = 20
            call u2mesk('E', 'FONCT0_21', 1, prolgd(1:1))
            goto 9999
        endif
!
!     --- PROLONGEMENT A DROITE ---
    else if (x.ge.vale(n)) then
        i = n
        if (prolgd(2:2) .eq. 'E') then
            tole = epsi * abs( vale(n) - vale(n-1) )
            if (abs(vale(n)-x) .le. tole) then
                coli = 'C'
                goto 9999
            endif
            ier = 40
            valr (1) = x
            valr (2) = vale(n)
            call u2mesr('E', 'FONCT0_20', 2, valr)
            goto 9999
        else if (prolgd(2:2) .eq. 'C') then
            coli = 'C'
        else if (prolgd(2:2) .eq. 'I') then
            coli = 'T'
        else if (prolgd(2:2) .eq. 'L') then
            i = n - 1
            coli = 'E'
        else
            ier = 20
            call u2mesk('E', 'FONCT0_21', 1, prolgd(2:2))
            goto 9999
        endif
!
!     --- RECHERCHE DE LA VALEUR PAR DICHOTOMIE ---
    else
        if (i .lt. 1 .or. i .gt. n) i = n / 2
        if (vale(i) .le. x) then
            id = i
            ie = n
        else
            id = 1
            ie = i
        endif
        do 2 j = 1, n
            if (ie .eq. (id+1)) goto 3
            ind = id+(ie-id)/2
            if (x .ge. vale(ind)) then
                id = ind
            else
                ie = ind
            endif
 2      continue
 3      continue
        i = id
        coli = 'I'
    endif
    call assert(i.ge.1 .and. i.le.n)
!
9999  continue
!
end subroutine
