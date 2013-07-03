subroutine raxini(vsec1, vsec2, vsec3, vsec4, nptsec,&
                  nbordr, umin, umax, vmin, vmax,&
                  axeini)
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jean.angles at edf.fr
    implicit      none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/proax0.h"
    integer :: nptsec(4), nbordr
    real(kind=8) :: vsec1(2*nbordr), vsec2(2*nbordr), vsec3(2*nbordr)
    real(kind=8) :: vsec4(2*nbordr), umin, umax, vmin, vmax
    character(len=4) :: axeini
! ----------------------------------------------------------------------
! BUT: RECHERCHER l'AXE INITIAL PARMI LES AXES 1 ET 2.
! ----------------------------------------------------------------------
! ARGUMENTS:
! VSEC1     IN   R  : VECTEUR CONTENANT LES COMPOSANTES u ET v DES
!                     POINTS INCLUS DANS LE SECTEUR 1, POUR LE VECTEUR
!                     NORMAL COURANT.
! VSEC2     IN   R  : VECTEUR CONTENANT LES COMPOSANTES u ET v DES
!                     POINTS INCLUS DANS LE SECTEUR 2, POUR LE VECTEUR
!                     NORMAL COURANT.
! VSEC3     IN   R  : VECTEUR CONTENANT LES COMPOSANTES u ET v DES
!                     POINTS INCLUS DANS LE SECTEUR 3, POUR LE VECTEUR
!                     NORMAL COURANT.
! VSEC4     IN   R  : VECTEUR CONTENANT LES COMPOSANTES u ET v DES
!                     POINTS INCLUS DANS LE SECTEUR 4, POUR LE VECTEUR
!                     NORMAL COURANT.
! NPTSEC    IN   I  : VECTEUR CONTANT LE NOMBRE DE POINTS DE CHAQUE
!                     SECTEUR.
! NBORDR    IN   I  : NOMBRE DE NUMERO D'ORDRE STOCKE DANS LA
!                     STRUCTURE DE DONNEES RESULTAT.
! UMIN      IN   R  : VALEUR MINIMALE DES u, POUR LE VECTEUR COURANT.
! UMAX      IN   R  : VALEUR MAXIMALE DES u, POUR LE VECTEUR COURANT.
! VMIN      IN   R  : VALEUR MINIMALE DES v, POUR LE VECTEUR COURANT.
! VMAX      IN   R  : VALEUR MAXIMALE DES v, POUR LE VECTEUR COURANT.
! AXEINI    OUT  K4 : AXE INITIAL.
!
!-----------------------------------------------------------------------
    integer :: i
    real(kind=8) :: u0, v0, csta, cstb, a1, b1, ui, vi
    real(kind=8) :: amaxs1, amaxs2, amaxs3, amaxs4, rpax1, rpax2
    real(kind=8) :: amp1, amp2
!
!-----------------------------------------------------------------------
!234567                                                              012
!
    call jemarq()
!
    u0 = umin + (umax-umin)/2.0d0
    v0 = vmin + (vmax-vmin)/2.0d0
!
! PROJECTION SUR L'AXE 1
!
    csta = (umax - umin)
    cstb = (vmax - vmin)
    a1 = (vmax-vmin)/(umax-umin)
    b1 = (umax*vmin - umin*vmax)/(umax-umin)
    amaxs2 = 0.0d0
    amaxs4 = 0.0d0
!
    do 10 i = 1, nptsec(2)
        ui = vsec2(2*i - 1)
        vi = vsec2(2*i)
        call proax0(ui, vi, csta, cstb, a1,&
                    b1, u0, v0, rpax1)
        if (rpax1 .gt. amaxs2) amaxs2 = rpax1
10  end do
!
    do 20 i = 1, nptsec(4)
        ui = vsec4(2*i - 1)
        vi = vsec4(2*i)
        call proax0(ui, vi, csta, cstb, a1,&
                    b1, u0, v0, rpax1)
        if (rpax1 .lt. amaxs4) amaxs4 = rpax1
20  end do
!
! PROJECTION SUR L'AXE 2
!
    csta = -(umax - umin)
    cstb = (vmax - vmin)
    a1 = -(vmax - vmin)/(umax - umin)
    b1 = (umax*vmax - umin*vmin)/(umax-umin)
    amaxs1 = 0.0d0
    amaxs3 = 0.0d0
!
    do 30 i = 1, nptsec(1)
        ui = vsec1(2*i - 1)
        vi = vsec1(2*i)
        call proax0(ui, vi, csta, cstb, a1,&
                    b1, u0, v0, rpax2)
        if (rpax2 .lt. amaxs1) amaxs1 = rpax2
30  end do
!
    do 40 i = 1, nptsec(3)
        ui = vsec3(2*i - 1)
        vi = vsec3(2*i)
        call proax0(ui, vi, csta, cstb, a1,&
                    b1, u0, v0, rpax2)
        if (rpax2 .gt. amaxs3) amaxs3 = rpax2
40  end do
!
! CALCUL DE L'AMPLITUDE MAX SUR CHACUN DES AXES
!
    amp1 = amaxs2 - amaxs4
    amp2 = amaxs3 - amaxs1
    if (amp1 .gt. amp2) then
        axeini = 'AXE1'
    else
        axeini = 'AXE2'
    endif
!
    call jedema()
end subroutine
