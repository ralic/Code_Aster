subroutine fgdoba(nommat, nbcycl, sigmin, sigmax, lke,&
                  rke, lhaigh, rcorr, dom)
    implicit none
#include "jeveux.h"
#include "asterfort/rcvale.h"
    character(len=8) :: nommat
    real(kind=8) :: sigmin(*), sigmax(*)
    real(kind=8) :: rcorr(*), dom(*), rke(*)
    integer :: nbcycl
    logical :: lhaigh, lke
!     ------------------------------------------------------------------
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
!     -----------------------------------------------------------------
!     CALCUL DU DOMMAGE ELEMENTAIRE PAR INTERPOLATION SUR
!     UNE COURBE DE WOHLER DONNEE PAR 1/N = A (DSIGMA)**BETA
!     FORMULE DE BASKIN
!     ------------------------------------------------------------------
! IN  NOMMAT : K8  : NOM DU MATERIAU
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
! IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
! IN  LKE    : L   : PRISE EN COMPTE DU COEFFICIENT KE
! IN  RKE    : R   : VALEURS DU COEFFICIENT KE
! IN  LHAIGH : L   : PRISE EN COMPTE CORRECTION DE HAIGH
! IN  RCORR  : R   : VALEURS DE LA CORRECTION DE HAIGH
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    real(kind=8) :: delta, val(2), rbid
    integer :: icodre(2)
    character(len=8) :: nomres(2), nompar
!
!-----------------------------------------------------------------------
    integer :: i, nbpar
!-----------------------------------------------------------------------
    nompar = ' '
    nbpar = 0
    nomres(1) = 'A_BASQUI'
    nomres(2) = 'BETA_BAS'
    call rcvale(nommat, 'FATIGUE', nbpar, nompar, rbid,&
                2, nomres, val, icodre, 2)
    do 10 i = 1, nbcycl
        delta = (1.d0/2.d0)*abs(sigmax(i)-sigmin(i))
        if (lke) delta = delta * rke(i)
        if (lhaigh) delta = delta / rcorr(i)
        dom(i) = val(1)* delta**val(2)
10  end do
!
end subroutine
