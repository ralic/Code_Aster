subroutine fgdohs(nommat, nbcycl, sigmin, sigmax, lke,&
                  rke, lhaigh, rcorr, dom)
    implicit none
#include "jeveux.h"
#include "asterfort/rcvale.h"
    character(len=8) :: nommat
    real(kind=8) :: sigmin(*), sigmax(*)
    real(kind=8) :: rke(*), rcorr(*), dom(*)
    integer :: nbcycl
    logical(kind=1) :: lke, lhaigh
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
!     UNE COURBE DE WOHLER DONNEE HORS ZONE SINGULIERE
!     ------------------------------------------------------------------
! IN  NOMMAT : K8  : NOM DU MATERIAU
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
! IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
! IN  LKE    : L   : PRISE EN COMPTE DU COEFFICIENT KE
! IN  RKE    : R   : VALEURS DES COEFFICIENTS KE
! IN  LHAIGH : L   : PRISE EN COMPTE DE LA CORRECTION DE HAIGH
! IN  RCORR  : R   : VALEURS DES CORRECTIONS DE HAIGH
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    integer :: icodre(6)
    character(len=8) :: nompar
    character(len=8) :: nomres(6)
!
    real(kind=8) :: delta, salt, x, y, nrupt, slmodi, val(6), rbid, re(1)
!
!-----------------------------------------------------------------------
    integer :: i, nbpar
!-----------------------------------------------------------------------
    rbid = 0.d0
    nomres(1) = 'E_REFE'
    nomres(2) = 'A0'
    nomres(3) = 'A1'
    nomres(4) = 'A2'
    nomres(5) = 'A3'
    nomres(6) = 'SL'
    nbpar = 0
    nompar = ' '
    call rcvale(nommat, 'FATIGUE', nbpar, nompar, [rbid],&
                6, nomres, val, icodre, 2)
    nomres(1) = 'E'
    call rcvale(nommat, 'ELAS', nbpar, nompar, [rbid],&
                1, nomres, re(1), icodre, 2)
    do 10 i = 1, nbcycl
        delta = abs(sigmax(i)-sigmin(i))
        if (lke) delta = delta * rke(i)
        if (lhaigh) then
            delta = delta / rcorr(i)
            slmodi = val(6) / rcorr(i)
        else
            slmodi = val(6)
        endif
        salt = 1.d0/2.d0*(val(1)/re(1))*delta
        x = log10 (salt)
        if (salt .ge. slmodi) then
            y = val(2) + val(3)*x + val(4)*x**2 + val(5)*x**3
            nrupt = 10**y
            dom(i) = 1.d0 / nrupt
        else
            dom(i) = 0.d0
        endif
10  end do
!
end subroutine
