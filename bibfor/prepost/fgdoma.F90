subroutine fgdoma(nommat, nbcycl, epsmin, epsmax, dom)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/rcvale.h'
    character(len=*) :: nommat
    real(kind=8) :: epsmin(*), epsmax(*)
    real(kind=8) :: dom(*)
    integer :: nbcycl
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
!     UNE COURBE DE MANSON_COFFIN DONNEE POINT PAR POINT
!     ------------------------------------------------------------------
! IN  NOMMAT : K   : NOM DU MATERIAU
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
! IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    integer :: icodre
    character(len=8) :: nomres, nompar
    character(len=10) :: pheno
    real(kind=8) :: nrupt, delta
!
!-----------------------------------------------------------------------
    integer :: i, nbpar
!-----------------------------------------------------------------------
    call jemarq()
    nomres = 'MANSON_C '
    nbpar = 1
    pheno = 'FATIGUE   '
    nompar = 'EPSI    '
!
    do 10 i = 1, nbcycl
        delta = (abs(epsmax(i)-epsmin(i)))/2.d0
        call rcvale(nommat, pheno, nbpar, nompar, delta,&
                    1, nomres, nrupt, icodre, 2)
        dom(i) = 1.d0/nrupt
10  end do
!
    call jedema()
end subroutine
