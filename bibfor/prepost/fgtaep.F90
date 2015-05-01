subroutine fgtaep(nommat, nomfo1, nomnap, nbcycl, epsmin,&
                  epsmax, dom)
    implicit none
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rcvale.h"
    character(len=*) :: nommat, nomfo1, nomnap
    real(kind=8) :: epsmin(*), epsmax(*)
    real(kind=8) :: dom(*)
    integer :: nbcycl
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DU DOMMAGE ELEMENTAIRE PAR TAHERI_MANSON_COFFIN
!     ------------------------------------------------------------------
! IN  NOMMAT : K   : NOM DU MATERIAU
! IN  NOMFO1 : K   : NOM DE LA FONCTION
! IN  NOMNAP : K   : NOM DE LA NAPPE
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  EPSMIN : R   : DEFORMATIONS MINIMALES DES CYCLES
! IN  EPSMAX : R   : DEFORMATIONS MAXIMALES DES CYCLES
! OUT DOM    : R   : VALEURS DES DOMMAGES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    integer :: icodre(1)
    character(len=16) :: nomres
    character(len=8) :: nompar, nomp(2)
    character(len=32) :: pheno
    real(kind=8) :: nrupt(1), delta, dsigm, depsi, epmax, valp(2)
!-----------------------------------------------------------------------
    integer :: i, ier, nbpar
    real(kind=8) :: zero
!-----------------------------------------------------------------------
    data zero  /1.d-13/
!
    call jemarq()
!
    epmax = 0.d0
    nomres = 'MANSON_COFFIN'
    nbpar = 1
    pheno = 'FATIGUE   '
    nompar = 'EPSI    '
    do 10 i = 1, nbcycl
        delta = (abs(epsmax(i)-epsmin(i)))/2.d0
        if (delta .gt. epmax-zero) then
            epmax = delta
            call rcvale(nommat, pheno, nbpar, nompar, [delta],&
                        1, nomres, nrupt(1), icodre(1), 2)
            dom(i) = 1.d0/nrupt(1)
        else
            nomp(1) = 'X'
            nomp(2) = 'EPSI'
            valp(1) = epmax
            valp(2) = delta
            call fointe('F ', nomnap, 2, nomp, valp, dsigm, ier)
            nomp(2) = 'SIGM'
            call fointe('F ', nomfo1, 1, [nomp(2)], [dsigm], depsi, ier)
            call rcvale(nommat, pheno, nbpar, nompar, [depsi],&
                        1, nomres, nrupt(1), icodre(1), 2)
            dom(i) = 1.d0/nrupt(1)
        endif
10  end do
!
    call jedema()
end subroutine
