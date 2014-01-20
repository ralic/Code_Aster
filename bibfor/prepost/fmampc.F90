subroutine fmampc(nbfonc, nbptot, sigm, rampmx)
    implicit   none
#include "jeveux.h"
#include "asterfort/fmdevi.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbfonc, nbptot
    real(kind=8) :: sigm(*), rampmx
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
!     NBFONC  : IN  : NOMBRE DE FONCTIONS (6 EN 3D 4 EN 2D)
!     NBPTOT  : IN  : NOMBRE DE PAS DE TEMPS DE CALCUL
!     SIGM    : IN  : VECTEUR DES CONTRAINTES EN TOUS LES PAS DE TEMPS
!     RAMPMX  : OUT : VALEUR AMPLITUDE DE CISSION
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    integer ::  i1, i2, j
    real(kind=8) :: sig(6), rampc
    real(kind=8), pointer :: dev(:) => null()
!     ------------------------------------------------------------------
!
!------- CALCUL DU DEVIATEUR -------
!
    AS_ALLOCATE(vr=dev, size=nbfonc*nbptot)
    call fmdevi(nbfonc, nbptot, sigm,dev)
!
! -------- CALCUL AMPLITUDE DE CISSION ------
!
    rampmx = 0.d0
    do 100 i1 = 1, nbptot-1
        do 200 i2 = i1+1, nbptot
            do 300 j = 1, nbfonc
                sig(j) = dev(1+(i2-1)*nbfonc+j-1)- dev(1+(i1-1)* nbfonc+j-1 )
300          continue
            if (nbfonc .eq. 6) then
                rampc = (&
                        sig(1)*sig(1)+sig(2)*sig(2)+sig(3)*sig(3))/ 2.d0 + sig(4)*sig(4) + sig(5)&
                        &*sig(5) + sig(6)*sig(6&
                        )
            else if (nbfonc .eq. 4) then
                rampc = ( sig(1)*sig(1)+sig(2)*sig(2)+sig(3)*sig(3))/ 2.d0 + sig(4)*sig(4 )
            endif
            if (rampc .gt. rampmx) rampmx = rampc
200      continue
100  end do
    rampmx = 1.d0/2.d0*sqrt(rampmx)
!
    AS_DEALLOCATE(vr=dev)
!
end subroutine
