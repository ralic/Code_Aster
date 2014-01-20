subroutine vpreco(nbvect, neq, vecred, vect)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: nbvect, neq
    real(kind=8) :: vect(neq, nbvect), vecred(nbvect, nbvect)
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
!     EFFECTUE LE PROLONGEMENT DES VECTEURS PROPRES : CALCUL DES
!     VECTEURS PROPRES DU SYSTEME COMPLET A PARTIR DES VECTEURS
!     PROPRES DU SYSTEME  REDUIT ET D'UNE MATRICE DE PASSAGE
!     (VECTEURS DE LANCZOS )
!     ------------------------------------------------------------------
!     NBVECT : IN  : NOMBRE DE MODES
!     NEQ    : IN  : NOMBRE D'INCONNUES
!     VECRED : IN  : MATRICE MODALE (CARREE) DU SYSTEME REDUIT
!     VECT   : IN  : MATRICE DE PASSAGE (RECTANGULAIRE)
!              OUT : MATRICE MODALE (RECTANGULAIRE) DU SYSTEME COMPLET
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i,  j, k, l
    real(kind=8) :: rt
    real(kind=8), pointer :: vilig(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    AS_ALLOCATE(vr=vilig, size=nbvect)
!
    do 10 i = 1, neq
        do 20 j = 1, nbvect
            vilig(j) = vect(i,j)
20      continue
        do 30 k = 1, nbvect
            rt = 0.d0
            do 40 l = 1, nbvect
                rt = rt + vilig(l) * vecred(l,k)
40          continue
            vect(i,k) = rt
30      continue
10  end do
!
    AS_DEALLOCATE(vr=vilig)
!
    call jedema()
end subroutine
