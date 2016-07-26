subroutine gmeth1(nnoff, ndeg, gthi, gs, objcur, xl, gi)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/glegen.h"
#include "asterfort/gmatr1.h"
#include "asterfort/gsyste.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"

    integer           :: nnoff, ndeg
    real(kind=8)      :: gthi(1), gs(1), gi(1),xl
    character(len=24) :: objcur

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!      METHODE THETA-LEGENDRE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   NDEG     --> NOMBRE+1 PREMIERS CHAMPS THETA CHOISIS
!   GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!   OBJCUR   --> ABSCISSES CURVILIGNES S
!   XL       --> LONGUEUR DE LA FISSURE

! SORTIE
!   GS      --> VALEUR DE G(S)
!   GI      --> VALEUR DE GI
! ......................................................................

    integer           :: iadrt3
    integer           :: i, j
    character(len=24) :: matr
    real(kind=8)      :: som

!.......................................................................
   
    call jemarq()

!   CALCUL DE LA MATRICE DU SYSTEME LINÉAIRE [A] {GI} = {GTHI}
    matr = '&&METHO1.MATRIC'
    call gmatr1(nnoff, ndeg, objcur, xl, matr)
!
!   RESOLUTION DU SYSTEME LINEAIRE:  MATR*GI = GTHI
    call gsyste(matr, ndeg+1, ndeg+1, gthi, gi)
!
!   VALEURS DES POLYNOMES DE LEGENDRE POUR LES NOEUDS DU FOND DE FISSURE
    call wkvect('&&METHO1.THETA', 'V V R8', (ndeg+1)*nnoff, iadrt3)
    call glegen(ndeg, nnoff, xl, objcur, zr(iadrt3))
!
!   VALEURS DE G(S)
    do i = 1, nnoff
        som = 0.d0
        do j = 1, ndeg+1
            som = som + gi(j)*zr(iadrt3+(j-1)*nnoff+i-1)
        end do
        gs(i) = som
    end do
!
    call jedetr('&&METHO1.MATRIC')
    call jedetr('&&METHO1.THETA')
    call detrsd('CHAMP_GD', '&&GMETH1.G2        ')
!
    call jedema()
end subroutine
