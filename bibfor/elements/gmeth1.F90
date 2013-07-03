subroutine gmeth1(nnoff, ndeg, gthi, gs, objcur,&
                  xl, gi)
    implicit none
!
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
!
! ......................................................................
!      METHODE THETA-LEGENDRE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   NDEG     --> NOMBRE+1 PREMIERS CHAMPS THETA CHOISIS
!   GTHI     --> VALEURS DE G POUR LES CHAMPS THETAI
!   OBJCUR   --> ABSCISSES CURVILIGNES S
!   XL     : LONGUEUR DE LA FISSURE
!
! SORTIE
!
!   GS      --> VALEUR DE G(S)
!   GI      --> VALEUR DE GI
! ......................................................................
!
#include "jeveux.h"
#include "asterfort/detrsd.h"
#include "asterfort/glegen.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    integer :: nnoff, ndeg, iadrt3, i, j
    real(kind=8) :: xl, som, gthi(1), gs(1), gi(1)
    character(len=24) :: objcur
!
!
    call jemarq()
!
! VALEURS DU MODULE DU CHAMP THETA POUR LES NOEUDS DU FOND DE FISSURE
!
    call wkvect('&&METHO1.THETA', 'V V R8', (ndeg+1)*nnoff, iadrt3)
!
    call glegen(ndeg, nnoff, xl, objcur, zr(iadrt3))
!
! VALEURS DE GI
!
    do 10 i = 1, ndeg+1
        gi(i) = gthi(i)
10  end do
!
! VALEURS DE G(S)
!
    do 30 i = 1, nnoff
        som = 0.d0
        do 20 j = 1, ndeg+1
            som = som + gi(j)*zr(iadrt3+(j-1)*nnoff+i-1)
20      continue
        gs(i) = som
30  end do
!
    call jedetr('&&METHO1.THETA')
    call detrsd('CHAMP_GD', '&&GMETH1.G2        ')
!
    call jedema()
end subroutine
