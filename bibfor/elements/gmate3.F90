subroutine gmate3(abscur, elrefe, conn, nno, mele)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elrfvf.h"
#include "asterfort/elrfdf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"

    integer           :: conn(3)
    integer           :: nno
    real(kind=8)      :: mele(3, 3)
    character(len=8)  :: elrefe
    character(len=24) :: abscur

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
!      CALCUL DE LA MATRICE ELEMENTAIRE POUR L'ELEMENT COURANT
!      METHODE THETA-LAGRANGE ET G-LAGRANGE
!
! ENTREE
!   CONN     --> CONNECTIVITÉ DES NOEUDS
!   ELREFE   --> TYPE DE SEGMENT (LINEAIRE OU QUADRATIQUE)   
!   ABSCUR   --> ABSCISSES CURVILIGNES S
!
! SORTIE
!   NNO      --> DIMMENSION DE LA MATRICE ELEMENTAIRE
!   MELE     --> MATRICE DE MASSE ELEMENTAIRE
!
! ......................................................................

    integer, parameter :: npg = 14, nbnomx = 3
    integer            :: i, j, ipg, js, ndim
    real(kind=8)       :: xpg(npg), wpg(npg)
    real(kind=8)       :: ff(nbnomx), dff(3, nbnomx)
    real(kind=8)       :: ksi(1), jac

! ......................................................................

!   COOR ET POID DU POINT DE GAUSS

        data xpg /-.1080549487073437,&
     &             .1080549487073437,&
     &            -.3191123689278897,&
     &             .3191123689278897,&
     &            -.5152486363581541,&
     &             .5152486363581541,&
     &            -.6872929048116855,&
     &             .6872929048116855,&
     &            -.8272013150697650,&
     &             .8272013150697650,&
     &            -.9284348836635735,&
     &             .9284348836635735,&
     &            -.9862838086968123,&
     &             .9862838086968123/
!
! VALEURS DES POIDS ASSOCIES
!
    data wpg /    .2152638534631578,&
     &            .2152638534631578,&
     &            .2051984637212956,&
     &            .2051984637212956,&
     &            .1855383974779378,&
     &            .1855383974779378,&
     &            .1572031671581935,&
     &            .1572031671581935,&
     &            .1215185706879032,&
     &            .1215185706879032,&
     &            .0801580871597602,&
     &            .0801580871597602,&
     &            .0351194603317519,&
     &            .0351194603317519/

! ......................................................................
    call jemarq()

    mele = 0.d0

!   BOUCLE SUR LE SEGMENT DU FONDFISS
    call jeveuo(abscur, 'L', js)

!   BOUCLE SUR LES POINTS DE GAUSS DU SEGMENT
    do ipg = 1, npg

!       CALCUL DES FONCTIONS DE FORMES ET DERIVEES
        ksi(1)=xpg(ipg)
        call elrfvf(elrefe, ksi, nbnomx, ff, nno)
        call elrfdf(elrefe, ksi, 3*nbnomx, dff, nno, ndim)
        
!       CALCUL DU JACOBIEN (SEGM DE REFERENCE --> SEGM REEL)
        jac = 0.5d0*(zr(js-1+conn(2)) - zr(js-1+conn(1)))

!       CONTRIBUTION DU POINT DE GAUSS A LA MATRICE ELEMENTAIRE   
        do i = 1, nno
            do j = 1, nno
               mele(i, j) = mele(i, j) + ff(i)*ff(j)*jac*wpg(ipg)
            end do
        end do
!
    end do

    call jedema()

end subroutine
