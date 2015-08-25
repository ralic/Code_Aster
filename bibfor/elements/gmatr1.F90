subroutine gmatr1(nnoff, ndeg, abscur, xl, matr)

implicit none

#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/elrfvf.h"
#include "asterfort/elrfdf.h"
#include "asterfort/glegen.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"

    integer           :: nnoff, ndeg
    real(kind=8)      :: xl
    character(len=24) :: abscur, matr

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
!      CALCUL DE LA MATRICE DU SYSTEME LINEAIRE [A] {GS} = {GTHI}
!      METHODE THETA-LEGENDRE ET G-LEGENDRE POUR LE CALCUL DE G(S)
!
! ENTREE
!
!   NNOFF    --> NOMBRE DE NOEUDS DU FOND DE FISSURE
!   NDEG     --> NOMBRE+1 PREMIERS CHAMPS THETA CHOISIS
!   ABSCUR   --> ABSCISSES CURVILIGNES S
!   XL       --> LONGUEUR DE LA FISSURE
!
! SORTIE
!
!   MATR     --> MATRICE DU SYTEME A RESOUDRE
!
! ......................................................................

    integer, parameter :: npg=14,nbnomx=3
    integer            :: nseg, iadpol, i ,j
    integer            :: ipg, js, iseg, jstmp, istok
    integer            :: ino, nno, ndim
    integer            :: conn(3)
    real(kind=8)       :: xpg(npg), wpg(npg)
    real(kind=8)       :: a(ndeg+1, ndeg+1)
    real(kind=8)       :: g1, g2,jac
    real(kind=8)       :: ff(nbnomx), dff(3, nbnomx), ksi(1)
    character(len=24)  :: stemp
    character(len=8)   :: elrefe

!....................................................................

!    COOR ET POID DU POINT DE GAUSS
    data xpg /  -.1080549487073437,&
     &           .1080549487073437,&
     &          -.3191123689278897,&
     &           .3191123689278897,&
     &          -.5152486363581541,&
     &           .5152486363581541,&
     &          -.6872929048116855,&
     &           .6872929048116855,&
     &          -.8272013150697650,&
     &           .8272013150697650,&
     &          -.9284348836635735,&
     &           .9284348836635735,&
     &          -.9862838086968123,&
     &           .9862838086968123/
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

!....................................................................
    call jemarq()

    conn(1:3) = 0

!   NOMBRE DE SEGMENT DU FOND DE FISSURE
    nseg = nnoff-1    
    elrefe = 'SE2'
!
!   ABSCISSES CURVILIGNES S
    call jeveuo(abscur, 'L', js)
!
!   CREA OBJET TEMP POUR LA VAL DE GLEGEN A ABSC CURV S
    stemp = '&&GMETH1.STEMP'
    call wkvect(stemp, 'V V R8', npg, jstmp)
!
    call wkvect(matr, 'V V R8', (ndeg+1)*(ndeg+1), istok)
!
    call wkvect('&&METHO1.VALPOL', 'V V R8', npg*(ndeg+1), iadpol)
!
!   INITIALISATION DE LA MATRICE 
    do i = 1, ndeg+1
        do j = 1, ndeg+1
            a(i,j) = 0.d0
        end do
    end do
!
!   BOUCLE SUR LES SEGMENTS
    do iseg = 1, nseg      

        conn(1) = iseg
        conn(2) = iseg+1

!       CALCUL DES COORDONNEES DES POINTS DE GAUSS DU SEGMENT DANS L'ESPACE REEL
        do ipg = 1,npg
            ksi(1) = xpg(ipg)
            call elrfvf(elrefe, ksi, nbnomx, ff, nno)

            zr(jstmp-1+ipg) = 0.d0
            do ino = 1, nno
               zr(jstmp-1+ipg) = zr(jstmp-1+ipg) + zr(js-1+conn(ino))*ff(ino)
            end do

        end do

!       EVALUATION DES POLYNOMES DE LEGENDRE AUX POINTS DE GAUSS DU SEGMENT
        call glegen(ndeg, npg, xl, stemp, zr(iadpol))
!
!       BOUCLE SUR LES POINTS DE GAUSS DU SEGMENT
        do ipg = 1,npg
!
!          CALCUL DES FONCTIONS DE FORMES ET DERIVEES
            ksi(1) = xpg(ipg)
            call elrfvf(elrefe, ksi, nbnomx, ff, nno)
            call elrfdf(elrefe, ksi, 3*nbnomx, dff, nno, ndim)

!           CALCUL DU JACOBIEN (SEGM DE REFERENCE --> SEGM REEL)
            jac = 0.d0
            do ino = 1, nno
               jac = jac + zr(js-1+conn(ino))*dff(1, ino)
            end do

!           CONTRIBUTION DU POINT DE GAUSS A LA MATRICE ELEMENTAIRE  
            do i = 1, ndeg+1

                g1 = zr(iadpol+(i-1)*npg+ipg-1)

                do j = 1, ndeg+1

                    g2 = zr(iadpol+(j-1)*npg+ipg-1)
                    a(i,j) = a(i,j)+g1*g2*wpg(ipg)*jac
                    zr(istok+(i-1)*(ndeg+1)+j-1) = a(i,j)

                end do
            end do
        end do
    end do

    call jedetr('&&GMETH1.STEMP')
    call jedetr('&&METHO1.VALPOL')
!
    call jedema()
!
end subroutine
