subroutine xmcoor(jcesd, jcesv, jcesl, ifiss, ndim,&
                  npte, nummae, ifac, xp, yp,&
                  coord)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecini.h"
    integer :: jcesd(10), jcesv(10), jcesl(10)
    integer :: ndim, nummae, ifac, npte, ifiss
    real(kind=8) :: xp, yp
    real(kind=8) :: coord(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! CALCUL DES COORDONNEES LOCAUX DU PT D'INTEGRATION OU DE SON PROJETE
! DANS LES MAILLES MAITRES OU ESCLAVES RESPECTIVEMENT
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC L'I.F.P.
!
! ----------------------------------------------------------------------
!
!
!
!  JCES*(3)  : POINTEURS DE LA SD SIMPLE DES COOR DES PT D'INTER
!  JCES*(4)  : POINTEURS DE LA SD SIMPLE DE CONNECTIVITÉ DES FACETTES
! IN IFISS  : NUMÉRO DE FISSURE LOCALE DANS NUMMAE
! IN  NDIM  : DIMENSION DU PROBLEME
! IN  NUMMAE: POSITION DE LA MAILLE ESCLAVE OU MAITRE
! IN  IFAC  : NUMERO LOCAL DE LA FACETTE ESCLAVE OU MAITRE
! IN  XP    : COORDONNEE X DU POINT D'INTEGRATION DE CONTACT SUR
!             LA MAILLE ESCLAVE OU MAITRE
! IN  YP    : COORDONNEE Y DU POINT D'INTEGRATION DE CONTACT SUR
!             LA MAILLE ESCLAVE OU MAITRE
! OUT COORD : COORDONNEES DU POINT D'INTEGRATION DANS L'ELEMENT
!             PARENT
!
!
!
!
    real(kind=8) :: coor(npte)
    integer :: i, j, iad, numpi(npte)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DES NUM LOCAUX DES PTS D'INTER DE LA FACETTE
!
    call vecini(npte, 0.d0, coor)
    do 10 i = 1, npte
        numpi(i)=0
10  end do
    call vecini(3, 0.d0, coord)
!
    do 20 i = 1, npte
        call cesexi('S', jcesd(4), jcesl(4), nummae, 1,&
                    ifiss, (ifac-1)* ndim+i, iad)
        ASSERT(iad.gt.0)
        numpi(i) = zi(jcesv(4)-1+iad)
20  end do
    do 30 i = 1, ndim
! --- BOUCLE SUR LES DIMENSIONS
        do 40 j = 1, npte
! --- BOUCLE SUR LES POINTS D'INTERSECTIONS
! --- RECUPERATION DE LA COMPOSANTE LOCALE I DE CHACUN DES POINTS
! --- D'INTERSECTIONS J DE LA FACETTE
            call cesexi('S', jcesd(3), jcesl(3), nummae, 1,&
                        ifiss, ndim*( numpi(j)-1)+i, iad)
            ASSERT(iad.gt.0)
            coor(j) = zr(jcesv(3)-1+iad)
40      continue
! --- CALCUL DE LA COMPOSANTE I POUR LE POINT DE CONTACT DANS LA
! --- MAILLE PARENTE
        if (ndim .eq. 2) then
            if (npte .le. 2) then
                coord(i) = coor(1)*(1-xp)/2 + coor(2)*(1+xp)/2
            else if (npte.eq.3) then
                coord(i) = -coor(1)*xp*(1-xp)/2 + coor(2)*xp*(1+xp)/2 + coor(3)*(1+xp)*(1-xp)
            endif
        else if (ndim.eq.3) then
            coord(i) = coor(1)*(1-xp-yp) + coor(2)*xp + coor(3)*yp
        endif
30  end do
!
    call jedema()
end subroutine
