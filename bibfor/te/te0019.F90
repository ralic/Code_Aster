subroutine te0019(option, nomte)
    implicit none
!.......................................................................
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
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT EN PRESSION
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!          (LA PRESSION EST DONNEE SOUS FORME D'UNE FONCTION)
!
!          OPTION : 'CHAR_MECA_PRES_F '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/nmpr3d.h"
    character(len=8) :: nompar(4)
    character(len=16) :: nomte, option
    integer :: ier, ndim, nno, npg, nnos, jgano, kpg, kdec, n
    integer :: ipoids, ivf, idf, igeom, ipres, itemps, ires
    real(kind=8) :: valpar(4), x, y, z
!                                   9*27*27
    real(kind=8) :: pr, p(27), matr(6561)
!
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idf, jgano)
!
!    AUTRES VARIABLES DE L'OPTION
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PPRESSF', 'L', ipres)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PVECTUR', 'E', ires)
!
!    CALCUL DE LA PRESSION AUX PTS DE GAUSS (FONCTION DE T ET/OU X,Y,Z)
    valpar(4) = zr(itemps)
    nompar(4) = 'INST'
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
!
    do 100 kpg = 0, npg-1
        kdec = kpg*nno
!
!      COORDONNEES DU POINT DE GAUSS
        x = 0.d0
        y = 0.d0
        z = 0.d0
        do 105 n = 0, nno-1
            x = x + zr(igeom+3*n ) * zr(ivf+kdec+n)
            y = y + zr(igeom+3*n+1) * zr(ivf+kdec+n)
            z = z + zr(igeom+3*n+2) * zr(ivf+kdec+n)
105      continue
!
!      VALEUR DE LA PRESSION
        valpar(1) = x
        valpar(2) = y
        valpar(3) = z
        call fointe('FM', zk8(ipres), 4, nompar, valpar,&
                    pr, ier)
        p(kpg+1) = pr
100  end do
!
!    CALCUL EFFECTIF DU SECOND MEMBRE
    call nmpr3d(1, nno, npg, zr(ipoids), zr(ivf),&
                zr(idf), zr(igeom), p, zr(ires), matr)
!
end subroutine
