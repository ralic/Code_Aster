subroutine te0467(option, nomte)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/dfdm1d.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!     CALCUL DE L OPTION COOR_ELGA
!     POUR LES ELEMENTS 1D SEG2
!
!     POUR CHAQUE POINT DE GAUSS :
!     1) COORDONNEES DU POINT DE GAUSS
!     2) POIDS X JACOBIEN AU POINT DE GAUSS
!
!
!
!
    integer :: ndim, nno, nnos, npg, jgano, kp, icopg, ino
    integer :: idfde, ipoids, ivf, igeom
    real(kind=8) :: xx, yy, poids, jacp, rbid, rbid2(2)
    logical :: laxi
!
    call elref4('SE2', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    laxi = .false.
    if (lteatt('AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCOORPG', 'E', icopg)
!
    do 100 kp = 1, npg
        xx=0.d0
        yy=0.d0
        do 50 ino = 1, nno
            xx=xx+zr(igeom+2*(ino-1)+0)*zr(ivf+(kp-1)*nno+ino-1)
            yy=yy+zr(igeom+2*(ino-1)+1)*zr(ivf+(kp-1)*nno+ino-1)
50      continue
        zr(icopg+3*(kp-1)+0)=xx
        zr(icopg+3*(kp-1)+1)=yy
        poids=zr(ipoids-1+kp)
        call dfdm1d(nno, poids, zr(idfde), zr(igeom), rbid2,&
                    rbid, jacp, rbid, rbid)
!       EN AXI R C'EST XX
        if (laxi) jacp=jacp*xx
        zr(icopg+3*(kp-1)+2)=jacp
100  end do
!
end subroutine
