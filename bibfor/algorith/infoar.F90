subroutine infoar(ndim, ar, ia, j, geom,&
                  lsn, a, b, m, lsna,&
                  lsnb, lsnm)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/vecini.h"
    integer :: ar(12, 3), ndim, ia, j
    real(kind=8) :: a(ndim), b(ndim), m(ndim), lsna, lsnb, lsnm, geom(*), lsn(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!                      CALCUL ET STOCKAGE DES COORDONNEES DU MILIEU DE
!                      L'ARETE CREEE LORS DU SOUS DECOUPAGE
!
!......................................................................
!
    integer :: na, nb, nm, i
!
!......................................................................
!
    call jemarq()
!
    na=ar(ia,j)
    nb=ar(ia,3-j)
    nm=ar(ia,3)
!
    call vecini(ndim, 0.d0, a)
    call vecini(ndim, 0.d0, b)
    call vecini(ndim, 0.d0, m)
!
    do 100 i = 1, ndim
        a(i)=geom(ndim*(na-1)+i)
        b(i)=geom(ndim*(nb-1)+i)
        m(i)=geom(ndim*(nm-1)+i)
100  continue
!
    lsna=lsn(na)
    lsnb=lsn(nb)
    lsnm=lsn(nm)
!
    call jedema()
end subroutine
