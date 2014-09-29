subroutine xinter(ndim, ndime, elrefp, geom, lsn, ia, ib,&
                  lsnm, inref, inter) 
    implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/vecini.h"
#include "asterfort/xelrex.h"
#include "asterfort/xveri0.h"
#include "asterfort/xnewto.h"
    character(len=8) :: elrefp
    integer :: ndim, ndime, ia, ib
    real(kind=8), intent(in), optional :: lsnm
    real(kind=8) :: lsn(*), geom(*), inter(3), inref(3)
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
!                      TROUVER LE PT D'INTERSECTION ENTRE L'ARETE
!                      ET LA FISSURE
!
!     ENTREE
!
!     SORTIE
!
!     ----------------------------------------------------------------
!
    character(len=6) :: name
    real(kind=8) :: ksi(ndime), ptxx(2*ndime), lsna, lsnb, x(81)
    real(kind=8) :: epsmax, a , b , c
    integer :: itemax, ibid, n(3), j, nno, iret
!
!---------------------------------------------------------------------
!     DEBUT
!---------------------------------------------------------------------
    call jemarq()
!
    itemax=100
    epsmax=1.d-9
    name='XINTER'
    n(1)=ia
    n(2)=ib
    n(3)=0
!   COORDONNEES DANS L ELEMENT DE REFERENCE PARENT
    call xelrex(elrefp, nno, x)
!  ON STOCKE LES COORDONEES DE REFERENCE DE A ET B DANS <ptxx>
    do j = 1,ndime
      ptxx(j)=x(ndime*(ib-1)+j)-x(ndime*(ia-1)+j)
      ptxx(j+ndime)=x(ndime*(ia-1)+j)
    enddo
!!!!!ATTENTION INITIALISATION DU NEWTON: INTERPOLATION LINEAIRE DE LSN
    call vecini(ndime, 0.d0, ksi)
!   INITIALISATION DU NEWTON
    lsna=lsn(ia)
    lsnb=lsn(ib)
    ASSERT(abs(lsna-lsnb) .gt. 1.d0/r8gaem())
    if (present(lsnm)) then
       a = (lsna + lsnb - 2*lsnm)/2.d0
       b = (lsnb - lsna)/2.d0
       c = lsnm
       ASSERT(b**2.ge.(4*a*c))
       if (abs(a).lt.1.d-8) then
          ksi(1) = lsna/(lsna-lsnb)
       else 
          ksi(1) = (-b-sqrt(b**2-4*a*c))/(2.d0*a)
          if (abs(ksi(1)).gt.1) ksi(1) = (-b+sqrt(b**2-4*a*c))/(2.d0*a)
          ASSERT(abs(ksi(1)).le.1)
          ksi(1) = (ksi(1)+1)/2.d0
       endif
    else
       ksi(1)=lsna/(lsna-lsnb)
    endif
    call xnewto(elrefp, name, n,&
                ndime, ptxx, ndim, geom, lsn,&
                ibid, ibid, itemax,&
                epsmax, ksi)
!  FIN DE RECHERCHE SUR SEGMENT AB
    do j = 1, ndime
        inref(j)=ksi(1)*ptxx(j)+ptxx(j+ndime)
    enddo
!
    call xveri0(ndime, elrefp, inref, iret)  
    ASSERT(iret .eq. 0)
!
    call reerel(elrefp, nno, ndim, geom, inref,&
                inter)
!
!---------------------------------------------------------------------
!     FIN
!---------------------------------------------------------------------
    call jedema()
end subroutine
