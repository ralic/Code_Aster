subroutine xmilfa(elp, nno, ndime, n,&
                  typma, ip1, ip2, pm1a,&
                  pm1b, pm2, pinter, ndim, tabco1,&
                 tabco2, pmilie, ainter, milfa)
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/conare.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/xnewto.h"
#include "asterfort/xxmmvd.h"
    integer :: ndim, ndime, ip1, ip2, pm1a, pm1b, pm2, nno
    integer :: n(3)
    real(kind=8) :: ainter(*), pinter(*), pmilie(*), milfa(ndim), tabco1(*)
    real(kind=8) :: tabco2(*)
    character(len=8) :: typma, elp
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
!                      TROUVER LES COORDONNES DU PT MILIEU ENTRE LES
!                      DEUX POINTS D'INTERSECTION
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       N       : NUMERO PARENT DES TROIS NOEUDS DE FACE
!       TYPMA   : TYPE DE LA MAILLE (TYPE_MAILLE)
!       IP1     : PREMIER IP DANS DANS UN FACE
!       IP2     : DEUXIEME IP DANS UN FACE
!       PM1A    : PREMIER PM1 DANS UN FACE
!       PM1B    : DEUXIEME PM1 DANS UN FACE
!       PM2     : PM2 DANS UN FACE
!       PINTER  : TABLEAU DES COORDONNÉES DES POINTS D'INTERSECTION
!       TABCO   : TABLEAU DES COORDONNÉES DES POINTS
!       PMILIE  : TABLEAU DES COORDONNÉES DES POINTS MILIEUX EXISTANT
!       AINTER  : INFOS ARETE ASSOCIEE AU POINT D'INTERSECTION
!     SORTIE
!       MILFA   : COORDONNES DU TROISIME TYPE DE PM
!     ----------------------------------------------------------------
!
    integer :: ar(12, 3), nbar, a1, a2, a, b, d
    integer :: i, j, zxain
    real(kind=8) :: tab(8, ndim)
    real(kind=8) :: ksi(ndim)
    real(kind=8) :: epsmax, rbid, rbid3(3)
    integer :: ibid, itemax
    character(len=6) :: name
!
! --------------------------------------------------------------------
    call jemarq()
    zxain=xxmmvd('ZXAIN')
    itemax=500
    epsmax=1.d-9
    name='XMILFA'
    call conare(typma, ar, nbar)
    a1=ainter(zxain*(ip1-1)+1)
    a2=ainter(zxain*(ip2-1)+1)
!
    a=0
    b=0
    d=0
!
    do i = 1, 2
        do 31 j = 1, 2
            if (ar(a1,i) .eq. ar(a2,j)) then
                a=ar(a1,3-i)
                b=ar(a2,3-j)
            endif
31      continue
  end do
    do i = 1, nbar
        do 41 j = 1, 2
          if ((ar(i,j).eq.a) .and. (ar(i,3-j).eq.b)) d=ar(i,3)
41      continue
  end do
    ASSERT((a*b*d).gt.0)
!
    do i = 1, ndim
        tab(1,i)=pinter(ndim*(ip1-1)+i)
        tab(2,i)=pinter(ndim*(ip2-1)+i)
        tab(3,i)=tabco2(ndim*(b-1)+i)
        tab(4,i)=tabco2(ndim*(a-1)+i)
        tab(5,i)=pmilie(ndim*(pm2-1)+i)
        tab(6,i)=pmilie(ndim*(pm1b-1)+i)
        tab(7,i)=tabco2(ndim*(d-1)+i)
        tab(8,i)=pmilie(ndim*(pm1a-1)+i)
  end do
!POUR LE MOMENT BLINDAGE ET SOUS DECOUPAGE DROIT

!
!     CALCUL DES COORDONNEES DE REFERENCE
!     DU POINT PAR UN ALGO DE NEWTON
    call xnewto(elp, name, ibid, nno, n,&
                ndime, rbid3, ndim,tabco1, rbid3, rbid3,&
                tab, ibid, ibid, rbid, itemax,&
                epsmax, ksi)
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
!
    call reerel(elp, nno, ndim, tabco1, ksi,&
                milfa)
!
!
    if(.true.) then
    do i = 1, ndim
      milfa(i)=(tab(1,i)+tab(3,i))/2.d0
    end do
    endif

    call jedema()
end subroutine
