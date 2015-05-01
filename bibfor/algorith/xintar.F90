subroutine xintar(lsna, lsnb, lsnm, a, b,&
                  m, ndim, intar)
    implicit none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/reerel.h"
#include "asterfort/vecini.h"
#include "asterfort/xnewto.h"
    integer :: ndim
    real(kind=8) :: lsna, lsnb, lsnm, a(3), b(3), m(3), intar(3)
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
!                      TROUVER LE PT D'INTERSECTION ENTRE L'ARETE
!                      ET LA FISSURE
!
!     ENTREE
!       LSNA    :VALEURS DES LEVELSET DES NOEUDS DE L'ELEMENT
!       LSNB    :VALEURS DES LEVELSET DES NOEUDS DE L'ELEMENT
!       LSNC    :VALEURS DES LEVELSET DES NOEUDS DE L'ELEMENT
!       A       :COORDONNEES DES NOEUDS DE L'ELEMENT
!       B       :COORDONNEES DES NOEUDS DE L'ELEMENT
!       M       :COORDONNEES DES NOEUDS DE L'ELEMENT
!       NDIM    :DIMENSION TOPOLOGIQUE DU MAILLAGE
!     SORTIE
!       INTAR   : COORDONNEES DES POINTS D'INTERSECTION
!     ----------------------------------------------------------------
!
    character(len=8) :: elp
    character(len=6) :: name
    real(kind=8) :: lsnl(3), col(ndim*3)
    real(kind=8) :: epsmax, rbid, xe(ndim)
    integer :: nno, itemax, i, ibid, n(3)
    parameter       (elp='SE3')
!
!---------------------------------------------------------------------
!     DEBUT
!---------------------------------------------------------------------
    call jemarq()
!
    itemax=500
    epsmax=1.d-9
    name='XINTAR'
    nno=3
!
    lsnl(1)=lsna
    lsnl(2)=lsnb
    lsnl(3)=lsnm
!
    do 100 i = 1, ndim
        col(i)=a(i)
        col(ndim+i)=b(i)
        col(ndim*2+i)=m(i)
100  end do
!
    rbid = 0.d0
    call vecini(ndim, 0.d0, xe)
    call xnewto(elp, name, n,&
                ndim, [rbid], ndim, [rbid], lsnl,&
                ibid, ibid, itemax,&
                epsmax, xe)
    call reerel(elp, nno, ndim, col, xe,&
                intar)
!
!---------------------------------------------------------------------
!     FIN
!---------------------------------------------------------------------
    call jedema()
end subroutine
