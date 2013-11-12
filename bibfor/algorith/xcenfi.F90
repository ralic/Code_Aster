subroutine xcenfi(elp, ndim, nno, ptint, pmilie,&
                  jtabco, jtabls, cenfi)
!
    implicit none
!
#   include "jeveux.h"
#   include "asterfort/assert.h"
#   include "asterfort/jedema.h"
#   include "asterfort/jemarq.h"
#   include "asterfort/reerel.h"
#   include "asterfort/xnewto.h"
    integer :: ndim, nno, jtabco, jtabls
    character(len=8) :: elp
    real(kind=8) :: cenfi(ndim), ptint(*), pmilie(*)
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
!       ELP     : TYPE DE L'ELEMENT
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       PTINT  : COORDONNÉES DES POINTS D'INTERSECTION
!       JTABCO  : ADRESSE DES COORDONNEES DES NOEUDS DE L'ELEMENT
!       JTABLS  : ADRESSE DES LSN DES NOEUDS DE L'ELEMENT
!       NNO     : NOMBRE DE NOEUX DE L'ELEMENT
!     SORTIE
!       CENFI   : COORDONNES DU PT MILIEU AU CENTRE DE LA FISSURE
!     ----------------------------------------------------------------
!
    real(kind=8) :: ksi(ndim)
    real(kind=8) :: epsmax, rbid, tab(8, ndim)
    integer :: ibid, itemax, i, n(3)
    character(len=6) :: name
!
! --------------------------------------------------------------------
!
    call jemarq()
!
    itemax=500
    epsmax=1.d-9
    name='XCENFI'
    do i = 1,3
      n(i)=0
    end do
!
!     CALCUL DES COORDONNEES DE REFERENCE
!     DU POINT PAR UN ALGO DE NEWTON
!POUR LE MOMENT BLINDAGE ET SOUS DECOUPAGE DROIT
    call xnewto(elp, name, nno, n,&
                ndim, ptint, ndim, zr(jtabco), pmilie, zr(jtabls),&
                tab, ibid, ibid, rbid, itemax,&
                epsmax, ksi)
!
    ASSERT(ksi(1).ge.-1.d0 .and. ksi(1).le.1.d0)
    ASSERT(ksi(2).ge.-1.d0 .and. ksi(2).le.1.d0)
!
! --- COORDONNES DU POINT DANS L'ELEMENT REEL
    call reerel(elp, nno, ndim, zr(jtabco), ksi,&
                cenfi)
    if(.true.) then
    do i = 1, ndim
      cenfi(i)=(ptint((1-1)*ndim+i)+ptint((4-1)*ndim+i))/2.d0
    end do
    endif
!
    call jedema()
end subroutine
