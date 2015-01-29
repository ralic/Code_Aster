subroutine xdelt0(elrefp, ndime, tabls, ptxx, ksi, delta)
    implicit none
!
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/elrfdf.h"
#include "asterfort/elrfvf.h"
    character(len=8) :: elrefp
    integer :: ndime
    real(kind=8) :: tabls(*), ksi, delta, ptxx(*)
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
!                 CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!                    DE L INTERSECTION DE LA DROITE v*ksi+ptm AVEC 
!                     L ISO ZERO DE LA LEVEL SET
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       KSI     : COORDONNEES DE REFERENCE DU POINT
!       LSNL    : VALEUR DES LSN DES NOEUDS DE L'ARETE
!
!     SORTIE
!       DELTA   : QUANTITE A MINIMISER
!     ----------------------------------------------------------------
!
!
    integer :: nbfct
    parameter    ( nbfct=27)   
    real(kind=8) :: ff(nbfct), dff(3, nbfct), v(ndime), ptm(ndime), pt(ndime)
    integer :: i, nderiv, nno, k
    real(kind=8) :: fctg, dfctg
!
!
!......................................................................
!
! --- CALCUL DES FONCTIONS DE FORME ET DE LEUR DERIVEES EN UN POINT
! --- DANS LA MAILLE
!
!
!
    call jemarq()
    fctg = 0.d0
    dfctg = 0.d0
    do i = 1,ndime
      v(i)=ptxx(i)
      ptm(i)=ptxx(i+ndime)
    enddo
!   COORDONNEES SUIVANT LE SEGMENT AB DANS L ELEMENT DE REFERENCE
    do i = 1,ndime
      pt(i)=ksi*v(i)+ptm(i)
    enddo
!
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf(elrefp, pt, nbfct, ff, nno)
!
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf(elrefp, pt, ndime*nno, dff, nno,&
                nderiv)
!
! ---           FCTG : LEVEL SET NORMALE
    do i = 1, nno
        fctg = fctg + ff(i)*tabls(i)
    enddo
    dfctg=0.d0
    do k=1,ndime
       do i = 1, nno
         dfctg=dfctg+tabls(i)*dff(k,i)*v(k)
       enddo
    enddo
    ASSERT( abs(dfctg) .gt. 1.d0/r8gaem())
!
! --- CALCUL DES QUANTITES A MINIMISER
!     CALCUL DE DELTAS
!
    delta=fctg/dfctg
!
    call jedema()
end subroutine
