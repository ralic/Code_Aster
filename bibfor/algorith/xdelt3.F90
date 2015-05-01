subroutine xdelt3(ndim, ksi, tabls, delta)
    implicit none
!
#    include "jeveux.h"
#    include "asterfort/jedema.h"
#    include "asterfort/jemarq.h"
#    include "asterfort/elrfdf.h"
#    include "asterfort/elrfvf.h"
    integer :: ndim
    real(kind=8) :: tabls(3), ksi(ndim), delta
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
!                 CALCUL DE LA QUANTITE A MINIMISER POUR LE CALCUL
!                    DES COORDONNEES DU POINT D'INTERSECTION
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
    real(kind=8) :: ff(3), dff(3, 3)
    integer :: i, nderiv, nno
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
!
!
!     CALCUL DES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfvf('SE3', ksi, 3, ff, nno)
!
!     CALCUL DES DERIVEES FONCTIONS DE FORME DE L'ELEMENT EN KSI
    call elrfdf('SE3', ksi, ndim*nno, dff, nno,&
                nderiv)
!
!
! --- CALCUL DE FCTG,D1FCTG,D2FCTG EN KSI
! ---           FCTG : LEVEL SET NORMALE
    do 105 i = 1, nno
        fctg = fctg + ff(i)*tabls(i)
        dfctg=dfctg+tabls(i)*dff(1,i)
105  continue
!
! --- CALCUL DES QUANTITES A MINIMISER
!     CALCUL DE DELTAS
!
    delta=fctg/dfctg
!
    call jedema()
end subroutine
