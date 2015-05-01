subroutine arlt1d(mlv,ndim,ndml2,mcpln2)

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"

    integer :: ndml2,ndim
    real(kind=8) ::  mlv(78)

    real(kind=8) ::  mcpln2(2*ndim*ndml2,2*ndim*ndml2)
    integer ::       iaux,jaux,kaux

! ----------------------------------------------------------------------

! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
! OPTION ARLQ_MATR : CALCUL DES INTEGRALES DE COUPLAGE 1D - 1D
! ----------------------------------------------------------------------
    call jemarq()

! --- CALCUL DES TERMES DE COUPLAGE - MATRICE STOCKAGE LINEAIRE

    do 10 jaux = 1,2*ndim*ndml2
        do 20 iaux = 1,2*ndim*ndml2
            mcpln2(iaux,jaux) = 0.d0
        20 end do
    10 end do
    kaux = 0
    do 40 iaux = 1,2*ndim*ndml2
        do 50 jaux = 1,iaux
            kaux = kaux + 1
            mcpln2(iaux,jaux) = mcpln2(iaux,jaux) + mlv(kaux)
            mcpln2(jaux,iaux) = mcpln2(iaux,jaux)
        50 end do
    40 end do

    call jedema()

end subroutine
