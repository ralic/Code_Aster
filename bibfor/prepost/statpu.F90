subroutine statpu(nbobst, nbpt, temps, fcho, vgli,&
                  iadh, wk1, wk2, wk3, iwk4,&
                  idebut, nbloc, nbval, ifires, inoe,&
                  impr, pusurn)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     CALCUL DE LA PUISSANCE D'USURE AU SENS D'ARCHARD
!
! IN  : NBOBST : NB DE NOEUDS DE CHOC
! IN  : NBPT   : NB DE PAS DE TEMPS TEMPORELS ARCHIVES
! IN  : TEMPS  : INSTANTS DE CALCUL
! IN  : FCHO   : VECTEUR DES FORCES DE CHOC
! IN  : VGLI   : VECTEUR DES VITESSES DE GLISSEMENT
! IN  : NBLOC  : NB DE BLOCS POUR LE MOYENNAGE
! IN  : INOE   : NUMERO DE NOEUD TRAITE
! IN  : IMPR   : IMPRESSION
! OUT : PUSURN : PUISSANCE D'USURE MOYENNEE
!-----------------------------------------------------------------------
    implicit none
#include "asterfort/impus.h"
#include "asterfort/pusure.h"
#include "blas/dcopy.h"
    real(kind=8) :: temps(*), fcho(*), vgli(*), wk1(*), wk2(*), wk3(*)
    integer :: iwk4(*), iadh(*)
!
!-----------------------------------------------------------------------
    integer :: i, ibl, idebut, ifires, impr, inoe, nbloc
    integer :: nbobst, nbpt, nbval
    real(kind=8) :: pusee, pusurn
!-----------------------------------------------------------------------
    pusurn = 0.d0
    call dcopy(nbpt, fcho(3*(inoe-1)+1), 3*nbobst, wk1, 1)
    call dcopy(nbpt, vgli(3*(inoe-1)+2), 3*nbobst, wk2, 1)
    call dcopy(nbpt, vgli(3*(inoe-1)+3), 3*nbobst, wk3, 1)
!     CALL DCOPY(NBPT,IADH(1*(INOE-1)+1),NBOBST,IWK4,1)
    do 5 i = 1, nbpt
        iwk4(i) = iadh(1*(inoe-1)+1+(i-1)*nbobst)
 5  end do
    do 10 ibl = 1, nbloc
        call pusure(nbval, wk1((ibl-1)*nbval+idebut), wk2((ibl-1)* nbval+idebut),&
                    wk3((ibl-1)*nbval+idebut), iwk4((ibl-1)*nbval+ idebut),&
                    temps((ibl-1)*nbval+idebut), pusee)
        pusurn = pusurn + pusee
!C       --- IMPRESSION DE LA PUISSANCE D USURE ---
        if (impr .eq. 2) call impus(ifires, ibl, pusee)
10  end do
    pusurn = pusurn / nbloc
    if (ibl .gt. 1 .and. impr .eq. 2) call impus(ifires, 0, pusurn)
!
end subroutine
