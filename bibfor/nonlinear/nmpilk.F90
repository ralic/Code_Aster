subroutine nmpilk(incpr1, incpr2, ddincc, neq, eta,&
                  rho, offset)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: neq
    real(kind=8) :: eta, rho, offset
    character(len=19) :: incpr1, incpr2, ddincc
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! AJUSTEMENT DE LA DIRECTION DE DESCENTE
!
! ----------------------------------------------------------------------
!
! CORR = RHO * PRED(1) + (ETA-OFFSET) * PRED(2)
!
! IN  NEQ    : LONGUEUR DES CHAM_NO
! IN  INCPR1 : INCREMENT SOLUTION PHASE PREDICTION 1
! IN  INCPR2 : INCREMENT SOLUTION PHASE PREDICTION 2 (TERME PILOTAGE)
! OUT DDINNC : INCREMENT SOLUTION APRES PILOTAGE/RECH. LINE.
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  RHO    : PARAMETRE DE RECHERCHE LINEAIRE
! IN  OFFSET : DECALAGE DU PARMAETRE DE PILOTAGE
!
!
!
!
    integer :: i
    real(kind=8), pointer :: ddepl(:) => null()
    real(kind=8), pointer :: du0(:) => null()
    real(kind=8), pointer :: du1(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call jeveuo(incpr1(1:19)//'.VALE', 'L', vr=du0)
    call jeveuo(incpr2(1:19)//'.VALE', 'L', vr=du1)
    call jeveuo(ddincc(1:19)//'.VALE', 'E', vr=ddepl)
!
! --- CALCUL
!
    do 10 i = 1, neq
        ddepl(i) = rho*du0(i) + (eta-offset)*du1(i)
10  end do
!
    call jedema()
end subroutine
