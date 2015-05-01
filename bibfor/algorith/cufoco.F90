subroutine cufoco(numedd, resocu, cnunil)
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
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: resocu
    character(len=24) :: numedd
    character(len=24) :: cnunil
!
! ----------------------------------------------------------------------
!
! ROUTINE LIAISON_UNILATER (CALCUL)
!
! CALCUL DU VECTEUR ASSEMBLE DES FORCES DE LIAISON_UNILATER
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUME_DDL DE LA MATRICE
! IN  RESOCU : SD DE RESOLUTION DE LIAISON_UNILATER
! OUT CNUNIL : VECT_ASSE DES FORCES DE LIAISON_UNILATER
!
!
!
!
    integer :: neq, i
    real(kind=8), pointer :: vale(:) => null()
    real(kind=8), pointer :: atmu(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    call jeveuo(cnunil(1:19)//'.VALE', 'E', vr=vale)
!
! --- CALCUL DU VECT_ASSE
!
    call jeveuo(resocu(1:14)//'.ATMU', 'L', vr=atmu)
    do i = 1, neq
        vale(i) = atmu(i)
    end do
!
    call jedema()
end subroutine
