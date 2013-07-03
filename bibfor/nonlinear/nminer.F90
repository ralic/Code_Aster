subroutine nminer(masse, accplu, cniner)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
    character(len=19) :: masse
    character(len=19) :: accplu, cniner
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES FORCES D'INERTIE
!
! ----------------------------------------------------------------------
!
!
! IN  ACCPLU : ACCELERATION COURANTE
! IN  MASSE  : MATR_ASSE MASSE
! OUT CNINER : VECT_ASSE FORCES INERTIE
!
!
!
!
    integer :: jaccp, jmass, jiner
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES OBJETS JEVEUX
!
    call jeveuo(accplu(1:19)//'.VALE', 'L', jaccp)
    call jeveuo(masse(1:19) //'.&INT', 'L', jmass)
    call jeveuo(cniner(1:19)//'.VALE', 'E', jiner)
!
! --- CALCUL FORCES INERTIE
!
    call mrmult('ZERO', jmass, zr(jaccp), zr(jiner), 1,&
                .true.)
!
    call jedema()
end subroutine
