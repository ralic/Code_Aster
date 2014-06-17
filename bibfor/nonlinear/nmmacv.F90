subroutine nmmacv(vecdep, sstru, vecass)
! ----------------------------------------------------------------------
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
    character(len=19) :: sstru
    character(len=19) :: vecdep
    character(len=24) :: vecass
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - MACRO_ELEMENT)
!
! CALCUL DE LA CONTRIBUTION AU SECOND MEMBRE DES MACRO-ELEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  VECDEP : VECTEUR DEPLACEMENT
! IN  SSTRU  : MATRICE ASSEMBLEE DES SOUS-ELEMENTS STATIQUES
! OUT VECASS : VECT_ASSE CALCULE
!
!
!
!
    integer ::   jrsst
    real(kind=8), pointer :: cnfi(:) => null()
    real(kind=8), pointer :: depl(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CALCUL VECT_ASSE(MACR_ELEM) = MATR_ASSE(MACR_ELEM) * VECT_DEPL
!
    call jeveuo(vecass(1:19)//'.VALE', 'E', vr=cnfi)
    call jeveuo(vecdep(1:19)//'.VALE', 'L', vr=depl)
    call jeveuo(sstru(1:19) //'.&INT', 'L', jrsst)
    call mrmult('ZERO', jrsst, depl, cnfi, 1,&
                .true.)
!
    call jedema()
!
end subroutine
