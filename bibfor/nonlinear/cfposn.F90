subroutine cfposn(defico, posmai, posnno, nnomai)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnben.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: defico
    integer :: posmai
    integer :: posnno(9)
    integer :: nnomai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! INDICES DANS CONTNO DES NOEUDS POUR UNE MAILLE DONNEE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE CONTACT (DEFINITION)
! IN  POSMAI : INDICE DE LA MAILLE (DANS SD CONTACT)
! OUT POSNNO : INDICES DANS CONTNO DES NOEUDS
! OUT NNOMAI : NOMBRE DE NOEUDS DE LA MAILLE (DANS LES SD DE CONTACT)
!
!
!
!
    integer :: nbnmax
    parameter   (nbnmax = 9)
!
    character(len=24) :: nomaco, pnoma
    integer :: jnoma, jpono
    integer :: ino, jdec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    nomaco = defico(1:16)//'.NOMACO'
    pnoma = defico(1:16)//'.PNOMACO'
    call jeveuo(nomaco, 'L', jnoma)
    call jeveuo(pnoma, 'L', jpono)
!
! --- NOMBRE DE NOEUDS ATTACHES A CETTE MAILLE
!
    call cfnben(defico, posmai, 'CONNEX', nnomai)
    ASSERT(nnomai.le.nbnmax)
!
! --- NUMERO DES NOEUDS ATTACHES A CETTE MAILLE
!
    jdec = zi(jpono+posmai-1)
    do ino = 1, nnomai
        posnno(ino) = zi(jnoma+jdec+ino-1)
    end do
!
    call jedema()
end subroutine
