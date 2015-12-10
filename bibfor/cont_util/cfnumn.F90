subroutine cfnumn(defico, nno, posnno, numnno)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
    character(len=24), intent(in) :: defico
    integer, intent(in) :: nno
    integer, intent(in) :: posnno(nno)
    integer, intent(out) :: numnno(nno)
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! DONNE LES NUMEROS ABSOLUS DES NOEUDS DE CONTACT
!
! --------------------------------------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE CONTACT (DEFINITION)
! IN  NNO    : NOMBRE DE NOEUDS
! IN  POSNNO : INDICE DANS CONTNO DES NOEUDS
! OUT NUMNNO : INDICE ABSOLUS DES NOEUDS DANS LE MAILLAGE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ino, posno
    character(len=24) :: contno
    integer :: jnoco
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONTACT
!
    contno = defico(1:16)//'.NOEUCO'
    call jeveuo(contno, 'L', jnoco)
!
! --- NUMERO DES NOEUDS
!
    do ino = 1, nno
        posno = posnno(ino)
        numnno(ino) = zi(jnoco+posno-1)
    end do
!
    call jedema()
!
end subroutine
