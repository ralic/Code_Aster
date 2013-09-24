subroutine cfnumm(defico, posnma, numnma)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24), intent(in) :: defico
    integer, intent(in)  :: posnma
    integer, intent(out) :: numnma
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! DONNE LES NUMEROS ABSOLUS DES MAILLES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! in  defico : sd de contact (definition)
! in  posnma : indice dans contno d'une maille
! out numnma : indice absolu de la maille dans le maillage
!
!
!
!
    integer :: ima, posma
    character(len=24) :: contma
    integer :: jmaco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- acces sd contact
!
    contma = defico(1:16)//'.MAILCO'
    call jeveuo(contma, 'L', jmaco)
!
! --- numero de la maille
!
    numnma = zi(jmaco+posnma-1)
!
    call jedema()
!
end subroutine
