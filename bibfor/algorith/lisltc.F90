subroutine lisltc(lischa, ichar, typech)
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lisnnb.h"
    character(len=19) :: lischa
    integer :: ichar
    character(len=8) :: typech
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! RETOURNE LE TYPE DE LA CHARGE
!
! ----------------------------------------------------------------------
!
!
! IN  LISCHA : SD LISTE DES CHARGES
! IN  ICHAR  : INDICE DE LA CHARGE
! OUT TYPECH : TYPE DE LA CHARGE
!               'REEL'    - CHARGE CONSTANTE REELLE
!               'COMP'    - CHARGE CONSTANTE COMPLEXE
!               'FONC_F0' - CHARGE FONCTION QUELCONQUE
!               'FONC_FT' - CHARGE FONCTION DU TEMPS
!
!
!
!
    character(len=24) :: typcha
    integer :: jtypc
    integer :: nbchar
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    typech = ' '
    call lisnnb(lischa, nbchar)
!
    if (nbchar .ne. 0) then
        typcha = lischa(1:19)//'.TYPC'
        call jeveuo(typcha, 'L', jtypc)
        typech = zk8(jtypc-1+ichar)
    endif
!
    call jedema()
end subroutine
