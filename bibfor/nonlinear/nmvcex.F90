subroutine nmvcex(index, varcz, chamz)
!
    implicit none
!
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=4), intent(in) :: index
    character(len=*), intent(in) :: varcz
    character(len=*), intent(out) :: chamz
!
! --------------------------------------------------------------------------------------------------
!
! Command variables utility
!
! Extract command variable
!
! --------------------------------------------------------------------------------------------------
!
! IN   INDEX   K4  INDEX DE LA VARIABLE DE COMMANDE
! IN   COM     K14 SD VARI_COM
! OUT  CHAMP   K19 SD CHAMP_GD  DE LA VARIABLE DE COMMANDE EXTRAITE
!                  ' ' SI INEXISTANT
!
! --------------------------------------------------------------------------------------------------
!
    character(len=14) :: varc
    character(len=19) :: champ
    integer :: iret
!
! --------------------------------------------------------------------------------------------------
!
    varc  = varcz
    champ = varc//'.'// index
    call exisd('CHAMP_GD', champ, iret)
    if (iret .eq. 0) then
        champ = ' '
    endif
    chamz = champ
end subroutine
