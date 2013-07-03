subroutine op0034()
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
    implicit none
!      OPERATEURS :     AFFE_CHAR_THER AFFE_CHAR_THER_F
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/charth.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=4) :: type
    character(len=8) :: char
    character(len=16) :: concep, oper
    integer :: iatype
!
    call jemarq()
    call infmaj()
!
    call getres(char, concep, oper)
    call wkvect(char//'.TYPE', 'G V K8', 1, iatype)
    if (oper .eq. 'AFFE_CHAR_THER') then
        type = 'REEL'
        zk8(iatype) = 'THER_RE'
    else if (oper .eq. 'AFFE_CHAR_THER_F') then
        type = 'FONC'
        zk8(iatype) = 'THER_FO'
    endif
!
    call charth(type)
!
    call jedema()
end subroutine
