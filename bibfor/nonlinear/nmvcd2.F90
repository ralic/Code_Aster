subroutine nmvcd2(indez, chmat, exivc, exiref)
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
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: indez
    character(len=*) :: chmat
    aster_logical :: exivc, exiref
!
!
! ------------------------------------------------------------------
!  TEST SI UNE VARIABLE DE COMMANDE EST PRESENTE
! ------------------------------------------------------------------
! IN   INDEX   K8  INDEX DE LA VARIABLE DE COMMANDE
! IN   CHMAT   K*  SD CHMAT
! OUT  EXIVC    L  TRUE : VARIABLE DE COMMANDE EST PRESENTE
! OUT  EXIREF   L  TRUE :VARIABLE DE COMMANDE (VALE_REF) EST PRESENTE
! ----------------------------------------------------------------------
!
!
!
!
!
    integer :: nmax, i, iret1
    character(len=8) :: index, chmat8
    character(len=8), pointer :: cvrcvarc(:) => null()
!
    call jemarq()
    chmat8=chmat
    index=indez
    exivc=.false.
    call jeexin(chmat8// '.CVRCVARC', iret1)
    if (iret1 .ne. 0) then
        call jelira(chmat8// '.CVRCVARC', 'LONMAX', ival=nmax)
        call jeveuo(chmat8// '.CVRCVARC', 'L', vk8=cvrcvarc)
        do 1 i = 1, nmax
            if (cvrcvarc(i) .eq. index) then
                exivc=.true.
                goto 2
            endif
  1     continue
  2     continue
    endif
!
!
    exiref=.false.
    call jeexin(chmat8//'.'//index//'.1.VALE', iret1)
    if (iret1 .ne. 0) exiref=.true.
!
!
    call jedema()
end subroutine
