subroutine moytem(fami, npg, nspg, poum, temp,&
                  iret)
    implicit   none
#include "asterfort/rcvarc.h"
    integer :: npg, nspg, iret
    real(kind=8) :: temp
    character(len=*) :: fami, poum
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: kpg, ksp
    real(kind=8) :: tg, tgtot
!
    tgtot = 0.d0
    do 10 kpg = 1, npg
        do 20 ksp = 1, nspg
            call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                        ksp, tg, iret)
            if (iret .eq. 1) then
                temp = tg
                goto 9999
            endif
            tgtot = tgtot + tg
20      continue
10  end do
    temp = tgtot/(npg*nspg)
9999  continue
end subroutine
