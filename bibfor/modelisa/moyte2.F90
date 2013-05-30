subroutine moyte2(fami, npg, poum, temp, iret)
    implicit   none
    include 'asterfort/rcvarc.h'
    integer :: npg, iret, iretm
    real(kind=8) :: temp
    character(len=*) :: fami, poum
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: kpg
    real(kind=8) :: tg, tgtot, tm, ti, ts
!
    tgtot = 0.d0
    do 10 kpg = 1, npg
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    1, tm, iretm)
        call rcvarc(' ', 'TEMP_INF', poum, fami, kpg,&
                    1, ti, iret)
        call rcvarc(' ', 'TEMP_SUP', poum, fami, kpg,&
                    1, ts, iret)
        if (iret .eq. 1) then
            temp = tm
            goto 9999
        else if (iretm .ne. 0) then
            tm=(ti+ts)/2.d0
        endif
        tg=(4.d0*tm+ti+ts)/6.0d0
        tgtot = tgtot + tg
10  end do
    temp = tgtot/npg
9999  continue
end subroutine
