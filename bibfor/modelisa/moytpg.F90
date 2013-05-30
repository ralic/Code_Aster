subroutine moytpg(fami, kp, nspg, poum, temp,&
                  iret)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit   none
    include 'asterfort/rcvarc.h'
    integer :: kp, nspg, iret
    character(len=*) :: fami, poum
    real(kind=8) :: temp
!
!
!
    integer :: kpg
    real(kind=8) :: tg, tgtot
!
    tgtot = 0.d0
    do 20 kpg = 1, nspg
        call rcvarc(' ', 'TEMP', poum, fami, kp,&
                    kpg, tg, iret)
        if (iret .eq. 1) goto 9999
        tgtot = tgtot + tg
20  end do
    temp = tgtot/nspg
9999  continue
end subroutine
