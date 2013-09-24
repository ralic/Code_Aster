!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine pecap2(chgeoz, iy, iz, s, alpha,&
                      xg, yg, temp1z, temp2z, ay,&
                      az, ey, ez, pctx, pcty)
        character(len=*) :: chgeoz
        real(kind=8) :: iy
        real(kind=8) :: iz
        real(kind=8) :: s
        real(kind=8) :: alpha
        real(kind=8) :: xg
        real(kind=8) :: yg
        character(len=*) :: temp1z
        character(len=*) :: temp2z
        real(kind=8) :: ay
        real(kind=8) :: az
        real(kind=8) :: ey
        real(kind=8) :: ez
        real(kind=8) :: pctx
        real(kind=8) :: pcty
    end subroutine pecap2
end interface
