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
    subroutine ratu3d(iprno, lonlis, klisno, noepou, noma,&
                      ligrel, mod, cara, numddl, typlag,&
                      lisrel, coorig, sectio)
        integer :: lonlis
        integer :: iprno(*)
        character(len=8) :: klisno(lonlis)
        character(len=8) :: noepou
        character(len=8) :: noma
        character(len=19) :: ligrel
        character(len=8) :: mod
        character(len=8) :: cara
        character(len=14) :: numddl
        character(len=2) :: typlag
        character(len=19) :: lisrel
        real(kind=8) :: coorig(3)
        real(kind=8) :: sectio
    end subroutine ratu3d
end interface
