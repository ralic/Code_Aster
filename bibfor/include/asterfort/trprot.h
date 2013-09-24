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
    subroutine trprot(model, bamo, tgeom, imodg, iadx,&
                      iady, iadz, isst, iadrp, norm1,&
                      norm2, ndble, num, nu, ma,&
                      mate, moint, ilires, k, icor)
        character(len=2) :: model
        character(len=8) :: bamo
        real(kind=8) :: tgeom(6)
        integer :: imodg
        integer :: iadx
        integer :: iady
        integer :: iadz
        integer :: isst
        integer :: iadrp
        real(kind=8) :: norm1
        real(kind=8) :: norm2
        integer :: ndble
        character(len=14) :: num
        character(len=14) :: nu
        character(len=8) :: ma
        character(*) :: mate
        character(len=8) :: moint
        integer :: ilires
        integer :: k
        integer :: icor(2)
    end subroutine trprot
end interface
