!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine fonno6(resu, noma, ndim, ina, nbnose,&
                      iseg, nseg, noe, indr, nbnoel,&
                      vnor, vdir, basseg, vect, sens)
        integer :: ndim
        character(len=8) :: resu
        character(len=8) :: noma
        integer :: ina
        integer :: nbnose
        integer :: iseg
        integer :: nseg
        integer :: noe(4, 4)
        integer :: indr(2)
        integer :: nbnoel
        real(kind=8) :: vnor(2, 3)
        real(kind=8) :: vdir(2, 3)
        character(len=19) :: basseg
        real(kind=8) :: vect(3)
        real(kind=8) :: sens
    end subroutine fonno6
end interface
