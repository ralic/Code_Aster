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
    subroutine nirela(irela, jp, gm, gp, am,&
                      ap, bp, boa, aa, bb,&
                      daa, dbb, dboa, d2boa)
        integer :: irela
        real(kind=8) :: jp
        real(kind=8) :: gm
        real(kind=8) :: gp
        real(kind=8) :: am
        real(kind=8) :: ap
        real(kind=8) :: bp
        real(kind=8) :: boa
        real(kind=8) :: aa
        real(kind=8) :: bb
        real(kind=8) :: daa
        real(kind=8) :: dbb
        real(kind=8) :: dboa
        real(kind=8) :: d2boa
    end subroutine nirela
end interface
