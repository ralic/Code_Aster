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
    subroutine coplas(tempa, k1a, k1b, matrev, lrev,&
                      deklag, prodef, oridef, kal, kbl,&
                      dkma, dkmb, k1acp, k1bcp)
        real(kind=8) :: tempa
        real(kind=8) :: k1a
        real(kind=8) :: k1b
        character(len=8) :: matrev
        real(kind=8) :: lrev
        real(kind=8) :: deklag
        real(kind=8) :: prodef
        character(len=8) :: oridef
        real(kind=8) :: kal
        real(kind=8) :: kbl
        real(kind=8) :: dkma
        real(kind=8) :: dkmb
        real(kind=8) :: k1acp
        real(kind=8) :: k1bcp
    end subroutine coplas
end interface
