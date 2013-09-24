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
    subroutine rc32sa(typz, nommat, mati, matj, snpq,&
                      spij, typeke, spmeca, spther, kemeca,&
                      kether, saltij, sm, fuij)
        character(len=*) :: typz
        character(len=8) :: nommat
        real(kind=8) :: mati(*)
        real(kind=8) :: matj(*)
        real(kind=8) :: snpq
        real(kind=8) :: spij(2)
        real(kind=8) :: typeke
        real(kind=8) :: spmeca(2)
        real(kind=8) :: spther(2)
        real(kind=8) :: kemeca
        real(kind=8) :: kether
        real(kind=8) :: saltij(2)
        real(kind=8) :: sm
        real(kind=8) :: fuij(2)
    end subroutine rc32sa
end interface
