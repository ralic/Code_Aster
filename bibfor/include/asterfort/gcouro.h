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
    subroutine gcouro(base, resu, noma, nomno, coorn,&
                      lobj2, trav1, trav2, trav3, dir,&
                      nomnoe, fond, direc, stok4)
        character(len=1) :: base
        character(len=24) :: resu
        character(len=8) :: noma
        character(len=24) :: nomno
        character(len=24) :: coorn
        integer :: lobj2
        character(len=24) :: trav1
        character(len=24) :: trav2
        character(len=24) :: trav3
        real(kind=8) :: dir(3)
        character(len=8) :: nomnoe(*)
        character(len=8) :: fond
        logical(kind=1) :: direc
        character(len=24) :: stok4
    end subroutine gcouro
end interface
