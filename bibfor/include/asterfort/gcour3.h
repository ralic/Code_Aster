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
    subroutine gcour3(resu, noma, coorn, lnoff, trav1,&
                      trav2, trav3, chfond, connex, grlt, thlagr,&
                      thlag2, basfon, nbre, milieu, pair,&
                      ndimte)
        character(len=8) :: resu
        character(len=8) :: noma
        character(len=24) :: coorn
        integer :: lnoff
        character(len=24) :: trav1
        character(len=24) :: trav2
        character(len=24) :: trav3
        character(len=24) :: chfond
        logical(kind=1) :: connex
        character(len=19) :: grlt
        logical(kind=1) :: thlagr
        logical(kind=1) :: thlag2
        character(len=24) :: basfon
        integer :: nbre
        logical(kind=1) :: milieu
        logical(kind=1) :: pair
        integer :: ndimte
    end subroutine gcour3
end interface
