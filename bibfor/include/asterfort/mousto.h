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
    subroutine mousto(guidag, dimtub, voltub, tubuse, dimobs,&
                      volobs, obsuse, rcray, rcarte, sect,&
                      arete, arete2, ns, obcont, epais,&
                      ecray, resu, denc, perce)
        character(len=8) :: guidag
        integer :: dimtub
        real(kind=8) :: voltub(*)
        real(kind=8) :: tubuse(*)
        integer :: dimobs
        real(kind=8) :: volobs(*)
        real(kind=8) :: obsuse(*)
        real(kind=8) :: rcray
        real(kind=8) :: rcarte
        real(kind=8) :: sect(*)
        real(kind=8) :: arete
        real(kind=8) :: arete2
        integer :: ns
        character(len=8) :: obcont
        real(kind=8) :: epais
        real(kind=8) :: ecray
        character(len=19) :: resu
        real(kind=8) :: denc
        real(kind=8) :: perce
    end subroutine mousto
end interface
