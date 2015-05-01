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
    subroutine xgrdhm(nomte, ndim, mecani, press1, press2,&
                      tempe, enrmec, dimdef, dimcon, nmec,&
                      np1, np2, nenr, dimenr, enrhyd)
        character(len=16) :: nomte
        integer :: ndim
        integer :: mecani(5)
        integer :: press1(7)
        integer :: press2(7)
        integer :: tempe(5)
        integer :: enrmec(3)
        integer :: dimdef
        integer :: dimcon
        integer :: nmec
        integer :: np1
        integer :: np2
        integer :: nenr
        integer :: dimenr
        integer :: enrhyd(3)
    end subroutine xgrdhm
end interface 
