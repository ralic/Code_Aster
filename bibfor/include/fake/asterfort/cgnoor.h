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
    subroutine cgnoor(mafour, nomail, motfac, iocc, nbmc,&
                      motcle, typmcl, typlig, nbma, ndorig,&
                      ndextr, typm, vecori)
        character(len=24) :: mafour
        character(len=8) :: nomail
        character(*) :: motfac
        integer :: iocc
        integer :: nbmc
        character(len=16) :: motcle(*)
        character(len=16) :: typmcl(*)
        character(*) :: typlig
        integer :: nbma
        character(len=8) :: ndorig
        character(len=8) :: ndextr
        character(len=8) :: typm
        real(kind=8) :: vecori(3)
    end subroutine cgnoor
end interface
