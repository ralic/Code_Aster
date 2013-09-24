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
    subroutine rvaffe(mcf, iocc, sdlieu, sdeval, sdmail,&
                      typaff, quant, option, rep, nomtab,&
                      xnovar, ncheff, i1, isd)
        character(len=*) :: mcf
        integer :: iocc
        character(len=24) :: sdlieu
        character(len=19) :: sdeval
        character(len=24) :: sdmail
        character(len=1) :: typaff
        character(len=*) :: quant
        character(len=*) :: option
        character(len=*) :: rep
        character(len=19) :: nomtab
        character(len=24) :: xnovar
        character(len=16) :: ncheff
        integer :: i1
        integer :: isd
    end subroutine rvaffe
end interface
