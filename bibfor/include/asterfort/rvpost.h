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
    subroutine rvpost(mcf, iocc, dim, i1, i2,&
                      ncheff, xnomcp, nresu, nch19, nlsmac,&
                      nlsnac, nomtab, xnovar)
        character(len=*) :: mcf
        integer :: iocc
        character(len=2) :: dim
        integer :: i1
        integer :: i2
        character(len=16) :: ncheff
        character(len=24) :: xnomcp
        character(len=8) :: nresu
        character(len=19) :: nch19
        character(len=24) :: nlsmac
        character(len=24) :: nlsnac
        character(len=19) :: nomtab
        character(len=24) :: xnovar
    end subroutine rvpost
end interface
