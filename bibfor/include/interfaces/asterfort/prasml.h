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
    subroutine prasml(option, nugene, tminbl, nomprn, modgen,&
                      tmnobl, tmadbl, knombl, inumbl, conleq,&
                      conlbl)
        character(len=11) :: option
        character(len=14) :: nugene
        character(len=24) :: tminbl
        character(len=8) :: nomprn
        character(len=8) :: modgen
        character(len=24) :: tmnobl
        character(len=24) :: tmadbl
        character(len=24) :: knombl(*)
        integer :: inumbl(*)
        real(kind=8) :: conleq(*)
        real(kind=8) :: conlbl(*)
    end subroutine prasml
end interface
