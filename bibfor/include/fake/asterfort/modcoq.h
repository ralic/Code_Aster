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
    subroutine modcoq(base, nuor, nbm, mater1, mater2,&
                      noma, nomgrp, iaxe, kec, geom,&
                      vicoq, torco, tcoef, ifreba)
        integer :: nbm
        character(len=8) :: base
        integer :: nuor(nbm)
        character(len=8) :: mater1
        character(len=8) :: mater2
        character(len=8) :: noma
        character(len=24) :: nomgrp(*)
        integer :: iaxe
        integer :: kec
        real(kind=8) :: geom(9)
        integer :: vicoq(nbm)
        real(kind=8) :: torco(4, nbm)
        real(kind=8) :: tcoef(10, nbm)
        integer :: ifreba
    end subroutine modcoq
end interface
