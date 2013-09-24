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
    subroutine ircmpn(nofimd, ncmprf, ncmpve, numcmp, exicmp,&
                      nbvato, nbnoec, linoec, adsl, caimpi,&
                      caimpk, profas, innoce)
        integer :: nbvato
        integer :: ncmprf
        character(len=*) :: nofimd
        integer :: ncmpve
        integer :: numcmp(ncmprf)
        logical :: exicmp(nbvato)
        integer :: nbnoec
        integer :: linoec(*)
        integer :: adsl
        integer :: caimpi(10, 1)
        character(len=80) :: caimpk(3, 1)
        integer :: profas(nbvato)
        integer :: innoce(nbvato)
    end subroutine ircmpn
end interface
