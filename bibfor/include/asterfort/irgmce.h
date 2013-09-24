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
    subroutine irgmce(chamsy, partie, ifi, nomcon, ordr,&
                      nbordr, coord, connx, point, nobj,&
                      nbel, nbcmpi, nomcmp, lresu, para,&
                      nomaou, nomain, versio, tycha)
        character(len=*) :: chamsy
        character(len=*) :: partie
        integer :: ifi
        character(len=*) :: nomcon
        integer :: ordr(*)
        integer :: nbordr
        real(kind=8) :: coord(*)
        integer :: connx(*)
        integer :: point(*)
        character(len=24) :: nobj(28)
        integer :: nbel(28)
        integer :: nbcmpi
        character(len=*) :: nomcmp(*)
        logical :: lresu
        real(kind=8) :: para(*)
        character(len=8) :: nomaou
        character(len=8) :: nomain
        integer :: versio
        character(len=8) :: tycha
    end subroutine irgmce
end interface
