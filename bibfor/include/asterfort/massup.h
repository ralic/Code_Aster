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
    subroutine massup(option, ndim, dlns, nno, nnos,&
                      mate, phenom, npg, ipoids, idfde,&
                      geom, vff1, imatuu, icodre, igeom,&
                      ivf)
        integer :: npg
        integer :: nno
        integer :: ndim
        character(len=16) :: option
        integer :: dlns
        integer :: nnos
        integer :: mate
        character(len=16) :: phenom
        integer :: ipoids
        integer :: idfde
        real(kind=8) :: geom(ndim, nno)
        real(kind=8) :: vff1(nno, npg)
        integer :: imatuu
        integer :: icodre(1)
        integer :: igeom
        integer :: ivf
    end subroutine massup
end interface
