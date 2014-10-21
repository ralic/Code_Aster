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
    subroutine xitghm(modint, mecani, press1, ndim, nno,&
                      nnos, nnom, npi, npg, nddls,&
                      nddlm, dimuel, ddld, ddlm, nnop,&
                      nnops, nnopm, ipoids, ivf, idfde, ddlp)
        character(len=3) :: modint
        integer :: mecani(5)
        integer :: press1(7)
        integer :: ndim
        integer :: nno
        integer :: nnos
        integer :: nnom
        integer :: npi
        integer :: npg
        integer :: nddls
        integer :: nddlm
        integer :: dimuel
        integer :: ddld
        integer :: ddlm
        integer :: nnop
        integer :: nnops
        integer :: nnopm
        integer :: ipoids
        integer :: ivf
        integer :: idfde
        integer :: ddlp
    end subroutine xitghm
end interface 
