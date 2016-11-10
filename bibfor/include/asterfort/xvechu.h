!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine xvechu(ndim, nnop, nnops, ddls, ddlm, pla,&
                      lamb, am, delta, r, p, ffp, jac, ffc, vect,&
                      ncompn, jheavn, ifiss, nfiss, nfh,&
                      ifa, jheafa, ncomph)
                           
        integer :: ndim
        integer :: nnop
        integer :: nnops
        integer :: ddls
        integer :: ddlm
        integer :: pla(27)
        real(kind=8) :: lamb(3)
        real(kind=8) :: am(3)
        real(kind=8) :: delta(6)
        real(kind=8) :: r
        real(kind=8) :: p(3,3)
        real(kind=8) :: ffp(27)
        real(kind=8) :: jac
        real(kind=8) :: ffc(16)
        real(kind=8) :: vect(560)
        integer :: ncompn
        integer :: jheavn
        integer :: nfiss
        integer :: ifiss
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer :: ncomph
    end subroutine xvechu
end interface
