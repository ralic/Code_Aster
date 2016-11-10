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
    subroutine xvechb(nnops, ddls, ddlm, ndim,&
                      ffp2, q1, dt, ta, jac, q1m, ta1,&
                      q2, q2m, vect, ncompn, jheavn, ifiss,&
                      nfiss, nfh, ifa, jheafa, ncomph)
                           
        integer :: ndim
        integer :: ddls
        integer :: ddlm
        integer :: nnops
        real(kind=8) :: ffp2(27)
        real(kind=8) :: q1
        real(kind=8) :: q1m
        real(kind=8) :: q2
        real(kind=8) :: q2m
        real(kind=8) :: dt
        real(kind=8) :: ta
        real(kind=8) :: jac
        real(kind=8) :: ta1
        real(kind=8) :: vect(560)
        integer :: ncompn
        integer :: jheavn
        integer :: ifiss
        integer :: nfiss
        integer :: nfh
        integer :: ifa
        integer :: jheafa
        integer :: ncomph
    end subroutine xvechb
end interface
