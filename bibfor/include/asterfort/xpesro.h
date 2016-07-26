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
    subroutine xpesro(elrefp, ndim, coorse, igeom, jheavt, ncomp,&
                      heavn, nfh, ddlc, nfe, nfiss,&
                      ise, nnop, jlsn, jlst, ivectu,&
                      fno, imate, jbaslo, jstno)
        integer :: nnop
        integer :: ndim
        character(len=8) :: elrefp
        real(kind=8) :: coorse(*)
        integer :: igeom
        integer :: jheavt
        integer :: ncomp
        integer :: heavn(27,5)
        integer :: nfh
        integer :: ddlc
        integer :: nfe
        integer :: nfiss
        integer :: ise
        integer :: jlsn
        integer :: jlst
        integer :: imate
        integer :: jbaslo
        integer :: jstno
        integer :: ivectu
        real(kind=8) :: fno(ndim*nnop)
    end subroutine xpesro
end interface
