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
    subroutine xbsir2(elref, contac, ddlc, ddlm, ddls,&
                      igeom, jheavn, jlst, ivectu, singu,&
                      nddl, ndim, nfe, nfh, nfiss,&
                      nno, nnom, nnos, depref, sigref,&
                      jbaslo, jstno, jlsn)
        character(len=8) :: elref
        integer :: contac
        integer :: ddlc
        integer :: ddlm
        integer :: ddls
        integer :: igeom
        integer :: jheavn
        integer :: jlst
        integer :: ivectu
        integer :: singu
        integer :: nddl
        integer :: ndim
        integer :: nfe
        integer :: nfh
        integer :: nfiss
        integer :: nno
        integer :: nnom
        integer :: nnos
        real(kind=8) :: depref
        real(kind=8) :: sigref
        integer :: jbaslo
        integer :: jstno
        integer :: jlsn
    end subroutine xbsir2
end interface
