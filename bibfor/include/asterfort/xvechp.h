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
    subroutine xvechp(ndim, elrefp, nnop, igeom, itemp,&
                      itps, ihechp, jptint, jcface,&
                      jlonch, jlst, jbasec, nfh, nfe,&
                      fonree, ivectt, heavn)
        integer :: nfe
        integer :: nfh
        integer :: nnop
        integer :: ndim
        character(len=8) :: elrefp
        integer :: igeom
        integer :: itemp
        integer :: itps
        integer :: ihechp
        integer :: jptint
        integer :: jcface
        integer :: jlonch
        integer :: jlst
        integer :: jbasec
        integer :: heavn(27,5)
        character(len=4) :: fonree
        integer :: ivectt
    end subroutine xvechp
end interface
