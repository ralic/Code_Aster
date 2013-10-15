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
    subroutine xteddl(ndim, nfh, nfe, ddls, nddl,&
                      nno, nnos, stano, lcontx, matsym,&
                      option, nomte, ddlm,&
                      nfiss, jfisno, mat, vect)
        integer, intent(in) :: nfiss
        integer, intent(in) :: nno
        integer, intent(in) :: ndim
        integer, intent(in) :: nfh
        integer, intent(in) :: nfe
        integer, intent(in) :: ddls
        integer, intent(in) :: nddl
        integer, intent(in) :: nnos
        integer, intent(in) :: stano(*)
        logical, intent(in) :: lcontx
        logical, intent(in) :: matsym
        character(len=16), intent(in) :: option
        character(len=16), intent(in) :: nomte
        integer, intent(in) :: ddlm
        integer, intent(in) :: jfisno
        real(kind=8), optional, intent(inout) :: mat(*)
        real(kind=8), optional, intent(out) :: vect(*)
    end subroutine xteddl
end interface
