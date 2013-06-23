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
    subroutine dichoc(nbt, neq, nno, nc, icodma,&
                      dul, utl, xg, pgl, klv,&
                      duly, dvl, dpe, dve, force,&
                      varmo, varpl, dimele)
        integer :: neq
        integer :: nbt
        integer :: nno
        integer :: nc
        integer :: icodma
        real(kind=8) :: dul(neq)
        real(kind=8) :: utl(neq)
        real(kind=8) :: xg(6)
        real(kind=8) :: pgl(3, 3)
        real(kind=8) :: klv(nbt)
        real(kind=8) :: duly
        real(kind=8) :: dvl(neq)
        real(kind=8) :: dpe(neq)
        real(kind=8) :: dve(neq)
        real(kind=8) :: force(3)
        real(kind=8) :: varmo(8)
        real(kind=8) :: varpl(8)
        integer :: dimele
    end subroutine dichoc
end interface
