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
    subroutine nmpoel(nomte, npg, klv, xl, nno,&
                      nc, pgl, pgl1, pgl2, ugl,&
                      epsthe, e, em, effm, fl,&
                      effl, angs2, rad)
        character(len=*) :: nomte
        integer :: npg
        real(kind=8) :: klv(*)
        real(kind=8) :: xl
        integer :: nno
        integer :: nc
        real(kind=8) :: pgl(*)
        real(kind=8) :: pgl1(*)
        real(kind=8) :: pgl2(*)
        real(kind=8) :: ugl(*)
        real(kind=8) :: epsthe
        real(kind=8) :: e
        real(kind=8) :: em
        real(kind=8) :: effm(*)
        real(kind=8) :: fl(*)
        real(kind=8) :: effl(*)
        real(kind=8) :: angs2
        real(kind=8) :: rad
    end subroutine nmpoel
end interface
