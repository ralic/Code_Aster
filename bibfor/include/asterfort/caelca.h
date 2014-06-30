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
    subroutine caelca(modele, chmat, caelem, irana1, icabl,&
                      nbnoca, numaca, quad, regl, relax, &
                      ea, rh1000, prelax, fprg, frco, &
                      frli, sa)
        character(len=8) :: modele
        character(len=8) :: chmat
        character(len=8) :: caelem
        integer :: irana1
        integer :: icabl
        integer :: nbnoca(*)
        character(len=19) :: numaca
        logical(kind=1) :: quad
        character(len=4) :: regl
        logical(kind=1) :: relax
        real(kind=8) :: ea
        real(kind=8) :: rh1000
        real(kind=8) :: prelax
        real(kind=8) :: fprg
        real(kind=8) :: frco
        real(kind=8) :: frli
        real(kind=8) :: sa
    end subroutine caelca
end interface
