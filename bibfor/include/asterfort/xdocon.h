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
    subroutine xdocon(algocr, algofr, cface, contac, coefcp,&
                      coeffp, coefcr, coeffr, elc, fpg,&
                      ifiss, ivff, jcface, jdonco, jlonch,&
                      mu, nspfis, ncompd, ndim, nface,&
                      ninter, nnof, nomte, npgf, nptf,&
                      rela)
        integer :: algocr
        integer :: algofr
        integer :: cface(18, 6)
        integer :: contac
        real(kind=8) :: coefcp
        real(kind=8) :: coeffp
        real(kind=8) :: coefcr
        real(kind=8) :: coeffr
        character(len=8) :: elc
        character(len=8) :: fpg
        integer :: ifiss
        integer :: ivff
        integer :: jcface
        integer :: jdonco
        integer :: jlonch
        real(kind=8) :: mu
        integer :: nspfis
        integer :: ncompd
        integer :: ndim
        integer :: nface
        integer :: ninter
        integer :: nnof
        character(len=16) :: nomte
        integer :: npgf
        integer :: nptf
        real(kind=8) :: rela
    end subroutine xdocon
end interface
