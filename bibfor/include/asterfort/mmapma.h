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
    subroutine mmapma(noma, defico, resoco, ndimg, izone,&
                      lexfro, typint, aliase, posmae, nummae,&
                      nnomae, posmam, nummam, ksipr1, ksipr2,&
                      tau1m, tau2m, iptm, iptc, norm,&
                      nommam)
        character(len=8) :: noma
        character(len=24) :: defico
        character(len=24) :: resoco
        integer :: ndimg
        integer :: izone
        logical(kind=1) :: lexfro
        integer :: typint
        character(len=8) :: aliase
        integer :: posmae
        integer :: nummae
        integer :: nnomae
        integer :: posmam
        integer :: nummam
        real(kind=8) :: ksipr1
        real(kind=8) :: ksipr2
        real(kind=8) :: tau1m(3)
        real(kind=8) :: tau2m(3)
        integer :: iptm
        integer :: iptc
        real(kind=8) :: norm(3)
        character(len=8) :: nommam
    end subroutine mmapma
end interface
