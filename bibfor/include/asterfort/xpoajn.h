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
    subroutine xpoajn(maxfem, ino, lsn, jdirno, prefno,&
                      nfiss, he, nnn, inn, inntot,&
                      nbnoc, nbnofi, inofi, co, iacoo2)
        integer :: nfiss
        character(len=8) :: maxfem
        integer :: ino
        real(kind=8) :: lsn(nfiss)
        integer :: jdirno
        character(len=2) :: prefno(4)
        integer :: he(nfiss)
        integer :: nnn
        integer :: inn
        integer :: inntot
        integer :: nbnoc
        integer :: nbnofi
        integer :: inofi
        real(kind=8) :: co(3)
        integer :: iacoo2
    end subroutine xpoajn
end interface
