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
    subroutine mefist(melflu, ndim, som, alpha, ru,&
                      promas, provis, matma, numgrp, nuor,&
                      freq, masg, fact, facpar, vite,&
                      xint, yint, rint, z, phix,&
                      phiy, defm, itypg, zg, hg,&
                      dg, tg, cdg, cpg, rugg,&
                      base)
        character(len=19) :: melflu
        integer :: ndim(14)
        real(kind=8) :: som(9)
        real(kind=8) :: alpha
        real(kind=8) :: ru
        character(len=8) :: promas
        character(len=8) :: provis
        real(kind=8) :: matma(*)
        integer :: numgrp(*)
        integer :: nuor(*)
        real(kind=8) :: freq(*)
        real(kind=8) :: masg(*)
        real(kind=8) :: fact(*)
        real(kind=8) :: facpar(*)
        real(kind=8) :: vite(*)
        real(kind=8) :: xint(*)
        real(kind=8) :: yint(*)
        real(kind=8) :: rint(*)
        real(kind=8) :: z(*)
        real(kind=8) :: phix(*)
        real(kind=8) :: phiy(*)
        real(kind=8) :: defm(*)
        integer :: itypg(*)
        real(kind=8) :: zg(*)
        real(kind=8) :: hg(*)
        real(kind=8) :: dg(*)
        real(kind=8) :: tg(*)
        real(kind=8) :: cdg(*)
        real(kind=8) :: cpg(*)
        real(kind=8) :: rugg(*)
        character(len=8) :: base
    end subroutine mefist
end interface
