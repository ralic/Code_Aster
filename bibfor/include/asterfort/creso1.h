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
    subroutine creso1(solveu, method, preco, renum, syme,&
                      sdfeti, eps, resire, tbloc, nprec,&
                      nmaxit, istop, niremp, ifm, numsd,&
                      nbma, verif, testco, nbreor, tyreor,&
                      scalin, inumsd, imail, infofe, stogi,&
                      testok, nbreoi, acma, acsm, reacre)
        character(len=19) :: solveu
        character(len=16) :: method
        character(len=8) :: preco
        character(len=8) :: renum
        character(len=3) :: syme
        character(len=24) :: sdfeti
        real(kind=8) :: eps
        real(kind=8) :: resire
        real(kind=8) :: tbloc
        integer :: nprec
        integer :: nmaxit
        integer :: istop
        integer :: niremp
        integer :: ifm
        integer :: numsd
        integer :: nbma
        character(len=8) :: verif
        real(kind=8) :: testco
        integer :: nbreor
        character(len=8) :: tyreor
        character(len=8) :: scalin
        integer :: inumsd
        integer :: imail
        character(len=24) :: infofe
        character(len=8) :: stogi
        logical :: testok
        integer :: nbreoi
        character(len=8) :: acma
        character(len=8) :: acsm
        integer :: reacre
    end subroutine creso1
end interface
