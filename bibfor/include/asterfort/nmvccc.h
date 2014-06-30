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
    subroutine nmvccc(modele, nbin, nbout, lpain, lchin,&
                      lpaout, lchout, exitem, exihyd, exipto,&
                      exisec, exiepa, exipha, vecel)
        integer :: nbout
        integer :: nbin
        character(len=8) :: modele
        character(len=8) :: lpain(nbin)
        character(len=19) :: lchin(nbin)
        character(len=8) :: lpaout(nbout)
        character(len=19) :: lchout(nbout)
        logical(kind=1) :: exitem
        logical(kind=1) :: exihyd
        logical(kind=1) :: exipto
        logical(kind=1) :: exisec
        logical(kind=1) :: exiepa
        logical(kind=1) :: exipha
        character(len=19) :: vecel
    end subroutine nmvccc
end interface
