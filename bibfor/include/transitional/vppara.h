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
    subroutine vppara(modes, typcon, knega, lraide, lmasse,&
                      lamor, mxresf, neq, nfreq, omecor,&
                      dlagr, dbloq, vectr, vectc, nbpari,&
                      nbparr, nbpark, nopara, mod45, resui,&
                      resur, resuk, ktyp, lcomod, icom1,&
                      icom2, typres, nfreqg)
        character(len=8) :: modes
        character(len=16) :: typcon
        character(len=8) :: knega
        integer :: lraide
        integer :: lmasse
        integer :: lamor
        integer :: mxresf
        integer :: neq
        integer :: nfreq
        real(kind=8) :: omecor
        integer :: dlagr(*)
        integer :: dbloq(*)
        real(kind=8) :: vectr(*)
        complex(kind=8) :: vectc(*)
        integer :: nbpari
        integer :: nbparr
        integer :: nbpark
        character(len=*) :: nopara(*)
        character(len=4) :: mod45
        integer :: resui(*)
        real(kind=8) :: resur(*)
        character(len=*) :: resuk(*)
        character(len=1) :: ktyp
        logical :: lcomod
        integer :: icom1
        integer :: icom2
        character(len=16) :: typres
        integer :: nfreqg
    end subroutine vppara
end interface
