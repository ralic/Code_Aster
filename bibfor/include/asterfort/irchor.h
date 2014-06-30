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
    subroutine irchor(ioccur, leresu, lresul, nchsym, nnuord,&
                      nlicmp, novcmp, nnopar, nbnosy, nbordr,&
                      nbrcmp, nbcmdu, nbpara, codret)
        integer :: ioccur
        character(len=8) :: leresu
        logical(kind=1) :: lresul
        character(len=*) :: nchsym
        character(len=*) :: nnuord
        character(len=*) :: nlicmp
        character(len=*) :: novcmp
        character(len=*) :: nnopar
        integer :: nbnosy
        integer :: nbordr
        integer :: nbrcmp
        integer :: nbcmdu
        integer :: nbpara
        integer :: codret
    end subroutine irchor
end interface
