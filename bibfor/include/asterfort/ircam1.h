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
    subroutine ircam1(nofimd, nochmd, existc, ncmprf, numpt,&
                      instan, numord, adsd, adsv, adsl,&
                      adsk, partie, ncmpve, ntlcmp, ntncmp,&
                      ntucmp, ntproa, nbimpr, caimpi, caimpk,&
                      typech, nomamd, nomtyp, modnum, nuanom,&
                      codret)
        integer :: nbimpr
        character(len=*) :: nofimd
        character(len=64) :: nochmd
        integer :: existc
        integer :: ncmprf
        integer :: numpt
        real(kind=8) :: instan
        integer :: numord
        integer :: adsd
        integer :: adsv
        integer :: adsl
        integer :: adsk
        character(len=*) :: partie
        integer :: ncmpve
        character(len=24) :: ntlcmp
        character(len=24) :: ntncmp
        character(len=24) :: ntucmp
        character(len=24) :: ntproa
        integer :: caimpi(10, nbimpr)
        character(len=*) :: caimpk(3, nbimpr)
        character(len=8) :: typech
        character(len=*) :: nomamd
        character(len=8) :: nomtyp(*)
        integer :: modnum(69)
        integer :: nuanom(69, *)
        integer :: codret
    end subroutine ircam1
end interface
