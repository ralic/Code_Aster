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
    subroutine lrcame(nrofic, nochmd, nomamd, nomaas, ligrel,&
                      option, param, typech, typen, npgma,&
                      npgmm, nbcmpv, ncmpva, ncmpvm, iinst,&
                      numpt, numord, inst, crit, prec,&
                      nomgd, ncmprf, jnocmp, chames, codret)
        integer :: nrofic
        character(*) :: nochmd
        character(*) :: nomamd
        character(len=8) :: nomaas
        character(len=19) :: ligrel
        character(len=24) :: option
        character(len=8) :: param
        character(*) :: typech
        integer :: typen
        integer :: npgma(*)
        integer :: npgmm(*)
        integer :: nbcmpv
        character(*) :: ncmpva
        character(*) :: ncmpvm
        integer :: iinst
        integer :: numpt
        integer :: numord
        real(kind=8) :: inst
        character(len=8) :: crit
        real(kind=8) :: prec
        character(len=8) :: nomgd
        integer :: ncmprf
        integer :: jnocmp
        character(len=19) :: chames
        integer :: codret
    end subroutine lrcame
end interface
