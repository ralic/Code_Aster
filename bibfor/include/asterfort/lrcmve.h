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
    subroutine lrcmve(ntvale, nmatyp, nbnoma, ntproa, lgproa,&
                      ncmprf, nomcmr, ntypel, npgmax, indpg,&
                      nbcmfi, nmcmfi, nbcmpv, ncmpvm, numcmp,&
                      jnumma, nochmd, nbma, npgma, npgmm,&
                      typech, nutyma, adsl, adsv, adsd,&
                      lrenum, nuanom, codret)
        integer :: nbma
        integer :: npgmax
        integer :: ntypel
        character(len=*) :: ntvale
        integer :: nmatyp
        integer :: nbnoma
        character(len=*) :: ntproa
        integer :: lgproa
        integer :: ncmprf
        character(len=*) :: nomcmr(*)
        integer :: indpg(ntypel, npgmax)
        integer :: nbcmfi
        character(len=*) :: nmcmfi
        integer :: nbcmpv
        character(len=*) :: ncmpvm
        character(len=*) :: numcmp
        integer :: jnumma
        character(len=*) :: nochmd
        integer :: npgma(nbma)
        integer :: npgmm(nbma)
        character(len=*) :: typech
        integer :: nutyma
        integer :: adsl
        integer :: adsv
        integer :: adsd
        integer :: nuanom(69, 27)
        aster_logical :: lrenum
        integer :: codret
    end subroutine lrcmve
end interface
