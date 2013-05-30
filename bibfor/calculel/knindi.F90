function knindi(long, kn, lkn, nbkn)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/indk16.h'
    include 'asterfort/indk24.h'
    integer :: nbkn, knindi, long
    character(len=*) :: kn, lkn(*)
! ----------------------------------------------------------------------
! BUT: TROUVER LE RANG D'UNE CHAINE DE CARACTERES DANS UNE LISTE DE
!      CHAINES. ON CHERCHE LE RANG DE SA 1ERE OCCURENCE.
!      SI LA CHAINE N'EST PAS TROUVEE : KNINDI=0
!
! LONG    IN   I  : 8/16/24 : LONGUEUR DES CHAINES DE LKN
! KN      IN   K* : CHAINE A CHERCHER
! LKN     IN   V(K*)  : LISTE DE CHAINES (K8/K16/24)
! NBKN    IN   I  : LONGUEUR DE LA LISTE LKN
! KNINDI  IN   I  : RANG DE LA 1ERE OCCURENCE DE KN DANS LKN
!                   SI KN EST ABSENT: INDK=0
!
! ----------------------------------------------------------------------
    character(len=8) :: k8
    character(len=16) :: k16
    character(len=24) :: k24
! DEB-------------------------------------------------------------------
!
!
    call assert((long.eq.8).or.(long.eq.16).or.(long.eq.24))
!
    if (long .eq. 8) then
        k8 = kn
        knindi = indik8(lkn,k8,1,nbkn)
    else if (long.eq.16) then
        k16 = kn
        knindi = indk16(lkn,k16,1,nbkn)
    else if (long.eq.24) then
        k24 = kn
        knindi = indk24(lkn,k24,1,nbkn)
    endif
!
end function
