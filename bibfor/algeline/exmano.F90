subroutine exmano(noma, numnoe, numano, nbmano)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! EXTRACTION DES NUMEROS DES MAILLES DE TYPE SEG2 DONT L'UNE DES
! EXTREMITES EST UN NOEUD DE NUMERO DONNE
!-----------------------------------------------------------------------
!  IN : NOMA   : NOM DU CONCEPT DE TYPE MAILLAGE
!  IN : NUMNOE : NUMERO DU NOEUD CONSIDERE
!  OUT: NUMANO : LISTE DES NUMEROS DES MAILLES SEG2 DONT L'UNE DES
!                EXTREMITES EST LE NOEUD DE NUMERO NUMNOE (CE VECTEUR
!                EST SURDIMENSIONNE LORS DE SA CREATION PAR L'APPELANT)
!  OUT: NBMANO : NOMBRE DE MAILLES SEG2 DONT L'UNE DES EXTREMITES EST
!                LE NOEUD DE NUMERO NUMNOE
!-----------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    character(len=8) :: noma
    integer :: numnoe, numano(*), nbmano
!
    character(len=1) :: k1bid
    character(len=24) :: mlgnma, mlgtma, mlgcnx
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: jdno, jdtm, nbmail, no1, no2, ntseg, numail
    integer :: nutyma
!-----------------------------------------------------------------------
    call jemarq()
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
!
    mlgnma = noma//'.NOMMAI'
    call jelira(mlgnma, 'NOMMAX', nbmail, k1bid)
    mlgtma = noma//'.TYPMAIL'
    call jeveuo(mlgtma, 'L', jdtm)
    mlgcnx = noma//'.CONNEX'
!
    nbmano = 0
    do 10 numail = 1, nbmail
        nutyma = zi(jdtm+numail-1)
        if (nutyma .eq. ntseg) then
            call jeveuo(jexnum(mlgcnx, numail), 'L', jdno)
            no1 = zi(jdno)
            no2 = zi(jdno+1)
            if (no1 .eq. numnoe .or. no2 .eq. numnoe) then
                nbmano = nbmano + 1
                numano(nbmano) = numail
            endif
        endif
10  end do
!
    call jedema()
!
end subroutine
