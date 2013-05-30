subroutine vtcopy(chin, chout, kstop, codret)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/vtcop1.h'
    character(len=*) :: chin, chout
    character(len=1) :: kstop
    integer :: codret
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
!     RECOPIE LES VALEURS DU CHAM_NO CHIN DANS LE CHAM_NO CHOUT
!     CETTE ROUTINE PERMET DE CHANGER LA NUMEROTATION D'UN CHAM_NO
!     SI KSTOP.EQ.' ' EN ENTREE ET QUE CODRET != 0 EN SORTIE, ALORS
!     DES COMPOSANTES DU CHAMP CHIN N'ONT PAS PU ETRE RECOPIEES ET
!     ONT ETE MISES A ZERO
!
!     PRECAUTIONS D'EMPLOI :
!     - LES CHAM_NOS DOIVENT EXISTER.
!     - LES DDLS DE "LAGRANGE" SONT MIS A ZERO DANS CHOUT.
!
!     ------------------------------------------------------------------
!
!
!
!
    integer :: iret, nbsd, ilimpi, iret1, ifetc1, ifetc2, idd
    character(len=8) :: k8bid
    character(len=19) :: ch1, ch2
    character(len=24) :: ch1esc, ch2esc
!     ------------------------------------------------------------------
!
    call jemarq()
    ch1 = chin
    ch2 = chout
!
! --- RECOPIE DES CHAM_NOS "MAITRE"
!
    call vtcop1(ch1, ch2, kstop, codret)
    call jeexin(ch1(1:19)//'.FETC', iret)
!
! --- SI FETI, ON DUPLIQUE AUSSI LES CHAM_NO ESCLAVES
    if (iret .ne. 0) then
        call jelira(ch1(1:19)//'.FETC', 'LONMAX', nbsd, k8bid)
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        call jeveuo(ch1(1:19)//'.FETC', 'L', ifetc1)
        call jeexin(ch2(1:19)//'.FETC', iret1)
! --- SI LE CHAM_NO CHOUT N'EST PAS FETI, ON S'ARRETE EN ERREUR FATALE
        if (iret1 .eq. 0) then
            call u2mess('F', 'ALGELINE3_96')
        else
            call jeveuo(ch2(1:19)//'.FETC', 'L', ifetc2)
        endif
! --- BOUCLE SUR LES SOUS-DOMAINES
        do 10 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
                ch1esc=zk24(ifetc1+idd-1)
                ch2esc=zk24(ifetc2+idd-1)
                call vtcop1(ch1esc, ch2esc, kstop, codret)
            endif
10      continue
    endif
!
    call jedema()
end subroutine
