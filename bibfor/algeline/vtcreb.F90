subroutine vtcreb(champz, numedz, basez, typcz, neq)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     CREATION D'UNE STRUCTURE CHAM_NO "CHAMP"
!
!     IN  CHAMPZ : K19 : NOM DU CHAM_NO A CREER
!     IN  NUMEDZ : K24 : PROF_CHNO DU CHAM_NO
!     IN  BASEZ  : CH1 : NOM DE LA BASE SUR LAQUELLE LE CHAM_NO DOIT
!                        ETRE CREE
!     IN  TYPCZ  :     : TYPE DES VALEURS DU CHAM_NO A CREER
!                  'R'    ==> COEFFICIENTS REELS
!                  'C'    ==> COEFFICIENTS COMPLEXES
!     OUT   NEQ   : I   : INTEGER
!     REMARQUE:  AUCUN CONTROLE SUR LE "TYPC" QUE L'ON PASSE TEL QUEL
!                A JEVEUX_MON_NEVEU
!     PRECAUTIONS D'EMPLOI :
!       1) LE CHAM_NO "CHAMP" NE DOIT PAS EXISTER
!       2) LES COEFFICIENTS DU CHAM_NO "CHAMP" NE SONT PAS AFFECTES
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/gcncon.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/vtcre1.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: champz, numedz, basez, typcz
    integer :: neq
!
!
! DECLARATION VARIABLES LOCALES
    integer :: idime, nbsd, ifetc, idd, ilimpi, ifetn, neql
    character(len=1) :: classe, typc
    character(len=8) :: k8bid
    character(len=11) :: k11b
    character(len=24) :: champ, numedd, method, sdfeti, k24bid, k24b
!
!------------------------------------------------------------------
! INIT.
    call jemarq()
    champ = champz
    numedd = numedz
    classe = basez(1:1)
    typc = typcz(1:1)
!
! --------------------------------------------------------------
! CREATION ET REMPLISSAGE DE LA SD CHAM_NO "MAITRE"
! --------------------------------------------------------------
!
    call vtcre1(champ, numedd, classe, typc, method,&
                sdfeti, neq)
! --------------------------------------------------------------
! CREATION ET REMPLISSAGE DE LA SD CHAM_NO.REFE "ESCLAVE" LIEE A
! CHAQUE SOUS-DOMAINE
! --------------------------------------------------------------
    if (method(1:4) .eq. 'FETI') then
!
        call jeveuo(sdfeti(1:19)//'.FDIM', 'L', idime)
        nbsd=zi(idime)
!
! CONSTITUTION DE L'OBJET JEVEUX .FETC
        call wkvect(champ(1:19)//'.FETC', classe//' V K24', nbsd, ifetc)
! STOCKE &&//NOMPRO(1:6)//'.2.' POUR COHERENCE AVEC L'EXISTANT
        k11b=champ(1:10)//'.'
!
        k24b=' '
        call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
        do 10 idd = 1, nbsd
            if (zi(ilimpi+idd) .eq. 1) then
!
                call jemarq()
! REMPLISSAGE OBJET .FETC
! NOUVELLE CONVENTION POUR LES CHAM_NOS FILS, GESTTION DE NOMS
! ALEATOIRES
                call gcncon('.', k8bid)
                k8bid(1:1)='F'
                k24b(1:19)=k11b//k8bid
                zk24(ifetc+idd-1)=k24b
                call jeveuo(numedd(1:14)//'.FETN', 'L', ifetn)
                call vtcre1(k24b, zk24(ifetn+idd-1), classe, typc, k24bid,&
                            k24bid, neql)
                call jedema()
!
            endif
10      continue
!
    endif
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
!
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
