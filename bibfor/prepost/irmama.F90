subroutine irmama(noma, nbma, nomai, nbgr, nogrm,&
                  nummai, nbmat, noltop)
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
! person_in_charge: nicolas.sellenet at edf.fr
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: noma, nomai(*), nogrm(*), nummai, noltop
    integer :: nbma, nbgr, nbmat
! ----------------------------------------------------------------------
!     BUT :   TROUVER LES NUMEROS DES MAILLES TROUVES DANS
!             UNE LISTE DE MAILLES ET DE GROUP_MA
!     ENTREES:
!        NOMA   : NOM DU MAILLAGE.
!        NBMA   : NOMBRE DE MAILLES
!        NBGR   : NOMBRE DE GROUPES DE MAILLES
!        NOMAI  : NOM DES  MAILLES
!        NOGRM  : NOM DES  GROUP_MA
!     SORTIES:
!        NBMAT  : NOMBRE TOTAL DE NOEUDS A IMPRIMER
!        NUMMAI : NOM DE L'OBJET CONTENANT LES NUMEROS
!                 DES MAILLES TROUVES.
! ----------------------------------------------------------------------
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
    character(len=8) :: nomma, k8bid
    integer :: jnuma, ima, iad, in, jtopo, imai, igr, iret, nbn, lnuma
    integer :: jdime, nbmama, jexma, numa
!
!
    call jemarq()
    nomma=noma
    nbmat= 0
    call jeveuo(noltop, 'E', jtopo)
    call jeveuo(nummai, 'E', jnuma)
    call jelira(nummai, 'LONMAX', lnuma, k8bid)
!
!  --- TRAITEMENT DES LISTES DE MAILLES----
    if (nbma .ne. 0) then
!     --- RECUPERATION DU NUMERO DE MAILLE----
        do 12 imai = 1, nbma
            call jenonu(jexnom(nomma//'.NOMMAI', nomai(imai)), ima)
            if (ima .eq. 0) then
                valk (1) = nomai(imai)
                call u2mesg('A', 'PREPOST5_30', 1, valk, 0,&
                            0, 0, 0.d0)
                nomai(imai) = ' '
            else
                zi(jtopo-1+6) = zi(jtopo-1+6) + 1
                nbmat = nbmat + 1
                if (nbmat .gt. lnuma) then
                    lnuma=2*lnuma
                    call juveca(nummai, lnuma)
                    call jeveuo(nummai, 'E', jnuma)
                endif
                zi(jnuma-1+nbmat)=ima
            endif
12      continue
    endif
!  --- TRAITEMENT DES LISTES DE GROUPES DE MAILLES---
    if (nbgr .ne. 0) then
!     --- RECUPERATION DU NUMERO DE MAILLE----
        call jeveuo(nomma//'.DIME', 'L', jdime)
        nbmama = zi(jdime+3-1)
        call wkvect('&&IRMAMA.MAILLES', 'V V I', nbmama, jexma)
        do 13 igr = 1, nbgr
            call jeexin(jexnom(nomma//'.GROUPEMA', nogrm(igr)), iret)
            if (iret .eq. 0) then
                valk (1) = nogrm(igr)
                call u2mesg('A', 'PREPOST5_31', 1, valk, 0,&
                            0, 0, 0.d0)
                nogrm(igr) = ' '
            else
                call jelira(jexnom(nomma//'.GROUPEMA', nogrm(igr)), 'LONMAX', nbn, k8bid)
                if (nbn .eq. 0) then
                    valk (1) = nogrm(igr)
                    valk (2) = ' '
                    call u2mesg('A', 'PREPOST5_32', 2, valk, 0,&
                                0, 0, 0.d0)
                    nogrm(igr) = ' '
                else
                    zi(jtopo-1+8) = zi(jtopo-1+8) + 1
                    call jeveuo(jexnom(nomma//'.GROUPEMA', nogrm(igr)), 'L', iad)
                    do 14 in = 1, nbn
                        numa = zi(iad+in-1)
                        if (zi(jexma+numa-1) .eq. 0) then
                            nbmat=nbmat+1
                            if (nbmat .gt. lnuma) then
                                lnuma=2*lnuma
                                call juveca(nummai, lnuma)
                                call jeveuo(nummai, 'E', jnuma)
                            endif
                            zi(jnuma-1+nbmat)=numa
                            zi(jexma+numa-1)=1
                        endif
14                  continue
                endif
            endif
13      continue
        call jedetr('&&IRMAMA.MAILLES')
    endif
!
    call jedema()
end subroutine
