subroutine ssgngm(noma, iocc, nbgnaj)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/gmgnre.h'
    include 'asterfort/jecroc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: noma
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
!     BUT: AJOUTER S'IL LE FAUT A UN MAILLAGE DES GROUP_NO DE MEME NOM
!          QUE LES GROUP_MA ET CONTENANT LES NOEUDS DE CES MAILLES.
!
!     IN: NOMA  : NOM DU MAILLAGE.
!         IOCC  : NUMERO D'OCCURENCE DU MOT CLEF CREA_GROUP_NO
!-----------------------------------------------------------------------
!
    character(len=8) :: k8b, koui
    character(len=16) :: selec
    character(len=24) :: grpma, grpno, nomgno, nomgma
    integer :: iarg
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad2, ialgma, ialima, ialino, ianbno, iangno
    integer :: ibid, ier, ierd, iocc, iret, j, jtrav
    integer :: n1, nb, nbgma, nbgnaj, nbgno, nbma, nbnoto
    integer :: no
!-----------------------------------------------------------------------
    call jemarq()
    grpma = noma//'.GROUPEMA       '
    grpno = noma//'.GROUPENO       '
    nbgnaj = 0
    selec = 'TOUS'
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnoto,&
                k8b, ierd)
    if (nbnoto .eq. 0) goto 60
!
!     ---  CAS : "TOUT_GROUP_MA"
!     --------------------------
    call getvtx('CREA_GROUP_NO', 'TOUT_GROUP_MA', iocc, iarg, 1,&
                koui, n1)
    if (n1 .eq. 1) then
        call jelira(noma//'.GROUPEMA', 'NMAXOC', nbgma, k8b)
        call wkvect('&&SSGNGM.LISTE_GMA', 'V V K24', nbgma, ialgma)
        do 10 i = 1, nbgma
            call jenuno(jexnum(grpma, i), zk24(ialgma-1+i))
10      continue
        iangno = ialgma
!
!     ---  CAS : "GROUP_MA"
!     ---------------------
    else
!
!      CRITERE DE SELECTION
        call getvtx('CREA_GROUP_NO', 'CRIT_NOEUD', iocc, iarg, 1,&
                    selec, ibid)
        call getvem(noma, 'GROUP_MA', 'CREA_GROUP_NO', 'GROUP_MA', iocc,&
                    iarg, 0, k8b, nb)
        call getvtx('CREA_GROUP_NO', 'NOM', iocc, iarg, 0,&
                    k8b, no)
        nbgma = -nb
        call wkvect('&&SSGNGM.LISTE_GMA', 'V V K24', nbgma, ialgma)
        call getvem(noma, 'GROUP_MA', 'CREA_GROUP_NO', 'GROUP_MA', iocc,&
                    iarg, nbgma, zk24(ialgma), nb)
        if (no .ne. 0) then
            nbgno = -no
            if (nbgno .ne. nbgma) call u2mess('F', 'MODELISA7_8')
            call wkvect('&&SSGNGM.NOM_GNO', 'V V K24', nbgno, iangno)
            call getvtx('CREA_GROUP_NO', 'NOM', iocc, iarg, nbgno,&
                        zk24(iangno), no)
        else
            iangno = ialgma
        endif
        ier = 0
        do 20 i = 1, nbgma
            nomgma = zk24(ialgma-1+i)
            call jeexin(jexnom(grpma, nomgma), iret)
            if (iret .eq. 0) then
                ier = ier + 1
                call u2mesk('E', 'ELEMENTS_62', 1, nomgma)
            endif
20      continue
        call assert(ier.eq.0)
    endif
    if (nbgma .eq. 0) goto 60
!
    call wkvect('&&SSGNGM.LISTE_NO ', 'V V I', nbnoto, ialino)
    call wkvect('&&SSGNGM.TRAV ', 'V V I', nbnoto, jtrav)
    call wkvect('&&SSGNGM.NB_NO    ', 'V V I', nbgma, ianbno)
!
! ---------------------------------------------------------------------
!     -- ON AJOUTE LES NOUVEAUX GROUPES:
!
    do 30 i = 1, nbgma
        nomgma = zk24(ialgma-1+i)
        call jelira(jexnom(grpma, nomgma), 'LONUTI', nbma, k8b)
        call jeveuo(jexnom(grpma, nomgma), 'L', ialima)
        call gmgnre(noma, nbnoto, zi(jtrav), zi(ialima), nbma,&
                    zi(ialino), zi(ianbno-1+i), selec)
!
        nomgno = zk24(iangno-1+i)
        n1 = zi(ianbno-1+i)
        call jeexin(jexnom(grpno, nomgno), iret)
        if (iret .gt. 0) then
            call u2mesk('A', 'MODELISA7_9', 1, nomgno)
        else
            call jecroc(jexnom(grpno, nomgno))
            call jeecra(jexnom(grpno, nomgno), 'LONMAX', max(n1, 1), k8b)
            call jeecra(jexnom(grpno, nomgno), 'LONUTI', n1, k8b)
            call jeveuo(jexnom(grpno, nomgno), 'E', iad2)
            do 40 j = 1, n1
                zi(iad2-1+j) = zi(ialino-1+j)
40          continue
            nbgnaj = nbgnaj + 1
        endif
30  end do
!
60  continue
    call jedetr('&&SSGNGM.LISTE_GMA')
    call jedetr('&&SSGNGM.NOM_GNO')
    call jedetr('&&SSGNGM.LISTE_NO')
    call jedetr('&&SSGNGM.TRAV')
    call jedetr('&&SSGNGM.NB_NO')
    call jedema()
end subroutine
