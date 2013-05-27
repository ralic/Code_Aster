subroutine malino(motfaz, chargz, iocc, lisnoz, lonlis)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: motfaz, chargz, lisnoz
! ----------------------------------------------------------------------
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
!
!     CREATION DU VECTEUR DE K8 DE NOM LISNOZ ET DE LONGUEUR
!     LONLIS.
!     CE VECTEUR CONTIENT LA LISTE DES NOMS DES NOEUDS DEFINIS
!     PAR LES MOTS-CLES : GROUP_MA, GROUP_NO, MAILLE OU NOEUD
!     APRES LES MOTS-FACTEURS LIAISON_UNIF OU LIAISON_SOLIDE.
!     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES NOEUDS.
!
! IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_UNIF' OU
!                                     'LIAISON_SOLIDE'
! IN       : CHARGZ : NOM D'UNE SD CHARGE
! IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
! OUT      : LISNOZ : NOM DE LA LISTE DES NOEUDS
! OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
! ----------------------------------------------------------------------
!
    character(len=8) :: charge
    character(len=8) :: k8bid, noma, nomnoe, nomail
    character(len=8) :: monoeu, mogrno, momail, mogrma
    character(len=16) :: motfac
    character(len=24) :: noeuma, grnoma, mailma, grmama, lisnoe
    character(len=1) :: k1bid
    integer :: iarg
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, idim1, idim2, idim3, idim4, idimax, ier
    integer :: igr, ima, in, in1, indlis, indnoe, ino
    integer :: iocc, jdes, jgro, jind, jjj, jlist, lonlis
    integer :: m, n1, n3, n4, nbma, nbmail, nbno
    integer :: ng, ngr, nliai, nmai, nno, numail
!-----------------------------------------------------------------------
    call jemarq()
    charge = chargz
    motfac = motfaz
    lisnoe = lisnoz
!
    monoeu = 'NOEUD'
    mogrno = 'GROUP_NO'
    momail = 'MAILLE'
    mogrma = 'GROUP_MA'
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 9999
!
    call dismoi('F', 'NOM_MAILLA', charge, 'CHARGE', ibid,&
                noma, ier)
!
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
    mailma = noma//'.NOMMAI'
    grmama = noma//'.GROUPEMA'
!
    idimax = 0
    idim1 = 0
    idim2 = 0
    idim3 = 0
    idim4 = 0
!
!     -- CALCUL DE IDIM1 = NB_NOEUD/GROUP_NO*NB_GROUP_NO
!        ET VERIFICATION DE L'APPARTENANCE DES GROUP_NO
!        AUX GROUP_NO DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, mogrno, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call wkvect('&&MALINO.TRAV', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_NO', motfac, mogrno, iocc,&
                    iarg, ng, zk24( jjj), ngr)
        do 10 igr = 1, ngr
            call jelira(jexnom(grnoma, zk24(jjj+igr-1)), 'LONUTI', n1, k1bid)
            idim1 = idim1 + n1
10      continue
    endif
!
!     -- CALCUL DE IDIM2 = NB_NOEUD DE LA LISTE DE NOEUDS
!        ET VERIFICATION DE L'APPARTENANCE DES NOEUDS
!        AUX NOEUDS DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, monoeu, iocc, iarg, 0,&
                k8bid, nbno)
    if (nbno .ne. 0) then
        nbno = -nbno
        call wkvect('&&MALINO.TRAV', 'V V K8', nbno, jjj)
        call getvem(noma, 'NOEUD', motfac, monoeu, iocc,&
                    iarg, nbno, zk8( jjj), nno)
        idim2 = idim2 + nno
    endif
!
!     -- CALCUL DE IDIM3=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
!        ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
!        AUX GROUP_MA DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, mogrma, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call wkvect('&&MALINO.TRAV', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', motfac, mogrma, iocc,&
                    iarg, ng, zk24( jjj), ngr)
        do 20 igr = 1, ngr
            call jeveuo(jexnom(grmama, zk24(jjj+igr-1)), 'L', jgro)
            call jelira(jexnom(grmama, zk24(jjj+igr-1)), 'LONUTI', nbmail, k1bid)
            do 30 m = 1, nbmail
                numail = zi(jgro-1+m)
                call jenuno(jexnum(mailma, numail), nomail)
                call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n3, k1bid)
                idim3 = idim3 + n3
30          continue
20      continue
    endif
!
!     -- CALCUL DE IDIM4=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
!        ET VERIFICATION DE L'APPARTENANCE DES MAILLES
!        AUX MAILLES DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, momail, iocc, iarg, 0,&
                k8bid, nbma)
    if (nbma .ne. 0) then
        nbma = -nbma
        call wkvect('&&MALINO.TRAV', 'V V K8', nbma, jjj)
        call getvem(noma, 'MAILLE', motfac, momail, iocc,&
                    iarg, nbma, zk8( jjj), nmai)
        do 40 ima = 1, nmai
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
            call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n4, k1bid)
            idim4 = idim4 + n4
40      continue
    endif
!
!     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS
!    ----------------------------------------------------------
    idimax = max(idim1,idimax)
    idimax = max(idim2,idimax)
    idimax = max(idim3,idimax)
    idimax = max(idim4,idimax)
!
!     -- ALLOCATION DES TABLEAUX DES NOMS DE NOEUDS
!    ----------------------------------------------
    call wkvect(lisnoe, 'V V K8', idimax, jlist)
!
    indnoe = 0
    call getvtx(motfac, mogrno, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call getvtx(motfac, mogrno, iocc, iarg, ng,&
                    zk24(jjj), ngr)
        do 50 igr = 1, ngr
            call jeveuo(jexnom(grnoma, zk24(jjj+igr-1)), 'L', jgro)
            call jelira(jexnom(grnoma, zk24(jjj+igr-1)), 'LONUTI', n1, k1bid)
            do 60 ino = 1, n1
                in = zi(jgro+ino-1)
                indnoe = indnoe + 1
                call jenuno(jexnum(noeuma, in), nomnoe)
                zk8(jlist+indnoe-1) = nomnoe
60          continue
50      continue
    endif
!
    call getvtx(motfac, monoeu, iocc, iarg, 0,&
                k8bid, nbno)
    if (nbno .ne. 0) then
        nbno = -nbno
        call getvtx(motfac, monoeu, iocc, iarg, nbno,&
                    zk8(jjj), nno)
        do 70 ino = 1, nno
            indnoe = indnoe + 1
            zk8(jlist+indnoe-1) = zk8(jjj+ino-1)
70      continue
    endif
!
    call getvtx(motfac, mogrma, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call getvtx(motfac, mogrma, iocc, iarg, ng,&
                    zk24(jjj), ngr)
        do 80 igr = 1, ngr
            call jeveuo(jexnom(grmama, zk24(jjj+igr-1)), 'L', jgro)
            call jelira(jexnom(grmama, zk24(jjj+igr-1)), 'LONUTI', nbmail, k1bid)
            do 90 m = 1, nbmail
                numail = zi(jgro-1+m)
                call jenuno(jexnum(mailma, numail), nomail)
                call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n3, k1bid)
                do 100 ino = 1, n3
                    call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                    indnoe = indnoe + 1
                    zk8(jlist+indnoe-1) = nomnoe
100              continue
90          continue
80      continue
    endif
!
    call getvtx(motfac, momail, iocc, iarg, 0,&
                k8bid, nbma)
    if (nbma .ne. 0) then
        nbma = -nbma
        call getvtx(motfac, momail, iocc, iarg, nbma,&
                    zk8(jjj), nmai)
        do 110 ima = 1, nmai
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
            call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
            call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n4, k1bid)
            do 120 ino = 1, n4
                call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                indnoe = indnoe + 1
                zk8(jlist+indnoe-1) = nomnoe
120          continue
110      continue
    endif
!
!     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
!        DE LA LISTE
!    -------------------------------------------------------------
    call wkvect('&&MALINO.INDICE', 'V V I', idimax, jind)
!
    do 130 ino = 1, idimax
        do 140 in1 = ino+1, idimax
            if (zk8(jlist+in1-1) .eq. zk8(jlist+ino-1)) then
                zi(jind+in1-1) = 1
            endif
140      continue
130  continue
!
    indlis = 0
    do 150 ino = 1, idimax
        if (zi(jind+ino-1) .eq. 0) then
            indlis = indlis + 1
            zk8(jlist+indlis-1) = zk8(jlist+ino-1)
        endif
150  end do
!
    lonlis = indlis
!
    call jedetr('&&MALINO.TRAV')
    call jedetr('&&MALINO.INDICE')
!
9999  continue
    call jedema()
end subroutine
