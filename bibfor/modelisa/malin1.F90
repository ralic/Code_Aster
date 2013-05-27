subroutine malin1(motfaz, chargz, iocc, indmot, lisnoz,&
                  lonlis)
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
!     PAR LES MOTS-CLES : GROUP_MA OU MAILLE
!     APRES LE MOT-FACTEUR LIAISON_ELEM.
!     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES NOEUDS.
!
! IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_ELEM'
! IN       : CHARGZ : NOM D'UNE SD CHARGE
! IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
! IN       : INDMOT : INDICE = 0 --> TRAITEMENT DES MOTS-CLES
!                                    'GROUP_MA' OU 'MAILLE'
!                            = 1 --> TRAITEMENT DES MOTS-CLES
!                                     'GROUP_MA_1' OU 'MAILLE_1'
!                            = 2 --> TRAITEMENT DES MOTS-CLES
!                                     'GROUP_MA_2' OU 'MAILLE_2
! OUT      : LISNOZ : NOM DE LA LISTE DES NOEUDS
! OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
! ----------------------------------------------------------------------
!
    character(len=8) :: charge
    character(len=8) :: k8bid, noma, nomnoe, nomail
    character(len=16) :: momail, mogrma
    character(len=16) :: motfac
    character(len=24) :: noeuma, mailma, grmama, lisnoe
    character(len=1) :: k1bid
    integer :: iarg
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ibid, idim1, idim2, idimax, ier, igr, ima
    integer :: in1, indlis, indmot, indnoe, ino, iocc, jdes
    integer :: jgro, jind, jjj1, jjj2, jlist, lonlis, m
    integer :: n1, n2, nbma, nbmail, ng, ngr, nliai
    integer :: nmai, numail
!-----------------------------------------------------------------------
    call jemarq()
    charge = chargz
    motfac = motfaz
    lisnoe = lisnoz
!
    if (indmot .eq. 0) then
        momail = 'MAILLE'
        mogrma = 'GROUP_MA'
    else if (indmot.eq.1) then
        momail = 'MAILLE_1'
        mogrma = 'GROUP_MA_1'
    else if (indmot.eq.2) then
        momail = 'MAILLE_2'
        mogrma = 'GROUP_MA_2'
    endif
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 9999
!
    call dismoi('F', 'NOM_MAILLA', charge, 'CHARGE', ibid,&
                noma, ier)
!
    noeuma = noma//'.NOMNOE'
    mailma = noma//'.NOMMAI'
    grmama = noma//'.GROUPEMA'
!
    idimax = 0
    idim1 = 0
    idim2 = 0
!
!     -- CALCUL DE IDIM1=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
!        ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
!        AUX GROUP_MA DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, mogrma, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call wkvect('&&MALIN1.TRAV1', 'V V K24', ng, jjj1)
        call getvem(noma, 'GROUP_MA', motfac, mogrma, iocc,&
                    iarg, ng, zk24( jjj1), ngr)
        do 10 igr = 1, ngr
            call jeveuo(jexnom(grmama, zk24(jjj1+igr-1)), 'L', jgro)
            call jelira(jexnom(grmama, zk24(jjj1+igr-1)), 'LONUTI', nbmail, k1bid)
            do 20 m = 1, nbmail
                numail = zi(jgro-1+m)
                call jenuno(jexnum(mailma, numail), nomail)
                call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n1, k1bid)
                idim1 = idim1 + n1
20          continue
10      continue
    endif
!
!     -- CALCUL DE IDIM2=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
!        ET VERIFICATION DE L'APPARTENANCE DES MAILLES
!        AUX MAILLES DU MAILLAGE
!        -------------------------------------------------------
    call getvtx(motfac, momail, iocc, iarg, 0,&
                k8bid, nbma)
    if (nbma .ne. 0) then
        nbma = -nbma
        call wkvect('&&MALIN1.TRAV2', 'V V K8', nbma, jjj2)
        call getvem(noma, 'MAILLE', motfac, momail, iocc,&
                    iarg, nbma, zk8( jjj2), nmai)
        do 30 ima = 1, nmai
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj2+ima-1)), ibid)
            call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n2, k1bid)
            idim2 = idim2 + n2
30      continue
    endif
!
!     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS
!    ----------------------------------------------------------
    idimax = idim1 + idim2
!
!     -- ALLOCATION DU TABLEAU DES NOMS DE NOEUDS
!    ----------------------------------------------
    call wkvect(lisnoe, 'V V K8', idimax, jlist)
!
    indnoe = 0
!
    call getvtx(motfac, mogrma, iocc, iarg, 0,&
                k8bid, ng)
    if (ng .ne. 0) then
        ng = -ng
        call getvtx(motfac, mogrma, iocc, iarg, ng,&
                    zk24(jjj1), ngr)
        do 40 igr = 1, ngr
            call jeveuo(jexnom(grmama, zk24(jjj1+igr-1)), 'L', jgro)
            call jelira(jexnom(grmama, zk24(jjj1+igr-1)), 'LONUTI', nbmail, k1bid)
            do 50 m = 1, nbmail
                numail = zi(jgro-1+m)
                call jenuno(jexnum(mailma, numail), nomail)
                call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n1, k1bid)
                do 60 ino = 1, n1
                    call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                    indnoe = indnoe + 1
                    zk8(jlist+indnoe-1) = nomnoe
60              continue
50          continue
40      continue
    endif
!
    call getvtx(motfac, momail, iocc, iarg, 0,&
                k8bid, nbma)
    if (nbma .ne. 0) then
        nbma = -nbma
        call getvtx(motfac, momail, iocc, iarg, nbma,&
                    zk8(jjj2), nmai)
        do 70 ima = 1, nmai
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj2+ima-1)), ibid)
            call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
            call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj2+ima-1)), ibid)
            call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n2, k1bid)
            do 80 ino = 1, n2
                call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                indnoe = indnoe + 1
                zk8(jlist+indnoe-1) = nomnoe
80          continue
70      continue
    endif
!
!     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
!        DE LA LISTE
!    -------------------------------------------------------------
    call wkvect('&&MALIN1.TRAV3', 'V V I', idimax, jind)
!
    do 90 ino = 1, idimax
        do 100 in1 = ino+1, idimax
            if (zk8(jlist+in1-1) .eq. zk8(jlist+ino-1)) then
                zi(jind+in1-1) = 1
            endif
100      continue
90  end do
!
    indlis = 0
!
    do 110 ino = 1, idimax
        if (zi(jind+ino-1) .eq. 0) then
            indlis = indlis + 1
            zk8(jlist+indlis-1) = zk8(jlist+ino-1)
        endif
110  end do
!
    lonlis = indlis
!
    call jedetr('&&MALIN1.TRAV1')
    call jedetr('&&MALIN1.TRAV2')
    call jedetr('&&MALIN1.TRAV3')
!
9999  continue
    call jedema()
end subroutine
