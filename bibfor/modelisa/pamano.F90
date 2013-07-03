subroutine pamano(motfaz, moclez, nomaz, listyz, iocc,&
                  lisnoz, lonlis)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=*) :: motfaz, moclez, nomaz, listyz, lisnoz
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
!
!     CREATION DU VECTEUR DE K8 DE NOM LISNOZ ET DE LONGUEUR
!     LONLIS.
!     CE VECTEUR CONTIENT LA LISTE DES NOMS DES NOEUDS DEFINIS
!     PAR LE MOT-CLE : GROUP_MA_1 OU GROUP_MA_2 OU MAILLE_1
!     OU MAILLE_2 APRES LE MOT-FACTEUR LIAISON_GROUP OU
!     LIAISON_UNIL_NO ,
!                   OU GROUP_NO_1 OU GROUP_NO_2
!     APRES LE MOT-FACTEUR LIAISON_GROUP.
!     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES NOEUDS.
!
! IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_GROUP'
! IN       : MOCLEZ : MOT-CLE GROUP_MA_1 OU GROUP_MA_2
!                     OU      MAILLE_1   OU MAILLE_2
!                     OU      GROUP_NO_1 OU GROUP_NO_2
!                     OU      NOEUD_1    OU NOEUD_2
! IN       : NOMAZ  : NOM DU MAILLAGE
! IN       : LISTYZ : LISTE DE NOMS DE TYPES (GEOMETRIQUES) D'ELEMENTS
!                     SI CETTE LISTE N'EST PAS VIDE ON TESTE SI LE
!                     DES ELEMENTS DONNES APRES LES MOTS CLES
!                     MAILLE_I ET GROUP_MA_I APPARTIENT A CETTE LISTE
!                     SI CE N'EST PAS LE CAS ON S'ARRETE EN ERREUR
!                     FATALE
! IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
! OUT      : LISNOZ : NOM DE LA LISTE DES NOEUDS
! OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
!          : LONLIS : <0 SI CONTACT ET POI1
! ----------------------------------------------------------------------
!
    character(len=1) :: k1bid
    character(len=8) :: k8bid, noma, nomnoe, nomail
    character(len=16) :: motfac, motcle
    character(len=16) :: mgrma1, mgrma2, mgrno1, mgrno2
    character(len=16) :: mmail1, mmail2, mnoeu1, mnoeu2
    character(len=24) :: noeuma, grnoma, mailma, grmama, listyp, lisnoe
    character(len=24) :: valk(9)
    integer :: iocc, lonlis, nliai, idimax, idim1, idim2, idim3, idim4, idimp1
    integer :: n1max, ng, jjj, ngr, igr, jgro, nbmail, m, numail, n1, nbma, nmai
    integer :: ima, n2, n3, nbno, nno, ier, jlist, indnoe, jdes, ino, in, jind
    integer :: in1
    integer :: indlis
    integer :: iarg
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ibid, idtyma, idtypi, idtypk, indic, iret
    integer :: lonlit, nutyma
!-----------------------------------------------------------------------
    call jemarq()
    motfac = motfaz
    noma = nomaz
    lisnoe = lisnoz
    listyp = listyz
    motcle = moclez
    lonlit = 0
!
    call getfac(motfac, nliai)
    if (nliai .eq. 0) goto 9999
!
    call jeexin(listyp, iret)
    if (iret .ne. 0) then
        call jelira(listyp, 'LONMAX', lonlit, k1bid)
        call jeveuo(listyp, 'L', idtypk)
        if (lonlit .ne. 0) then
            call jedetr('&&PAMANO.LISTYP')
            call wkvect('&&PAMANO.LISTYP', 'V V I', lonlit, idtypi)
            do 1 i = 1, lonlit
                call jenonu(jexnom('&CATA.TM.NOMTM', zk8(idtypk+i-1)), zi(idtypi+i-1))
 1          continue
            call jeveuo(noma//'.TYPMAIL', 'L', idtyma)
        endif
    endif
!
    mgrma1 = 'GROUP_MA_1'
    mgrma2 = 'GROUP_MA_2'
    mgrno1 = 'GROUP_NO_1'
    mgrno2 = 'GROUP_NO_2'
    mmail1 = 'MAILLE_1'
    mmail2 = 'MAILLE_2'
    mnoeu1 = 'NOEUD_1'
    mnoeu2 = 'NOEUD_2'
!
    noeuma = noma//'.NOMNOE'
    grnoma = noma//'.GROUPENO'
    mailma = noma//'.NOMMAI'
    grmama = noma//'.GROUPEMA'
!
    lonlis = 0
    idimax = 0
    idim1 = 0
    idim2 = 0
    idim3 = 0
    idim4 = 0
    idimp1 = 0
    n1max = 0
!
!     -- TRAITEMENT DES MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
!        -------------------------------------------------------
    if (motcle .eq. mgrma1 .or. motcle .eq. mgrma2) then
!
!           -- CALCUL DE
!              IDIM1=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
!              ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
!              AUX GROUP_MA DU MAILLAGE
!              -----------------------------------------------------
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, ng)
        if (ng .ne. 0) then
            ng = -ng
            call wkvect('&&PAMANO.TRAV', 'V V K24', ng, jjj)
            call getvem(noma, 'GROUP_MA', motfac, motcle, iocc,&
                        iarg, ng, zk24(jjj), ngr)
            do 10 igr = 1, ngr
                call jeveuo(jexnom(grmama, zk24(jjj+igr-1)), 'L', jgro)
                call jelira(jexnom(grmama, zk24(jjj+igr-1)), 'LONUTI', nbmail, k1bid)
                do 20 m = 1, nbmail
                    numail = zi(jgro-1+m)
                    call jenuno(jexnum(mailma, numail), nomail)
                    if (lonlit .ne. 0) then
                        nutyma = zi(idtyma+numail-1)
                        indic = 0
                        do 21 i = 1, lonlit
                            if (nutyma .eq. zi(idtypi+i-1)) then
                                indic = 1
                                goto 22
                            endif
21                      continue
22                      continue
                        if (indic .eq. 0) then
                            valk(1) = nomail
                            valk(2) = zk24(jjj+igr-1)
                            valk(3) = motcle
                            call u2mesk('F', 'MODELISA6_17', 3, valk)
                        endif
                    endif
                    call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                    call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n1, k1bid)
                    idim1 = idim1 + n1
                    n1max = max ( n1max, n1)
20              continue
10          continue
        endif
!
!
!     -- TRAITEMENT DES MOTS-CLES MAILLE_1 ET MAILLE_2
!        -------------------------------------------------------
    else if (motcle.eq.mmail1.or.motcle.eq.mmail2) then
!
!           -- CALCUL DE
!              IDIM2=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
!              ET VERIFICATION DE L'APPARTENANCE DES MAILLES
!              AUX MAILLES DU MAILLAGE
!              ----------------------------------------------------
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, nbma)
        if (nbma .ne. 0) then
            nbma = -nbma
            call wkvect('&&PAMANO.TRAV', 'V V K8', nbma, jjj)
            call getvem(noma, 'MAILLE', motfac, motcle, iocc,&
                        iarg, nbma, zk8(jjj), nmai)
            do 30 ima = 1, nmai
                if (lonlit .ne. 0) then
                    call jenonu(jexnom(mailma, zk8(jjj+ima-1)), numail)
                    nutyma = zi(idtyma+numail-1)
                    indic = 0
                    do 31 i = 1, lonlit
                        if (nutyma .eq. zi(idtypi+i-1)) then
                            indic = 1
                            goto 32
                        endif
31                  continue
32                  continue
                    if (indic .eq. 0) then
                        valk(1) = zk8(jjj+ima-1)
                        valk(2) = motcle
                        call u2mesk('F', 'MODELISA6_18', 2, valk)
                    endif
                endif
                call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n2, k1bid)
                idim2 = idim2 + n2
                n1max = max ( n1max, n2)
30          continue
        endif
        if ((n1max.eq.1) .and. (motfac.eq.'LIAISON_UNIL_NO')) then
            idimp1 = 1
        endif
!
!     -- TRAITEMENT DES MOTS-CLES GROUP_NO_1 ET GROUP_NO_2
!        -------------------------------------------------------
    else if (motcle.eq.mgrno1.or.motcle.eq.mgrno2) then
!
!           -- CALCUL DE
!              IDIM3 = NB_NOEUD/GROUP_NO*NB_GROUP_NO
!              ET VERIFICATION DE L'APPARTENANCE DES GROUP_NO
!              AUX GROUP_NO DU MAILLAGE
!              ------------------------------------------------
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, ng)
        if (ng .ne. 0) then
            ng = -ng
            call wkvect('&&PAMANO.TRAV', 'V V K24', ng, jjj)
            call getvem(noma, 'GROUP_NO', motfac, motcle, iocc,&
                        iarg, ng, zk24(jjj), ngr)
            do 40 igr = 1, ngr
                call jelira(jexnom(grnoma, zk24(jjj+igr-1)), 'LONUTI', n3, k1bid)
                idim3 = idim3 + n3
40          continue
        endif
!
!     -- TRAITEMENT DES MOTS-CLES NOEUD_1 ET NOEUD_2
!        -------------------------------------------------------
    else if (motcle.eq.mnoeu1.or.motcle.eq.mnoeu2) then
!
!           -- CALCUL DE
!              IDIM4 = NB_NOEUD DE LA LISTE DE NOEUDS
!              ET VERIFICATION DE L'APPARTENANCE DES NOEUDS
!              AUX NOEUDS DU MAILLAGE
!              ---------------------------------------------
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, nbno)
        if (nbno .ne. 0) then
            nbno = -nbno
            call wkvect('&&PAMANO.TRAV', 'V V K8', nbno, jjj)
            call getvem(noma, 'NOEUD', motfac, motcle, iocc,&
                        iarg, nbno, zk8(jjj), nno)
            idim4 = idim4 + nno
        endif
!
!     -- MOTCLE NON ADMIS
!        -------------------------------------------------------
    else
        call assert(.false.)
    endif
!
!     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS
!    -----------------------------------------------------------
    idimax = max(idim1,idimax)
    idimax = max(idim2,idimax)
    idimax = max(idim3,idimax)
    idimax = max(idim4,idimax)
!
    if (idimax .eq. 0) goto 9999
!
!     -- ALLOCATION DES TABLEAUX DES NOMS DE NOEUDS
!    ----------------------------------------------
    call jeexin(lisnoe, ier)
    if (ier .ne. 0) then
        call jedetr(lisnoe)
    endif
    call wkvect(lisnoe, 'V V K8', idimax, jlist)
!
    indnoe = 0
!
!     --  MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
!        -------------------------------------------------------
    if (motcle .eq. mgrma1 .or. motcle .eq. mgrma2) then
!
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, ng)
        if (ng .ne. 0) then
            ng = -ng
            call getvtx(motfac, motcle, iocc, iarg, ng,&
                        zk24(jjj), ngr)
            do 50 igr = 1, ngr
                call jeveuo(jexnom(grmama, zk24(jjj+igr-1)), 'L', jgro)
                call jelira(jexnom(grmama, zk24(jjj+igr-1)), 'LONUTI', nbmail, k1bid)
                do 60 m = 1, nbmail
                    numail = zi(jgro-1+m)
                    call jenuno(jexnum(mailma, numail), nomail)
                    call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
                    call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
                    call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n1, k1bid)
                    do 70 ino = 1, n1
                        call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                        indnoe = indnoe + 1
                        zk8(jlist+indnoe-1) = nomnoe
70                  continue
60              continue
50          continue
        endif
!
!     --  MOTS-CLES MAILLE_1 ET MAILLE_2
!        -------------------------------------------------------
    else if (motcle.eq.mmail1.or.motcle.eq.mmail2) then
!
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, nbma)
        if (nbma .ne. 0) then
            nbma = -nbma
            call getvtx(motfac, motcle, iocc, iarg, nbma,&
                        zk8(jjj), nmai)
            do 80 ima = 1, nmai
                call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
                call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', jdes)
                call jenonu(jexnom(noma//'.NOMMAI', zk8(jjj+ima-1)), ibid)
                call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', n2, k1bid)
                do 90 ino = 1, n2
                    call jenuno(jexnum(noeuma, zi(jdes+ino-1)), nomnoe)
                    indnoe = indnoe + 1
                    zk8(jlist+indnoe-1) = nomnoe
90              continue
80          continue
        endif
!
!     --  MOTS-CLES GROUP_NO_1 ET GROUP_NO_2
!        -------------------------------------------------------
    else if (motcle.eq.mgrno1.or.motcle.eq.mgrno2) then
!
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, ng)
        if (ng .ne. 0) then
            ng = -ng
            call getvtx(motfac, motcle, iocc, iarg, ng,&
                        zk24(jjj), ngr)
            do 100 igr = 1, ngr
                call jeveuo(jexnom(grnoma, zk24(jjj+igr-1)), 'L', jgro)
                call jelira(jexnom(grnoma, zk24(jjj+igr-1)), 'LONUTI', n3, k1bid)
                do 110 ino = 1, n3
                    in = zi(jgro+ino-1)
                    indnoe = indnoe + 1
                    call jenuno(jexnum(noeuma, in), nomnoe)
                    zk8(jlist+indnoe-1) = nomnoe
110              continue
100          continue
        endif
!
!     --  MOTS-CLES NOEUD_1 ET NOEUD_2
!        -------------------------------------------------------
    else if (motcle.eq.mnoeu1.or.motcle.eq.mnoeu2) then
!
        call getvtx(motfac, motcle, iocc, iarg, 0,&
                    k8bid, nbno)
        if (nbno .ne. 0) then
            nbno = -nbno
            call getvtx(motfac, motcle, iocc, iarg, nbno,&
                        zk8(jjj), nno)
            do 120 ino = 1, nno
                indnoe = indnoe + 1
                zk8(jlist+indnoe-1) = zk8(jjj+ino-1)
120          continue
        endif
!
    endif
!
!     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
!        DE LA LISTE
!    -------------------------------------------------------------
    call wkvect('&&PAMANO.INDICE', 'V V I', idimax, jind)
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
    if (idimp1 .gt. 0) then
        lonlis = - lonlis
    endif
!
9999  continue
!
    call jedetr('&&PAMANO.TRAV')
    call jedetr('&&PAMANO.INDICE')
    call jedetr('&&PAMANO.LISTYP')
!
    call jedema()
end subroutine
