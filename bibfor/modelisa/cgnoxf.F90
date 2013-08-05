subroutine cgnoxf(mofaz, iocc, nomaz, lisnoz, nbno)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: iocc, nbno
    character(len=*) :: mofaz, nomaz, lisnoz
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!       CGNOXF -- TRAITEMENT DE L'OPTION FISS_XFEM
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
!      DE TOUS LES NOEUDS DE TYPE XFEM DEFINI PAR L'UTILISATEUR.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
!                                   DU TYPE XFEM DEMANDE PAR
!                                   L'UTILISATEUR
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
!
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: ibid, iret
    integer :: n1, ifiss, nfiss
    integer :: ino, valeno, nbnot
    integer :: idlist, jnoeu, jfiss, jstno, jlst, jlsn
    character(len=8) :: noma, k8bid, nomnoe, fiss, nomofi, nomafi, nomogr
    character(len=8) :: nomagr, valk(2), ma
    character(len=16) :: motfac, typgrp
    character(len=19) :: stno, cnslt, cnsln
    character(len=24) :: stnot
    character(len=24) :: lisnoe
    logical :: grille
    real(kind=8) :: rayon, dist
    integer :: iarg
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
!
! --- INITIALISATIONS :
!     ================
    motfac = mofaz
    noma = nomaz
    lisnoe = lisnoz
    nbno = 0
!
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbnot,&
                k8bid, iret)
    call wkvect('&&CGNOXF.NOEU', 'V V I', nbnot, jnoeu)
!
! --  RECUPERATION DU TYPE GROUPE :
!     ============================
    call getvtx(motfac, 'TYPE_GROUP', iocc, iarg, 1,&
                typgrp, n1)
!
! --  RECUPERATION DES NOMS DES FISSURES :
!     ===================================
    call getvid(motfac, 'FISSURE', iocc, iarg, 0,&
                k8bid, nfiss)
    nfiss = -nfiss
    call wkvect('&&CGNOXF.FISS', 'V V K8', nfiss, jfiss)
    call getvid(motfac, 'FISSURE', iocc, iarg, nfiss,&
                zk8(jfiss), ibid)
!
! --- TYPE DE NOEUD = 'HEAVISIDE'
!     ============================
    if (typgrp .eq. 'HEAVISIDE') then
        do 10 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            stno = fiss//'.STNO'
            call jeveuo(stno//'.VALE', 'L', jstno)
            do 110 ino = 1, nbnot
                valeno = zi(jstno+ino-1)
                if (valeno .eq. 1) then
                    nbno = nbno + 1
                    zi(jnoeu+nbno-1) = ino
                endif
110          continue
            call jedetr(stno)
10      continue
!
! --- TYPE DE NOEUD = 'CRACKTIP'
!     ============================
    else if (typgrp.eq.'CRACKTIP') then
        do 11 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            stno = fiss//'.STNO'
            call jeveuo(stno//'.VALE', 'L', jstno)
            do 111 ino = 1, nbnot
                valeno = zi(jstno+ino-1)
                if (valeno .eq. 2) then
                    nbno = nbno + 1
                    zi(jnoeu+nbno-1) = ino
                endif
111          continue
            call jedetr(stno)
11      continue
!
! --- TYPE DE NOEUD = 'MIXTE'
!     ============================
    else if (typgrp.eq.'MIXTE') then
        do 12 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            stno = fiss//'.STNO'
            call jeveuo(stno//'.VALE', 'L', jstno)
            do 112 ino = 1, nbnot
                valeno = zi(jstno+ino-1)
                if (valeno .eq. 3) then
                    nbno = nbno + 1
                    zi(jnoeu+nbno-1) = ino
                endif
112          continue
12      continue
!
! --- TYPE DE NOEUD = 'XFEM'
!     ============================
    else if (typgrp.eq.'XFEM') then
        do 13 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
            stno = fiss//'.STNO'
            call jeveuo(stno//'.VALE', 'L', jstno)
            do 113 ino = 1, nbnot
                valeno = zi(jstno+ino-1)
                if (valeno .ne. 0) then
                    nbno = nbno + 1
                    zi(jnoeu+nbno-1) = ino
                endif
113          continue
            call jedetr(stno)
13      continue
!
!
! --- TYPE DE NOEUD = 'TORE'
!     ============================
    else if ((typgrp.eq.'TORE').or.(typgrp.eq.'ZONE_MAJ')) then
!
        cnslt = '&&CGNOXF.CNSLT'
        cnsln = '&&CGNOXF.CNSLN'
!
        do 15 ifiss = 1, nfiss
            fiss = zk8(jfiss-1+ifiss)
!
!           CHECK IF THE LOCALISATION HAS BEEN USED
            stnot = fiss//'.PRO.RAYON_TORE'
            call jeexin(stnot, ibid)
            if ((ibid.gt.0) .and. (typgrp.eq.'TORE')) then
                typgrp='ZONE_MAJ'
                call u2mesk('A', 'XFEM2_92', 1, fiss)
            endif
!
            if (typgrp .eq. 'TORE') then
!
!              GET THE CRACK MESH
                call dismoi('F', 'NOM_MODELE', fiss, 'FISS_XFEM', ibid,&
                            nomofi, iret)
                stnot = nomofi//'.MODELE    .LGRF'
                call jeveuo(stnot, 'L', ibid)
                nomafi = zk8(ibid)
!
                call getvr8(motfac, 'RAYON_TORE', 1, iarg, 1,&
                            rayon, ibid)
                rayon = rayon**2
!
!              RETREIVE THE TWO LEVEL SETS
                call getvid(' ', 'MAILLAGE', 1, iarg, 1,&
                            ma, ibid)
                if (ibid .eq. 0) then
                    call getvid(' ', 'GRILLE', 1, iarg, 1,&
                                ma, ibid)
!                  CHECK FOR THE PRESENCE OF THE GRID
                    stnot = fiss//'.GRI.MODELE'
                    call jeexin(stnot, ibid)
                    if (ibid .gt. 0) then
                        call jeveuo(stnot, 'L', ibid)
!                    GRID MODEL NAME
                        nomogr = zk8(ibid)
!                    GRID NAME
                        stnot = nomogr//'.MODELE    .LGRF'
                        call jeveuo(stnot, 'L', ibid)
                        nomagr = zk8(ibid)
                        if (nomagr .ne. ma) call u2mess('F', 'XFEM2_86')
                    else
                        call u2mess('F', 'XFEM2_86')
                    endif
                    call cnocns(fiss//'.GRI.LTNO', 'V', cnslt)
                    call cnocns(fiss//'.GRI.LNNO', 'V', cnsln)
                else
                    if (nomafi .ne. ma) call u2mess('F', 'XFEM2_86')
                    call cnocns(fiss//'.LTNO', 'V', cnslt)
                    call cnocns(fiss//'.LNNO', 'V', cnsln)
                endif
                call jeveuo(cnslt//'.CNSV', 'L', jlst)
                call jeveuo(cnsln//'.CNSV', 'L', jlsn)
!
                do 116 ino = 1, nbnot
                    dist=zr(jlst-1+ino)**2+zr(jlsn-1+ino)**2
                    if (dist .le. rayon) then
                        nbno = nbno + 1
                        zi(jnoeu+nbno-1) = ino
                    endif
116              continue
!
                call jedetr(cnslt)
                call jedetr(cnsln)
!
            endif
!
!
! --- TYPE DE NOEUD = 'ZONE_MAJ'
!     ============================
            if (typgrp .eq. 'ZONE_MAJ') then
!
!             GET THE CRACK MESH
                call dismoi('F', 'NOM_MODELE', fiss, 'FISS_XFEM', ibid,&
                            nomofi, iret)
                stnot = nomofi//'.MODELE    .LGRF'
                call jeveuo(stnot, 'L', ibid)
                nomafi = zk8(ibid)
!
!             CHECK FOR THE PRESENCE OF THE GRID
                stnot = fiss//'.GRI.MODELE'
                call jeexin(stnot, ibid)
                if (ibid .gt. 0) then
                    grille = .true.
                    call jeveuo(stnot, 'L', ibid)
!                GRID MODEL NAME
                    nomogr = zk8(ibid)
!                GRID NAME
                    stnot = nomogr//'.MODELE    .LGRF'
                    call jeveuo(stnot, 'L', ibid)
                    nomagr = zk8(ibid)
                else
                    grille = .false.
                endif
!
                if (noma .eq. nomafi) then
                    if (grille) then
                        stnot = fiss//'.PRO.NOEUD_PROJ'
                    else
                        stnot = fiss//'.PRO.NOEUD_TORE'
                    endif
                else if (grille.and.(noma.eq.nomagr)) then
                    stnot = fiss//'.PRO.NOEUD_TORE'
                else
                    valk(1) = nomafi
                    if (grille) then
                        valk(2) = nomagr
                    else
                        valk(2) = 'AUCUN'
                    endif
                    call u2mesk('F', 'XFEM2_96', 2, valk)
                endif
!
                call jeexin(stnot, ibid)
                if (ibid .gt. 0) then
                    call jeveuo(stnot, 'L', jstno)
                    do 114 ino = 1, nbnot
                        if (zl(jstno+ino-1)) then
                            nbno = nbno + 1
                            zi(jnoeu+nbno-1) = ino
                        endif
114                  continue
                else
!                THE LOCALISATION HAS NOT BEEN USED. ZONE_MAJ IS
!                COINCIDENT WITH THE WHOLE MODEL.
                    do 115 ino = 1, nbnot
                        nbno = nbno + 1
                        zi(jnoeu+nbno-1) = ino
115                  continue
                endif
            endif
!
15      continue
!
    else
        ASSERT(.false.)
    endif
!
    if (nbno .ne. 0) then
        call wkvect(lisnoe, 'V V I', nbno, idlist)
!
        do 20 ino = 1, nbno
            zi(idlist+ino-1)=zi(jnoeu+ino-1)
            call jenuno(jexnum(noma//'.NOMNOE', zi(idlist+ino-1)), nomnoe)
20      continue
    endif
!
! --- FIN
!     ===
!
! --- MENAGE
!
!
    call jedetr('&&CGNOXF.FISS')
    call jedetr('&&CGNOXF.NOEU')
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
