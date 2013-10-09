subroutine gver2d(noma, nocc, motfaz, nomno, noeud,&
                  rinf, rsup, module)
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
    implicit none
!
!     ------------------------------------------------------------------
!
! FONCTION REALISEE:
!
!     MOT CLE FACTEUR THETA:
!
!     POUR LE NOEUD DU FOND DE FISSURE ON RECUPERE
!     LE TRIPLET ( MODULE(THETA), R_INF, R_SUP )
!
!     PUIS ON VERIFIE:
!                     QUE LE NOM DU GROUPE OU D'ELEMENTS (NOEUD)
!                     APPARTIENNENT BIEN AU MAILLAGE
!
!     ------------------------------------------------------------------
! ENTREE:
!
!     NOMA   : NOM DU MAILLAGE
!     NOCC   : NOMBRE D'OCCURENCES
!     NOMNO  : NOM DE L'OBJET CONTENANT LES NOMS DES NOEUDS
!
! SORTIE:
!
!     NOEUD      : NOEUD DU FOND DE FISSURE
!     R_INF       : RAYON INFERIEUR DE LA COURONNE
!     R_SUP       : RAYON SUPERIEUR DE LA COURONNE
!     MODULE     : MODULE THETA
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
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
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: motfaz
    character(len=8) :: config, noma, noeud, k8b, fond, kfon
    character(len=16) :: motfac, nomcmd, k16b
    character(len=24) :: trav, trav2
    character(len=24) :: grpno, nomno, chfond, taillr
!
    integer :: jjj, ngro, nent, nsom, iocc, nocc, ndim, iadr, i, l, n1
    integer :: igr, ngr, nno, iret, nbm, n2, lnoff, numfon, ibid, jjj2
    integer :: iatmno
!
    real(kind=8) :: rbid, rinf, rsup, module, valr(2)
!
    character(len=24) :: valk(2)
    character(len=8) :: k8bid
    integer :: iarg, l2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call getres(k8b, k16b, nomcmd)
!
    motfac = motfaz
    l = len(motfac)
    l2 = lxlgut(motfac)
! OBJET DEFINISSANT LES GROUP_NO DU MAILLAGE
!
    grpno = noma//'.GROUPENO'
!
    if (nomcmd .ne. 'CALC_G') then
        do iocc = 1, nocc
!
            call getvem(noma, 'GROUP_NO', motfac(1:l), 'GROUP_NO', iocc,&
                        iarg, 0, k8bid, ngro)
            call getvem(noma, 'NOEUD', motfac(1:l), 'NOEUD', iocc,&
                        iarg, 0, k8bid, nent)
            nsom = ngro + nent
            if (nsom .eq. ngro) then
                ngro = -ngro
            else if (nsom.eq.nent) then
                nent = -nent
            endif
!
        end do
!
        ndim = max(ngro,nent)
!
! ALLOCATION D'UN OBJET DE TRAVAIL
!
        trav = '&&GVER2D.'//motfac(1:l)
        trav2 = '&&GVER2D.'//motfac(1:l2)//'2'
        call wkvect(trav, 'V V K8', ndim, jjj)
        call wkvect(trav2, 'V V K24', ndim, jjj2)
!
    else
        ndim = 1
    endif
!
    do iocc = 1, nocc
!
        call getvr8(motfac(1:l), 'MODULE', iocc=iocc, scal=module, nbret=nbm)
        call getvr8(motfac(1:l), 'R_INF', iocc=iocc, scal=rinf, nbret=nbm)
        call getvr8(motfac(1:l), 'R_SUP', iocc=iocc, scal=rsup, nbret=nbm)
!
        if (nbm .ne. 0 .and. rsup .le. rinf) then
            call utmess('F', 'RUPTURE1_6')
        endif
!
        if (nomcmd .ne. 'CALC_G') then
!
! MOT CLE GROUP_NO
!
! LE GROUP_NO DOIT APPARTENIR AU MAILLAGE
!
            call getvem(noma, 'GROUP_NO', motfac(1:l), 'GROUP_NO', iocc,&
                        iarg, ndim, zk24(jjj2), ngr)
!
            do igr = 1, ngr
!
                call jeexin(jexnom(grpno, zk24(jjj2+igr-1)), iret)
!
                if (iret .eq. 0) then
                    valk(1) = zk24(jjj+igr-1)
                    valk(2) = noma
                    call utmess('F', 'RUPTURE1_8', nk=2, valk=valk)
                else
!
                    call jelira(jexnom(grpno, zk24(jjj2+igr-1)), 'LONUTI', n1)
                    if (n1 .gt. 1) then
                        call utmess('F', 'RUPTURE1_10')
                    else
                        call jeveuo(jexnom(grpno, zk24(jjj2+igr-1)), 'L', iadr)
                        call jenuno(jexnum(nomno, zi(iadr)), noeud)
                    endif
!
                endif
            end do
!
! MOT CLE NOEUD
!
            call getvem(noma, 'NOEUD', motfac(1:l), 'NOEUD', iocc,&
                        iarg, ndim, zk8(jjj), nno)
!
            do i = 1, nno
                if (nno .gt. 1) then
                    call utmess('F', 'RUPTURE1_10')
                else
!
                    call jenonu(jexnom(nomno, zk8(jjj+i-1)), iret)
                    if (iret .eq. 0) then
                        valk(1) = zk8(jjj+i-1)
                        valk(2) = noma
                        call utmess('F', 'RUPTURE0_14', nk=2, valk=valk)
                    else
                        call jenuno(jexnum(nomno, iret), noeud)
                    endif
!
                endif
            end do
!
        else
            call getvr8(motfac(1:l), 'R_INF_FO', iocc=iocc, scal=rbid, nbret=ibid)
            if (ibid .ne. 0) then
                call utmess('F', 'RUPTURE1_18')
            endif
!
            call getvid('THETA', 'FOND_FISS', iocc=1, scal=fond, nbret=n1)
            if (n1 .ne. 0) then
!           CAS CLASSIQUE
                chfond = fond//'.FOND.NOEU'
                call jelira(chfond, 'LONMAX', lnoff)
                if (lnoff .ne. 1) then
                    call utmess('F', 'RUPTURE1_10')
                else
                    call jeveuo(chfond, 'L', n1)
                    noeud=zk8(n1)
                endif
                numfon = 1
                if (nbm .eq. 0) then
                    call dismoi('CONFIG_INIT', fond, 'FOND_FISS', repk=config)
                    if (config .eq. 'DECOLLEE') then
                        call utmess('F', 'RUPTURE1_7')
                    endif
                endif
            else
!           CAS X-FEM
                call getvid('THETA', 'FISSURE', iocc=1, scal=fond, nbret=n2)
                if (n2 .eq. 0) then
                    call utmess('F', 'RUPTURE1_11')
                endif
!           RECUPERATION DU NUMERO DU FOND DE FISSURE DEMANDE
                call getvis('THETA', 'NUME_FOND', iocc=1, scal=numfon, nbret=ibid)
!           ON ECRIT 'NUM'+_i OU i=NUMFON
!           A LA PLACE DU NOM DU NOEUD EN FOND DE FISSURE
                call codent(numfon, 'G', kfon)
                noeud(1:8)='NUM_'//kfon
            endif
!
!         RECUPERATION DE RINF ET DE RSUP DANS LA SD
            if (nbm .eq. 0) then
                taillr = fond//'.FOND.TAILLE_R'
                call jeveuo(taillr, 'L', iatmno)
                rinf = 2*zr(iatmno-1+numfon)
                rsup = 4*zr(iatmno-1+numfon)
                valr(1) = rinf
                valr(2) = rsup
                call utmess('I', 'RUPTURE1_5', nr=2, valr=valr)
            endif
        endif
    end do
!
! DESTRUCTION DE L'OBJET DE TRAVAIL
!
    if (nomcmd .ne. 'CALC_G') then
        call jedetr(trav)
        call jedetr(trav2)
    endif
!
    call jedema()
end subroutine
