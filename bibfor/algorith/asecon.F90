subroutine asecon(nomsy, neq, mome, resu)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsexis.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesg.h"
#include "asterfort/vtdefs.h"
#include "asterfort/wkvect.h"
!
    integer :: neq
    character(len=16) :: nomsy
    character(len=*) :: mome, resu
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        CALCUL DES TERMES D'ENTRAINEMENT
!     ------------------------------------------------------------------
! IN  : NOMSY  : OPTION DE CALCUL
! IN  : NEQ    : NOMBRE D'EQUATIONS
! IN  : NOME   : MODES MECANIQUES
! IN  : RESU   : NOM UTILISATEUR DE LA COMMANDE
!     ------------------------------------------------------------------
    integer :: iad, ibid, icas, idep, idir, ier, ii, in, ino, ioc, iocc, iordr
    integer :: iorst, iret, jabs, jaux, jcas, jcum, jdir, jlin, jno, jord, jqua
    integer :: jrep, jsta, jtyp, jvale, jval1, lnod, nbmode, nbno, nboc, nbtrou
    integer :: ncas, ndep, nucas, nume
    real(kind=8) :: r8b, epsmac, xxx, xx1, xx2, xx3
    complex(kind=8) :: cbid
    character(len=8) :: k8b, noeu, cmp, nomcmp(3), knum, kdir, stat
    character(len=8) :: meca, occur
    character(len=16) :: monacc, concep, nomcmd, def
    character(len=19) :: chextr, champ, moncha
    character(len=24) :: vale, noms2, valk(3)
    integer :: iarg
!     ------------------------------------------------------------------
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
    data  vale / '                   .VALE' /
!     ------------------------------------------------------------------
!
    call jemarq()
    call getfac('COMB_DEPL_APPUI', nboc)
    call getfac('DEPL_MULT_APPUI', ndep)
    call getvid(' ', 'MODE_MECA', scal=meca, nbret=ibid)
!
    call wkvect('&&ASECON.CUMUL', 'V V R', neq, jcum)
    call wkvect('&&ASECON.AUX', 'V V R', neq*nboc, jaux)
!
    epsmac = r8vide()
    nbmode = 10
    ii = 0
!
! -- PREPARATION DU STOCKAGE DE LA REPONSE SECONDAIRE
!
    call getres(k8b, concep, nomcmd)
!     --- CREATION DE LA STRUCTURE D'ACCUEIL ---
    call rsexis(resu, ier)
    if (ier .eq. 0) call rscrsd('G', resu, concep, nbmode)
    noms2 = nomsy
    if (nomsy(1:4) .eq. 'VITE') noms2 = 'DEPL'
!
    call rsorac(mome, 'TOUT_ORDRE', ibid, r8b, k8b,&
                cbid, r8b, k8b, iordr, 1,&
                nbtrou)
!
    call rsexch('F', meca, noms2, iordr, moncha,&
                ier)
    def = 'SECONDAIRE'
    iordr = 200
!           --- CHAMP PAR OCCURENCE DE COMB_DPL_APPUI ---
!
    call jeveuo('&&ASENAP.TYPE', 'L', jtyp)
    call wkvect('&&ASECON.REP', 'V V R', neq, jrep)
    call wkvect('&&ASECON.QUAD', 'V V R', neq, jqua)
    call wkvect('&&ASECON.LINE', 'V V R', neq, jlin)
    call wkvect('&&ASECON.ABS', 'V V R', neq, jabs)
    call jeexin('&&ASECON.NORD', iret)
    if (iret .eq. 0) then
        call wkvect('&&ASECON.NORD', 'V V I', nboc+1, jord)
    else
        call jeveuo('&&ASECON.NORD', 'E', jord)
    endif
!
    do 10 iocc = 1, nboc
!
! POUR CHAQUE OCCURENCE ON STOQUE LE CHAMP
!
        call rsexch(' ', resu, nomsy, iordr, champ,&
                    ier)
        if (ier .eq. 100) then
            call vtdefs(champ, moncha, 'G', 'R')
        else
            valk (1) = nomsy
            valk (2) = champ
            call u2mesg('F', 'SEISME_25', 2, valk, 1,&
                        iocc, 0, 0.d0)
        endif
        vale(1:19) = champ
        call jeexin(vale(1:19)//'.VALE', ibid)
        if (ibid .gt. 0) then
            vale(20:24)='.VALE'
        else
            vale(20:24)='.CELV'
        endif
        call jeveuo(vale, 'E', jvale)
!
        do 4 in = 1, neq
            zr(jqua+in-1)= 0.0d0
            zr(jlin+in-1)= 0.0d0
            zr(jabs+in-1)= 0.0d0
 4      continue
        call jelira(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
        call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'L', jcas)
        do 20 icas = 1, ncas
            nucas = zi(jcas+icas-1)
            do 40 idep = 1, ndep
                call getvis('DEPL_MULT_APPUI', 'NUME_CAS', iocc=idep, scal=nume, nbret=ibid)
                if (nume .eq. nucas) then
                    knum = 'N       '
                    call codent(nucas, 'D0', knum(2:8))
                    kdir = 'D       '
                    call codent(nucas, 'D0', kdir(2:8))
                    call jelira(jexnom('&&ASENAP.LINOEU', knum), 'LONMAX', nbno)
                    call jeveuo(jexnom('&&ASENAP.LINOEU', knum), 'L', jno)
                    lnod = 3*nbno
                    call jelira(jexnom('&&ASENAP.LIDIR', kdir), 'LONMAX', lnod)
                    call jeveuo(jexnom('&&ASENAP.LIDIR', kdir), 'L', jdir)
                    call jeveuo('&&ASENAP.STAT', 'L', jsta)
                    stat = zk8(jsta+icas-1)
                    do 12 ino = 1, nbno
                        noeu =zk8(jno+ino-1)
                        do 14 idir = 1, 3
                            if (zr(jdir+3*(ino-1)+idir-1) .ne. epsmac) then
                                cmp = nomcmp(idir)
                                monacc = noeu//cmp
                                xx1 = zr(jdir+3*(ino-1)+idir-1)
                                call rsorac(stat, 'NOEUD_CMP', ibid, r8b, monacc,&
                                            cbid, r8b, k8b, iorst, 1,&
                                            nbtrou)
                                call rsexch('F', stat, nomsy, iorst, chextr,&
                                            iret)
                                call jeexin(chextr//'.VALE', ibid)
                                if (ibid .gt. 0) then
                                    call jeveuo(chextr//'.VALE', 'L', jval1)
                                else
                                    call jeveuo(chextr//'.CELV', 'L', jval1)
                                endif
                                do 16 in = 1, neq
                                    zr(jrep+in-1) = zr(jval1+in-1) * xx1
16                              continue
                                if (zi(jtyp+iocc-1) .eq. 1) then
!                 --- COMBINAISON QUADRATIQUE ---
                                    do 24 in = 1, neq
                                        xxx = zr(jrep+in-1)
                                        zr(jqua+in-1)= zr(jqua+in-1)+&
                                        xxx*xxx
24                                  continue
                                else if (zi(jtyp+iocc-1).eq.2) then
!               --- COMBINAISON LINEAIRE ---
                                    do 18 in = 1, neq
                                        zr(jlin+in-1)= zr(jlin+in-1)+&
                                        zr(jrep+in-1)
18                                  continue
                                else
!              --- COMBINAISON VALEUR ABSOLUE ---
                                    do 22 in = 1, neq
                                        xx1 = abs(zr(jrep+in-1))
                                        zr(jabs+in-1)= zr(jabs+in-1)+&
                                        xx1
22                                  continue
                                endif
                            endif
14                      continue
12                  continue
                endif
40          continue
20      continue
        do 26 in = 1, neq
            xx1 = zr(jlin+in-1)
            xx2 = zr(jabs+in-1)
            xx3 = sqrt(zr(jqua+in-1))
            zr(jvale+in-1) = xx1 + xx2 + xx3
            ii = ii + 1
            zr(jaux+ii-1) = zr(jvale+in-1)
26      continue
!
        call rsnoch(resu, nomsy, iordr)
        call rsadpa(resu, 'E', 1, 'NOEUD_CMP', iordr,&
                    0, iad, k8b)
        call codent(iocc, 'D', occur)
        zk16(iad) = 'COMBI'// occur
        call rsadpa(resu, 'E', 1, 'TYPE_DEFO', iordr,&
                    0, iad, k8b)
        zk16(iad) = def
        call jelibe(vale)
!
        zi(jord+iocc-1) = iordr
        iordr = iordr + 1
10  end do
    zi(jord+nboc) = iordr
!
    call rsexch(' ', resu, nomsy, iordr, champ,&
                ier)
    if (ier .eq. 100) then
        call vtdefs(champ, moncha, 'G', 'R')
    else
        valk(1) = nomsy
        valk(2) = champ
        call u2mesg('F', 'SEISME_25', 2, valk, 1,&
                    iordr, 0, 0.d0)
    endif
    vale(1:19) = champ
    call jeexin(vale(1:19)//'.VALE', ibid)
    if (ibid .gt. 0) then
        vale(20:24)='.VALE'
    else
        vale(20:24)='.CELV'
    endif
    call jeveuo(vale, 'E', jvale)
!
    do 32 ioc = 1, nboc
        do 30 in = 1, neq
            xx1 = zr(jaux+(ioc-1)*neq+in-1)
            zr(jcum+in-1) = zr(jcum+in-1)+xx1*xx1
30      continue
32  end do
! STOCKAGE DU CUMUL QUADRATIQUE
    do 34 in = 1, neq
        zr(jvale+in-1) = sqrt( abs ( zr(jcum+in-1) ) )
34  end do
    call jelibe(vale)
    call rsnoch(resu, nomsy, iordr)
!
!        --- PARAMETRE ---
    call rsadpa(resu, 'E', 1, 'NOEUD_CMP', iordr,&
                0, iad, k8b)
    zk16(iad) = 'CUMUL'//' QUAD'
    call rsadpa(resu, 'E', 1, 'TYPE_DEFO', iordr,&
                0, iad, k8b)
    zk16(iad) = def
!
    call jedetr('&&ASECON.CUMUL')
    call jedetr('&&ASECON.AUX')
    call jedetr('&&ASECON.REP')
    call jedetr('&&ASECON.QUAD')
    call jedetr('&&ASECON.LINE')
    call jedetr('&&ASECON.ABS')
    call jedema()
end subroutine
