subroutine asimpr(nbsup, tcosup, nomsup)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/codent.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: nbsup, tcosup(nbsup, *)
    character(len=8) :: nomsup(nbsup, *)
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
!      IMPRIME LES RESULTATS DES REPONSES PRIMAIRES ET SECONDAIRES
!     ------------------------------------------------------------------
!
    character(len=4) :: typdir
    character(len=8) ::  noeu, cmp, noref
    character(len=8) :: knum, kdir
    character(len=19) :: knoeu, didi, lnore, nbnor, ordr
    character(len=80) :: nomcas, chainq, chainl, chaina
    integer :: iarg
!
!-----------------------------------------------------------------------
    integer :: ia, ibid, ic, icas, id, idep, ifm
    integer :: ii, il, ino, iocc, iordr, iq, is
    integer :: jcas, jdir, jkno, jno, jnoe, jnref
    integer :: jord, jref, jtyp, lnod, nbno, nboc, ncas
    integer :: ndep, nucas, nume
    real(kind=8) :: epsima, vale
!-----------------------------------------------------------------------
    epsima = r8vide()
    call jemarq()
    ifm = iunifi('MESSAGE')
    knoeu = '&&OP0109.NOM_SUPPOR'
    didi = '&&OP0109.DIRECTION'
    lnore = '&&ASENAP.NOREF'
    nbnor = '&&ASENAP.NREF'
    ordr ='&&ASECON.NORD'
!
    write(ifm,1167)
    write(ifm,1170)
    call jeveuo(knoeu, 'L', jkno)
    call jeveuo(didi, 'L', jdir)
    call jeveuo(lnore, 'L', jnref)
    call jeveuo(nbnor, 'L', jref)
    call jeveuo(ordr, 'L', jord)
!
!  CAS DES IMPRESSIONS POUR LA REPONSE PRIMAIRE
!
    chainq = ' '
    chainl = ' '
    chaina = ' '
!
    chainq(1 : 4) ='QUAD'
    chainl(1 : 4) ='LINE'
    chaina(1 : 4) ='ABS '
    iq = 1
    il = 1
    ia = 1
    call wkvect('&&ASIMPR.NOEUDS', 'V V K8', 3*nbsup, jnoe)
    ino = 1
    do 40 id = 1, 3
        do 42 is = 1, nbsup
            if (tcosup(is,1) .eq. 1) then
                do 44 ii = 1, ino
                    if (nomsup(is,id) .eq. zk8(jnoe+ii-1)) then
                        goto 42
                    endif
44              continue
                zk8(jnoe+ino-1) = nomsup(is,id)
                chainq(iq+5 : iq+12)= zk8(jnoe+ino-1)
                ino = ino+1
                iq = iq+9
            else if (tcosup(is,1).eq.2) then
                do 46 ii = 1, ino
                    if (nomsup(is,id) .eq. zk8(jnoe+ino-1)) goto 42
46              continue
                zk8(jnoe+ino-1)= nomsup(is,id)
                chainl(il+5 : il+12)= nomsup(is,id)
                ino = ino+1
                il = il+9
            else if (tcosup(is,1).eq.3) then
                do 48 ii = 1, ino
                    if (nomsup(is,id) .eq. zk8(jnoe+ino-1)) goto 42
48              continue
                chaina(ia+5 : ia+12)= nomsup(is,id)
                zk8(jnoe+ino-1)= nomsup(is,id)
                ino = ino+1
                ia = ia+9
            endif
42      continue
40  end do
!
    if (iq .ne. 1) write(ifm,1180) chainq
    if (il .ne. 1) write(ifm,1180) chainl
    if (ia .ne. 1) write(ifm,1180) chaina
!
!
!
!  CAS DES IMPRESSIONS POUR LA REPONSE SECONDAIRE
!
    call getfac('COMB_DEPL_APPUI', nboc)
    call getfac('DEPL_MULT_APPUI', ndep)
    call jeveuo('&&ASENAP.TYPE', 'L', jtyp)
    noref = '-'
    write(ifm,1190)
    write(ifm,1200)
    do 10 iocc = 1, nboc
        call jelira(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
        call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'L', jcas)
        do 20 icas = 1, ncas
            nucas = zi(jcas+icas-1)
            do 30 idep = 1, ndep
                call getvis('DEPL_MULT_APPUI', 'NUME_CAS', idep, iarg, 1,&
                            nume, ibid)
                if (nume .eq. nucas) then
                    call getvtx('DEPL_MULT_APPUI', 'NOM_CAS', idep, iarg, 1,&
                                nomcas, ibid)
                    knum = 'N       '
                    call codent(nucas, 'D0', knum(2:8))
                    kdir = 'D       '
                    call codent(nucas, 'D0', kdir(2:8))
                    call jelira(jexnom('&&ASENAP.LINOEU', knum), 'LONMAX', nbno)
                    call jeveuo(jexnom('&&ASENAP.LINOEU', knum), 'L', jno)
                    lnod =3*nbno
                    call jelira(jexnom('&&ASENAP.LIDIR', kdir), 'LONMAX', lnod)
                    call jeveuo(jexnom('&&ASENAP.LIDIR', kdir), 'L', jdir)
                    if (zi(jref+icas-1) .eq. 1) noref = zk8(jnref+icas- 1)
                    do 12 ino = 1, nbno
                        noeu =zk8(jno+ino-1)
                        if (zr(jdir+3*(ino-1)) .ne. epsima) then
                            cmp = 'DX'
                            vale = zr(jdir+3*(ino-1))
                            write(ifm,1210)nucas,noeu, cmp,vale,noref,&
                            nomcas
                        endif
                        if (zr(jdir+3*(ino-1)+1) .ne. epsima) then
                            cmp = 'DY'
                            vale = zr(jdir+3*(ino-1)+1)
                            write(ifm,1210)nucas,noeu, cmp,vale,noref,&
                            nomcas
                        endif
                        if (zr(jdir+3*(ino-1)+2) .ne. epsima) then
                            cmp = 'DZ'
                            vale = zr(jdir+3*(ino-1)+2)
                            write(ifm,1210)nucas,noeu, cmp,vale,noref,&
                            nomcas
                        endif
12                  continue
                endif
30          continue
20      continue
10  end do
    do 50 iocc = 1, nboc
        call jelira(jexnum('&&ASENAP.LISTCAS', iocc), 'LONMAX', ncas)
        call jeveuo(jexnum('&&ASENAP.LISTCAS', iocc), 'L', jcas)
        write(ifm,1220)
        if (zi(jtyp+iocc-1) .eq. 1) typdir = 'QUAD'
        if (zi(jtyp+iocc-1) .eq. 2) typdir = 'LINE'
        if (zi(jtyp+iocc-1) .eq. 3) typdir = 'ABS'
        iordr = zi(jord+iocc-1)
        write(ifm,1230)
        write(ifm,1240)iordr,typdir,( zi(jcas+ic-1),ic=1,ncas)
50  end do
    iordr = zi(jord+nboc)
    typdir ='QUAD'
    write(ifm,1250)
    write(ifm,1260)
    write(ifm,1270)iordr,typdir
!
! ----LISTE DES FORMATS D IMPRESSIONS
!
!
    1167 format(/,1x,'--- COMPOSANTE PRIMAIRE ---')
    1170 format(1x,&
     &      'COMBI SUPPORT')
    1180 format(2x,a80)
    1190 format(/,1x,' --- COMPOSANTE SECONDAIRE ---')
    1200 format(1x,&
     & '  CAS      SUPPORT     CMP        VALEUR       '//&
     &  ' NOEUD_REFE  NOM_CAS')
    1210 format(1p,1x,i5,5x,a8,3x,a8,5x,d12.5,3x,a8,3x,a80)
    1220 format(1x,&
     &      'GROUPE DE CAS')
    1230 format(1x,'NUME_ORDRE     COMBI     LIST_CAS')
    1240 format(1p,1x,i5,8x,a8,5x,100(i3,1x))
    1250 format(/,1x,' SOMME QUADRATIQUE DES OCCURENCES '//&
     &   ' DE COMB_DEPL_APPUI    ')
    1260 format(/,1x,'NUME_ORDRE     CUMUL     ')
    1270 format(1p,1x,i5,8x,a8)
!
!
! --- MENAGE
!
    call jedetr('&&ASECON.NORD')
    call jedetr('&&ASIMPR.NOEUDS')
!
    call jedema()
end subroutine
