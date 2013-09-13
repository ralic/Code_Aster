subroutine pemain(resu, modele, mate, cara, nh,&
                  nbocc, deform)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/exlim3.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/mecham.h"
#include "asterfort/pemica.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/vtgpld.h"
#include "asterfort/wkvect.h"
!
    integer :: nh, nbocc
    character(len=*) :: resu, modele, mate, cara, deform
!     ------------------------------------------------------------------
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "MASS_INER"
!     ------------------------------------------------------------------
!
    integer :: mxvale, nbparr, ibid, iret, lvale, iocc, nt, ng, nr, nm, nbgrma, jgr, ig, nbma, jad
    integer :: nbmail, jma, im, nume, nb, ifm, niv, mxval1, nbpar1, mxval2, nbpar2, iorig, nre
    integer :: icage
    parameter (mxval1=16,nbpar1=18)
    parameter (mxval2=25,nbpar2=27)
    real(kind=8) :: zero, orig(3), r8b
    character(len=8) :: k8b, noma, lpain(16), lpaout(5), typarr(nbpar2), valk(2)
    character(len=16) :: noparr(nbpar2)
    character(len=19) :: chelem, chdef
    character(len=24) :: lchin(16), lchout(1), mlggma, mlgnma, valk2(2)
    character(len=24) :: chgeom, chgeo2, chcara(18), chharm, ligrel
    complex(kind=8) :: c16b
    integer :: iarg
!
    data noparr/'LIEU','ENTITE','MASSE','CDG_X','CDG_Y','CDG_Z',&
     &     'IX_G','IY_G','IZ_G','IXY_G','IXZ_G','IYZ_G','IX_PRIN_G',&
     &     'IY_PRIN_G','IZ_PRIN_G','ALPHA','BETA','GAMMA','X_P','Y_P',&
     &     'Z_P','IX_P','IY_P','IZ_P','IXY_P','IXZ_P','IYZ_P'/
    data typarr/'K24','K8','R','R','R','R','R','R','R','R','R','R',&
     &     'R','R','R','R','R','R','R','R','R','R','R','R','R','R','R'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    icage = 0
    zero = 0.0d0
    r8b = 0.0d0
    chdef = deform
    call mecham('MASS_INER', modele, cara, nh, chgeom,&
                chcara, chharm, iret)
    if (iret .ne. 0) goto 60
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
!
    call exlim3('MASS_INER', 'V', modele, ligrel)
!
!     --- CALCUL DE L'OPTION ---
    chelem = '&&PEMAIN.MASS_INER'
    lpain(1) = 'PGEOMER'
    if (chdef .ne. ' ') then
        chgeo2 = '&&PEMAIN.CH_GEOMER'
        call vtgpld('CUMU', chgeom, 1.d0, chdef, 'V',&
                    chgeo2)
        lchin(1) = chgeo2
    else
        lchin(1) = chgeom
    endif
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)
    lpain(4) = 'PCADISM'
    lchin(4) = chcara(3)
    lpain(5) = 'PCAGNPO'
    lchin(5) = chcara(6)
    lpain(6) = 'PCACOQU'
    lchin(6) = chcara(7)
    lpain(7) = 'PCASECT'
    lchin(7) = chcara(8)
    lpain(8) = 'PCAARPO'
    lchin(8) = chcara(9)
    lpain(9) = 'PCAGNBA'
    lchin(9) = chcara(11)
    lpain(10) = 'PCAGEPO'
    lchin(10) = chcara(5)
    lpain(11) = 'PNBSP_I'
    lchin(11) = chcara(16)
    lpain(12) = 'PFIBRES'
    lchin(12) = chcara(17)
    lpain(13) = 'PCOMPOR'
    lchin(13) = mate(1:8)//'.COMPOR'
    lpain(14) = 'PCAPOUF'
    lchin(14) = chcara(13)
    lpain(15) = 'PCINFDI'
    lchin(15) = chcara(15)
    nb = 15
    lpaout(1) = 'PMASSINE'
    lchout(1) = chelem
!
    call calcul('S', 'MASS_INER', ligrel, nb, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    mxvale = mxval1
    nbparr = nbpar1
    do 10 iocc = 1, nbocc
        call getvr8('MASS_INER', 'ORIG_INER', iocc=iocc, nbval=0, nbret=nr)
        if (nr .ne. 0) then
            mxvale = mxval2
            nbparr = nbpar2
            goto 20
        endif
10  continue
20  continue
!
!     --- CREATION DE LA TABLE ---
    call tbcrsd(resu, 'G')
    call tbajpa(resu, nbparr, noparr, typarr)
!
    call wkvect('&&PEMAIN.TRAV1', 'V V R', mxvale, lvale)
    do 50 iocc = 1, nbocc
        iorig = 0
        orig(1) = zero
        orig(2) = zero
        orig(3) = zero
        call getvtx('MASS_INER', 'TOUT', iocc=iocc, nbval=0, nbret=nt)
        call getvem(noma, 'GROUP_MA', 'MASS_INER', 'GROUP_MA', iocc,&
                    iarg, 0, k8b, ng)
        call getvem(noma, 'MAILLE', 'MASS_INER', 'MAILLE', iocc,&
                    iarg, 0, k8b, nm)
        call getvr8('MASS_INER', 'ORIG_INER', iocc=iocc, nbval=0, nbret=nr)
        if (nr .ne. 0) then
            iorig = 1
            nre = -nr
            call getvr8('MASS_INER', 'ORIG_INER', iocc=iocc, nbval=nre, vect=orig,&
                        nbret=nr)
        endif
        if (nt .ne. 0) then
            call pemica(chelem, mxvale, zr(lvale), 0, ibid,&
                        orig, iorig, icage)
            valk(1) = noma
            valk(2) = 'TOUT'
            call tbajli(resu, nbparr, noparr, ibid, zr(lvale),&
                        c16b, valk, 0)
        endif
        if (ng .ne. 0) then
            nbgrma = -ng
            call wkvect('&&PEMAIN_GROUPM', 'V V K24', nbgrma, jgr)
            call getvem(noma, 'GROUP_MA', 'MASS_INER', 'GROUP_MA', iocc,&
                        iarg, nbgrma, zk24(jgr), ng)
            valk2(2) = 'GROUP_MA'
            do 30 ig = 1, nbgrma
                call jeexin(jexnom(mlggma, zk24(jgr+ig-1)), iret)
                if (iret .eq. 0) then
                    call utmess('A', 'UTILITAI3_46', sk=zk24(jgr+ig-1))
                    goto 30
                endif
                call jelira(jexnom(mlggma, zk24(jgr+ig-1)), 'LONUTI', nbma)
                if (nbma .eq. 0) then
                    call utmess('A', 'UTILITAI3_47', sk=zk24(jgr+ig-1))
                    goto 30
                endif
                call jeveuo(jexnom(noma//'.GROUPEMA', zk24(jgr+ig-1)), 'L', jad)
                call pemica(chelem, mxvale, zr(lvale), nbma, zi(jad),&
                            orig, iorig, icage)
                valk2(1) = zk24(jgr+ig-1)
                call tbajli(resu, nbparr, noparr, ibid, zr(lvale),&
                            c16b, valk2, 0)
30          continue
            call jedetr('&&PEMAIN_GROUPM')
        endif
        if (nm .ne. 0) then
            nbmail = -nm
            call wkvect('&&PEMAIN_MAILLE', 'V V K8', nbmail, jma)
            call getvem(noma, 'MAILLE', 'MASS_INER', 'MAILLE', iocc,&
                        iarg, nbmail, zk8(jma), nm)
            valk(2) = 'MAILLE'
            do 40 im = 1, nbmail
                call jeexin(jexnom(mlgnma, zk8(jma+im-1)), iret)
                if (iret .eq. 0) then
                    call utmess('A', 'UTILITAI3_49', sk=zk8(jma+im-1))
                    goto 40
                endif
                call jenonu(jexnom(mlgnma, zk8(jma+im-1)), nume)
                call pemica(chelem, mxvale, zr(lvale), 1, nume,&
                            orig, iorig, icage)
                valk(1) = zk8(jma+im-1)
                call tbajli(resu, nbparr, noparr, ibid, zr(lvale),&
                            c16b, valk, 0)
40          continue
            call jedetr('&&PEMAIN_MAILLE')
        endif
50  continue
!
! --- MENAGE
    call detrsd('CHAM_ELEM', '&&PEMAIN.MASS_INER')
    call detrsd('CHAMP_GD', '&&PEMAIN.CH_GEOMER')
    call jedetr('&&PEMAIN.TRAV1')
!
60  continue
!
    call jedema()
end subroutine
