subroutine pecage(resu, modele, nbocc)
    implicit none
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim3.h"
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
#include "asterfort/megeom.h"
#include "asterfort/pecag2.h"
#include "asterfort/pecag3.h"
#include "asterfort/pemica.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbocc
    character(len=*) :: resu, modele
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
!     TRAITEMENT DU MOT CLE-FACTEUR "CARA_GEOM"
!     ------------------------------------------------------------------
!
    integer :: mxvale, nbparr, ibid, iret, lvale, iocc, nt, ng, nm, nbgrma, jgr
    integer :: ig, nbma, jad, nbmail, jma, im, nume, ndim, ns1, ns2, ie, nbparc
    integer :: np, ifm, niv, iorig, i, icage
    parameter (mxvale=29,nbparr=46)
    real(kind=8) :: valpar(nbparr), r8b, xyp(2), orig(3), zero
    character(len=3) :: symex, symey, typarr(nbparr)
    character(len=8) :: k8b, noma, lpain(15), lpaout(5)
    character(len=24) :: valk(2)
    character(len=16) :: option, noparr(nbparr)
    character(len=19) :: chelem
    character(len=24) :: lchin(15), lchout(1), ligrel, mlggma, mlgnma
    character(len=24) :: chgeom
    complex(kind=8) :: c16b
    logical :: nsymx, nsymy
!     ------------------------------------------------------------------
    data noparr/'LIEU','ENTITE','A_M','CDG_Y_M','CDG_Z_M','IY_G_M',&
     &     'IZ_G_M','IYZ_G_M','Y_MAX','Z_MAX','Y_MIN','Z_MIN','R_MAX',&
     &     'A','CDG_Y','CDG_Z','IY_G','IZ_G','IYZ_G','IY',&
     &     'IZ','ALPHA','Y_P','Z_P','IY_P','IZ_P','IYZ_P','JX',&
     &     'AY','AZ','EY','EZ','PCTY','PCTZ','JG','KY','KZ','IYR2_G',&
     &     'IZR2_G','IYR2','IZR2','IYR2_P','IZR2_P','RY','RZ',&
     &     'MAILLAGE'/
    data typarr/  'K24','K8','R','R','R','R','R','R','R','R','R','R',&
     &     'R','R','R','R','R','R','R','R','R','R','R','R','R','R','R',&
     &     'R','R','R','R','R','R','R','R','R','R','R','R','R','R','R',&
     &     'R','R','R','K8'/
!     ------------------------------------------------------------------
!
    call jemarq()
    c16b=(0.d0,0.d0)
    r8b=0.d0
    iorig = 0
    icage = 1
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    option = 'CARA_GEOM'
!      CALL MECHAM ( OPTION, MODELE, NCHAR, LCHAR, CARA, NH,
!     &                              CHGEOM, CHCARA, CHHARM, IRET )
!      IF ( IRET .NE. 0 ) GOTO 9999
    call megeom(modele, chgeom)
    zero = 0.0d0
    orig(1) = zero
    orig(2) = zero
    orig(3) = zero
    noma = chgeom(1:8)
    mlgnma = noma//'.NOMMAI'
    mlggma = noma//'.GROUPEMA'
    ndim = 3
    call dismoi('F', 'Z_CST', modele, 'MODELE', ibid,&
                k8b, ie)
    if (k8b(1:3) .eq. 'OUI') ndim = 2
!
    call exlim3('CARA_GEOM', 'V', modele, ligrel)
!     --- CALCUL DE L'OPTION ---
    chelem = '&&PECAGE.CARA_GEOM'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpaout(1) = 'PCARAGE'
    lchout(1) = chelem
!
    call calcul('S', option, ligrel, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    nsymx = .false.
    nsymy = .false.
    call getvtx('CARA_GEOM', 'SYME_X', iocc=1, scal=symex, nbret=ns1)
    call getvtx('CARA_GEOM', 'SYME_Y', iocc=1, scal=symey, nbret=ns2)
    if (symex .eq. 'OUI') nsymx = .true.
    if (symey .eq. 'OUI') nsymy = .true.
    call getvr8('CARA_GEOM', 'ORIG_INER', iocc=1, nbval=2, vect=xyp,&
                nbret=np)
!
!     --- CREATION DE LA TABLE ---
!
    if (ndim .eq. 2) then
        call tbcrsd(resu, 'G')
        call tbajpa(resu, nbparr, noparr, typarr)
    else
        call utmess('F', 'UTILITAI3_48')
    endif
    nbparc = nbparr - 1
    call tbajli(resu, 1, noparr(nbparr), [ibid], [r8b],&
                [c16b], noma, 0)
!
    call wkvect('&&PECAGE.TRAV1', 'V V R', mxvale, lvale)
    do 40 iocc = 1, nbocc
!
        call getvtx('CARA_GEOM', 'TOUT', iocc=iocc, nbval=0, nbret=nt)
        call getvtx('CARA_GEOM', 'GROUP_MA', iocc=iocc, nbval=0, nbret=ng)
        call getvtx('CARA_GEOM', 'MAILLE', iocc=iocc, nbval=0, nbret=nm)
!
        do 10 i = 1, nbparr
            valpar(i) = r8vide()
10      continue
        valk(1) = '????????'
        valk(2) = '????????'
!
        if (nt .ne. 0) then
            call pemica(chelem, mxvale, zr(lvale), 0, [ibid],&
                        orig, iorig, icage)
            call pecag2(ndim, nsymx, nsymy, np, xyp,&
                        zr(lvale), valpar)
            call pecag3(ndim, nsymx, nsymy, noma, 'TOUT',&
                        0, k8b, valpar)
            valk(1) = noma
            valk(2) = 'TOUT'
            call tbajli(resu, nbparc, noparr, [ibid], valpar,&
                        [c16b], valk, 0)
        endif
!
        if (ng .ne. 0) then
            nbgrma = -ng
            call wkvect('&&PECAGE_GROUPM', 'V V K24', nbgrma, jgr)
            call getvtx('CARA_GEOM', 'GROUP_MA', iocc=iocc, nbval=nbgrma, vect=zk24(jgr),&
                        nbret=ng)
            valk(2) = 'GROUP_MA'
            do 20 ig = 1, nbgrma
                call jeexin(jexnom(mlggma, zk24(jgr+ig-1)), iret)
                if (iret .eq. 0) then
                    call utmess('F', 'UTILITAI3_46', sk=zk24(jgr+ig-1))
                    goto 20
                endif
                call jelira(jexnom(mlggma, zk24(jgr+ig-1)), 'LONUTI', nbma)
                if (nbma .eq. 0) then
                    call utmess('A', 'UTILITAI3_47', sk=zk24(jgr+ig-1))
                    goto 20
                endif
                call jeveuo(jexnom(noma//'.GROUPEMA', zk24(jgr+ig-1)), 'L', jad)
                call pemica(chelem, mxvale, zr(lvale), nbma, zi(jad),&
                            orig, iorig, icage)
                call pecag2(ndim, nsymx, nsymy, np, xyp,&
                            zr(lvale), valpar)
                call pecag3(ndim, nsymx, nsymy, noma, 'GROUP_MA',&
                            1, zk24(jgr+ig-1), valpar)
                valk(1) = zk24(jgr+ig-1)
                call tbajli(resu, nbparc, noparr, [ibid], valpar,&
                            [c16b], valk, 0)
20          continue
            call jedetr('&&PECAGE_GROUPM')
        endif
!
        if (nm .ne. 0) then
            nbmail = -nm
            call wkvect('&&PECAGE_MAILLE', 'V V K8', nbmail, jma)
            call getvtx('CARA_GEOM', 'MAILLE', iocc=iocc, nbval=nbmail, vect=zk8(jma),&
                        nbret=nm)
            valk(2) = 'MAILLE'
            do 30 im = 1, nbmail
                call jeexin(jexnom(mlgnma, zk8(jma+im-1)), iret)
                if (iret .eq. 0) then
                    call utmess('A', 'UTILITAI3_49', sk=zk8(jma+im-1))
                    goto 30
                endif
                call jenonu(jexnom(mlgnma, zk8(jma+im-1)), nume)
                call pemica(chelem, mxvale, zr(lvale), 1, [nume],&
                            orig, iorig, icage)
                call pecag2(ndim, nsymx, nsymy, np, xyp,&
                            zr(lvale), valpar)
                call pecag3(ndim, nsymx, nsymy, noma, 'MAILLE',&
                            nbmail, zk8( jma), valpar)
                valk(1) = zk8(jma+im-1)
                call tbajli(resu, nbparc, noparr, [ibid], valpar,&
                            [c16b], valk, 0)
30          continue
!
            call jedetr('&&PECAGE_MAILLE')
        endif
40  end do
!
    call detrsd('CHAM_ELEM', '&&PECAGE.CARA_GEOM')
    call jedetr('&&PECAGE.TRAV1')
!
    call jedema()
end subroutine
