subroutine op0174()
    implicit   none
! ----------------------------------------------------------------------
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
!     COMMANDE:  RECU_TABLE
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getltx.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ltnotb.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexpa.h"
#include "asterfort/rsorac.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: iret, lonord, iord, ipara, i, n, numord, ibid
    integer :: nbpara, inom, ityp, ilong, itabi, itabr, itabc
    integer :: pi, pc, pr
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    character(len=8) :: table, concpt, typ
    character(len=16) :: nomcmd, typcon, nomsym, k16b, nom
    character(len=24) :: nomtab, kbid
    integer :: iarg
!
!     ------------------------------------------------------------------
!
!
    call jemarq()
    call infmaj()
!
    call getres(table, typcon, nomcmd)
    call getvid(' ', 'CO', 1, iarg, 1,&
                concpt, iret)
!
!
! --------------------------
!   EXTRACTION D'UNE TABLE
! --------------------------
!
    call getvtx(' ', 'NOM_TABLE', 0, iarg, 1,&
                nomsym, iret)
    if (iret .ne. 0) then
        call ltnotb(concpt, nomsym, nomtab)
        call copisd('TABLE', 'G', nomtab, table)
        call titre()
        goto 9999
    endif
!
!
!
! ----------------------------
!   EXTRACTION DE PARAMETRES
! ----------------------------
!
!    NOMBRE DE PARAMETRES
    call getvtx(' ', 'NOM_PARA', 0, iarg, 0,&
                k16b, nbpara)
    call assert(nbpara.ne.0)
    nbpara = -nbpara
!
!
! -- LECTURE DES NUMEROS D'ORDRE
!
    call rsorac(concpt, 'LONUTI', ibid, rbid, kbid,&
                cbid, rbid, kbid, lonord, 1,&
                ibid)
    call wkvect('&&OP0174.NUME_ORDRE', 'V V I', lonord, iord)
    call rsorac(concpt, 'TOUT_ORDRE', ibid, rbid, kbid,&
                cbid, rbid, kbid, zi(iord), lonord,&
                ibid)
!
!
! -- NOMS DES PARAMETRES A EXTRAIRE
!
    call wkvect('&&OP0174.NOM_PARA', 'V V K16', nbpara+1, inom)
    call wkvect('&&OP0174.LONG_NOM_PARA', 'V V I', nbpara, ilong)
!
    call getltx(' ', 'NOM_PARA', 1, 16, nbpara,&
                zi(ilong), ibid)
    do 10 i = 1, nbpara
        if (zi(ilong-1+i) .gt. 16) call u2mess('F', 'UTILITAI3_4')
10  end do
!
    zk16(inom) = 'NUME_ORDRE'
    call getvtx(' ', 'NOM_PARA', 0, iarg, nbpara,&
                zk16(inom+1), ibid)
!
    do 20 i = 1, nbpara
        nom = zk16(inom + i)
        call rsexpa(concpt, 2, nom, iret)
        if (iret .eq. 0) call u2mesk('F', 'UTILITAI3_5', 1, nom)
20  continue
!
!
! -- TYPES DES PARAMETRES A EXTRAIRE
!
    call wkvect('&&OP0174.TYPE_PARA', 'V V K8 ', nbpara+1, ityp)
    zk8(ityp) = 'I'
    numord = zi(iord)
    do 30 i = 1, nbpara
        nom = zk16(inom + i)
        call rsadpa(concpt, 'L', 1, nom, numord,&
                    1, ipara, typ)
        if (typ(1:1) .ne. 'R' .and. typ(1:1) .ne. 'I' .and. typ(1:1) .ne. 'C') then
            call u2mess('F', 'UTILITAI3_6')
        endif
        zk8(ityp+i) = typ(1:1)
30  end do
!
!
! -- INITIALISATION DE LA TABLE
!
    call tbcrsd(table, 'G')
    call titre()
    call tbajpa(table, 1+nbpara, zk16(inom), zk8(ityp))
!
!
! -- EXTRACTION DES PARAMETRES
!
    call wkvect('&&OP0174.PARA_R', 'V V R', nbpara+1, itabr)
    call wkvect('&&OP0174.PARA_I', 'V V I', nbpara+1, itabi)
    call wkvect('&&OP0174.PARA_C', 'V V C', nbpara+1, itabc)
!
    do 40 n = 1, lonord
        numord = zi(iord-1+n)
!
        pi = 0
        pr = 0
        pc = 0
!
        zi(itabi+pi) = numord
        pi = pi+1
!
        do 50 i = 1, nbpara
            nom = zk16(inom + i)
            call rsadpa(concpt, 'L', 1, nom, numord,&
                        1, ipara, typ)
            if (typ(1:1) .eq. 'R') then
                zr(itabr+pr) = zr(ipara)
                pr = pr+1
            else if (typ(1:1).eq.'I') then
                zi(itabi+pi) = zi(ipara)
                pi = pi+1
            else if (typ(1:1).eq.'C') then
                zc(itabc+pc) = zc(ipara)
                pc = pc+1
            else
                call u2mess('F', 'UTILITAI3_7')
            endif
50      continue
!
        call tbajli(table, 1+nbpara, zk16(inom), zi(itabi), zr(itabr),&
                    zc(itabc), kbid, 0)
40  end do
!
!
!
9999  continue
    call jedema()
end subroutine
