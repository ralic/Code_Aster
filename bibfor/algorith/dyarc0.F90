subroutine dyarc0(resuz, nbnosy, nbarch, lisarc, nbchex,&
                  lichex)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnu.h"
#include "asterfort/rsutrg.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbarch, nbchex, nbnosy
    character(len=*) :: resuz, lisarc, lichex
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     COMMANDE EXTR_RESU :
!        SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
!
! IN  : RESU   : NOM DE LA SD RESULTAT A EXTRAIRE
! IN  : NBNOSY : NOMBRE DE NOMS SYMBOLIQUES DANS LA SD
! OUT : NBARCH : NOMBRE DE NUMEROS D'ORDRE A ARCHIVER
! OUT : LISARC : NUMEROS D'ORDRE A ARCHIVER
! OUT : NBCHEX : NOMBRE DE NOMS DES CHAMPS EXCLUS
! OUT : LICHEX : NOMS DES CHAMPS EXCLUS
! ----------------------------------------------------------------------
    integer :: ibid, jarch, jchex, n1, nbocc,  lnum, k, ier, ipach, karch
    integer :: jordr, nbtrou, nbordr(1), iocc, n2, nbcham, i, j, iret, iflag
    integer :: irang
    real(kind=8) :: r8b, prec
    complex(kind=8) :: c16b
    character(len=8) :: k8b, crit
    character(len=16) :: motcle, nomsym
    character(len=19) :: numarc, knum, resu
    character(len=16), pointer :: trav1(:) => null()
    integer, pointer :: vale(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    motcle = 'ARCHIVAGE'
    lichex = '&&OP0176.LISTE.CHAM'
    iocc = 1
    resu = resuz
    call rsorac(resu, 'LONUTI', 0, r8b, k8b,&
                c16b, r8b, k8b, nbordr, 1,&
                ibid)
!
    nbchex = 0
    call wkvect(lisarc, 'V V I', nbordr(1), jarch)
!
!     --- LES CHAMPS EN SORTIE ---
!
    call getvtx(motcle, 'CHAM_EXCLU', iocc=iocc, nbval=0, nbret=n1)
!
    if (n1 .ne. 0) then
        nbchex = -n1
        call wkvect(lichex, 'V V K16', nbchex, jchex)
        call getvtx(motcle, 'CHAM_EXCLU', iocc=iocc, nbval=nbchex, vect=zk16( jchex),&
                    nbret=n1)
    else
        call wkvect(lichex, 'V V K16', 1, jchex)
    endif
!
!
    call getvtx(motcle, 'NOM_CHAM', iocc=iocc, nbval=0, nbret=n2)
!
! --- ON REGENERE UNE LISTE DE CHAMPS EXCLUS A PARTIR DES CHAMPS
! --- A GARDER
!
!
    if (n2 .ne. 0) then
        nbcham = -n2
        nbchex = nbnosy - nbcham
!
        call jeexin(lichex, iret)
        if (iret .ne. 0) call jedetr(lichex)
!
        call wkvect(lichex, 'V V K16', nbchex, jchex)
        AS_ALLOCATE(vk16=trav1, size=nbcham)
        call getvtx(motcle, 'NOM_CHAM', iocc=iocc, nbval=nbcham, vect=trav1,&
                    nbret=ibid)
!
! ---   ON TESTE SI LES NOM_CHAM EXISTENT DANS LA SD
! ---
        do 70 i = 1, nbcham
            iflag = 0
            do 80 j = 1, nbnosy
                call jenuno(jexnum(resu//'.DESC', j), nomsym)
                if (trav1(i) .eq. nomsym) then
                    iflag = 1
                    goto 70
                endif
80          continue
            if (iflag .eq. 0) then
                call utmess('F', 'ALGORITH3_40', sk=trav1(i))
            endif
70      continue
!
        k = 1
        do 50 i = 1, nbnosy
            call jenuno(jexnum(resu//'.DESC', i), nomsym)
            do 60 j = 1, nbcham
                if (nomsym .eq. trav1(j)) then
                    goto 50
                endif
60          continue
            zk16( jchex+k-1 )= nomsym
            k = k + 1
50      continue
    endif
!
!
    call getfac(motcle, nbocc)
    if (nbocc .eq. 0) then
        do 90 k = 1, nbordr(1)
            zi(jarch+k-1)=1
90      continue
        goto 9999
    endif
!
!     --- LES NUMEROS D'ORDRE EN SORTIE ---
!
    call getvid(motcle, 'LIST_ARCH', iocc=iocc, scal=numarc, nbret=n1)
    if (n1 .ne. 0) then
        call jeveuo(numarc//'.VALE', 'L', vi=vale)
        call jelira(numarc//'.VALE', 'LONUTI', lnum)
        do 10 k = 1, lnum
            karch = vale(k)
            if (karch .le. 0) then
                goto 10
            else if (karch .gt. nbordr(1)) then
                goto 12
            else
                zi(jarch+karch-1) = 1
            endif
10      continue
12      continue
        goto 9999
    endif
!
    call getvis(motcle, 'PAS_ARCH', iocc=iocc, scal=ipach, nbret=n1)
    if (n1 .ne. 0) then
        ipach = 1
        do 20 k = ipach, nbordr(1), ipach
            zi(jarch+k-1) = 1
20      continue
        goto 9999
    endif
!
    call getvtx(motcle, 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
    call getvr8(motcle, 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    knum = '&&DYARC0.NUME_ORDRE'
    call rsutnu(resu, motcle, iocc, knum, nbtrou,&
                prec, crit, ier)
    if (ier .ne. 0) then
        call utmess('F', 'ALGORITH3_41')
    endif
    call jeveuo(knum, 'L', jordr)
    do 30 k = 1, nbtrou
        karch = zi(jordr+k-1)
        call rsutrg(resu, karch, irang, ibid)
        zi(jarch+irang-1) = 1
30  end do
    call jedetr(knum)
!
9999  continue
!
    nbarch = 0
    do 40 k = 1, nbordr(1)
        nbarch = nbarch + zi(jarch+k-1)
40  end do
!
    AS_DEALLOCATE(vk16=trav1)
!
    call jedema()
!
end subroutine
