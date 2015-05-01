subroutine mpmod3(basemo, nommes, nbmesu, nbmtot, vcham,&
                  vnoeud, vrange, vorien, nnoema, ncmpma)
!
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!
!     PROJ_MESU_MODAL : ESTIMATION DE NBMESU ET VERIFICATION EXISTENCE
!                       DES VECTEURS DE BASE
!
!     IN  : BASEMO : NOM DE LA BASE DE PROJECTION
!     IN  : NOMMES : NOM DE LA MESURE
!
!     OUT  : NBMESU : NOMBRE DE MESURE (DATASET 58)
!     OUT  : NBMTOT : NOMBRE DE VECTEURS DE BASE
!     OUT  : VNOEUD : NOM RANGEMENT NOEUD MESURE
!     OUT  : VRANGE : NOM CORRESPONDANCE CMP SUIVANT VNOEUD
!     OUT  : VORIEN : NOM CORRESPONDANCE ORIENTATION SUIVANT VNOEUD
!     OUT  : VCHAM : NOM CORRESPONDANCE CHAMP SUIVANT VNOEUD
!     OUT  : NNOEMA : NOMBRE DE NOEUDS MAXI
!     OUT  : NCMPMA : NOMBRE DE CMP MAXI
!
    implicit none
!     ------------------------------------------------------------------
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/r8prem.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/scalai.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: basemo, nommes
    character(len=24) :: vnoeud, vrange, vcham, vorien
    integer :: nbmesu, nbmtot, nnoema, ncmpma
!
    character(len=1) :: typval
    character(len=8) :: nomres, k8bid, scal, compms, comcap
    character(len=8) :: nomgd, licmp(30), modmes
    character(len=16) :: nomcha, typres, k16bid
    character(len=19) :: chamno, ch1s, ch2s, chs, chames, chacap
    character(len=24) :: vref
!
    integer :: lord, lori, lrange, lref
    integer :: imesu, ii, imode, iret, nbord, tmod(1)
    integer :: icmp, ino, inomes, inocap
    integer :: lnoeud, gd, nbnoeu, nbcmp
    integer :: jcnsd, jcnsc, jcnsv, jcnsl
    integer :: ibid, indice, nbcham, lch, ich, lcham
!
    aster_logical :: zcmplx, orien, dcapt
!
    real(kind=8) :: val, rbid
!
    complex(kind=8) :: cbid
    integer, pointer :: desc(:) => null()
    character(len=8), pointer :: cnsk(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! RECUPERATION DU NOM DU CONCEPT RESULTAT
    call getres(nomres, typres, k16bid)
!
! RECUPERATION DU CHAMP MESURE
!
    call getvtx('MODELE_MESURE', 'NOM_CHAM', iocc=1, nbval=0, nbret=nbcham)
    if (nbcham .ne. 0) then
        nbcham = -nbcham
    else
        call utmess('A', 'ALGORITH10_93')
    endif
    call wkvect('&&LISTE_CHAM', 'V V K16', nbcham, lch)
    call getvtx('MODELE_MESURE', 'NOM_CHAM', iocc=1, nbval=nbcham, vect=zk16(lch),&
                nbret=ibid)
!
! RECUPERATION DU NB DE VECTEURS DE BASE : NBMTOT
!
    call rsorac(basemo, 'LONUTI', 0, rbid, k8bid,&
                cbid, rbid, 'ABSOLU', tmod, 1,&
                ibid)
    nbmtot=tmod(1)            
!
! RECUPERATION DES OBJETS LIES A LA MESURE
!
    call jeveuo(nommes//'           .ORDR', 'L', lord)
!
! RECUPERATION DU NB DE NUMERO D ORDRE : NBORD
!
    call rsorac(nommes, 'LONUTI', 0, rbid, k8bid,&
                cbid, rbid, 'ABSOLU', tmod, 1,&
                ibid)
    nbord=tmod(1)            
!
    chs = '&&MESURE.CHS'
    nbmesu = 0
!
    nnoema = 0
    ncmpma = 0
!
!     CALCUL DE NNOEMA ET NCMPMA
!
    do 150 ich = 1, nbcham
        nomcha = zk16(lch-1 +ich)
        call rsexch('F', nommes, nomcha, zi(lord), chamno,&
                    iret)
!
! TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
        call cnocns(chamno, 'V', chs)
        call jeveuo(chs//'.CNSD', 'L', jcnsd)
!
        nbnoeu = zi(jcnsd-1 + 1)
        nbcmp = zi(jcnsd-1 + 2)
        nnoema = nnoema + nbnoeu
        ncmpma = ncmpma + nbcmp
!
150 end do
!
! ORDRE DE RANGEMENT MESURE SELON VRANGE ET VNOEUD
    vnoeud = nomres//'.PROJM    .PJMNO'
    vrange = nomres//'.PROJM    .PJMRG'
    vcham = nomres//'.PROJM    .PJMCH'
    vorien = nomres//'.PROJM    .PJMOR'
    vref = nomres//'.PROJM    .PJMRF'
!
    call wkvect(vnoeud, 'G V I', nnoema*ncmpma, lnoeud)
    call wkvect(vrange, 'G V K8', nnoema*ncmpma, lrange)
    call wkvect(vcham, 'V V K16', nnoema*ncmpma, lcham)
    call wkvect(vorien, 'G V R', nnoema*ncmpma*3, lori)
    call wkvect(vref, 'G V K16', 5, lref)
!
! BOUCLE SUR LES CHAMPS MESURES
!
    do 151 ich = 1, nbcham
        nomcha = zk16(lch-1 +ich)
        call rsexch('F', nommes, nomcha, zi(lord), chamno,&
                    iret)
!
        call jeveuo(chamno//'.DESC', 'L', vi=desc)
        gd = desc(1)
        scal = scalai(gd)
        typval = scal(1:1)
        if (typval .eq. 'C') then
            zcmplx = .true.
        else
            zcmplx = .false.
        endif
!
! TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
        call cnocns(chamno, 'V', chs)
        call jeveuo(chs//'.CNSK', 'L', vk8=cnsk)
        call jeveuo(chs//'.CNSD', 'L', jcnsd)
        call jeveuo(chs//'.CNSC', 'L', jcnsc)
        call jeveuo(chs//'.CNSV', 'L', jcnsv)
        call jeveuo(chs//'.CNSL', 'L', jcnsl)
!
        nbnoeu = zi(jcnsd-1 + 1)
        nbcmp = zi(jcnsd-1 + 2)
        nomgd = cnsk(2)
!
        if (nomgd(1:4) .eq. 'DEPL') then
! RECUPERATION DE L ORIENTATION
            if ((typres(1:9).eq.'HARM_GENE') .or. (typres(1:9) .eq.'TRAN_GENE')) then
!  SI RESULTAT DE TYPE *_GENE ON SUPPOSE QUE L'ORIENTATION
!  EST DEFINI PAR LIRE_RESU AU FORMAT DATASET 58
                licmp(1) = 'D1X'
                licmp(2) = 'D1Y'
                licmp(3) = 'D1Z'
                licmp(4) = 'D2X'
                licmp(5) = 'D2Y'
                licmp(6) = 'D2Z'
                licmp(7) = 'D3X'
                licmp(8) = 'D3Y'
                licmp(9) = 'D3Z'
                do 120 ino = 1, nbnoeu
                    do 130 icmp = 1, nbcmp
                        indice = (ino-1)*nbcmp+icmp
                        orien = .false.
                        if (zl(jcnsl-1 + indice)) then
                            do 140 ii = 1, 9
                                if (zk8(jcnsc-1 +icmp) .eq. licmp(ii)) then
                                    orien = .true.
                                endif
140                         continue
! ON NE TRAITE PAS NON PLUS LES DRX DRY DRZ
                            if (zk8(jcnsc-1 +icmp)(1:2) .eq. 'DR') then
                                orien = .true.
                            endif
                            if (.not. orien) then
                                nbmesu = nbmesu+1
                                zi(lnoeud-1 +nbmesu) = ino
                                zk16(lcham-1 +nbmesu) = nomcha
                                if (zk8(jcnsc-1 +icmp) .eq. 'D1') then
                                    zk8(lrange-1 +nbmesu) = 'D1'
                                    do 141 ii = 1, nbcmp
                                        if (zcmplx) then
                                            val = dble( zc(jcnsv-1 +(ino-1) *nbcmp+ii ) )
                                        else
                                            val = zr(jcnsv-1 +(ino-1)* nbcmp+ii )
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(1)) then
                                            zr(lori-1 +(nbmesu-1)*3+1) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(2)) then
                                            zr(lori-1 +(nbmesu-1)*3+2) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(3)) then
                                            zr(lori-1 +(nbmesu-1)*3+3) =&
                                        val
                                        endif
141                                 continue
                                endif
                                if (zk8(jcnsc-1 +icmp) .eq. 'D2') then
                                    zk8(lrange-1 +nbmesu) = 'D2'
                                    do 142 ii = 1, nbcmp
                                        if (zcmplx) then
                                            val = dble( zc(jcnsv-1 +(ino-1) *nbcmp+ii ) )
                                        else
                                            val = zr(jcnsv-1 +(ino-1)* nbcmp+ii )
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(4)) then
                                            zr(lori-1 +(nbmesu-1)*3+1) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(5)) then
                                            zr(lori-1 +(nbmesu-1)*3+2) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(6)) then
                                            zr(lori-1 +(nbmesu-1)*3+3) =&
                                        val
                                        endif
142                                 continue
                                endif
                                if (zk8(jcnsc-1 +icmp) .eq. 'D3') then
                                    zk8(lrange-1 +nbmesu) = 'D3'
                                    do 143 ii = 1, nbcmp
                                        if (zcmplx) then
                                            val = dble( zc(jcnsv-1 +(ino-1) *nbcmp+ii ) )
                                        else
                                            val = zr(jcnsv-1 +(ino-1)* nbcmp+ii )
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(7)) then
                                            zr(lori-1 +(nbmesu-1)*3+1) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(8)) then
                                            zr(lori-1 +(nbmesu-1)*3+2) =&
                                        val
                                        endif
                                        if (zk8(jcnsc-1 +ii) .eq. licmp(9)) then
                                            zr(lori-1 +(nbmesu-1)*3+3) =&
                                        val
                                        endif
143                                 continue
                                endif
!
                                if (zk8(jcnsc-1 +icmp)(1:2) .eq. 'DX') then
                                    zk8(lrange-1 +nbmesu) = 'DX'
                                    zr(lori-1 +(nbmesu-1)*3+1) =&
                                    1.0d0
                                    zr(lori-1 +(nbmesu-1)*3+2) =&
                                    0.0d0
                                    zr(lori-1 +(nbmesu-1)*3+3) =&
                                    0.0d0
                                endif
                                if (zk8(jcnsc-1 +icmp)(1:2) .eq. 'DY') then
                                    zk8(lrange-1 +nbmesu) = 'DY'
                                    zr(lori-1 +(nbmesu-1)*3+1) =&
                                    0.0d0
                                    zr(lori-1 +(nbmesu-1)*3+2) =&
                                    1.0d0
                                    zr(lori-1 +(nbmesu-1)*3+3) =&
                                    0.0d0
                                endif
                                if (zk8(jcnsc-1 +icmp)(1:2) .eq. 'DZ') then
                                    zk8(lrange-1 +nbmesu) = 'DZ'
                                    zr(lori-1 +(nbmesu-1)*3+1) =&
                                    0.0d0
                                    zr(lori-1 +(nbmesu-1)*3+2) =&
                                    0.0d0
                                    zr(lori-1 +(nbmesu-1)*3+3) =&
                                    1.0d0
                                endif
                            endif
                        endif
130                 continue
120             continue
            else if (typres(1:9).eq.'MODE_GENE') then
!  SI RESULTAT DE TYPE MODE_GENE ON SUPPOSE QUE L'ORIENTATION
!  EST DEFINI PAR LIRE_RESU AU FORMAT DATASET 55
!  BOUCLE SUR LES NUMEROS D ORDRE POUR RECUPERATION DES DDL MESURE
                do 200 imode = 1, nbord
                    call rsexch('F', nommes, nomcha, zi(lord-1+imode), chamno,&
                                iret)
                    call cnocns(chamno, 'V', chs)
                    call jeveuo(chs//'.CNSC', 'L', jcnsc)
                    call jeveuo(chs//'.CNSV', 'L', jcnsv)
                    call jeveuo(chs//'.CNSL', 'L', jcnsl)
                    do 221 ino = 1, nbnoeu
                        do 231 icmp = 1, nbcmp
                            indice = (ino-1)*nbcmp+icmp
                            if (zl(jcnsl-1 + indice)) then
                                dcapt = .false.
                                if (zcmplx) then
                                    val = dble(zc(jcnsv-1 +(ino-1)* nbcmp+icmp) )
                                else
                                    val = zr(jcnsv-1 +(ino-1)*nbcmp+ icmp)
                                endif
                                if (abs(val) .gt. (100*r8prem())) then
                                    if ((zk8(jcnsc-1 +icmp).eq.'DX') .or.&
                                        (zk8(jcnsc-1 +icmp).eq.'DY') .or.&
                                        (zk8(jcnsc-1 +icmp).eq.'DZ')) then
                                        inomes = ino
                                        chames = nomcha
                                        compms = zk8(jcnsc-1 +icmp)
                                        dcapt = .true.
                                    endif
                                    do 210 imesu = 1, nbmesu
                                        inocap = zi(lnoeud-1 +imesu)
                                        chacap = zk16(lcham-1 +imesu)
                                        comcap = zk8(lrange-1 +imesu)
                                        if ((comcap .eq. compms) .and. (chacap .eq. chames)&
                                            .and. (inocap .eq. inomes)) then
                                            dcapt = .false.
                                        endif
210                                 continue
                                    if (dcapt) then
                                        nbmesu = nbmesu+1
                                        zi(lnoeud-1 +nbmesu) = ino
                                        zk16(lcham-1 +nbmesu) =&
                                        nomcha
                                        zk8(lrange-1 +nbmesu) = zk8( jcnsc-1 +icmp)
                                    endif
                                endif
                            endif
231                     continue
221                 continue
200             continue
!
                do 121 imesu = 1, nbmesu
                    if (zk8(lrange-1 +imesu) .eq. 'DX') then
                        zr(lori-1 +(imesu-1)*3+1) = 1.0d0
                        zr(lori-1 +(imesu-1)*3+2) = 0.0d0
                        zr(lori-1 +(imesu-1)*3+3) = 0.0d0
                    endif
                    if (zk8(lrange-1 +imesu) .eq. 'DY') then
                        zr(lori-1 +(imesu-1)*3+1) = 0.0d0
                        zr(lori-1 +(imesu-1)*3+2) = 1.0d0
                        zr(lori-1 +(imesu-1)*3+3) = 0.0d0
                    endif
                    if (zk8(lrange-1 +imesu) .eq. 'DZ') then
                        zr(lori-1 +(imesu-1)*3+1) = 0.0d0
                        zr(lori-1 +(imesu-1)*3+2) = 0.0d0
                        zr(lori-1 +(imesu-1)*3+3) = 1.0d0
                    endif
121             continue
            endif
        endif
!
        if (nomgd(1:4) .eq. 'SIEF' .or. nomgd(1:4) .eq. 'EPSI') then
            do 220 ino = 1, nbnoeu
                do 230 icmp = 1, nbcmp
                    indice = (ino-1)*nbcmp+icmp
                    if (zl(jcnsl-1 + indice)) then
                        nbmesu = nbmesu+1
                        zi(lnoeud-1 +nbmesu) = ino
                        zk16(lcham-1 +nbmesu) = nomcha
                        zk8(lrange-1 +nbmesu) = zk8(jcnsc-1 +icmp)
                    endif
230             continue
220         continue
        endif
!
        call jeveuo(basemo//'           .ORDR', 'L', lord)
!
        ch1s='&&PJEFPR.CH1S'
        ch2s='&&PJEFPR.CH2S'
!
! FIN BOUCLE SUR LES NOMCHA
151 end do
!
    call getvid('MODELE_MESURE', 'MODELE', iocc=1, scal=modmes, nbret=ibid)
!
    call jeecra(vnoeud, 'LONUTI', nbmesu)
    call jeecra(vrange, 'LONUTI', nbmesu)
!
    zk16(lref-1 +1) = modmes
! PAS DE CALCUL DE MODIF STRUCTURALE POUR LES SDMIXTES
    if (nbcham .gt. 1) then
        call utmess('A', 'SOUSTRUC2_11')
    endif
    zk16(lref-1 +2) = nomcha
    zk16(lref-1 +3) = basemo
!
! DESTRUCTION DES VECTEURS DE TRAVAIL
!
    call detrsd('CHAM_NO_S', chs)
    call detrsd('CHAM_NO_S', ch1s)
    call detrsd('CHAM_NO_S', ch2s)
!
    call jedema()
!
end subroutine
