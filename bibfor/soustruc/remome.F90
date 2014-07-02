subroutine remome(promes, modmes, nommac)
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
!     RECUPERATION DES MODES MESURES ET CREATION DE  .PJMMM
!
!     IN  : PROMES : NOM DU CONCEPT PROJ_MESU_MODAL ASSOCIE A LA MESURE
!     IN  : MODMES : NOM DU CONCEPT MODES PROPRES IDENTIFIES
!     IN  : NOMMAC : NOM DU CONCEPT MACR_ELEM_STAT CONCERNE
!
    implicit none
!     ------------------------------------------------------------------
!
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/scalai.h"
#include "asterfort/wkvect.h"
    character(len=8) :: promes, modmes, nommac
!
    aster_logical :: zcmplx
!
    character(len=1) :: typval
    character(len=8) :: k8bid, scal
    character(len=16) :: nomcha
    character(len=19) :: chamno, chs
    character(len=24) :: vnoeud, vrange, vmes, vorien, vref
!
    integer :: nbmesu, nbmtot, numord, lmesu, imes, lrange, lori, ii
    integer :: iret, icmp, ino, lnoeud, gd, nbcmp, ibid, lref
    integer :: jcnsv, jcnsl, jcnsk, tord(1)
!
    real(kind=8) :: rbid, vori(3), val, vect(3)
!
    complex(kind=8) :: cbid, valc, vectc(3)
    character(len=8), pointer :: cnsc(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: cnsd(:) => null()
    integer, pointer :: ordr(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    vmes = nommac//'.PROJM    .PJMMM'
!
! RECUPERATION ORDRE DE RANGEMENT MESURE SELON VRANGE ET VNOEUD
    vnoeud = promes//'.PROJM    .PJMNO'
    vrange = promes//'.PROJM    .PJMRG'
    vorien = promes//'.PROJM    .PJMOR'
!
    call jelira(vnoeud, 'LONUTI', nbmesu)
!
    vref = nommac//'.PROJM    .PJMRF'
    call jeveuo(vref, 'L', lref)
    nomcha=zk16(lref-1 +2)
!
    call jeveuo(vrange, 'L', lrange)
    call jeveuo(vnoeud, 'L', lnoeud)
    call jeveuo(vorien, 'L', lori)
!
! RECUPERATION ADRESSE DES NUMEROS D'ORDRE ET DU NOM SYMBOLIQUE
!
    call jeveuo(modmes//'           .ORDR', 'L', vi=ordr)
!
    chs = '&&MESURE.CHS'
!
! RECUPERATION DU NB DE VECTEURS PROPRES IDENTIFIES : NBMTOT
    call rsorac(modmes, 'LONUTI', 0, rbid, k8bid,&
                cbid, rbid, 'ABSOLU', tord, 1,&
                ibid)
    nbmtot=tord(1)            
!
!
! BOUCLE SUR LES NUMEROS ORDRE
!
    do 110 numord = 1, nbmtot
!        -> EXISTENCE DES CHAMPS DANS LA STRUCTURE DE DONNEES MESURE
        call rsexch('F', modmes, nomcha, ordr(numord), chamno,&
                    iret)
        if (numord .le. 1) then
            call jeveuo(chamno//'.DESC', 'L', vi=desc)
            gd = desc(1)
            scal = scalai(gd)
            typval = scal(1:1)
            if (typval .eq. 'C') then
                zcmplx = .true.
                call wkvect(vmes, 'G V C', nbmesu*nbmtot, lmesu)
            else
                zcmplx = .false.
                call wkvect(vmes, 'G V R', nbmesu*nbmtot, lmesu)
            endif
            call jeecra(vmes, 'LONUTI', nbmesu*nbmtot)
        endif
!
! TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
        call detrsd('CHAM_NO_S', chs)
        call cnocns(chamno, 'V', chs)
        call jeveuo(chs//'.CNSK', 'L', jcnsk)
        call jeveuo(chs//'.CNSD', 'L', vi=cnsd)
        call jeveuo(chs//'.CNSC', 'L', vk8=cnsc)
        call jeveuo(chs//'.CNSV', 'L', jcnsv)
        call jeveuo(chs//'.CNSL', 'L', jcnsl)
!
        nbcmp = cnsd(2)
!
        do 120 imes = 1, nbmesu
            ino = zi(lnoeud-1 +imes)
!
! DIRECTION DE MESURE (VECTEUR DIRECTEUR)
            do 21 ii = 1, 3
                vori(ii) = zr(lori-1 + (imes-1)*3 +ii)
 21         continue
!
! NORMALISATION DU VECTEUR DIRECTEUR
            val = 0.d0
            do 22 ii = 1, 3
                val = val + vori(ii)*vori(ii)
 22         continue
            val = sqrt(val)
            do 23 ii = 1, 3
                vori(ii) = vori(ii)/val
 23         continue
!
            if (zcmplx) then
                do 130 icmp = 1, nbcmp
                    if (cnsc(icmp) .eq. 'DX') vectc(1) = zc(jcnsv-1 +( ino-1 )*nbcmp+icmp )
                    if (cnsc(icmp) .eq. 'DY') vectc(2) = zc(jcnsv-1 +( ino-1 )*nbcmp+icmp )
                    if (cnsc(icmp) .eq. 'DZ') vectc(3) = zc(jcnsv-1 +( ino-1 )*nbcmp+icmp )
130             continue
!
                valc = dcmplx(0.d0,0.d0)
!
                do 300 ii = 1, 3
                    valc = valc + vectc(ii) * vori(ii)
300             continue
                zc(lmesu-1 +(numord-1)*nbmesu+imes) = valc
            else
                do 230 icmp = 1, nbcmp
                    if (cnsc(icmp) .eq. 'DX') vect(1) = zr(jcnsv-1 +(ino-1 )*nbcmp+icmp)
                    if (cnsc(icmp) .eq. 'DY') vect(2) = zr(jcnsv-1 +(ino-1 )*nbcmp+icmp)
                    if (cnsc(icmp) .eq. 'DZ') vect(3) = zr(jcnsv-1 +(ino-1 )*nbcmp+icmp)
230             continue
                val = 0.d0
                do 320 ii = 1, 3
                    val = val + vect(ii) * vori(ii)
320             continue
                zr(lmesu-1 +(numord-1)*nbmesu+imes) = val
            endif
120     continue
!
! FIN BOUCLE SUR NUMERO ORDRE
110 continue
!
    call detrsd('CHAM_NO_S', chs)
!
    call jedema()
!
end subroutine
