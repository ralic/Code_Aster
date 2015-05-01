subroutine rvgacc(iocc, typac, nival, nrval, nbval)
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
    implicit none
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsorac.h"
#include "asterfort/wkvect.h"
    integer :: iocc, nbval
    character(len=2) :: typac
    character(len=24) :: nival, nrval
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     SAISIE DES VALEURS DES ENTIERS OU REELS PERMETTANT L' ACCES
!     AUX CHAMP EFFECTIFS D' UN CHAMP SYMBOLIQUE D' UNE SD RESULTAT
!     ------------------------------------------------------------------
! IN  IOCC   : I : NUMERO DE L' OCCURENCE TRAITEE
! IN  NIVAL  : K : NOM DE L' OJB DE SAISIE DES ENTIERS
! IN  NRVAL  : K : NOM DE L' OJB DE SAISIE DES REELS
! OUT NBVAL  : I : NOMBRE DE VALEURS SAISIES
! OUT TYPAC  : I : CODE DU TYPE D' ACCES (C.F. RVGCHF)
!     ------------------------------------------------------------------
!     LES OJB SONT DU GENRE 'V V SCAL' 'LONMAX' = NBVAL
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: aival, arval, alist, i, ibid, n1, nbcm, nbif, nbii, nbim, nbio
    integer :: nbis, nbnc, nbnf, nbni, nbnm, nbno, nbr8, nbto, nbtrou
    real(kind=8) :: prec, r8b
    complex(kind=8) :: c16b
    character(len=8) :: resu, crit
    character(len=16) :: nomcas
    character(len=24) :: nlist
!
!================== CORPS DE LA ROUTINE ===============================
!
    call jemarq()
    call getvid('ACTION', 'RESULTAT', iocc=iocc, scal=resu, nbret=n1)
    call getvr8('ACTION', 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    call getvtx('ACTION', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
    call getvtx('ACTION', 'TOUT_ORDRE', iocc=iocc, nbval=0, nbret=nbto)
    call getvis('ACTION', 'NUME_ORDRE', iocc=iocc, nbval=0, nbret=nbno)
    call getvid('ACTION', 'LIST_ORDRE', iocc=iocc, nbval=0, nbret=nbio)
    call getvis('ACTION', 'NUME_MODE', iocc=iocc, nbval=0, nbret=nbnm)
    call getvtx('ACTION', 'NOM_CAS', iocc=iocc, nbval=0, nbret=nbnc)
    call getvtx('ACTION', 'NOEUD_CMP', iocc=iocc, nbval=0, nbret=nbcm)
    call getvid('ACTION', 'LIST_MODE', iocc=iocc, nbval=0, nbret=nbim)
    call getvr8('ACTION', 'INST', iocc=iocc, nbval=0, nbret=nbni)
    call getvid('ACTION', 'LIST_INST', iocc=iocc, nbval=0, nbret=nbii)
    call getvr8('ACTION', 'FREQ', iocc=iocc, nbval=0, nbret=nbnf)
    call getvid('ACTION', 'LIST_FREQ', iocc=iocc, nbval=0, nbret=nbif)
    nbto = -nbto
    nbno = -nbno
    nbio = -nbio
    nbnm = -nbnm
    nbnc = -nbnc
    nbcm = -nbcm
    nbim = -nbim
    nbni = -nbni
    nbii = -nbii
    nbnf = -nbnf
    nbif = -nbif
    nbis = nbno + nbio + nbnm + nbnc + nbcm + nbim
    nbr8 = nbni + nbii + nbnf + nbif
    if (nbis .ne. 0) then
        call wkvect(nrval, 'V V R', 1, arval)
        zr(arval) = 0.0d0
        if (nbno .ne. 0) then
            typac = 'NO'
            nbval = nbno
            call wkvect(nival, 'V V I', nbno, aival)
            call getvis('ACTION', 'NUME_ORDRE', iocc=iocc, nbval=nbno, vect=zi( aival),&
                        nbret=n1)
        else if (nbnm .ne. 0) then
            typac = 'NM'
            nbval = nbnm
            call wkvect(nival, 'V V I', nbnm, aival)
            call getvis('ACTION', 'NUME_MODE', iocc=iocc, nbval=nbnm, vect=zi(aival),&
                        nbret=n1)
        else if (nbnc .ne. 0) then
            typac = 'NO'
            nbval = nbnc
            call wkvect(nival, 'V V I', nbnc, aival)
            call getvtx('ACTION', 'NOM_CAS', iocc=iocc, nbval=nbnc, vect=nomcas,&
                        nbret=n1)
            call rsorac(resu, 'NOM_CAS', ibid, r8b, nomcas,&
                        c16b, prec, crit, zi(aival), 1,&
                        nbtrou)
        else if (nbcm .ne. 0) then
            typac = 'NO'
            nbval = nbcm
            call wkvect(nival, 'V V I', nbnc, aival)
            call getvtx('ACTION', 'NOEUD_CMP', iocc=iocc, nbval=nbcm, vect=nomcas,&
                        nbret=n1)
            call rsorac(resu, 'NOEUD_CMP', ibid, r8b, nomcas,&
                        c16b, prec, crit, zi(aival), 1,&
                        nbtrou)
        else
            if (nbio .ne. 0) then
                typac = 'NO'
                call getvid('ACTION', 'LIST_ORDRE', iocc=iocc, scal=nlist, nbret=n1)
            else
                typac = 'NM'
                call getvid('ACTION', 'LIST_MODE', iocc=iocc, scal=nlist, nbret=n1)
            endif
            nlist(9:24) = '           .VALE'
            call jelira(nlist, 'LONMAX', nbval)
            call jeveuo(nlist, 'L', alist)
            call wkvect(nival, 'V V I', nbval, aival)
            do 10, i = 1, nbval, 1
            zi(aival + i-1) = zi(alist + i-1)
10          continue
        endif
    else if (nbr8 .ne. 0) then
        call wkvect(nival, 'V V I', 1, aival)
        zi(aival) = 0
        if (nbni .ne. 0) then
            typac = 'NI'
            nbval = nbni
            call wkvect(nrval, 'V V R', nbni, arval)
            call getvr8('ACTION', 'INST', iocc=iocc, nbval=nbni, vect=zr(arval),&
                        nbret=n1)
        else if (nbnf .ne. 0) then
            typac = 'NF'
            nbval = nbnf
            call wkvect(nrval, 'V V R', nbnf, arval)
            call getvr8('ACTION', 'FREQ', iocc=iocc, nbval=nbnf, vect=zr(arval),&
                        nbret=n1)
        else
            if (nbii .ne. 0) then
                typac = 'NI'
                call getvid('ACTION', 'LIST_INST', iocc=iocc, scal=nlist, nbret=n1)
            else
                typac = 'NF'
                call getvid('ACTION', 'LIST_FREQ', iocc=iocc, scal=nlist, nbret=n1)
            endif
            nlist(9:24) = '           .VALE'
            call jelira(nlist, 'LONMAX', nbval)
            call jeveuo(nlist, 'L', alist)
            call wkvect(nrval, 'V V R', nbval, arval)
            do 20, i = 1, nbval, 1
            zr(arval + i-1) = zr(alist + i-1)
20          continue
        endif
    else
        call wkvect(nrval, 'V V R', 1, arval)
        call wkvect(nival, 'V V I', 1, aival)
        zi(aival) = 0
        zr(arval) = 0.0d0
        nbval = 0
        typac = 'TO'
    endif
    call jedema()
end subroutine
