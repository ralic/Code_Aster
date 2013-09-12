subroutine dyarch(nbpas, lisins, lisarc, nbarch, ich,&
                  nbexcl, type)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dyarc1.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nbpas, nbarch, ich, nbexcl
    character(len=*) :: lisins, lisarc, type(*)
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
! ----------------------------------------------------------------------
!     SAISIE DU MOT CLE FACTEUR "ARCHIVAGE"
!
! IN  : NBPAS  : NOMBRE DE PAS DE CALCUL
! IN  : LISINS : NOM DE LA LISTE DES INSTANTS DE CALCUL
! IN  : LISARC : LISTE D'ARCHIVAGE DES PAS DE CALCUL
! OUT : NBARCH : NOMBRE DE PAS A ARCHIVER + CI
! IN  : ICH    : PRISE EN COMPTE DU MOT CLE "CHAM_EXCLU"
! OUT : NBEXCL : NOMBRE DE NOMS DES CHAMPS EXCLUS
! OUT : TYPE   : NOMS DES CHAMPS EXCLUS
! ----------------------------------------------------------------------
    integer :: jarch, nbocc, n1, jnum, lnum, k, ipach, jinsc
    real(kind=8) :: epsi
    character(len=8) :: k8b, rela
    character(len=19) :: numarc
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    nbexcl = 0
    call wkvect(lisarc, 'V V I', nbpas, jarch)
!
    call getfac('ARCHIVAGE', nbocc)
!
    if (nbocc .ne. 0) then
!
!
        call getvid('ARCHIVAGE', 'LIST_INST', iocc=1, scal=numarc, nbret=n1)
        if (n1 .ne. 0) then
            call jeveuo(lisins, 'L', jinsc)
            call getvr8('ARCHIVAGE', 'PRECISION', iocc=1, scal=epsi, nbret=n1)
            call getvtx('ARCHIVAGE', 'CRITERE', iocc=1, scal=rela, nbret=n1)
            call jeveuo(numarc//'.VALE', 'L', jnum)
            call jelira(numarc//'.VALE', 'LONUTI', lnum)
            call dyarc1(zr(jinsc), nbpas, zr(jnum), lnum, zi(jarch),&
                        epsi, rela)
            zi(jarch+nbpas-1) = 1
            goto 100
        endif
!
        call getvr8('ARCHIVAGE', 'INST', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call jeveuo(lisins, 'L', jinsc)
            lnum = -n1
            call getvr8('ARCHIVAGE', 'PRECISION', iocc=1, scal=epsi, nbret=n1)
            call getvtx('ARCHIVAGE', 'CRITERE', iocc=1, scal=rela, nbret=n1)
            call wkvect('&&DYARCH.VALE_INST', 'V V R', lnum, jnum)
            call getvr8('ARCHIVAGE', 'INST', iocc=1, nbval=lnum, vect=zr(jnum),&
                        nbret=n1)
            call dyarc1(zr(jinsc), nbpas, zr(jnum), lnum, zi(jarch),&
                        epsi, rela)
            call jedetr('&&DYARCH.VALE_INST')
            zi(jarch+nbpas-1) = 1
            goto 100
        endif
!
        call getvis('ARCHIVAGE', 'PAS_ARCH', iocc=1, scal=ipach, nbret=n1)
        if (n1 .eq. 0) ipach = 1
!
        do 10 k = ipach, nbpas, ipach
            zi(jarch+k-1) = 1
10      continue
        zi(jarch+nbpas-1) = 1
!
100      continue
!
        if (ich .ne. 0) then
!
!        --- LES SORTIES ---
            call getvtx('ARCHIVAGE', 'CHAM_EXCLU', iocc=1, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                nbexcl = -n1
                call getvtx('ARCHIVAGE', 'CHAM_EXCLU', iocc=1, nbval=nbexcl, vect=type,&
                            nbret=n1)
            endif
        endif
!
    else
!
        do 30 k = 1, nbpas
            zi(jarch+k-1) = 1
30      continue
    endif
!
!     --- 1 : CONDITIONS INITIALES ---
    nbarch = 1
    do 40 k = 1, nbpas
        nbarch = nbarch + zi(jarch+k-1)
40  end do
!
    call jedema()
end subroutine
