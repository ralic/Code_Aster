subroutine ss2mme(nomo, motfaz, vesstr, base)
!
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
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: nomo
    character(len=19) :: vesstr
    character(len=1) :: base
    character(len=*) :: motfaz
!
! ----------------------------------------------------------------------
!
! PREPARER LE VECT_ELEM DANS LE CAS DE SOUS-STRUCTURES
!
!
! ----------------------------------------------------------------------
!
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLEF FACTEUR DECRIVANT LES SOUS-STRUCTURES
! IN  BASE   : BASE DE CREATION DU VECT_ELEM
! I/O VESSTR : NOM DU VECT_ELEM
!                OUT : VESSTR EST (EVENTUELLEMENT) ENRICHI DE
!                L'OBJET .RELC
!
!
!
!
    character(len=8) :: noma, k8bid, nosma, nomcas, nomacr
    integer :: nbssa, nbsma, n1, n2, nboc
    integer :: iarefr,  ialsch, imas, iasssa, iamacr
    integer :: ier0, ioc, i, iret
    character(len=16) :: motfac, valk(2)
    character(len=8), pointer :: lmai(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    motfac = motfaz
    call getfac(motfac, nboc)
    if (nboc .eq. 0) goto 999
!
! --- INITIALISATIONS
!
    call dismoi('NOM_MAILLA', nomo, 'MODELE', repk=noma)
    call dismoi('NB_SS_ACTI', nomo, 'MODELE', repi=nbssa)
    call dismoi('NB_SM_MAILLA', nomo, 'MODELE', repi=nbsma)
!
    if (nbssa .eq. 0) then
        call utmess('F', 'SOUSTRUC_24')
    endif
!
    call jeveuo(nomo//'.MODELE    .SSSA', 'L', iasssa)
    call jeveuo(noma//'.NOMACR', 'L', iamacr)
!
    call jeveuo(vesstr(1:19)//'.RERR', 'E', iarefr)
    zk24(iarefr-1+3)='OUI_SOUS_STRUC'
!
    call jecrec(vesstr(1:19)//'.RELC', base//' V I', 'NO', 'CONTIG', 'CONSTANT',&
                nboc)
    call jeecra(vesstr(1:19)//'.RELC', 'LONMAX', nbsma)
!
    AS_ALLOCATE(vk8=lmai, size=nbsma)
!
! --- BOUCLE SUR LES CAS_DE_CHARGE
!
    ier0 = 0
    do ioc = 1, nboc
!
        call getvtx(motfac, 'CAS_CHARGE', iocc=ioc, scal=nomcas, nbret=n1)
        call jecroc(jexnom(vesstr(1:19)//'.RELC', nomcas))
        call jeveuo(jexnom(vesstr(1:19)//'.RELC', nomcas), 'E', ialsch)
!
!       -- CAS : TOUT: 'OUI'
!
        call getvtx(motfac, 'TOUT', iocc=ioc, scal=k8bid, nbret=n1)
        if (n1 .eq. 1) then
            do i = 1, nbsma
                if (zi(iasssa-1+i) .eq. 1) zi(ialsch-1+i)=1
            end do
            goto 5
        endif
!
!       -- CAS : MAILLE: L_MAIL
!
        call getvtx(motfac, 'SUPER_MAILLE', iocc=ioc, nbval=0, nbret=n2)
        if (-n2 .gt. nbsma) then
            call utmess('F', 'SOUSTRUC_25')
        else
            call getvtx(motfac, 'SUPER_MAILLE', iocc=ioc, nbval=nbsma, vect=lmai,&
                        nbret=n2)
        endif
        do i = 1, n2
            nosma = lmai(i)
            call jenonu(jexnom(noma//'.SUPMAIL', nosma), imas)
            if (imas .eq. 0) then
                valk(1) = nosma
                valk(2) = noma
                call utmess('F', 'SOUSTRUC_26', nk=2, valk=valk)
            else
                zi(ialsch-1+imas)=1
            endif
        end do
!
!       -- ON VERIFIE QUE LES VECTEURS ELEMENTAIRES SONT CALCULES
!
  5     continue
        do i = 1, nbsma
            if (zi(ialsch-1+i) .ne. 0) then
                call jenuno(jexnum(noma//'.SUPMAIL', i), nosma)
                if (zi(iasssa-1+i) .ne. 1) then
                    call utmess('F', 'SOUSTRUC_27', sk=nosma)
                endif
!
                nomacr = zk8(iamacr-1+i)
                call jeexin(jexnom(nomacr//'.LICA', nomcas), iret)
                if (iret .eq. 0) then
                    ier0 = 1
                    valk(1) = nosma
                    valk(2) = nomcas
                    call utmess('E', 'SOUSTRUC_28', nk=2, valk=valk)
                endif
            endif
        end do
!
    end do
!
    if (ier0 .eq. 1) then
        call utmess('F', 'SOUSTRUC_29')
    endif
!
    AS_DEALLOCATE(vk8=lmai)
!
999 continue
    call jedema()
end subroutine
