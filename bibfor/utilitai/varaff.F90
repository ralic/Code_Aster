subroutine varaff(noma, gran, base, ceselz)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=1) :: base
    character(len=8) :: noma, gran
    character(len=*) :: ceselz
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
! BUT :
!  - TRAITER L'OPTION 'AFFE' DE LA COMMANDE CREA_CHAMP POUR VARI_R
!  - CREER LE CHAM_ELEM_S / ELEM  (CESELZ)
!-----------------------------------------------------------------------
    integer :: nocc, nbtou, n1
    integer :: iad, k, iocc, nbmail
    character(len=8) :: kbid, typmcl(2)
    character(len=6) :: knuva
    character(len=16) :: motclf, motcls(2)
    character(len=19) :: ceselm
    character(len=24) :: mesmai
    integer :: nvarmx, jcesd, jcesl,    jlvavx
    integer :: jmesma, kvari, n2, numa, nuva, nuvamx, nbmato
    parameter  (nvarmx=10000)
    logical(kind=1) :: ltou
    character(len=8), pointer :: lnova(:) => null()
    character(len=8), pointer :: lnovx(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
!   ------------------------------------------------------------------
    call jemarq()
!
    if (noma .eq. ' ') then
        call utmess('F', 'UTILITAI_10')
    endif
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmato)
!
    ASSERT(gran.eq.'VARI_R')
    AS_ALLOCATE(vk8=lnovx, size=nvarmx)
    call wkvect('&&VARAFF.LVAVX', 'V V R', nvarmx, jlvavx)
!
    motclf = 'AFFE'
    call getfac(motclf, nocc)
!
    mesmai = '&&VARAFF.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
!
!     0- CALCUL DU PLUS GRAND NUMERO DE VARI UTILISE (NUVAMX):
!     --------------------------------------------------------
    nuvamx=0
    do iocc = 1, nocc
        call getvtx(motclf, 'NOM_CMP', iocc=iocc, nbval=nvarmx, vect=lnovx,&
                    nbret=n1)
        ASSERT(n1.gt.0)
        do k = 1, n1
            ASSERT(lnovx(k)(1:1).eq.'V')
            read (lnovx(k)(2:8),'(I7)') nuva
            nuvamx=max(nuvamx,nuva)
        end do
    end do
    ASSERT(nuvamx.gt.0)
!
!
!     1- ALLOCATION DE CESELM
!     --------------------------------------------
    AS_ALLOCATE(vk8=lnova, size=nuvamx)
    do k = 1, nuvamx
        call codent(k, 'G', knuva)
        lnova(k)='V'//knuva
    end do
    ceselm = ceselz
!     -- REMARQUE : LES CMPS SERONT DANS L'ORDRE V1,V2,...
    call cescre(base, ceselm, 'ELEM', noma, 'VARI_R',&
                nuvamx, lnova, [0], [-1], [-nuvamx])
!
    call jeveuo(ceselm//'.CESD', 'L', jcesd)
    call jeveuo(ceselm//'.CESV', 'E', vr=cesv)
    call jeveuo(ceselm//'.CESL', 'E', jcesl)
!
!
!     2- BOUCLE SUR LES OCCURENCES DU MOT CLE AFFE
!     --------------------------------------------
    do iocc = 1, nocc
!
        call getvtx(motclf, 'NOEUD', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call utmess('F', 'UTILITAI_12')
        endif
        call getvtx(motclf, 'GROUP_NO', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call utmess('F', 'UTILITAI_13')
        endif
!
        call getvtx(motclf, 'NOM_CMP', iocc=iocc, nbval=nvarmx, vect=lnovx,&
                    nbret=n1)
        call getvr8(motclf, 'VALE', iocc=iocc, nbval=nvarmx, vect=zr(jlvavx),&
                    nbret=n2)
        ASSERT(n1.eq.n2)
!
!
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=kbid, nbret=nbtou)
        if (nbtou .eq. 1) then
            ltou=.true.
            nbmail=nbmato
        else
            ltou=.false.
            call reliem(' ', noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcls, typmcl, mesmai, nbmail)
            ASSERT(nbmail.gt.0)
            call jeveuo(mesmai, 'L', jmesma)
        endif
!
        do kvari = 1, n1
            read (lnovx(kvari)(2:8),'(I7)') nuva
            ASSERT(nuva.gt.0.and.nuva.le.nuvamx)
            do k = 1, nbmail
                if (ltou) then
                    numa=k
                else
                    numa=zi(jmesma-1+k)
                endif
                ASSERT(numa.gt.0.and.numa.le.nbmato)
!
                call cesexi('C', jcesd, jcesl, numa, 1,&
                            1, nuva, iad)
                ASSERT(iad.ne.0)
!               -- On peut vouloir surcharger :
                iad=abs(iad)
!
!               -- RECOPIE DE LA VALEUR:
                zl(jcesl-1+iad) = .true.
                cesv(iad) = zr(jlvavx-1+kvari)
            end do
        end do
!
        call jedetr(mesmai)
    end do
!
!
    AS_DEALLOCATE(vk8=lnovx)
    AS_DEALLOCATE(vk8=lnova)
    call jedema()
end subroutine
