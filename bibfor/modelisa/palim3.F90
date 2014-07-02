subroutine palim3(mcfact, iocc, nomaz, nomvei, nomvek,&
                  nbmst)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/juveca.h"
#include "asterfort/lxlgut.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
!
    integer :: iocc, nbmst
    character(len=*) :: mcfact, nomaz, nomvei, nomvek
!-----------------------------------------------------------------------
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
! IN   NOMAZ  : NOM DU MAILLAGE
! INOUT  NBMST  : NOMBRE DE MAILLES DUPLIQUEES
!     ------------------------------------------------------------------
!
    integer :: n1, ier, im, numa, nume, lgp, lgm, ilist, klist, nbv1, i
    integer :: nbmc, nbma, jnoma
    parameter     ( nbmc = 3 )
    aster_logical :: lnume, lgrpma
    character(len=8) :: noma, prfm, nommai, knume
    character(len=16) :: tymocl(nbmc), motcle(nbmc)
    character(len=24) :: nomama, nomjv, grpma
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
!
    call jemarq()
!
    noma = nomaz
    nomama = noma//'.NOMMAI'
!
    call jeveuo(nomvei, 'E', ilist)
    call jeveuo(nomvek, 'E', klist)
    call jelira(nomvek, 'LONMAX', nbv1)
    ier = 0
!
    call getvtx(mcfact, 'PREF_MAILLE', iocc=iocc, scal=prfm, nbret=n1)
    lgp = lxlgut(prfm)
!
    lnume = .false.
    call getvis(mcfact, 'PREF_NUME', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        lnume = .true.
        call getvis(mcfact, 'PREF_NUME', iocc=iocc, scal=nume, nbret=n1)
    endif
!
    lgrpma = .false.
    call getvtx(mcfact, 'GROUP_MA', iocc=iocc, nbval=0, nbret=n1)
    if (n1 .ne. 0) then
        lgrpma= .true.
        call getvtx(mcfact, 'GROUP_MA', iocc=iocc, scal=grpma, nbret=n1)
    endif
!
    motcle(1) = 'TOUT'
    tymocl(1) = 'TOUT'
    motcle(2) = 'GROUP_MA'
    tymocl(2) = 'GROUP_MA'
    motcle(3) = 'MAILLE'
    tymocl(3) = 'MAILLE'
!
    nomjv = '&&OP0167.LISTE_MA'
    call reliem(' ', noma, 'NO_MAILLE', mcfact, iocc,&
                nbmc, motcle, tymocl, nomjv, nbma)
    call jeveuo(nomjv, 'L', jnoma)
!
    do im = 0, nbma-1
        nommai = zk8(jnoma+im)
        call jenonu(jexnom(nomama, nommai), numa)
        if (numa .eq. 0) then
            ier = ier + 1
            valk(1) = nommai
            valk(2) = noma
            call utmess('E', 'MODELISA6_10', nk=2, valk=valk)
        else
            if (lnume) then
                call codent(nume, 'G', knume)
                nume = nume + 1
                lgm = lxlgut(knume)
                if (lgm+lgp .gt. 8) then
                    call utmess('F', 'MODELISA6_11')
                endif
                nommai = prfm(1:lgp)//knume
            else
                lgm = lxlgut(nommai)
                if (lgm+lgp .gt. 8) then
                    valk (1) = prfm(1:lgp)//nommai
                    valk (2) = nommai
                    valk (3) = prfm
                    call utmess('F+', 'MODELISA9_53', nk=3, valk=valk)
                    if (lgrpma) then
                        valk(1) = grpma
                        call utmess('F+', 'MODELISA9_82', sk=valk(1))
                    endif
                    call utmess('F', 'MODELISA9_54')
                endif
                nommai = prfm(1:lgp)//nommai
            endif
            do 32 i = 1, nbmst
                if (zk8(klist+i-1) .eq. nommai) then
                    if (mcfact .eq. 'CREA_GROUP_MA') then
                        call utmess('F', 'MODELISA9_57', sk=nommai)
                    else
                        goto 34
                    endif
                endif
 32         continue
            nbmst = nbmst + 1
            if (nbmst .gt. nbv1) then
                call juveca(nomvek, 2*nbmst)
                call juveca(nomvei, 2*nbmst)
                call jeveuo(nomvei, 'E', ilist)
                call jeveuo(nomvek, 'E', klist)
                call jelira(nomvek, 'LONMAX', nbv1)
            endif
            zk8(klist+nbmst-1) = nommai
            zi(ilist+nbmst-1) = numa
 34         continue
        endif
    end do
    call jedetr(nomjv)
!
    if (ier .ne. 0) ASSERT(.false.)
!
    call jedema()
end subroutine
