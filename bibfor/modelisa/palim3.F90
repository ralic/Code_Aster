subroutine palim3(mcfact, iocc, nomaz, nomvei, nomvek,&
                  nbmst)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
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
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
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
    integer :: n1, ier, im, numa, nume, ibid, lgp, lgm, ilist, klist, nbv1, i
    integer :: nbmc, nbma, jnoma
    parameter     ( nbmc = 3 )
    logical :: lnume, lgrpma
    character(len=8) :: noma, prfm, nommai, knume, k8b
    character(len=16) :: tymocl(nbmc), motcle(nbmc)
    character(len=24) :: nomama, nomjv, grpma
    character(len=24) :: valk(3)
    integer :: iarg
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
    call getvtx(mcfact, 'PREF_MAILLE', iocc, iarg, 1,&
                prfm, n1)
    lgp = lxlgut(prfm)
!
    lnume = .false.
    call getvis(mcfact, 'PREF_NUME', iocc, iarg, 0,&
                ibid, n1)
    if (n1 .ne. 0) then
        lnume = .true.
        call getvis(mcfact, 'PREF_NUME', iocc, iarg, 1,&
                    nume, n1)
    endif
!
    lgrpma = .false.
    call getvtx(mcfact, 'GROUP_MA', iocc, iarg, 0,&
                k8b, n1)
    if (n1 .ne. 0) then
        lgrpma= .true.
        call getvtx(mcfact, 'GROUP_MA', iocc, iarg, 1,&
                    grpma, n1)
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
    do 30 im = 0, nbma-1
        nommai = zk8(jnoma+im)
        call jenonu(jexnom(nomama, nommai), numa)
        if (numa .eq. 0) then
            ier = ier + 1
            valk(1) = nommai
            valk(2) = noma
            call u2mesk('E', 'MODELISA6_10', 2, valk)
        else
            if (lnume) then
                call codent(nume, 'G', knume)
                nume = nume + 1
                lgm = lxlgut(knume)
                if (lgm+lgp .gt. 8) call u2mess('F', 'MODELISA6_11')
                nommai = prfm(1:lgp)//knume
            else
                lgm = lxlgut(nommai)
                if (lgm+lgp .gt. 8) then
                    valk (1) = prfm(1:lgp)//nommai
                    valk (2) = nommai
                    valk (3) = prfm
                    call u2mesg('F+', 'MODELISA9_53', 3, valk, 0,&
                                0, 0, 0.d0)
                    if (lgrpma) then
                        valk(1) = grpma
                        call u2mesg('F+', 'MODELISA9_82', 1, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                    call u2mesg('F', 'MODELISA9_54', 0, ' ', 0,&
                                0, 0, 0.d0)
                endif
                nommai = prfm(1:lgp)//nommai
            endif
            do 32 i = 1, nbmst
                if (zk8(klist+i-1) .eq. nommai) then
                    if (mcfact.eq.'CREA_GROUP_MA') then
                        call u2mesk('F','MODELISA9_57',1,nommai)
                    else
                        goto 34
                    endif
                endif
32          continue
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
34          continue
        endif
30  end do
    call jedetr(nomjv)
!
    if (ier .ne. 0) ASSERT(.false.)
!
    call jedema()
end subroutine
