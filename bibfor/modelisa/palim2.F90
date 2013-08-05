subroutine palim2(mcfact, iocc, nomaz, nomvei, nomvek,&
                  iadr)
    implicit none
#include "jeveux.h"
!
#include "asterc/getexm.h"
#include "asterc/getvis.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/i2rdl2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/lxlgut.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: iocc, iadr
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
! OUT  NOML   : NOM DE L'OBJET JEVEUX CREE SUR LA VOLATILE
! OUT  NBMAIL : NOMBRE DE MAILLES RECUPEREES
!     ------------------------------------------------------------------
!
    integer :: n1, ier, kotyp, im, numa, ityp, nume, ibid, iatyma, lgp, lgm
    integer :: ilist, klist, nbmc, nbma, jnoma
    parameter     ( nbmc = 3 )
    logical :: lnume, lmail
    integer :: lopt, lnom
    character(len=8) :: k8b, noma, option, oldtyp, prfm, nommai, knume
    character(len=16) :: tymocl(nbmc), motcle(nbmc)
    character(len=24) :: nomama, nomaty, nomjv
    character(len=24) :: valk(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    noma = nomaz
    nomama = noma//'.NOMMAI'
    nomaty = noma//'.TYPMAIL'
!
    lopt = getexm ( mcfact, 'OPTION' )
    lnom = getexm ( mcfact, 'PREF_MAILLE' )
    call jeveuo(nomvei, 'E', ilist)
    call jeveuo(nomvek, 'E', klist)
    ier = 0
!
    if (lopt .eq. 1) then
        call getvtx(mcfact, 'OPTION', iocc, iarg, 1,&
                    option, n1)
        if (option .eq. 'TRIA6_7') then
            oldtyp = 'TRIA6'
        else if (option .eq. 'QUAD8_9') then
            oldtyp = 'QUAD8'
        else if (option .eq. 'SEG3_4') then
            oldtyp = 'SEG3'
        endif
        call jenonu(jexnom('&CATA.TM.NOMTM', oldtyp), kotyp)
    endif
!
    lmail = .false.
    lnume = .false.
    if (lnom .eq. 1) then
        call getvtx(mcfact, 'PREF_MAILLE', iocc, iarg, 0,&
                    k8b, n1)
        if (n1 .ne. 0) then
            call getvtx(mcfact, 'PREF_MAILLE', iocc, iarg, 1,&
                        prfm, n1)
            lgp = lxlgut(prfm)
            lmail = .true.
        else
            lgp = 0
            prfm = ' '
        endif
        call getvis(mcfact, 'PREF_NUME', iocc, iarg, 0,&
                    ibid, n1)
        if (n1 .ne. 0) then
            lnume = .true.
            call getvis(mcfact, 'PREF_NUME', iocc, iarg, 1,&
                        nume, n1)
        endif
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
            if (lmail) then
                if (lnume) then
                    call codent(nume, 'G', knume)
                    nume = nume + 1
                    lgm = lxlgut(knume)
                    if (lgm+lgp .gt. 8) call u2mess('F', 'MODELISA6_11')
                    nommai = prfm(1:lgp)//knume
                else
                    lgm = lxlgut(nommai)
                    if (lgm+lgp .gt. 8) call u2mess('F', 'MODELISA6_12')
                    nommai = prfm(1:lgp)//nommai
                endif
            endif
            if (lopt .eq. 1) then
                call jeveuo(nomaty, 'L', iatyma)
                ityp=iatyma-1+numa
                if (zi(ityp) .eq. kotyp) then
                    call i2rdl2(numa, zi(ilist), nommai, zk8(klist), iadr)
                endif
            else
                call i2rdl2(numa, zi(ilist), nommai, zk8(klist), iadr)
            endif
        endif
30  end do
    call jedetr(nomjv)
!
    if (ier .ne. 0) ASSERT(.false.)
!
    call jedema()
end subroutine
