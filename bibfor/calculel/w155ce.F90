subroutine w155ce(nomres, resu, nbordr, liordr)
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!     COMMANDE :  POST_CHAMP / COQU_EXCENT
! ----------------------------------------------------------------------
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/exlima.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
    character(len=8) :: nomres, resu
    integer :: nbordr, liordr(nbordr)
!
    integer :: ifm, niv
    integer :: iret, i, nuordr, ibid, nocc, iocc
    character(len=8) :: modele, carele, mate, mplan
    character(len=8) :: modeav, lpain(2), lpaout(1)
    character(len=4) :: tsca
    character(len=16) :: motfac, nomsym
    character(len=19) :: chin, chextr, ligrel, resu19, lchin(2), lchout(1)
    character(len=19) :: excit
    integer :: vali(2), iexi
    aster_logical :: lreel, lnoeu, ldetli, lvide
!     ------------------------------------------------------------------
!
    call jemarq()
!
!
    call infmaj()
    call infniv(ifm, niv)
    resu19=resu
!
!
!
!     -- 1. : Y-A-T-IL QUELQUE CHOSE A FAIRE ?
!     ----------------------------------------
    call getfac('COQU_EXCENT', nocc)
    if (nocc .eq. 0) goto 30
    ASSERT(nocc.lt.10)
!
!
    modeav=' '
    ldetli=.false.
    lvide=.true.
    do 20 iocc = 1, nocc
!
!     -- 2.  : NOMSYM, MPLAN :
!     --------------------------------------------------
        motfac='COQU_EXCENT'
        call getvtx(motfac, 'NOM_CHAM', iocc=iocc, scal=nomsym, nbret=ibid)
        ASSERT(nomsym.eq.'EFGE_ELNO'.or.nomsym.eq.'EFGE_ELGA')
        call getvtx(motfac, 'MODI_PLAN', iocc=iocc, scal=mplan, nbret=ibid)
        ASSERT(mplan.eq.'OUI')
        lnoeu=nomsym.eq.'EFGE_ELNO'
!
!
!     -- 3. : BOUCLE SUR LES CHAMPS
!     --------------------------------------------------
        do 10 i = 1, nbordr
            nuordr=liordr(i)
            call rsexch(' ', resu19, nomsym, nuordr, chin,&
                        iret)
            if (iret .eq. 0) then
!
!         -- 3.1 : MODELE, CARELE, LIGREL :
                call rslesd(resu, nuordr, modele, mate, carele,&
                            excit, ibid)
                if (modele .ne. modeav) then
                    if (ldetli) call detrsd('LIGREL', ligrel)
                    call exlima(' ', 1, 'G', modele, ligrel)
                    modeav=modele
!             -- SI ON CREE UN LIGREL, IL FAUT VERIFIER QUE L'ON S'EN
!                SERT VRAIMENT. SINON, IL FAUT LE DETRUIRE:
                    ldetli=.false.
                    if (ligrel(1:8) .ne. modele) ldetli=.true.
                endif
!
                call rsexch(' ', nomres, nomsym, nuordr, chextr,&
                            iret)
                ASSERT(iret.eq.100)
!
                call jelira(chin//'.CELV', 'TYPE', cval=tsca)
                if (tsca .eq. 'R') then
                    lreel=.true.
                else if (tsca.eq.'C') then
                    lreel=.false.
                else
                    ASSERT(.false.)
                endif
!
                if (lnoeu) then
                    if (lreel) then
                        lpain(1)='PEFFONR'
                        lpaout(1)='PEFFOENR'
                    else
                        lpain(1)='PEFFONC'
                        lpaout(1)='PEFFOENC'
                    endif
                else
                    if (lreel) then
                        lpain(1)='PEFFOGR'
                        lpaout(1)='PEFFOEGR'
                    else
                        lpain(1)='PEFFOGC'
                        lpaout(1)='PEFFOEGC'
                    endif
                endif
!
                lchin(1)=chin
                lchout(1)=chextr
!
                lpain(2)='PCACOQU'
                lchin(2)=carele//'.CARCOQUE'
!
                call calcul('C', 'EFGE_EXCENT', ligrel, 2, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
!
                call jeexin(lchout(1)//'.CELV', iexi)
                if (iexi .eq. 0) then
                    vali(1)=iocc
                    vali(2)=nuordr
                    call utmess('A', 'CALCULEL2_19', ni=2, vali=vali)
                else
                    ldetli=.false.
                    lvide=.false.
                    call rsnoch(nomres, nomsym, nuordr)
                endif
            endif
 10     continue
 20 end do
!
    if (ldetli) call detrsd('LIGREL', ligrel)
    if (lvide) then
        call utmess('F', 'CALCULEL2_20')
    endif
!
!
 30 continue
    call jedema()
end subroutine
