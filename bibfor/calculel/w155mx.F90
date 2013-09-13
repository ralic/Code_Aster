subroutine w155mx(nomres, resu, nbordr, liordr)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     COMMANDE :  POST_CHAMP / MIN_MAX_SP
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/exlima.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsnoch.h"
#include "asterfort/utmess.h"
#include "asterfort/w155m2.h"
    character(len=8) :: nomres, resu
    integer :: nbordr, liordr(nbordr)
!
    integer :: ifm, niv, ico
    integer :: iret, i, nuordr, ibid, nocc, iocc, nchout
    character(len=8) :: modele, carele, mate
    character(len=8) :: modeav, nocmp, tymaxi
    character(len=4) :: tych
    character(len=16) :: motfac, nomsym, nomsy2
    character(len=19) :: chin, chextr, excit, ligrel, resu19
    character(len=24) :: nompar
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
    call getfac('MIN_MAX_SP', nocc)
    if (nocc .eq. 0) goto 30
    ASSERT(nocc.lt.10)
!
    do 20,iocc=1,nocc
!
!     -- 2.  : NOMSYM, NOCMP, TYMAXI, TYCH :
!     --------------------------------------------------
    motfac='MIN_MAX_SP'
    call getvtx(motfac, 'NOM_CHAM', iocc=iocc, scal=nomsym, nbret=ibid)
    call getvtx(motfac, 'NOM_CMP', iocc=iocc, scal=nocmp, nbret=ibid)
    call getvtx(motfac, 'TYPE_MAXI', iocc=iocc, scal=tymaxi, nbret=ibid)
    tych=nomsym(6:9)
    ASSERT(tych.eq.'ELNO' .or. tych.eq.'ELGA')
!
!
!     -- 3. : BOUCLE SUR LES NUME_ORDRE
!     --------------------------------------------------
    modeav=' '
    ico=0
    do 10,i=1,nbordr
    nuordr=liordr(i)
    call rsexch(' ', resu19, nomsym, nuordr, chin,&
                iret)
    if (iret .eq. 0) then
!
!         -- 3.1 : MODELE, CARELE, LIGREL :
        call rslesd(resu, nuordr, modele, mate, carele,&
                    excit, ibid)
        if (modele .ne. modeav) then
            call exlima(' ', 1, 'G', modele, ligrel)
            modeav=modele
        endif
!
        nomsy2='UTXX_'//tych
        call getvis(motfac, 'NUME_CHAM_RESU', iocc=iocc, scal=nchout, nbret=ibid)
        ASSERT(nchout.ge.1 .and. nchout.le.20)
        call codent(nchout, 'D0', nomsy2(3:4))
        if (tych .eq. 'ELGA') then
            nompar='PGAMIMA'
        else if (tych.eq.'ELNO') then
            nompar='PNOMIMA'
        else
            ASSERT(.false.)
        endif
!
        call rsexch(' ', nomres, nomsy2, nuordr, chextr,&
                    iret)
        ASSERT(iret.eq.100)
        call alchml(ligrel, 'MINMAX_SP', nompar, 'G', chextr,&
                    iret, ' ')
        ASSERT(iret.eq.0)
        call w155m2(chin, carele, ligrel, chextr, nomsym,&
                    nocmp, tymaxi)
        ico=ico+1
        call rsnoch(nomres, nomsy2, nuordr)
    endif
10  continue
    if (ico .eq. 0) then
        call utmess('F', 'CALCULEL2_62', sk=nomsym)
    endif
    20 end do
!
!
30  continue
    call jedema()
end subroutine
