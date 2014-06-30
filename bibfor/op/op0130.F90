subroutine op0130()
    implicit none
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
!     OPERATEUR "POST_DYNA_MODA_T"
!
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pochoc.h"
#include "asterfort/pochpv.h"
#include "asterfort/porefd.h"
#include "asterfort/titre.h"
    integer :: nbbloc, nbclas, n1, n2, i,  nbind
    real(kind=8) :: tdebut, tfin, offset, trepos
    character(len=8) :: trange, noeu, cmp, nomres
    character(len=16) :: nomcmd, concep, koptio
    logical(kind=1) :: loptio
    integer, pointer :: desc(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    nomres = ' '
    call getres(nomres, concep, nomcmd)
    call infmaj()
!
    call getvid(' ', 'RESU_GENE', scal=trange, nbret=n1)
    call jeveuo(trange//'           .DESC', 'L', vi=desc)
!
    call getfac('CHOC', nbind)
    if (nbind .ne. 0) then
        do 10 i = 1, nbind
            call getvis('CHOC', 'NB_BLOC', iocc=i, scal=nbbloc, nbret=n1)
            call getvr8('CHOC', 'INST_INIT', iocc=i, scal=tdebut, nbret=n1)
            call getvr8('CHOC', 'INST_FIN', iocc=i, scal=tfin, nbret=n1)
            call getvr8('CHOC', 'SEUIL_FORCE', iocc=i, scal=offset, nbret=n1)
            call getvr8('CHOC', 'DUREE_REPOS', iocc=i, scal=trepos, nbret=n1)
            call getvtx('CHOC', 'OPTION', iocc=i, scal=koptio, nbret=n1)
            call getvis('CHOC', 'NB_CLASSE', iocc=i, scal=nbclas, nbret=n1)
            if (koptio(1:6) .eq. 'USURE') then
                loptio = .true.
            else
                loptio = .false.
            endif
            if (desc(1) .eq. 2) then
                call pochoc(trange, nbbloc, tdebut, tfin, offset,&
                            trepos, nbclas, nomres, loptio)
            else if (desc(1) .eq. 3) then
                call pochpv(trange, nbbloc, tdebut, tfin, offset,&
                            trepos, nbclas, nomres, loptio)
            endif
10      continue
    endif
!
    call getfac('RELA_EFFO_DEPL', nbind)
    if (nbind .ne. 0 .and. desc(4) .ne. 0) then
        do 20 i = 1, nbind
            call getvtx('RELA_EFFO_DEPL', 'NOEUD', iocc=i, scal=noeu, nbret=n2)
            call getvtx('RELA_EFFO_DEPL', 'NOM_CMP', iocc=i, scal=cmp, nbret=n2)
!
            call porefd(trange, noeu, cmp, nomres)
20      continue
    endif
!
    call titre()
!
    call jedema()
end subroutine
