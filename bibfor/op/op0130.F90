subroutine op0130()
    implicit none
!-----------------------------------------------------------------------
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
!
!     OPERATEUR "POST_DYNA_MODA_T"
!
! ----------------------------------------------------------------------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pochoc.h"
#include "asterfort/pochpv.h"
#include "asterfort/porefd.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/titre.h"
    integer :: nbbloc, nbclas, i, nbind, nbno, jnomno
    real(kind=8) :: tdebut, tfin, offset, trepos
    character(len=8) :: base, modele, maillage
    character(len=8) :: trange, noeu, cmp, nomres
    character(len=16) :: nomcmd, concep, koptio, motcle(2), typmcl(2)
    character(len=24) :: nomno
    aster_logical :: loptio
    integer, pointer :: desc(:) => null()
!   ------------------------------------------------------------------
    data motcle  /'NOEUD','GROUP_NO'/
    data typmcl  /'NOEUD','GROUP_NO'/
!   ------------------------------------------------------------------

!
    call jemarq()
    nomres = ' '
    call getres(nomres, concep, nomcmd)
    call infmaj()
!
    call getvid(' ', 'RESU_GENE', scal=trange)
    call jeveuo(trange//'           .DESC', 'L', vi=desc)
!
    call getfac('CHOC', nbind)
    if (nbind .ne. 0) then
        do i = 1, nbind
            call getvis('CHOC', 'NB_BLOC', iocc=i, scal=nbbloc)
            call getvr8('CHOC', 'INST_INIT', iocc=i, scal=tdebut)
            call getvr8('CHOC', 'INST_FIN', iocc=i, scal=tfin)
            call getvr8('CHOC', 'SEUIL_FORCE', iocc=i, scal=offset)
            call getvr8('CHOC', 'DUREE_REPOS', iocc=i, scal=trepos)
            call getvtx('CHOC', 'OPTION', iocc=i, scal=koptio)
            call getvis('CHOC', 'NB_CLASSE', iocc=i, scal=nbclas)
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
        end do
    endif
!
    call getfac('RELA_EFFO_DEPL', nbind)
    nomno = '&&OP0130.MES_NOEUDS'
    if (nbind .ne. 0 .and. desc(4) .ne. 0) then
        do i = 1, nbind
                call dismoi('BASE_MODALE', trange, 'RESU_DYNA', repk=base, arret='F')
                call dismoi('NOM_MODELE', base, 'RESULTAT', repk=modele)
                call dismoi('NOM_MAILLA', base, 'RESULTAT', repk=maillage)
!
                call reliem(modele, maillage, 'NO_NOEUD', 'RELA_EFFO_DEPL', i,&
                            2, motcle, typmcl, nomno, nbno)
                call jeveuo(nomno, 'L', jnomno)
                if (nbno .ne. 1) then
                    call utmess('F', 'MODELISA5_67', sk='RELA_EFFO_DEPL', si=nbno)
                endif
                noeu = zk8(jnomno)
            call getvtx('RELA_EFFO_DEPL', 'NOM_CMP', iocc=i, scal=cmp)
!
            call porefd(trange, noeu, cmp, nomres)
        end do
    endif
    call jedetr(nomno)
!
    call titre()
!
    call jedema()
end subroutine
