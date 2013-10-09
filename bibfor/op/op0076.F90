subroutine op0076()
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
    implicit none
!     RECUPERE LES CHAMPS GENERALISES (DEPL, VITE, ACCE) D'UN CONCEPT
!     TRAN_GENE.
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/extrac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zxtrac.h"
!
    real(kind=8) :: temps, prec, freq
    character(len=8) :: nomres, trange, basemo, nomcha, interp, crit
    character(len=16) :: concep, nomcmd
    character(len=24) :: nddlge
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadesc, idcham, iddesc, idinst, idrefe, idvecg
    integer :: ierd, n1, nbinst, nbmode, nt, nf, ni
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomres, concep, nomcmd)
!
!     --- RECUPERATION DES ARGUMENTS DE LA COMMANDE ---
!
    call getvid(' ', 'RESU_GENE', scal=trange, nbret=n1)
    call getvtx(' ', 'NOM_CHAM', scal=nomcha, nbret=n1)
    call getvr8(' ', 'INST', scal=temps, nbret=nt)
    call getvr8(' ', 'FREQ', scal=freq, nbret=nf)
    call getvtx(' ', 'INTERPOL', scal=interp, nbret=ni)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=prec, nbret=n1)
!
    if (ni .eq. 0) interp(1:3) = 'NON'
!
!     --- RECUPERATION DES INFORMATIONS MODALES ---
!
    call jeveuo(trange//'           .DESC', 'L', iadesc)
    call jeveuo(trange//'           .DISC', 'L', idinst)
    call jelira(trange//'           .DISC', 'LONMAX', nbinst)
    call jeveuo(trange//'           .'//nomcha(1:4), 'L', idcham)
!
!     --- RECUPERATION DE LA NUMEROTATION GENERALISEE NUME_DDL_GENE
    call dismoi('NUME_DDL', trange, 'RESU_DYNA', repk=nddlge)
!     --- RECUPERATION DE LA BASE MODALE ET LE NOMBRE DE MODES
    call dismoi('BASE_MODALE', trange, 'RESU_DYNA', repk=basemo)
    nbmode = zi(iadesc+1)
!
    call wkvect(nomres//'           .REFE', 'G V K24', 2, idrefe)
    call wkvect(nomres//'           .DESC', 'G V I', 2, iddesc)
    call jeecra(nomres//'           .DESC', 'DOCU', cval='VGEN')
!
    zi(iddesc) = 1
    zi(iddesc+1) = nbmode
!
    zk24(idrefe) = basemo
    zk24(idrefe+1) = nddlge
!
!     --- CAS DU TRAN_GENE
    if (zi(iadesc) .ne. 4) then
!
!       ---  ON PLANTE SI LE MOT CLE DEMANDE EST FREQ POUR UN TRAN_GENE
        if (nf .ne. 0) then
            call utmess('E', 'ALGORITH9_51')
        endif
!
!       --- CREATION DU VECT_ASSE_GENE RESULTAT ---
        call wkvect(nomres//'           .VALE', 'G V R', nbmode, idvecg)
!
!       --- RECUPERATION DU CHAMP ---
        call extrac(interp, prec, crit, nbinst, zr(idinst),&
                    temps, zr( idcham), nbmode, zr(idvecg), ierd)
!
        if (ierd .ne. 0) then
            call utmess('E', 'ALGORITH9_49')
        endif
!
! --- CAS DU HARM_GENE
!
    else
!       ---  ON PLANTE SI LE MOT CLE DEMANDE EST INST POUR UN HARM_GENE
        if (nt .ne. 0) then
            call utmess('E', 'ALGORITH9_52')
        endif
!
!       --- CREATION DU VECT_ASSE_GENE RESULTAT ---
        call wkvect(nomres//'           .VALE', 'G V C', nbmode, idvecg)
!
!       --- RECUPERATION DU CHAMP ---
        call zxtrac('NON', 0.d0, 'RELATIF', nbinst, zr(idinst),&
                    freq, zc(idcham), nbmode, zc(idvecg), ierd)
!
        if (ierd .ne. 0) then
            call utmess('E', 'ALGORITH9_50')
        endif
!
    endif
!
    call jedema()
end subroutine
