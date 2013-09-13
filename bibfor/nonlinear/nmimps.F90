subroutine nmimps(sdimpr, sdconv, sderro)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmerge.h"
#include "asterfort/obgetb.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdconv, sdimpr, sderro
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (AFFICHAGE - ACCES SD)
!
! IMPRESSION DES RESIDUS (RECAP FIN DE PAS)
!
! ----------------------------------------------------------------------
!
!
! IN  SDCONV : SD GESTION DE LA CONVERGENCE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDERRO : GESTION DES ERREURS
!
! ----------------------------------------------------------------------
!
    character(len=24) :: cnvtyp, cnvlie, cnvval, cnvact
    integer :: jcnvty, jcnvli, jcnvva, jcnvac
    integer :: iresi, iarg, ibid, nresi
    real(kind=8) :: valr(1)
    character(len=16) :: valk(2)
    logical :: lprint, maxrel, maxnod
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD CONVERGENCE
!
    cnvtyp = sdconv(1:19)//'.TYPE'
    cnvlie = sdconv(1:19)//'.LIEU'
    cnvval = sdconv(1:19)//'.VALE'
    cnvact = sdconv(1:19)//'.ACTI'
    call jeveuo(cnvtyp, 'L', jcnvty)
    call jeveuo(cnvlie, 'L', jcnvli)
    call jeveuo(cnvval, 'L', jcnvva)
    call jeveuo(cnvact, 'L', jcnvac)
    call jelira(cnvtyp, 'LONMAX', ival=nresi)
!
! --- MESSAGE DE BASCULEMENT DES CONVERGENCES
!
    call nmerge(sderro, 'RESI_MAXR', maxrel)
    call nmerge(sderro, 'RESI_MAXN', maxnod)
!
! --- AFFICHAGE POUR CE PAS ?
!
    call obgetb(sdimpr, 'PRINT', lprint)
    if (.not.lprint) goto 99
!
! --- AFFICHAGE ENTETE ARCHIVAGE
!
    call utmess('I', 'MECANONLINE6_60')
    if (maxnod) then
        call utmess('I', 'MECANONLINE6_61')
    endif
    if (maxrel) then
        call utmess('I', 'MECANONLINE6_62')
    endif
!
! --- AFFICHAGE
!
    iarg = 0
    do 20 iresi = 1, nresi
        if (zl(jcnvac-1+iresi)) then
            iarg = iarg + 1
            valk(1) = zk16(jcnvty-1+iresi)
            valk(2) = zk16(jcnvli-1+iresi)
            valr(1) = zr(jcnvva-1+iresi)
            call utmess('I', 'MECANONLINE6_70', nk=2, valk=valk, sr=valr(1))
        endif
20  end do
!
99  continue
!
    call jedema()
!
end subroutine
