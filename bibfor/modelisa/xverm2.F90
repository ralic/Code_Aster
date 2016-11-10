subroutine xverm2(nfiss, fiss, mod)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
    integer :: nfiss
    character(len=8) :: fiss(nfiss), mod
!
! ----------------------------------------------------------------------
!
! routine XFEM (verification des SD)
!
! verifications specifiques dans le cas des modelisations
! thermiques, HM et multi-heaviside, afin d'interdir certaines
! configurations
!
! ----------------------------------------------------------------------
!
! in  nfiss  : nombre de fissures
! in  fiss   : liste des noms des fissures
! in  mod    : nom du modele sain en entree de MODI_MODELE_XFEM
!
    integer :: ifiss, iexi
    character(len=16) :: typdis, pheno, exithm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!   recuperation du phenomene
!
    call dismoi('PHENOMENE', mod, 'MODELE', repk=pheno)
    ASSERT(pheno.eq.'MECANIQUE' .or. pheno.eq.'THERMIQUE')
!
!   s'agit-il d'une modelisation HM
!
    call dismoi('EXI_THM', mod, 'MODELE', repk=exithm)
!
!   boucle surlles fissures
!
    do ifiss = 1, nfiss
!
!       seules les INTERFACEs sont autorisees en HM-XFEM
!
        call dismoi('TYPE_DISCONTINUITE', fiss(ifiss), 'FISS_XFEM', repk=typdis)
        if (exithm .eq. 'OUI' .and. typdis .eq. 'FISSURE') then
            call utmess('F', 'XFEM_78', sk='HM-XFEM')
        endif
!
!       on interdit le multi-heaviside en thermique
!
        call jeexin(fiss(ifiss)//'.JONFISS', iexi)
        if (iexi .ne. 0 .and. pheno .eq. 'THERMIQUE') then
                call utmess('F', 'XFEM_71', sk=mod)
        endif
!
    end do
!
    call jedema()
end subroutine
