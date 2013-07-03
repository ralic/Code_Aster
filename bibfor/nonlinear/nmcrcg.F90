subroutine nmcrcg(fonact, sdconv)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=24) :: sdconv
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CREATION ET INITIALISATION DE LA SD CONVERGENCE
!
! ----------------------------------------------------------------------
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! OUT SDCONV : SD GESTION DE LA CONVERGENCE
!
! ----------------------------------------------------------------------
!
    integer :: nresi
    parameter    (nresi=6)
!
    character(len=24) :: cnvtyp, cnvlie, cnvval, cnvact, cnvnco
    integer :: jcnvty, jcnvli, jcnvva, jcnvac, jcnvnc
    logical :: lnewtf, lnewtg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    lnewtf = isfonc(fonact,'FROT_NEWTON')
    lnewtg = isfonc(fonact,'GEOM_NEWTON')
!
! --- LISTE ET VALEURS DES TESTS DE CONVERGENCE
!
    cnvtyp = sdconv(1:19)//'.TYPE'
    cnvlie = sdconv(1:19)//'.LIEU'
    cnvval = sdconv(1:19)//'.VALE'
    cnvact = sdconv(1:19)//'.ACTI'
    cnvnco = sdconv(1:19)//'.NCOL'
    call wkvect(cnvtyp, 'V V K16', nresi, jcnvty)
    call wkvect(cnvlie, 'V V K16', nresi, jcnvli)
    call wkvect(cnvval, 'V V R', nresi, jcnvva)
    call wkvect(cnvact, 'V V L', nresi, jcnvac)
    call wkvect(cnvnco, 'V V K16', nresi, jcnvnc)
!
! --- TYPE DES RESIDUS
!
    zk16(jcnvty-1+1) = 'RESI_GLOB_RELA'
    zk16(jcnvty-1+2) = 'RESI_GLOB_MAXI'
    zk16(jcnvty-1+3) = 'RESI_GLOB_REFE'
    zk16(jcnvty-1+4) = 'RESI_COMP_RELA'
    zk16(jcnvty-1+5) = 'RESI_FROT'
    zk16(jcnvty-1+6) = 'RESI_GEOM'
!
! --- NOM DES COLONNES
!
    zk16(jcnvnc-1+1) = 'RESI_RELA'
    zk16(jcnvnc-1+2) = 'RESI_MAXI'
    zk16(jcnvnc-1+3) = 'RESI_REFE'
    zk16(jcnvnc-1+4) = 'RESI_COMP'
    zk16(jcnvnc-1+5) = 'FROT_NEWT'
    zk16(jcnvnc-1+6) = 'GEOM_NEWT'
!
! --- RESIDUS ACTIVES POUR LES TESTS DE CONVERGENCE
!
    if (lnewtf) zl(jcnvac-1+5) = .true.
    if (lnewtg) zl(jcnvac-1+6) = .true.
!
    call jedema()
end subroutine
