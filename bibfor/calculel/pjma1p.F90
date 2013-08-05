subroutine pjma1p(moa1, ma1p, cham1, corres)
! person_in_charge: jacques.pellet at edf.fr
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
!
! COMMANDE:  PROJ_CHAMP /ECLA_PG
! CREATION DU MAILLAGE 1 PRIME (MA1P)
! REMPLISSAGE DU .PJEF_MP DANS LA SD CORRES
!   QUI EST LE NOM DU MAILLAGE 1 PRIME
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
! MOA1 : MODELE1 A ECLATER
! MA1P : EST LE MAILLAGE 1 PRIME
! CORRES : STRUCTURE DE DONNEES CORRES
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/eclpgm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: moa1, ma1p
    character(len=19) :: cham1
! ----------------------------------------------------------------------
!
!
    real(kind=8) :: shrink, lonmin
    integer :: ibid
    integer :: jcorre
    character(len=16) :: corres, lisch(1)
    character(len=19) :: ligrel
!
!
    call jemarq()
!
    shrink=1.d0
    lonmin=0.d0
    ASSERT(cham1.ne.' ')
    call dismoi('F', 'NOM_LIGREL', cham1, 'CHAM_ELEM', ibid,&
                ligrel, ibid)
!
    call eclpgm(ma1p, moa1, cham1, ligrel, shrink,&
                lonmin, 0, lisch)
!
    call wkvect(corres//'.PJEF_MP', 'V V K8', 1, jcorre)
    zk8(jcorre+1-1)=ma1p
!
    call jedema()
!
end subroutine
