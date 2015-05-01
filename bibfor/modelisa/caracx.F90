subroutine caracx(char, nzoco)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    integer :: nzoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - SD)
!
! CREATION DES SDS DE DEFINITION DU CONTACT DEDIEES A LA
! FORMULATION XFEM
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    character(len=24) :: defico
    integer :: zcmxf
    character(len=24) :: caraxf, modcon
    integer :: jcmxf, jmoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
!
! --- SPECIFIQUE XFEM
!
    caraxf = defico(1:16)//'.CARAXF'
    zcmxf = cfmmvd('ZCMXF')
    call wkvect(caraxf, 'G V R', zcmxf*nzoco, jcmxf)
    modcon = defico(1:16)//'.MODELX'
    call wkvect(modcon, 'G V K8', 1, jmoco)
!
    call jedema()
!
end subroutine
