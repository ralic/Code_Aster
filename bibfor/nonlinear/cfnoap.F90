subroutine cfnoap(noma, defico, typapp, entapp, nomapp,&
                  type2)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnomm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=24) :: defico
    character(len=8) :: noma
    integer :: entapp, typapp
    character(len=8) :: nomapp
    character(len=4) :: type2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT - UTILITAIRE)
!
! NOM DE L'ENTITE APPARIEE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! IN  TYPAPP : TYPE D'ENTITE APAPRIEE
! IN  ENTAPP : POSITION DE L'ENTITE APPARIE DANS SD_CONTACT
! OUT NOMAPP : NOM DE L'ENTITE APPARIEE
! OUT TYPE2  : TYPE D'APPARIEMENT
!                TYPE2  = ' NON'
!                TYPE2  = '/ND '
!                TYPE2  = '/EL '
!
!
!
!
    integer :: posnom, posmam
    character(len=8) :: nomnom, nommam
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! ----- NOM ET TYPE (MAILLE OU NOEUD) DU MAITRE
!
    if (typapp .lt. 0) then
        type2 = ' NON'
        nomapp = ' APPARIE'
    else if (typapp.eq.1) then
        posnom = entapp
        call cfnomm(noma, defico, 'NOEU', posnom, nomnom)
        type2 = '/ND '
        nomapp = nomnom
    else if (typapp.eq.2) then
        posmam = entapp
        call cfnomm(noma, defico, 'MAIL', posmam, nommam)
        type2 = '/EL '
        nomapp = nommam
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
