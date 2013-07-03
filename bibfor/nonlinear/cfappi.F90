subroutine cfappi(noma, defico, nomnoe, typapp, posapp)
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
#include "asterfort/cfnomm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: noma
    character(len=24) :: defico
    integer :: posapp
    integer :: typapp
    character(len=8) :: nomnoe
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - APPARIEMENT)
!
! SAUVEGARDE DES PARAMETRES APPARIEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  TYPAPP : TYPE D'APPARIEMENT
!               -1  NON APPARIE CAR NOEUD EXCLU SANS_NOEUD
!               -2  NON APPARIE CAR NOEUD EXCLU PAR TOLE_APPA
!               -3  NON APPARIE CAR NOEUD EXCLU PAR TOLE_PROJ_EXT
! IN  NOMNOE : NOM DU NOEUD ESCLAVE
! IN  POSAPP : ENTITE APPARIEE
!
!
!
!
    character(len=8) :: nomapp, valk(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    valk(1) = nomnoe
    if (typapp .eq. -3) then
        call cfnomm(noma, defico, 'MAIL', posapp, nomapp)
        valk(2) = nomapp
        call u2mesk('I', 'CONTACTDEBG_11', 2, valk)
    else if (typapp.eq.-2) then
        call u2mesk('I', 'CONTACTDEBG_12', 1, valk)
    else if (typapp.eq.-1) then
        call u2mesk('I', 'CONTACTDEBG_13', 1, valk)
    endif
!
    call jedema()
end subroutine
