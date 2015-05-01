subroutine cfnomm(noma, defico, typent, posent, noment)
!
    implicit     none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
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
    character(len=8), intent(in) :: noma
    character(len=24), intent(in) :: defico
    integer, intent(in) :: posent
    character(len=4), intent(in) :: typent
    character(len=8), intent(out) :: noment
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! DONNE LE NOM DE L'ENTITE A PARTIR DE SON NUMERO
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  POSENT : POSITION DE L'ENTITE DANS LES SD CONTACT
! IN  TYPENT : TYPE D'ENTITE
!                <MAIL>  MAILLE
!                <NOEU>  NOEUD
! OUT NOMENT : NOM DE L'ENTITE
!
! ----------------------------------------------------------------------
!
    integer :: nummai, numnoe(1)
    integer :: posmai, posnoe(1)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    noment = ' '
!
! --- PREPARATION DES CHAINES POUR LES NOMS
!
    if (typent .eq. 'MAIL') then
        posmai = posent
        call cfnumm(defico, posmai, nummai)
        call jenuno(jexnum(noma//'.NOMMAI', nummai), noment)
!
    else if (typent.eq.'NOEU') then
        posnoe = abs(posent)
        call cfnumn(defico, 1, posnoe(1), numnoe(1))
        call jenuno(jexnum(noma//'.NOMNOE', numnoe(1)), noment)
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
