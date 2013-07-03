subroutine mmnpoi(noma, nommae, numnoe, iptm, nompt)
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
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
    character(len=8) :: noma
    character(len=8) :: nommae
    integer :: iptm, numnoe
    character(len=16) :: nompt
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - SD APPARIEMENT)
!
! NOM DU POINT DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMMAE : NOM DE LA MAILLE ESCLAVE
! IN  NUMNOE : NOM DU NOEUD
!               VAUT -1 SI CE LE POINT DE CONTACT N'EST PAS UN NOEUD
! IN  IPTM   : NUMERO DU PT D'INTEGRATION ( SI PAS INTEG. AUX NOEUDS)
! OUT NOMPT  : NOM DU POINT DE CONTACT
!
!
!
!
    character(len=4) :: for4
    character(len=8) :: nomnoe
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    write(for4,'(I4)') iptm
    if (numnoe .gt. 0) then
        call jenuno(jexnum(noma//'.NOMNOE', numnoe), nomnoe)
        nompt = 'NOEUD   '//nomnoe
    else
        write(for4,'(I4)') iptm
        nompt = nommae//'-PT '//for4
    endif
!
    call jedema()
!
end subroutine
