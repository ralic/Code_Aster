subroutine cfverd(noma, numedd, defico)
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
#include "asterfort/cfdisi.h"
#include "asterfort/cfnomm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/posddl.h"
#include "asterfort/u2mess.h"
    character(len=8) :: noma
    character(len=24) :: numedd
    character(len=24) :: defico
!
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - UTILITAIRE)
!
! VERIFICATION MODELE CONTACT PUR 2D
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  NUMEDD : NOM DU NUME_DDL
!
!
!
!
    character(len=8) :: nomno
    integer :: nnoco
    integer :: ino, jbid, verdim, posno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nnoco = cfdisi(defico,'NNOCO')
!
    do 10 ino = 1, nnoco
        posno = ino
        call cfnomm(noma, defico, 'NOEU', posno, nomno)
        call posddl('NUME_DDL', numedd, nomno, 'DZ', jbid,&
                    verdim)
        if (verdim .ne. 0) then
            call u2mess('F', 'CONTACT_85')
        endif
10  end do
!
    call jedema()
end subroutine
