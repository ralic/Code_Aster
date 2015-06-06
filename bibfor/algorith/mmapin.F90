subroutine mmapin(modele, noma, defico, resoco, numedd,&
                  numins, sdstat, sdtime)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmctcg.h"
    character(len=8) :: noma
    integer :: numins
    character(len=24) :: defico, resoco
    character(len=24) :: sdstat, sdtime
    character(len=24) :: modele, numedd
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE)
!
! APPARIEMENT INITIAL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NUMINS : NUMERO D'INSTANT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  NUMEDD : NOM DU NUME_DDL
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
! ----------------------------------------------------------------------
!
    aster_logical :: lctcc, ltfcm, lallv
    aster_logical :: loptin
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVES
!
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- ACTIVATION DES OPTIONS *_INIT
!
    loptin = .false.
    if (numins .le. 1) loptin = .true.
!
! --- REALISATION DE L'APPARIEMENT
!
    if (.not.lallv .and. (lctcc.or.ltfcm)) then
        call mmbouc(resoco, 'GEOM', 'INIT')
        call nmctcg(modele, noma, defico, resoco, loptin,&
                    sdstat, sdtime, numedd)
        call mmbouc(resoco, 'GEOM', 'INCR')
    endif
!
    call jedema()
end subroutine
