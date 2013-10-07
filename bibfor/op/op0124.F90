subroutine op0124()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE:  CREA_RESU
!
! ----------------------------------------------------------------------
#include "asterc/getfac.h"
#include "asterfort/crasse.h"
#include "asterfort/crperm.h"
#include "asterfort/crprol.h"
#include "asterfort/crtype.h"
#include "asterfort/crvrc1.h"
#include "asterfort/crvrc2.h"
#include "asterfort/eclpgr.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/ve0124.h"
    integer :: nbfac
    character(len=16) :: typres, valk(2)
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    call ve0124(typres)
    valk(1)=typres
!
! ----------------------------------------------------------------------
!                   TRAITEMENT DU MOT CLE "ECLA_PG"
! ----------------------------------------------------------------------
!
    call getfac('ECLA_PG', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_ELAS' .and. &
             typres .ne. 'EVOL_NOLI' .and. &
             typres .ne. 'EVOL_THER' ) then
           valk(2)='ECLA_PG'        
           call utmess ('F', 'ALGORITH17_41', nk=2, valk=valk)
        endif      
        call eclpgr()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!                   TRAITEMENT DU MOT CLE "PERM_CHAM"
! ----------------------------------------------------------------------
!
    call getfac('PERM_CHAM', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_NOLI' ) then
           valk(2)='PERM_CHAM' 
           call utmess ('F', 'ALGORITH17_41', nk=2, valk=valk )
        endif   
        call crperm()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "PROL_RTZ"
! ----------------------------------------------------------------------
!
    call getfac('PROL_RTZ', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_THER' ) then
           valk(2)='EVOL_THER' 
           call utmess ('F', 'ALGORITH17_41', nk=2, valk=valk )
        endif
        call crprol()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "AFFE"
! ----------------------------------------------------------------------
!
    call getfac('AFFE', nbfac)
    if (nbfac .gt. 0) then
        call crtype()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "ASSE"
! ----------------------------------------------------------------------
!
    call getfac('ASSE', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_THER' ) then
           valk(2)='ASSE' 
           call utmess ( 'F', 'ALGORITH17_41', nk=2, valk=valk )
        endif
        call crasse()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "PREP_VRC1"
! ----------------------------------------------------------------------
!
    call getfac('PREP_VRC1', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_THER' ) then
           valk(2)='PREP_VRC1' 
           call utmess ( 'F', 'ALGORITH17_41', nk=2, valk=valk )
        endif
        call crvrc1()
        goto 9999
    endif
!
! ----------------------------------------------------------------------
!               TRAITEMENT DU MOT CLE "PREP_VRC2"
! ----------------------------------------------------------------------
!
    call getfac('PREP_VRC2', nbfac)
    if (nbfac .gt. 0) then
        if ( typres .ne. 'EVOL_THER' ) then
             valk(2)='PREP_VRC2' 
             call utmess ( 'F', 'ALGORITH17_41', nk=2, valk=valk )
        endif
        call crvrc2()
        goto 9999
    endif
!
9999  continue
    call jedema()
end subroutine
