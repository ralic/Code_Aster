subroutine rc32rs(b3200, mater, lpmpb, lsn,&
                  lther, lfat, lefat)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/rc32r1env.h"
#include "asterfort/rcZ2r1env.h"
#include "asterfort/rc32r1.h"
#include "asterfort/rcZ2r1.h"
#include "asterfort/rc32r0.h"
#include "asterfort/rcZ2r0.h"
#include "asterfort/rc32r8.h"
#include "asterfort/rcZ2r8.h"
    character(len=8) :: mater
    aster_logical :: b3200, lpmpb, lsn, lther, lfat, lefat
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     AFFICHAGE DES RESULTATS DANS LA TABLE DE SORTIE
!
!     ------------------------------------------------------------------
    character(len=8) :: nomres
    character(len=16) :: concep, nomcmd
! DEB ------------------------------------------------------------------
!
    call getres(nomres, concep, nomcmd)
!
    call tbcrsd(nomres, 'G')
!
    if (lfat) then
        if (lefat) then
            if(b3200) then
                call rc32r1env(nomres)
            else
                call rcZ2r1env(nomres)
            endif
        else
            if(b3200) then
                call rc32r1(nomres)
            else
                call rcZ2r1(nomres)
            endif
        endif
    else
        if(b3200) then
            call rc32r0(nomres, lpmpb, lsn, lther)
        else
            call rcZ2r0(nomres, lsn, lther)
        endif
    endif
!
    if (b3200) then
        if (lther) call rc32r8(nomres, mater)
    else
        if (lther) call rcZ2r8(nomres, mater)
    endif
!
end subroutine
