subroutine rc3200()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvid.h"
#include "asterc/getfac.h"
#include "asterfort/rc32si.h"
#include "asterfort/rc32ma.h"
#include "asterfort/rc32in.h"
#include "asterfort/rc32cm.h"
#include "asterfort/rc32t.h"
#include "asterfort/rc32mu.h"
#include "asterfort/rc32ac.h"
#include "asterfort/rc32rs.h"
#include "asterfort/jedetc.h"
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200 et B3200
!
    character(len=8) :: mater
    integer :: n1, nbther, nbopt, iopt, nb
    character(len=16) :: typmec, kopt(3)
    aster_logical :: lpmpb, lsn, lther, lfat, lefat, ze200, b32unit
!
! DEB ------------------------------------------------------------------
!
    ze200 = .false.
    b32unit = .false.
    call getvtx(' ', 'TYPE_RESU_MECA', scal=typmec, nbret=n1)
    if (typmec .eq. 'ZE200a' .or. typmec .eq. 'ZE200b') ze200=.true.

!     ------------------------------------------------------------------
!              TRAITEMENT DES SITUATIONS (GROUPES, PASSAGE...)
!     ------------------------------------------------------------------
!
    call rc32si()
!
!     ------------------------------------------------------------------
!              RECUPERATION DES CARACTERISTIQUES MATERIAU
!     ------------------------------------------------------------------
!
    call rc32ma()
!
!     ------------------------------------------------------------------
!              RECUPERATION DES INDICES DE CONTRAINTES 
!              ET DES CARACTERISTIQUES DE LA TUYAUTERIE
!     ------------------------------------------------------------------
!
    call rc32in()
! 
!     ------------------------------------------------------------------
!              RECUPERATION DES CHARGES MECANIQUES
!              (SOUS CHAR_MECA et RESU_MECA_UNIT) 
!     ------------------------------------------------------------------
!
    call rc32cm()
    call rc32mu()
!
!     ------------------------------------------------------------------
!                  RECUPERATION DES TRANSITOIRES :
!                    - THERMIQUES(si RESU_THER)
!                    - DE PRESSION(si RESU_PRES)
!                    - EFFORTS EXTERNES(si RESU_MECA)
!     ------------------------------------------------------------------
!
    call rc32t()
!
!     ------------------------------------------------------------------
!              CALCULS DES AMPLITUDES DE CONTRAINTES
!                  ET DU FACTEUR D'USAGE
!     ------------------------------------------------------------------
!
    call getvid(' ', 'MATER', scal=mater, nbret=n1)
    call getfac('RESU_MECA_UNIT', nb)
!-- si on est en ZE200 ou B3200_T
    if (nb .ne. 0) b32unit=.true.
!
    lpmpb = .false.
    lsn   = .false.
    lther = .false.
    lfat  = .false.
    lefat = .false.
!
    call getfac('RESU_THER', nbther)
    if (nbther .ne. 0 .and. nb .ne. 0) then
        lther = .true.
    endif
!
    call getvtx(' ', 'OPTION', nbval=0, nbret=n1)
    nbopt = -n1
    call getvtx(' ', 'OPTION', nbval=nbopt, vect=kopt, nbret=n1)
    do 20 iopt = 1, nbopt
        if (kopt(iopt) .eq. 'PM_PB') then
            if (nb .ne. 0) lpmpb = .true.
        else if (kopt(iopt) .eq. 'SN') then
            lsn = .true.
        else if (kopt(iopt) .eq. 'FATIGUE') then
            if (nb .ne. 0) lpmpb = .true.
            lfat = .true.
            lsn = .true.
        else if (kopt(iopt) .eq. 'EFAT') then
            if (nb .ne. 0) lpmpb = .true.
            lfat = .true.
            lsn = .true.
            lefat = .true.
        endif
 20 continue
!
    call rc32ac(ze200, mater, lpmpb, lsn, lther, lfat, lefat)
!
!     ------------------------------------------------------------------
!                       STOCKAGE DES RESULTATS
!     ------------------------------------------------------------------
!
    call rc32rs(mater, lpmpb, lsn, lther,&
                lfat, lefat)
!
    call jedetc('V', '&&RC3200', 1)
!
end subroutine
