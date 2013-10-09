subroutine op0050()
    implicit none
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!          OPERATEUR INTE_MAIL_2D
!
! ----------------------------------------------------------------------
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/i2chem.h"
#include "asterfort/i2segm.h"
#include "asterfort/infmaj.h"
#include "asterfort/utmess.h"
    integer ::  nbparm, nbpars, nbpara, n1
    character(len=8) :: k8b, nomail
!----------------------------------------------------------------------
!
!----------------------------------------------------------------------
!
    call infmaj()
!----------------------------------------------------------------------
!
!                 LE MAILLAGE DOIT ETRE 2D OU PLAN
!
!----------------------------------------------------------------------
    call getvid(' ', 'MAILLAGE', scal=nomail, nbret=n1)
    call dismoi('Z_CST', nomail, 'MAILLAGE', repk=k8b)
    if (k8b(1:3) .eq. 'NON') then
        call utmess('F', 'INTEMAIL_10')
    endif
!----------------------------------------------------------------------
!
!                     D E F I _ C H E M I N
!
!----------------------------------------------------------------------
    call getfac('DEFI_CHEMIN', nbparm)
    if (nbparm .gt. 0) then
        call i2chem(nomail, nbparm)
        goto 9999
    endif
!----------------------------------------------------------------------
!
!            D E F I _ A R C   ET   D E F I _ S E G M E N T
!
!----------------------------------------------------------------------
    call getfac('DEFI_SEGMENT', nbpars)
    call getfac('DEFI_ARC', nbpara)
    if (nbpars .gt. 0 .or. nbpara .gt. 0) then
        call i2segm(nomail, nbpars, nbpara)
        goto 9999
    endif
!
9999 continue
!
end subroutine
