subroutine trprec(mcf, iocc, epsi, crit, prec,&
                  crit2)
    implicit none
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/get_tole_mach.h"
    character(len=*) :: mcf
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     Commande:  TEST_RESU
!
!     Remarques:  MCF=_F( ...
!                         TOLE_MACHINE=(epsi, prec)
!                         CRITERE=(crit, crit2)
!     epsi et crit  sont la tolérance et le critère du test
!     prec et crit2 sont la précision et le critère de l'extraction
! ----------------------------------------------------------------------
    integer :: iocc, nc
    real(kind=8) :: epsi, prec
    character(len=8) :: crit, crit2, critr(2)
!     ------------------------------------------------------------------
!
    call get_tole_mach(epsi, prec, mcf, iocc)
!
    call getvtx(mcf, 'CRITERE', iocc=iocc, nbval=0, nbret=nc)
    nc = -nc
    if (nc .eq. 0) then
        crit = 'RELATIF'
        crit2 = 'RELATIF'
    else if (nc.eq.1) then
        call getvtx(mcf, 'CRITERE', iocc=iocc, scal=crit, nbret=nc)
        crit2 = crit
    else
        call getvtx(mcf, 'CRITERE', iocc=iocc, nbval=2, vect=critr,&
                    nbret=nc)
        crit = critr(1)
        crit2 = critr(2)
    endif
!
end subroutine
