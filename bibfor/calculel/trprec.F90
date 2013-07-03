subroutine trprec(mcf, iocc, epsi, crit, prec,&
                  crit2)
    implicit   none
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
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
!     COMMANDE:  TEST_RESU
!
!     REMARQUES:  MCF=_F( ...
!                         TOLE_MACHINE: ( EPSI , PREC  )        L_R8
!                         CRITERE  : ( CRIT , CRIT2 )           L_TXM
!     EPSI ET CRIT  SONT LA PRECISION ET LE CRITERE DU TEST
!     PREC ET CRIT2 SONT LA PRECISION ET LE CRITERE DE L'EXTRACTION
! ----------------------------------------------------------------------
    integer :: iocc, np, nc
    real(kind=8) :: epsi, prec, epsir(2)
    character(len=8) :: crit, crit2, critr(2)
    integer :: iarg
!     ------------------------------------------------------------------
!
    call getvr8(mcf, 'TOLE_MACHINE', iocc, iarg, 0,&
                epsi, np)
    np = -np
    if (np .eq. 0) then
        epsi = 1.d-6
        prec = 1.d-6
    else if (np.eq.1) then
        call getvr8(mcf, 'TOLE_MACHINE', iocc, iarg, 1,&
                    epsi, np)
        prec = epsi
    else
        call getvr8(mcf, 'TOLE_MACHINE', iocc, iarg, 2,&
                    epsir, np)
        epsi = epsir(1)
        prec = epsir(2)
    endif
!
    call getvtx(mcf, 'CRITERE', iocc, iarg, 0,&
                crit, nc)
    nc = -nc
    if (nc .eq. 0) then
        crit = 'RELATIF'
        crit2 = 'RELATIF'
    else if (nc.eq.1) then
        call getvtx(mcf, 'CRITERE', iocc, iarg, 1,&
                    crit, nc)
        crit2 = crit
    else
        call getvtx(mcf, 'CRITERE', iocc, iarg, 2,&
                    critr, nc)
        crit = critr(1)
        crit2 = critr(2)
    endif
!
end subroutine
