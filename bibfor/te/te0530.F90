subroutine te0530(option, nomte)
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
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvarc.h"
    character(len=16) :: option, nomte
!
! ......................................................................
!  CALCUL DES VARIABLES DE COMMANDE UTILISEES DANS LES CALCULS
!  MECANIQUES
! ......................................................................
!
    real(kind=8) :: r1, rvid
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano, ipg, iret
    integer :: jpvarc, ivrc
    integer :: nbvarc
    parameter  ( nbvarc = 9)
    character(len=8) :: nomvrc(nbvarc)
!
! ---------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    call jevech('PVARC_R', 'E', jpvarc)
    rvid=r8vide()
!
!     VARC_R   = R    TEMP HYDR SECH IRRA CORR PTOT DIVU NEUT1 NEUT2
    nomvrc(1) = 'TEMP'
    nomvrc(2) = 'HYDR'
    nomvrc(3) = 'SECH'
    nomvrc(4) = 'IRRA'
    nomvrc(5) = 'CORR'
    nomvrc(6) = 'PTOT'
    nomvrc(7) = 'DIVU'
    nomvrc(8) = 'NEUT1'
    nomvrc(9) = 'NEUT2'
!
    do 1, ipg = 1, npg
!
    do 11, ivrc = 1 , nbvarc
    call rcvarc(' ', nomvrc(ivrc), '+', 'RIGI', ipg,&
                1, r1, iret)
    if (iret .eq. 0) then
        zr(jpvarc-1+nbvarc*(ipg-1)+ivrc)=r1
    else
        zr(jpvarc-1+nbvarc*(ipg-1)+ivrc)=rvid
    endif
11  continue
!
    1 end do
!
end subroutine
