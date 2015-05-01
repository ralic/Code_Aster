subroutine satuvg(vg, pc, sat, dsdpc)
! ======================================================================
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
! ======================================================================
!
! SATUVG : CALCUL DE LA SATURATION PAR VAN-GENUCHTEN + REGULARISATION
!  GAUCHE
    implicit none
!
! IN
#include "asterfort/pcapvg.h"
#include "asterfort/reguh1.h"
#include "asterfort/satfvg.h"
    real(kind=8) :: vg(5), pc
! OUT
    real(kind=8) :: sat, dsdpc
!
    real(kind=8) :: satuma, n, m, pr, smax, sr
    real(kind=8) :: s1max, pcmax, dpcmax, bidon
    real(kind=8) :: usn, usm, b1, c1
!
!
    n = vg(1)
    pr = vg(2)
    sr = vg(3)
    smax = vg(4)
    satuma = vg(5)
!
    m=1.d0-1.d0/n
    usn=1.d0/n
    usm=1.d0/m
!
    s1max=(smax-sr)/(1.d0-sr)
!
! FONCTION PROLONGATION A GAUCHE DE S(PC) (S > SMAX)
    call pcapvg(sr, pr, usm, usn, s1max,&
                pcmax, dpcmax, bidon)
    call reguh1(pcmax, smax, 1.d0/dpcmax, b1, c1)
!
    if ((pc.gt.pcmax)) then
!
        call satfvg(sr, pr, n, m, pc,&
                    sat, dsdpc)
!
    else if (pc.le.pcmax) then
!
        sat=1.d0-b1/(c1-pc)
        dsdpc=-b1/((c1-pc)**2.d0)
!
!
    endif
    sat=sat*satuma
!
end subroutine
