subroutine permvc(vg, s, krl, dklds, krg,&
                  dkgds)
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
! PERMVG : CALCUL DES PERMEABILITES RELATIVES
! PAR MUALEM-POUR L EAU ET CUBIQUE POUR LE GAZ
! AVEC REGULARISATION DROITE ET GAUCHE
!
    implicit none
!
! IN
#include "asterfort/kfomvc.h"
#include "asterfort/regup1.h"
#include "asterfort/regup2.h"
    real(kind=8) :: vg(5), s
! OUT
    real(kind=8) :: krl, dklds, krg, dkgds
!
    real(kind=8) :: n, sr, pr, smax
    real(kind=8) :: m, s1, usn, usm, s1max
    real(kind=8) :: x0, y0w, y0wp, y1, a1, b1, c1
    real(kind=8) :: smin, s1min, ar, br
! ======================================================================
!
!
    n = vg(1)
    pr = vg(2)
    sr = vg(3)
    smax = vg(4)
!      SATUMA = VG(5)
!
    krl = 0.0d0
    dklds = 0.0d0
    krg = 0.0d0
    dkgds = 0.0d0
!
    m=1.d0-1.d0/n
    usn=1.d0/n
    usm=1.d0/m
    s1=(s-sr)/(1.d0-sr)
    s1max=(smax-sr)/(1.d0-sr)
    s1min=1.d0-smax
    smin=sr+(1.d0-sr)*s1min
!
! NB : SMAX < SMIN PUISQUE SMAX = S(PCMAX) etc ..
!
    if ((s.lt.smax) .and. (s.gt.smin)) then
!
        call kfomvc(pr, sr, m, n, usm,&
                    usn, s, s1, krl, krg,&
                    dklds, dkgds)
!
    else if (s.ge.smax) then
        x0=smax
!
!  REGUL KL(S) A DROITE
!
        call kfomvc(pr, sr, m, n, usm,&
                    usn, x0, s1max, y0w, krg,&
                    y0wp, dkgds)
        y1=1.d0
        call regup2(x0, y0w, y0wp, y1, a1,&
                    b1, c1)
        krl=a1*s*s+b1*s+c1
        dklds=2.d0*a1*s+b1
!
    else if (s.le.smin) then
        x0=smin
!
!  REGUL KL(S) GAUCHE
!
        call kfomvc(pr, sr, m, n, usm,&
                    usn, x0, s1min, y0w, krg,&
                    y0wp, dkgds)
        call regup1(x0, y0w, y0wp, ar, br)
        krl=ar*s+br
        dklds=ar
!
    endif
!
! =====================================================================
! ======================================================================
!
end subroutine
