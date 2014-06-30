subroutine pipepl(ndim, compor, typmod, tau, mate,&
                  sigm, vim, epsp, epsd, a0,&
                  a1, a2, a3, etas)
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
#include "asterc/r8vide.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/zerop2.h"
#include "blas/ddot.h"
    character(len=8) :: typmod(*)
    character(len=16) :: compor
    integer :: ndim, mate
    real(kind=8) :: epsp(6), epsd(6), tau
    real(kind=8) :: vim(2), sigm(6)
    real(kind=8) :: a0, a1, a2, a3, etas
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
!
! LOI DE COMPORTEMENT PLASTIQUE VMIS_ISOT_*
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  COMPOR : NOM DU COMPORTEMENT
! IN  TYPMOD : TYPE DE MODELISATION
! IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
! IN  MATE   : NATURE DU MATERIAU
! IN  SIGM   : CONTRAINTE EN T-
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! OUT A0     : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
! OUT A1     : IDEM A0
! OUT A2     : IDEM A0 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
! OUT A3     : IDEM A1 POUR LA 2E SOLUTION EVENTUELLE. R8VIDE SINON
! OUT ETAS   : SI PAS DE SOLUTION : LE MINIMUM. R8VIDE SINON
!
! ----------------------------------------------------------------------
!
    integer :: nbres
    parameter   (nbres=4)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
    real(kind=8) :: valres(nbres)
!
    logical(kind=1) :: cplan
    integer :: ndimsi, k, nrac, jprol, jvale, nbvale, kpg, spt
    real(kind=8) :: sigmh, epsph, epsdh, s0h, s1h, s0(6), s1(6)
    real(kind=8) :: kron(6)
    real(kind=8) :: p0, p1, p2, eta, rac(2)
    real(kind=8) :: young, nu, deuxmu, rp, h, et, sy
!
    data        kron /1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
!
! -- OPTION ET MODELISATION
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    ndimsi = 2*ndim
    cplan = (typmod(1).eq.'C_PLAN  ')
!
    if (cplan) then
        call utmess('F', 'PILOTAGE_1')
    endif
!
! -- LECTURE DES CARACTERISTIQUES
!
    if (compor .eq. 'VMIS_ISOT_LINE') then
!
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'SY'
        nomres(4) = 'D_SIGM_EPSI'
        call rcvalb(fami, kpg, spt, poum, mate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    2, nomres, valres, icodre, 2)
        call rcvalb(fami, kpg, spt, poum, mate,&
                    ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                    2, nomres(3), valres(3), icodre(3), 2)
        young = valres(1)
        nu = valres(2)
        sy = valres(3)
        et = valres(4)
        h = young*et/(young-et)
        rp = sy + h*vim(1)
!
    else
        call rcvalb(fami, kpg, spt, poum, mate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'NU', valres, icodre, 2)
        nu=valres(1)            
        call rctrac(mate, 1, 'SIGM', 0.d0, jprol,&
                    jvale, nbvale, young)
        call rcfonc('V', 1, jprol, jvale, nbvale,&
                    p =  vim(1), rp = rp)
    endif
!
    deuxmu = young/(1.d0+nu)
!
! ======================================================================
!                CALCUL DES DEFORMATIONS POUR LINEARISATION
! ======================================================================
!
!    PARTITION TRACE / DEVIATEUR
!
    sigmh = (sigm(1)+sigm(2)+sigm(3))/3
    epsph = (epsp(1)+epsp(2)+epsp(3))/3
    epsdh = (epsd(1)+epsd(2)+epsd(3))/3
!
    s0h = deuxmu*epsph + sigmh
    s1h = deuxmu*epsdh
    do 10 k = 1, ndimsi
        s0(k) = sigm(k) + deuxmu*epsp(k) - s0h*kron(k)
        s1(k) = deuxmu*epsd(k) - s1h*kron(k)
10  end do
!
!
!    COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
!      FEL = SQRT(P0 + 2P1 ETA + P2 ETA**2) - 1
!
    p0 = ddot(ndimsi,s0,1,s0,1) * (1.5d0 / rp**2)
    p1 = ddot(ndimsi,s0,1,s1,1) * (1.5d0 / rp**2)
    p2 = ddot(ndimsi,s1,1,s1,1) * (1.5d0 / rp**2)
!
!
!    POINT A DEVIATEUR NUL : PAS DE PILOTAGE POSSIBLE
    if (p2 .eq. 0) then
        a0 = 0.d0
        a1 = 0.d0
        a2 = 0.d0
        a3 = 0.d0
        goto 9999
    endif
!
!    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE
    call zerop2(2*p1/p2, (p0-(1+tau)**2)/p2, rac, nrac)
!
!    PAS DE SOLUTION : POINT LE PLUS PROCHE
    if (nrac .eq. 0) then
        etas = - p1/p2
!
!    UNE OU DEUX SOLUTIONS : ON LINEARISE AUTOUR DES DEUX
    else if (nrac.eq.1) then
        eta = rac(1)
        a1 = (p2*eta+p1)/(1+tau)
        a0 = tau - a1*eta
        a2 = r8vide()
        a3 = r8vide()
    else
        eta = rac(1)
        a1 = (p2*eta+p1)/(1+tau)
        a0 = tau - a1*eta
        eta = rac(2)
        a3 = (p2*eta+p1)/(1+tau)
        a2 = tau - a3*eta
    endif
!
9999  continue
end subroutine
