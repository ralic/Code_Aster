subroutine critet(epsp, epsd, eta, lambda, deuxmu,&
                  fpd, seuil, crit, critp)
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/bptobg.h"
#include "asterfort/diagp3.h"
#include "asterfort/r8inir.h"
#include "blas/ddot.h"
    real(kind=8) :: epsp(6), epsd(6)
    real(kind=8) :: lambda, deuxmu
    real(kind=8) :: eta, fpd, seuil
    real(kind=8) :: crit, critp
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS - ENDO_ISOT_BETON)
!
! CALCUL DU CRITERE DE F(ETA) ET DE SA DERIVEE
!
! ----------------------------------------------------------------------
!
!
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  LAMBDA : COEFFICIENT DE LAME
! IN  DEUXMU : COEFFICIENT DE LAME
! IN  FPD    : PARTIE DEPENDANTE DE D DANS LA FORCE THERMO
!             VAUT (1+GAMMA)/(1+GAMMA*D)**2
! IN  SEUIL  : SEUIL DU CRITERE
! OUT CRIT   : VALEUR DU CRITERE POUR ETA DONNEE EN ENTREE
! OUT CRITP  : VALEUR DE LA DERIVEE DU CRITERE POUR ETA DONNEE EN ENTREE
!
! ----------------------------------------------------------------------
!
    integer :: k, i
    real(kind=8) :: tr(6), vecp(3, 3)
    real(kind=8) :: epm(3), tre, rac2
    real(kind=8) :: treps, sigel(3), ppeps(6), dfde(6)
!
! ----------------------------------------------------------------------
!
    rac2=sqrt(2.d0)
! -- ON DIAGONALISE LE TENSEUR DE DEFORMATION
    tr(1) = epsp(1)+eta*epsd(1)
    tr(2) = epsp(4)+eta*epsd(4)
    tr(3) = epsp(5)+eta*epsd(5)
    tr(4) = epsp(2)+eta*epsd(2)
    tr(5) = epsp(6)+eta*epsd(6)
    tr(6) = epsp(3)+eta*epsd(3)
!
    call diagp3(tr, vecp, epm)
!
! -- CALCUL DU CRITERE
!
    treps = epm(1)+epm(2)+epm(3)
    if (treps .gt. 0.d0) then
        do 70 k = 1, 3
            sigel(k) = lambda*treps
70      continue
    else
        do 71 k = 1, 3
            sigel(k) = 0.d0
71      continue
    endif
    do 25 k = 1, 3
        if (epm(k) .gt. 0.d0) then
            sigel(k) = sigel(k) + deuxmu*epm(k)
        endif
25  end do
    crit= fpd * 0.5d0 * ddot(3,epm,1,sigel,1) - seuil
!
    do 48 i = 1, 3
        if (epm(i) .lt. 0.d0) then
            tr(i)=0.d0
        else
            tr(i)=epm(i)
        endif
        tr(i+3)=0.d0
48  end do
!
! -- CALCUL DE LA DERIVEE DU CRITERE
!
    call bptobg(tr, ppeps, vecp)
    call r8inir(6, 0.d0, dfde, 1)
    tre=epm(1)+epm(2)+epm(3)
!
    if (tre .gt. 0.d0) then
        do 50 i = 1, 3
            dfde(i)=fpd*lambda*tre
50      continue
    endif
    do 51 i = 1, 3
        dfde(i)=dfde(i)+deuxmu*fpd*ppeps(i)
51  end do
    do 52 i = 4, 6
        dfde(i)=deuxmu*fpd*ppeps(i)*rac2
        epsd(i)=epsd(i)*rac2
52  end do
!
    critp=ddot(6,dfde,1,epsd,1)
!
    do 53 i = 4, 6
        epsd(i)=epsd(i)/rac2
53  end do
!
end subroutine
