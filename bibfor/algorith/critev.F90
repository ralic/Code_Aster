subroutine critev(epsp, epsd, eta, lambda, deuxmu,&
                  fpd, seuil, rd, crit, critp)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
    implicit none
    include 'asterfort/bptobg.h'
    include 'asterfort/diagp3.h'
    include 'asterfort/r8inir.h'
    include 'blas/ddot.h'
    real(kind=8) :: epsp(7), epsd(7), eta, lambda, deuxmu, fpd, seuil, rd
    real(kind=8) :: crit, critp
! ----------------------------------------------------------------------
!     CALCUL DU CRITERE DE ENDO_ISOT_BETON F(ETA) ET DE SA DERIVEE
!     EN NON LOCAL GRAD_EPSI
!
! IN EPSP    : DEFORMATIONS DUES AUX CHARGEMENTS ANTERIEURS ET FIXES
! IN EPSD    : DEFORMATIONS PROPORTIONNELLES A ETA
! IN ETA     : INTENSITE DU PILOTAGE
! IN LAMBDA  : |
! IN DEUXMU  : | COEFFICIENTS DE LAME
! IN FPD     : PARTIE DEPENDANTE DE D DANS LA FORCE THERMO
!             VAUT (1+GAMMA)/(1+GAMMA*D)**2
! IN SEUIL   : SEUIL DU CRITERE
! OUT CRIT   : VALEUR DU CRITERE POUR ETA DONNEE EN ENTREE
! OUT CRITP  : VALEUR DE LA DERIVEE DU CRITERE POUR ETA DONNEE EN ENTREE
! ----------------------------------------------------------------------
!
    integer :: k, i
    real(kind=8) :: tr(6), vecp(3, 3)
    real(kind=8) :: epm(3), tre, rac2, phi
    real(kind=8) :: treps, sigel(3), ppeps(6), dfde(6)
!
!
!
    rac2=sqrt(2.d0)
!
! -- ON DIAGONALISE LE TENSEUR DE DEFORMATION
    tr(1) = epsp(1)+eta*epsd(1)
    tr(2) = (epsp(4)+eta*epsd(4))/rac2
    tr(3) = (epsp(5)+eta*epsd(5))/rac2
    tr(4) = epsp(2)+eta*epsd(2)
    tr(5) = (epsp(6)+eta*epsd(6))/rac2
    tr(6) = epsp(3)+eta*epsd(3)
!
    call diagp3(tr, vecp, epm)
!
    phi = epsp(7)+eta*epsd(7)
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
!
    crit= fpd * 0.5d0 * ddot(3,epm,1,sigel,1)+phi - rd- seuil
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
52  end do
!
    critp=ddot(6,dfde,1,epsd,1)+epsd(7)
!
!
end subroutine
