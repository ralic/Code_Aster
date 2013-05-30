subroutine pipeef(ndim, typmod, tau, mate, vim,&
                  epsp, epsd, a0, a1, a2,&
                  a3, etas)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    include 'asterc/r8vide.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/zerop2.h'
    include 'blas/ddot.h'
    character(len=8) :: typmod(2)
    integer :: ndim, mate
    real(kind=8) :: epsp(6), epsd(6), tau
    real(kind=8) :: vim(2)
    real(kind=8) :: a0, a1, a2, a3, etas
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
!
! LOI DE COMPORTEMENT ELASTIQUE FRAGILE (EN DELOCALISE)
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  TYPMOD : TYPE DE MODELISATION
! IN  TAU    : 2ND MEMBRE DE L'EQUATION F(ETA)=TAU
! IN  MATE   : MATERIAU CODE
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
    parameter   (nbres=2)
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), fami, poum
    real(kind=8) :: valres(nbres)
!
    logical :: cplan
    integer :: ndimsi, k, nrac, kpg, spt
    real(kind=8) :: trepsp, trepsd, coplan, sigelp(6), sigeld(6)
    real(kind=8) :: kron(6)
    real(kind=8) :: p0, p1, p2, eta, rac(2)
    real(kind=8) :: e, nu, lambda, deuxmu, gamma, sy, wy, wrel
    real(kind=8) :: dm, dtau, gm, gtau, s
!
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
!
!
! -- OPTION ET MODELISATION
!
    cplan = (typmod(1).eq.'C_PLAN  ')
    ndimsi = 2*ndim
!
!
! -- CAS DE L'ENDOMMAGEMENT SATURE
!
    if (nint(vim(2)) .eq. 2) then
        a0 = 0.d0
        a1 = 0.d0
        a2 = 0.d0
        a3 = 0.d0
        etas = r8vide()
        goto 9999
    endif
!
! -- LECTURE DES CARACTERISTIQUES THERMOELASTIQUES
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                nbres, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
!
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
!
    nomres(1) = 'SY'
    nomres(2) = 'D_SIGM_EPSI'
    call rcvalb(fami, kpg, spt, poum, mate,&
                ' ', 'ECRO_LINE', 0, ' ', 0.d0,&
                nbres, nomres, valres, icodre, 1)
    sy = valres(1)
    gamma = - valres(2)/e
    wy = sy**2 / (2*e)
!
! -- CALCUL DES DEFORMATIONS EN PRESENCE DE CONTRAINTES PLANES
!
    if (cplan) then
        coplan = - nu/(1.d0-nu)
        epsp(3) = coplan * (epsp(1)+epsp(2))
        epsd(3) = coplan * (epsd(1)+epsd(2))
    endif
!
! ======================================================================
!                CALCUL DES DEFORMATIONS POUR LINEARISATION
! ======================================================================
!
!    ETAT MECANIQUE EN T-
!
    dm = vim(1)
    dtau = min(1+gamma/2, dm+tau)
    gm = wy * ((1+gamma)/(1+gamma-dm))**2
    gtau = wy * ((1+gamma)/(1+gamma-dtau))**2
    wrel = (gtau - gm)/tau
    s = gm / wrel
!
!    COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
!
    trepsp = epsp(1)+epsp(2)+epsp(3)
    trepsd = epsd(1)+epsd(2)+epsd(3)
    do 60 k = 1, ndimsi
        sigelp(k) = lambda*trepsp*kron(k) + deuxmu*epsp(k)
        sigeld(k) = lambda*trepsd*kron(k) + deuxmu*epsd(k)
60  end do
    p0 = 0.5d0 * ddot(ndimsi,epsp,1,sigelp,1) / wrel
    p1 = 1.0d0 * ddot(ndimsi,epsp,1,sigeld,1) / wrel
    p2 = 0.5d0 * ddot(ndimsi,epsd,1,sigeld,1) / wrel
!
!
!    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE
    call zerop2(p1/p2, (p0-s-tau)/p2, rac, nrac)
!
!    PAS DE SOLUTION : POINT LE PLUS PROCHE
    if (nrac .eq. 0) then
        etas = 0.d0
!
!    UNE OU DEUX SOLUTIONS : ON LINEARISE AUTOUR DES DEUX
    else if (nrac.eq.1) then
        eta = rac(1)
        a1 = 2*p2*eta+p1
        a0 = tau - a1*eta
    else
        eta = rac(1)
        a1 = 2*p2*eta+p1
        a0 = tau - a1*eta
        eta = rac(2)
        a3 = 2*p2*eta+p1
        a2 = tau - a3*eta
    endif
!
9999  continue
end subroutine
