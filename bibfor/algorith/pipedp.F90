subroutine pipedp(kpg, ksp, ndim, typmod, mate,&
                  epsm, sigm, vim, epsp, epsd,&
                  elgeom, a0, a1)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterc/matfpe.h'
    include 'asterfort/betfpp.h'
    include 'asterfort/betmat.h'
    include 'asterfort/zerop2.h'
    include 'blas/ddot.h'
    include 'blas/dnrm2.h'
    character(len=8) :: typmod(*)
    integer :: ndim, mate, kpg, ksp
    real(kind=8) :: epsp(6), epsd(6)
    real(kind=8) :: epsm(6), vim(2), sigm(6), a0, a1
    real(kind=8) :: elgeom(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS)
!
! LOI DE COMPORTEMENT BETON_DOUBLE_DP
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  TYPMOD : TYPE DE MODELISATION
! IN  MATE   : MATERIAU CODE
! IN  VIM    : VARIABLES INTERNES EN T-
! IN  EPSM   : DEFORMATIONS EN T-
! IN  SIGM   : CONTRAINTES EN T-
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! IN  KPG    : NUMERO DU POINT DE GAUSS
! IN  KPG    : NUMERO DU SOUS-POINT DE GAUSS
! IN  ELGEOM : TABLEAUX DES ELEMENTS GEOMETRIQUES SPECIFIQUES AUX
!               LOIS DE COMPORTEMENT (DIMENSION MAXIMALE FIXEE EN DUR)
! OUT A0     : LINEARISATION DU CRITERE : FEL = A0 + A1*ETA
! OUT A1     : IDEM A0
!
! ----------------------------------------------------------------------
!
    integer :: ndimsi, k, nrac1, nrac2
    logical :: trac, comp, notrac, nocomp
    real(kind=8) :: trsigp, trsigd, sigelp(6), sigeld(6)
    real(kind=8) :: eps1(6), eps2(6), pp(6), dd(6)
    real(kind=8) :: d1, d2, g1, g2, g3, g4
    real(kind=8) :: kron(6)
    real(kind=8) :: p0, p1, p2, q0, q1, q2, eta, eta1, eta2
    real(kind=8) :: rac1(2), rac2(2)
    real(kind=8) :: e, nu, lambda, deuxmu
    real(kind=8) :: fc, ft, beta
    real(kind=8) :: a, b, c, d
    real(kind=8) :: un, d23, d13, raci2, deux, trois, neuf
    parameter   ( un   =  1.d0   )
    parameter   ( deux =  2.d0   )
    parameter   ( trois = 3.d0   )
    parameter   ( neuf = 9.d0   )
    parameter   ( d23  =  .66666666666666D0 )
    parameter   ( d13  =  .33333333333333D0 )
!
!
    integer :: ndt, ndi, nr, nvi, nmat
    parameter   ( nmat = 90     )
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: pc, pt, kuc, kut, ke, tbid, rbid, fcp, ftp
    character(len=8) :: mod, fami
    character(len=3) :: matcst
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
    call matfpe(-1)
!
! --  OPTION ET MODELISATION
!
    ndimsi = 2*ndim
!
! --  RECUPERATION MATERIAU
    raci2 = sqrt (deux)
!
    tbid = 0.d0
    fami = 'RIGI'
    mod = typmod(1)
    call betmat(fami, kpg, ksp, mod, mate,&
                nmat, tbid, tbid, materd, materf,&
                matcst, ndt, ndi, nr, nvi)
!
    e = materd(1,1)
    nu = materd(2,1)
    beta = materd(3,2)
!
    pc = vim(1)
    pt = vim(2)
    call betfpp(materf, nmat, elgeom, pc, pt,&
                3, fc, ft, rbid, rbid,&
                kuc, kut, ke)
!
    fcp = materf(1,2)
    ftp = materf(2,2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
    a = raci2 * (beta - un) / (deux * beta - un)
    b = raci2 / trois * beta / (deux * beta - un)
    c = raci2
    d = deux * raci2 / trois
!
    nocomp = .false.
    if (pc .gt. kuc) nocomp = .true.
    notrac = .false.
    if (pt .gt. kut) notrac = .true.
!
! ======================================================================
!                CALCUL DES DEFORMATIONS POUR LINEARISATION
! ======================================================================
!
!     COEFFICIENTS DE LA FORME QUADRATIQUE DU CRITERE
    do 10 k = 1, ndimsi
        sigelp(k) = sigm(k) + (lambda*kron(k)+deuxmu)* (epsp(k)-epsm( k))
        sigeld(k) = (lambda*kron(k)+deuxmu)*epsd(k)
10  end do
!
    trsigp = sigelp(1) + sigelp(2) + sigelp(3)
    trsigd = sigeld(1) + sigeld(2) + sigeld(3)
!
    do 20 k = 1, ndimsi
        pp(k) = sigelp(k)-d13*trsigp*kron(k)
        dd(k) = sigeld(k)-d13*trsigd*kron(k)
20  end do
!
!     CRITERE DE TRACTION
    p0 = d13*ddot(ndimsi,pp,1,pp,1)-(d*ft)**2+d23*c*d*ft*trsigp -c**2/neuf*trsigp**2
    p1 = d13*ddot(ndimsi,pp,1,dd,1)+ d13*c*d*ft*trsigd -c**2/neuf*trsigp*trsigd
    p2 = d13*ddot(ndimsi,dd,1,dd,1)-c**2/neuf*trsigd**2
!
    p0=p0/(d*ftp)**2
    p1=p1/(d*ftp)**2
    p2=p2/(d*ftp)**2
!
!    CRITERE DE COMPRESSION
!
    q0 = d13*ddot(ndimsi,pp,1,pp,1)-(b*fc)**2+d23*a*b*fc*trsigp -a**2/neuf*trsigp**2
    q1 = d13*ddot(ndimsi,pp,1,dd,1)+ d13*a*b*fc*trsigd -a**2/neuf*trsigp*trsigd
    q2 = d13*ddot(ndimsi,dd,1,dd,1)-a**2/neuf*trsigd**2
!
    q0=q0/(b*fcp)**2
    q1=q1/(b*fcp)**2
    q2=q2/(b*fcp)**2
!
!    RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE (TRACTION)
    call zerop2(2*p1/p2, p0/p2, rac1, nrac1)
!
!     RECHERCHE DES INTERSECTIONS ELLIPSE / DROITE (COMPRESSION)
    call zerop2(2*q1/q2, q0/q2, rac2, nrac2)
!
!     CALCUL DES COEFFICIENTS LORSQUE LES DEUX CRITERES
!     SONT COMPLETEMENT ECROUIS
!     ---------------------------------------------------------
    if (notrac .and. nocomp) then
        a0 = 0.d0
        a1 = 0.d0
    endif
!
!     CALCUL DES COEFFICIENTS LORSQUE LE CRITERE DE TRACTION
!     EST COMPLETEMENT ECROUI
!     ---------------------------------------------------------
    if (notrac) then
        if (nrac2 .ne. 0) then
            do 30 k = 1, ndimsi
                eps1(k) = epsp(k) + rac2(1) * epsd(k) - epsm(k)
                eps2(k) = epsp(k) + rac2(2) * epsd(k) - epsm(k)
30          continue
            d1 = dnrm2(ndimsi, eps1,1)
            d2 = dnrm2(ndimsi, eps2,1)
            if (d1 .le. d2) then
                eta = rac2(1)
            else
                eta = rac2(2)
            endif
        else
            eta = -q1/q2
        endif
        a0 = - q2 * eta**2 + q0
        a1 = 2*(eta*q2+q1)
    endif
!
!     CALCUL DES COEFFICIENTS LORSQUE LE CRITERE DE COMPRESSION
!     EST COMPLETEMENT ECROUI
!     ---------------------------------------------------------
    if (nocomp) then
        if (nrac1 .ne. 0) then
            do 40 k = 1, ndimsi
                eps1(k) = epsp(k) + rac1(1) * epsd(k) - epsm(k)
                eps2(k) = epsp(k) + rac1(2) * epsd(k) - epsm(k)
40          continue
            d1 = dnrm2(ndimsi, eps1,1)
            d2 = dnrm2(ndimsi, eps2,1)
            if (d1 .le. d2) then
                eta = rac1(1)
            else
                eta = rac1(2)
            endif
        else
            eta = -p1/p2
        endif
        a0 = - p2 * eta**2 + p0
        a1 = 2*(eta*p2+p1)
    endif
!
!     CALCUL DES COEFFICIENTS LORSQU'AUCUN DES DEUX CRITERES
!     N'EST COMPLETEMENT ECROUI
!     ---------------------------------------------------------
!
    if (.not.nocomp .and. .not.notrac) then
!        LA DROITE COUPE LE CRITERE DE TRACTION
        if (nrac1 .ne. 0) then
            g1 = q0 + deux * rac1(1)*q1 + rac1(1)**2*q2
            g2 = q0 + deux * rac1(2)*q1 + rac1(2)**2*q2
            trac = .true.
            if ((g1.lt.0) .and. (g2.lt.0)) then
                do 50 k = 1, ndimsi
                    eps1(k) = epsp(k) + rac1(1) * epsd(k) - epsm(k)
                    eps2(k) = epsp(k) + rac1(2) * epsd(k) - epsm(k)
50              continue
                d1 = dnrm2(ndimsi, eps1,1)
                d2 = dnrm2(ndimsi, eps2,1)
                if (d1 .le. d2) then
                    eta1 = rac1(1)
                else
                    eta1 = rac1(2)
                endif
            else if ((g1.lt.0).and.(g2.gt.0)) then
                eta1 = rac1(1)
            else if ((g1.gt.0).and.(g2.lt.0)) then
                eta1 = rac1(2)
            else
                eta1 = -p1/p2
                trac = .false.
            endif
        else
            eta1 = -p1/p2
            trac = .false.
        endif
!
!        LA DROITE COUPE LE CRITERE DE COMPRESSION
        if (nrac2 .ne. 0) then
            g3 = p0 + deux * rac2(1)*p1 + rac2(1)**2*p2
            g4 = p0 + deux * rac2(2)*p1 + rac2(2)**2*p2
            comp = .true.
            if ((g3.lt.0) .and. (g4.lt.0)) then
                do 60 k = 1, ndimsi
                    eps1(k) = epsp(k) + rac2(1) * epsd(k) - epsm(k)
                    eps2(k) = epsp(k) + rac2(2) * epsd(k) - epsm(k)
60              continue
                d1 = dnrm2(ndimsi, eps1,1)
                d2 = dnrm2(ndimsi, eps2,1)
                if (d1 .le. d2) then
                    eta2 = rac2(1)
                else
                    eta2 = rac2(2)
                endif
            else if ((g3.lt.0).and.(g4.gt.0)) then
                eta2 = rac2(1)
            else if ((g3.gt.0).and.(g4.lt.0)) then
                eta2 = rac2(2)
            else
                eta2 = -q1/q2
                comp = .false.
            endif
        else
            eta2 = -q1/q2
            comp = .false.
        endif
!
        if (trac) then
            if (comp) then
                do 70 k = 1, ndimsi
                    eps1(k) = epsp(k) + eta1 * epsd(k) - epsm(k)
                    eps2(k) = epsp(k) + eta2 * epsd(k) - epsm(k)
70              continue
                d1 = dnrm2(ndimsi, eps1,1)
                d2 = dnrm2(ndimsi, eps2,1)
                if (d1 .le. d2) then
                    eta = eta1
                    a0 = - p2 * eta**2 + p0
                    a1 = 2*(eta*p2+p1)
                else
                    eta = eta2
                    a0 = - q2 * eta**2 + q0
                    a1 = 2*(eta*q2+q1)
                endif
            else
                eta = eta1
                a0 = - p2 * eta**2 + p0
                a1 = 2*(eta*p2+p1)
            endif
        else
            if (comp) then
                eta = eta2
                a0 = - q2 * eta**2 + q0
                a1 = 2*(eta*q2+q1)
            else
                do 80 k = 1, ndimsi
                    eps1(k) = epsp(k) + eta1 * epsd(k) - epsm(k)
                    eps2(k) = epsp(k) + eta2 * epsd(k) - epsm(k)
80              continue
                d1 = dnrm2(ndimsi, eps1,1)
                d2 = dnrm2(ndimsi, eps2,1)
                if (d1 .le. d2) then
                    eta = eta1
                    a0 = - p2 * eta**2 + p0
                    a1 = 2*(eta*p2+p1)
                else
                    eta = eta2
                    a0 = - q2 * eta**2 + q0
                    a1 = 2*(eta*q2+q1)
                endif
            endif
        endif
    endif
!
    call matfpe(1)
!
end subroutine
