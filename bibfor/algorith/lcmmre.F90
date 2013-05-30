subroutine lcmmre(typmod, nmat, materd, materf, comp,&
                  nbcomm, cpmono, pgl, nfs, nsg,&
                  toutms, hsr, nr, nvi, vind,&
                  itmax, toler, timed, timef, yd,&
                  yf, deps, dy, r, iret)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jean-michel.proix at edf.fr
! TOLE CRP_21 CRS_1404
!       ----------------------------------------------------------------
!     MONOCRISTAL  : CALCUL DES RESIDUS DU SYSTEME NL A RESOUDRE = R(DY)
!                    CF. R5.03.11
!                    DY =  DSIG (DBETA PAR SYSTEME)
!                    Y  =  SIG   (BETA  PAR SYSTEME)
!                    R  = ( R1  R2   )
!                    ATTENTION IL FAUT CALCULER -R
!
!     IN  TYPMOD :  TYPE DE MODELISATION
!         NMAT   :  DIMENSION MATER
!         MATERD :  COEFFICIENTS MATERIAU A T
!         MATERF :  COEFFICIENTS MATERIAU A T+DT
!         COMP   :  NOM COMPORTEMENT
!         NBCOMM :  INCIDES DES COEF MATERIAU
!         CPMONO :  NOM DES COMPORTEMENTS
!         PGL    :  MATRICE DE PASSAGE
!         TOUTMS :  TENSEURS D'ORIENTATION
!         HSR    :  MATRICE D'INTERACTION
!         NR     :  DIMENSION DECLAREE DRDY
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!         ITMAX  :  ITER_INTE_MAXI
!         TOLER  :  RESI_INTE_RELA
!         TIMED  :  ISTANT PRECEDENT
!         TIMEF  :  INSTANT ACTUEL
!         YD     :  VARIABLES A T       = ( SIGD BETAD )
!         YF     :  VARIABLES A T + DT  = ( SIGF BETAF )
!         DEPS   :  INCREMENT DE DEFORMATION OU GRADIENT DF
!         DY     :  SOLUTION  =  ( DSIG DBETA )
!         NR     :  DIMENSION DECLAREE DRDY
!     OUT R      :  RESIDU DU SYSTEME NL A T + DT
!         IRET   :  CODE RETOUR
!     ----------------------------------------------------------------
    include 'asterfort/calcfe.h'
    include 'asterfort/caltau.h'
    include 'asterfort/lcdive.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcgrla.h'
    include 'asterfort/lcmmlc.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/lcopil.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dscal.h'
    integer :: ndt, ndi, nmat, nr, nvi, nsfv, iret
    integer :: nbfsys, ifa, nbsys, is, itmax, nfs, nsg
    integer :: nbcomm(nmat, 3), nsfa, ifl, nuecou
!
    real(kind=8) :: dkooh(6, 6), fkooh(6, 6), timed, timef
    real(kind=8) :: sigf(6), sigd(6), msns(3, 3), pgl(3, 3), dgamm1
    real(kind=8) :: deps(*), depse(6), devi(6), dt
    real(kind=8) :: epsed(6), epsgl(6), h1sigf(6), vind(*)
    real(kind=8) :: materd(nmat*2), materf(nmat*2), epsef(6)
    real(kind=8) :: mus(6), ng(3), taus, dgamma, dalpha, dp, rp, depsdt
    real(kind=8) :: r(nr), dy(nr), yd(nr), yf(nr), toler, fe(3, 3)
    real(kind=8) :: toutms(nfs, nsg, 6), hsr(nsg, nsg), q(3, 3), lg(3)
    real(kind=8) :: gamsns(3, 3), fp(3, 3), depst(6)
    real(kind=8) :: crit, sgns, expbp(nsg)
    character(len=8) :: typmod
    character(len=16) :: comp(*), nomfam
    character(len=24) :: cpmono(5*nmat+1)
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
    common /tdim/   ndt , ndi
    common /deps6/depsdt
!     ----------------------------------------------------------------
!
    dt=timef-timed
!     INVERSE DE L'OPERATEUR D'ELASTICITE DE HOOKE
    if (materf(nmat) .eq. 0) then
        call lcopil('ISOTROPE', typmod, materd(1), dkooh)
        call lcopil('ISOTROPE', typmod, materf(1), fkooh)
    else if (materf(nmat).eq.1) then
        call lcopil('ORTHOTRO', typmod, materd(1), dkooh)
        call lcopil('ORTHOTRO', typmod, materf(1), fkooh)
    endif
!
    call r8inir(9, 0.d0, gamsns, 1)
    call lceqvn(ndt, yf(1), sigf)
    call r8inir(6, 0.d0, devi, 1)
!
!     POUR DD_CC
    if (gdef .eq. 1) then
        call lcgrla(deps, depst)
        call dscal(3, sqrt(2.d0), depst(4), 1)
    else
        call dcopy(6, deps, 1, depst, 1)
    endif
    depsdt=sqrt(ddot(6,depst,1,depst,1)/1.5d0)/dt
!
    nbfsys=nbcomm(nmat,2)
    iret=0
!
!
!     NSFA : debut de la famille IFA dans DY et YD, YF
    nsfa=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
!
    do 6 ifa = 1, nbfsys
!
        ifl=nbcomm(ifa,1)
        nuecou=nint(materf(nmat+ifl))
        nomfam=cpmono(5*(ifa-1)+1)(1:16)
!
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ng, lg, 0, q)
!
        do 7 is = 1, nbsys
!           CALCUL DE LA SCISSION REDUITE
            call caltau(comp, ifa, is, sigf, fkooh,&
                        nfs, nsg, toutms, taus, mus,&
                        msns)
!           CALCUL DE L'ECOULEMENT SUIVANT LE COMPORTEMENT
            call lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                        hsr, nsfv, nsfa, ifa, nbsys,&
                        is, dt, nvi, vind, yd,&
                        dy, itmax, toler, materf, expbp,&
                        taus, dalpha, dgamma, dp, crit,&
                        sgns, rp, iret)
!
            if (iret .gt. 0) then
                goto 9999
            endif
!
            if (nuecou .ge. 4) then
!           POUR LES LOIS DD_* ALPHA représente la variable principale
                r(nsfa+is)=-(dy(nsfa+is)-dalpha)
            else
                dgamm1=dy(nsfa+is)
                r(nsfa+is)=-(dgamm1-dgamma)
            endif
!
            if (gdef .eq. 0) then
                call daxpy(6, dgamma, mus, 1, devi,&
                           1)
            else
                call daxpy(9, dgamma, msns, 1, gamsns,&
                           1)
            endif
 7      continue
!
        nsfa=nsfa+nbsys
        nsfv=nsfv+nbsys*3
!
 6  end do
!
    if (gdef .eq. 1) then
        call calcfe(nr, ndt, nvi, vind, deps,&
                    gamsns, fe, fp, iret)
        if (iret .gt. 0) then
            goto 9999
        endif
        call lcgrla(fe, epsgl)
        call lcprmv(fkooh, sigf, h1sigf)
        call lcdive(epsgl, h1sigf, r(1))
    else
        call lceqvn(ndt, yd(1), sigd)
        call lcprmv(dkooh, sigd, epsed)
        call lcdive(deps, devi, depse)
        call lcsove(epsed, depse, epsef)
! LA PREMIERE EQUATION EST  (HF-1)SIGF -(HD-1)SIGD -(DEPS-DEPSP)=0
        call lcprmv(fkooh, sigf, h1sigf)
        call lcdive(epsef, h1sigf, r(1))
    endif
!
9999  continue
end subroutine
