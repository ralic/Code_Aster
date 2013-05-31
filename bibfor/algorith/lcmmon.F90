subroutine lcmmon(fami, kpg, ksp, comp, nbcomm,&
                  cpmono, nmat, nvi, vini, x,&
                  dtime, pgl, mod, coeft, neps,&
                  epsd, detot, coel, dvin, nfs,&
                  nsg, toutms, hsr, itmax, toler,&
                  iret)
! aslint: disable=W1306,W1504,W1504
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/calsig.h'
    include 'asterfort/caltau.h'
    include 'asterfort/lcgrla.h'
    include 'asterfort/lcmmlc.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/lcopil.h'
    include 'asterfort/lcrksg.h'
    include 'asterfort/pmat.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dscal.h'
    integer :: kpg, ksp, nmat, nbcomm(nmat, 3), nvi, itmax, iret, nfs, nsg, neps
    real(kind=8) :: vini(*), dvin(*), x, dtime, coeft(nmat), coel(nmat)
    real(kind=8) :: sigi(6), epsd(neps), detot(neps), pgl(3, 3), toler
    real(kind=8) :: hsr(nsg, nsg, 1)
    character(len=*) :: fami
    character(len=16) :: comp(*)
! person_in_charge: jean-michel.proix at edf.fr
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
! ======================================================================
!       IN FAMI     :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP   :  NUMERO DU (SOUS)POINT DE GAUSS
!          COMP     :  NOM DU MODELE DE COMPORTEMENT
!           MOD     :  TYPE DE MODELISATION
!           IMAT    :  ADRESSE DU MATERIAU CODE
!         NBCOMM :  NOMBRE DE COEF MATERIAU PAR FAMILLE
!         CPMONO :  NOMS DES LOIS MATERIAU PAR FAMILLE
!           PGL   : MATRICE DE PASSAGE GLOBAL LOCAL
!           NVI     :  NOMBRE DE VARIABLES INTERNES
!           VINI    :  VARIABLES INTERNES A T
!           X       :  INTERVALE DE TEMPS ADAPTATIF
!           DTIME   :  INTERVALE DE TEMPS
!              :  COEFFICIENTS MATERIAU INELASTIQUE A T
!           EPSD    :  DEFORMATION TOTALE A T
!           DETOT   :  INCREMENT DE DEFORMATION TOTALE
!     OUT:
!           DVIN    :  DERIVEES DES VARIABLES INTERNES A T
! INTEGRATION DES LOIS MONOCRISTALLINES PAR UNE METHODE DE RUNGE KUTTA
!
!     CETTE ROUTINE FOURNIT LA DERIVEE DE L ENSEMBLE DES VARIABLES
!     INTERNES DU MODELE
!
!     ------------------------------------------------------------------
    character(len=8) :: mod
    character(len=16) :: nomfam
    character(len=24) :: cpmono(5*nmat+1)
    real(kind=8) :: dt, dy(6+nsg), expbp(nsg), crit, sgns, q(3, 3), evi(6)
    real(kind=8) :: devi(6), mus(6), ng(3), taus, dgamma, dalpha, dp, rp
    real(kind=8) :: yd(6+nsg)
    real(kind=8) :: fkooh(6, 6), materf(nmat*2), msns(3, 3), gamsns(3, 3), lg(3)
    real(kind=8) :: toutms(nfs, nsg, 6), fp(3, 3), fp1(3, 3), deps(6), depsdt
    integer :: itens, nbfsys, i, nuvi, ifa, nbsys, is, nsfa, nsfv
    common /deps6/depsdt
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ------------------------------------------------------------------
! --  VARIABLES INTERNES
!
    call r8inir(9, 0.d0, gamsns, 1)
    call r8inir(nsg, 0.d0, dy, 1)
    do 5 itens = 1, 6
        evi(itens) = vini(itens)
        devi(itens) = 0.d0
 5  end do
!
    call dcopy(nmat, coeft, 1, materf(nmat+1), 1)
    call dcopy(nmat, coel, 1, materf(1), 1)
!
!     CALCUL DU NOMBRE TOTAL DE SYSTEMES DE GLISSEMENT
    nbfsys=nbcomm(nmat,2)
    nbsyst=0
    do 10 ifa = 1, nbfsys
        nomfam=cpmono(5*(ifa-1)+1)
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ng, lg, 0, q)
        nbsyst=nbsyst+nbsys
10  end do
!
    if (coeft(nbcomm(1,1)) .ge. 4) then
!         KOCKS-RAUCH ET DD_CFC : VARIABLE PRINCIPALE=DENSITE DISLOC
        call assert(nbcomm(nmat, 2).eq.1)
        do 102 i = 1, nbsyst
            yd(6+i)=vini(6+3*(i-1)+1)
102      continue
    else
!        AUTRES COMPORTEMENTS MONOCRISTALLINS
        do 103 i = 1, nbsyst
            yd(6+i)=vini(6+3*(i-1)+2)
103      continue
    endif
!
!
!     INVERSE DE L'OPERATEUR D'ELASTICITE DE HOOKE
    if (coel(nmat) .eq. 0) then
        call lcopil('ISOTROPE', mod, coel, fkooh)
    else if (coel(nmat).eq.1) then
        call lcopil('ORTHOTRO', mod, coel, fkooh)
    endif
!
    if (gdef .eq. 1) then
        call lcrksg(comp, nvi, vini, epsd, detot,&
                    nmat, coel, sigi)
        call lcgrla(detot, deps)
        call dscal(3, sqrt(2.d0), deps(4), 1)
    else
        call calsig(fami, kpg, ksp, evi, mod,&
                    comp, vini, x, dtime, epsd,&
                    detot, nmat, coel, sigi)
        call dcopy(6, detot, 1, deps, 1)
    endif
    depsdt=sqrt(ddot(6,deps,1,deps,1)/1.5d0)/dtime
    nbfsys=nbcomm(nmat,2)
!
    nuvi=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
!     NSFA : debut de la famille IFA dans DY et YD, YF
    nsfa=6
!
    do 6 ifa = 1, nbfsys
!
        nomfam=cpmono(5*(ifa-1)+1)
!
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ng, lg, 0, q)
!
        do 7 is = 1, nbsys
!
!           CALCUL DE LA SCISSION REDUITE =
!           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
!           TAU      : SCISSION REDUITE TAU=SIG:MUS
!
            call caltau(comp, ifa, is, sigi, fkooh,&
                        nfs, nsg, toutms, taus, mus,&
                        msns)
!
!           CALCUL DE L'ECOULEMENT SUIVANT LE COMPORTEMENT
!           ECOULEMENT VISCOPLASTIQUE:
!           ROUTINE COMMUNE A L'IMPLICITE (PLASTI-LCPLNL)
!           ET L'EXPLICITE (NMVPRK-GERPAS-RK21CO-RDIF01)
!           CAS IMPLICITE : IL FAUT PRENDRE EN COMPTE DTIME
!           CAS EXPLICITE : IL NE LE FAUT PAS (ON CALCULE DES VITESSES)
!           D'OU :
            dt=-1.d0
!
            call lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                        hsr, nsfv, nsfa, ifa, nbsys,&
                        is, dt, nvi, vini, yd,&
                        dy, itmax, toler, materf, expbp,&
                        taus, dalpha, dgamma, dp, crit,&
                        sgns, rp, iret)
!
            if (iret .gt. 0) then
                goto 9999
            endif
!
            nuvi=nuvi+3
!
            dvin(nuvi-2)=dalpha
            dvin(nuvi-1)=dgamma
            dvin(nuvi )=dp
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
! --    DERIVEES DES VARIABLES INTERNES
!
    do 30 itens = 1, 6
        dvin(itens)= devi(itens)
30  end do
    if (gdef .eq. 1) then
        call dcopy(9, vini(nvi-3-18+1 ), 1, fp, 1)
        call pmat(3, gamsns, fp, fp1)
        call dcopy(9, fp1, 1, dvin(nvi-3-18+1 ), 1)
    endif
!
9999  continue
end subroutine
