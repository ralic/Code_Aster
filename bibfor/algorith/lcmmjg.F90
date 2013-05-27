subroutine lcmmjg(comp, nmat, nbcomm, cpmono, hsr,&
                  dt, nvi, vind, yd, dy,&
                  itmax, toler, materf, sigf, fkooh,&
                  nfs, nsg, toutms, pgl, msnst,&
                  gamsns, dfpdga, iret)
    implicit none
! TOLE CRP_21 CRS_1404
! ----------------------------------------------------------------------
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
!       ----------------------------------------------------------------
!     MONOCRISTAL : POUR LE CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE
!                   CALCUL EN GDEF DE Gamma.Ms*Ns et dFp/Dgamma
!       IN
!           COMP   :  NOM COMPORTEMENT
!           NMAT   :  DIMENSION MATER
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           CPMONO :  NOM DES COMPORTEMENTS
!           HSR    :  MATRICE D'INTERACTION
!           DT     :  DELTA T
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           YD     :  VARIABLES A T
!           DY     :  SOLUTION
!           ITMAX  :  ITER_INTE_MAXI
!           TOLER  :  RESI_INTE_RELA
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           SIGF   :  CONTRAINTES A T+DT
!           FKOOH  :  INVERSE TENSEUR HOOKE
!           PGL    :  MATRICE DE PASSAGE
!           TOUTMS :  TENSEURS D'ORIENTATION
!       OUT MSNST  :  Ms*Ns pour chaque systele de glissement
!           GAMSNS :  Somme de GammaS*MS*NS
!           DFPDGA :  derivee de Fp / dGamma_S pour tous les systemes S
!       OUT IRET   :  CODE RETOUR
!       ----------------------------------------------------------------
    include 'asterfort/assert.h'
    include 'asterfort/caldfp.h'
    include 'asterfort/caltau.h'
    include 'asterfort/lcmmlc.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    integer :: nvi, nmat, nbfsys, nsfa, nsfv, nbsys, is, nfs, nsg
    integer :: nbcomm(nmat, 3), ifa, iret, itmax
    real(kind=8) :: vind(*), dy(*), materf(nmat*2)
    real(kind=8) :: pgl(3, 3), toutms(nfs, nsg, 6), hsr(nsg, nsg), gamsns(3, 3)
    real(kind=8) :: dt, fkooh(6, 6), sigf(6), toler, taus, dp, crit, sgns, rp
    real(kind=8) :: q(3, 3), mus(6), ns(3), ms(3), dfpdga(3, 3, nsg)
    real(kind=8) :: expbp(nsg), yd(*), msnst(3, 3, nsg), dalpha, dgamma
    character(len=16) :: nomfam, comp(*)
    character(len=24) :: cpmono(5*nmat+1)
!     ----------------------------------------------------------------
!
!     NSFA : debut de la famille IFA dans DY et YD, YF
    nsfa=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
    nbfsys=nbcomm(nmat,2)
!     PROGRAMMATION VALABLE POUR UNE SEULE FAMILLE DE SYSTEMES
    call assert(nbfsys.eq.1)
    do 16 ifa = 1, nbfsys
!        Calcul preliminaire de somme(dgamma*ms*ns)
        call r8inir(9, 0.d0, gamsns, 1)
        nomfam=cpmono(5*(ifa-1)+1)
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ns, ms, 0, q)
        do 17 is = 1, nbsys
            call caltau(comp, ifa, is, sigf, fkooh,&
                        nfs, nsg, toutms, taus, mus,&
                        msnst(1, 1, is))
            call lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                        hsr, nsfv, nsfa, ifa, nbsys,&
                        is, dt, nvi, vind, yd,&
                        dy, itmax, toler, materf, expbp,&
                        taus, dalpha, dgamma, dp, crit,&
                        sgns, rp, iret)
            call daxpy(9, dgamma, msnst(1, 1, is), 1, gamsns,&
                       1)
17      continue
        do 19 is = 1, nbsys
            call caldfp(msnst(1, 1, is), gamsns, dfpdga(1, 1, is), iret)
19      continue
        nsfa=nsfa+nbsys
        nsfv=nsfv+nbsys*3
16  end do
end subroutine
