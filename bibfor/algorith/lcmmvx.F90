subroutine lcmmvx(sigf, vin, nmat, materf, nbcomm,&
                  cpmono, pgl, nvi, hsr, nfs,&
                  nsg, toutms, timed, timef, deps,&
                  seuil)
    implicit none
! ----------------------------------------------------------------------
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
! TOLE CRS_1404
!     ----------------------------------------------------------------
!     MONOCRISTAL  :  CALCUL DU SEUIL POUR MONOCRISTAL
!     ----------------------------------------------------------------
!     IN  FAMI   :  FAMILLE DE POINTS DE GAUSS
!     IN  KPG    :  NUMERO DU POINT DE GAUSS
!     IN  KSP    :  NUMERO DU SOUS-POINT DE GAUSS
!     IN  SIGF   :  CONTRAINTE
!     IN  VIN    :  VARIABLES INTERNES = ( X1 X2 P )
!     IN  NMAT   :  DIMENSION MATER
!     IN  MATERF :  COEFFICIENTS MATERIAU A TEMP
!         COMP   :  NOM COMPORTEMENT
!         NBCOMM :  INCIDES DES COEF MATERIAU
!         CPMONO :  NOM DES COMPORTEMENTS
!         PGL    :  MATRICE DE PASSAGE
!         NR     :  DIMENSION DECLAREE DRDY
!         NVI    :  NOMBRE DE VARIABLES INTERNES
!         HSR    :  MATRICE D'INTERACTION
!         TOUTMS :  TENSEURS D'ORIENTATION
!     OUT SEUIL  :  SEUIL  ELASTICITE
!     ----------------------------------------------------------------
    include 'asterfort/lcmmfe.h'
    include 'asterfort/lcmmfi.h'
    include 'asterfort/lcmmsg.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    integer :: nmat, nvi, nsfa, nsfv, iexp, nfs, nsg
    integer :: nbfsys, i, nuvi, ifa, nbsys, is
    integer :: nbcomm(nmat, 3), iret
    real(kind=8) :: sigf(6), vin(nvi), rp, hsr(nsg, nsg), deps(6)
    real(kind=8) :: materf(nmat*2), seuil, dt, dy(nvi), alpham
    real(kind=8) :: ms(6), ng(3), q(3, 3), timed, timef, lg(3), depsdt
    real(kind=8) :: taus, dgamma, dalpha, dp, expbp(nsg), depst(6)
    real(kind=8) :: pgl(3, 3), crit, sgns, toutms(nfs, nsg, 6), gammam
    character(len=24) :: cpmono(5*nmat+1)
    character(len=16) :: nomfam, necoul, necris
    common /deps6/depsdt
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!
    seuil=-1.d0
    dt=timef-timed
    nbfsys=nbcomm(nmat,2)
    call r8inir(nvi, 0.d0, dy, 1)
    call dcopy(6, deps, 1, depst, 1)
    depsdt=sqrt(ddot(6,depst,1,depst,1)/1.5d0)/dt
!
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
!     NSFA : debut de la famille IFA dans DY et YD, YF
    nsfa=6
    do 6 ifa = 1, nbfsys
!
        nomfam=cpmono(5*(ifa-1)+1)
        necoul=cpmono(5*(ifa-1)+3)
        necris=cpmono(5*(ifa-1)+4)
!
        call lcmmsg(nomfam, nbsys, 0, pgl, ms,&
                    ng, lg, 0, q)
!
        if (nbsys .eq. 0) call u2mess('F', 'ALGORITH_70')
!
        do 7 is = 1, nbsys
!
            nuvi=nsfv+3*(is-1)
            alpham=vin(nuvi+1)
            gammam=vin(nuvi+2)
!
!           CALCUL DE LA SCISSION REDUITE =
!           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
!           TAU      : SCISSION REDUITE TAU=SIG:MS
            do 101 i = 1, 6
                ms(i)=toutms(ifa,is,i)
101          continue
!
            taus=0.d0
            do 10 i = 1, 6
                taus=taus+sigf(i)*ms(i)
10          continue
!
!           ECROUISSAGE ISOTROPE
!
            if (necoul .ne. 'MONO_DD_KR') then
                iexp=0
                if (is .eq. 1) iexp=1
                call lcmmfi(materf(nmat+1), ifa, nmat, nbcomm, necris,&
                            is, nbsys, vin, nsfv, dy(nsfa+1),&
                            nfs, nsg, hsr, iexp, expbp,&
                            rp)
            endif
!
!           ECOULEMENT VISCOPLASTIQUE
!
            decal=nsfv
            call lcmmfe(taus, materf(nmat+1), materf, ifa, nmat,&
                        nbcomm, necoul, is, nbsys, vin,&
                        dy(nsfa+1), rp, alpham, gammam, dt,&
                        dalpha, dgamma, dp, crit, sgns,&
                        nfs, nsg, hsr, iret)
!
            if (iret .gt. 0) then
                dp=1.d0
            endif
            if (dp .gt. 0.d0) then
                seuil=1.d0
                goto 9999
            endif
!
 7      continue
!
        nsfa=nsfa+nbsys
        nsfv=nsfv+3*nbsys
 6  end do
9999  continue
end subroutine
