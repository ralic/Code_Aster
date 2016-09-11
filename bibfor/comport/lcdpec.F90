subroutine lcdpec(vind, nbcomm, nmat, ndt, cpmono,&
                  materf, iter, nvi, itmax, toler,&
                  pgl, nfs, nsg, toutms, hsr,&
                  dt, dy, yd, vinf, tampon,&
                  sigf, df, nr, mod,&
                  codret)
! aslint: disable=W1306,W1504
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     POST-TRAITEMENTS POUR LE MONOCRISTAL
!     DEFORMATION PLASTIQUE EQUIVALENTE CUMULEE MACROSCOPIQUE
!     RECALCUL DES 3 VARIABLES INTERNES PAR SYSTEME
!  IN VIND   :  VARIABLE INTERNES A T
!     NBCOMM :  INCIDES DES COEF MATERIAU
!     NMAT   :  DIMENSION MATER ET DE NBCOMM
!     NDT    :  NOMBRE DE CMP DE SIG (6)
!     NBCOMM :  INCIDES DES COEF MATERIAU monocristal
!     MATERF :  COEF MATERIAU
!     ITER   :  NOMBRE D ITERATIONS POUR CONVERGER
!     NVI    :  NOMBRE DE VARIABLES INTERNES
!     ITMAX  :  ITER_INTE_MAXI
!     TOLER  :  RESI_INTE_RELA
!     PGL    :  MATRICE DE PASSAGE
!     TOUTMS :  TENSEURS D'ORIENTATION monocristal
!     HSR    :  MATRICE D'INTERACTION monocristal
!     DT     :  INCREMENT DE TEMPS
!     DY     :  INCREMENT DES VARIABLES Y
!     YD     :  VARIABLES A T   = ( SIGD  VARD  )
!     TAMPON :  DONNES GEOM SUIVANT LE TE APPELANT
!     COMP   :  COMPOR - LOI ET TYPE DE DEFORMATION
!     SIGF   :  CONRIANTES DE CAUCHY (HPP) OU KIRCHHOFF (GDEF)
!     DF     :  GRADIENT DF
!     NR     :  DIMENSION DECLAREE DRDY
!     MOD    :  TYPE DE MODELISATION
!     CODRET :  CODE RETOUR
! VAR VINF   :  VARIABLES INTERNES A L'INSTANT ACTUEL
!
!     ----------------------------------------------------------------
#include "asterc/r8miem.h"
#include "asterfort/calcfe.h"
#include "asterfort/caltau.h"
#include "asterfort/lcgrla.h"
#include "asterfort/lcmcli.h"
#include "asterfort/lcmmlc.h"
#include "asterfort/lcmmro.h"
#include "asterfort/lcmmsg.h"
#include "asterfort/lcnrte.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcprmv.h"
#include "asterfort/pk2sig.h"
#include "asterfort/r8inir.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    integer :: nmat, ndt, i, j, nbcomm(nmat, 3), nbsys, ifa, is, nbfsys, itmax
    integer :: nuvi, iter, nvi, iret, ir, nr, nsfa, nsfv, ifl, nuecou, codret
    integer :: nfs, nsg, ns, indtau, iei, is3, iv, iv3
    real(kind=8) :: vind(*), vinf(*), dy(*), materf(nmat*2)
    real(kind=8) :: epseq, pgl(3, 3), mus(6), ng(3), dgamma, dp, dalpha
    real(kind=8) :: devi(6), toutms(nfs, nsg, 6), toler, hsr(nsg, nsg)
    real(kind=8) :: taus, fkooh(6, 6), msns(3, 3), yd(*), iden(3, 3)
    real(kind=8) :: crit, sgns, dt, omp(3), qm(3, 3), fp(3, 3)
    real(kind=8) :: sicl, lg(3), tampon(*), rp, tau(60)
    real(kind=8) :: pk2(6), df(3, 3), id6(6), expbp(nsg)
    real(kind=8) :: fetfe6(6), gamsns(3, 3), fe(3, 3), sigf(6), rhoirr(12), xi
    real(kind=8) :: rhosat, phisat, dz, roloop(12), fivoid(12), sdp, dps(30)
    character(len=16) :: nomfam, necoul
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
    data iden/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
    data id6/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    codret=0
    iret=0
    sicl=-r8miem()
!     CAS MONO1 : ON RECALCULE LES VARIABLES INTERNES
    call r8inir(6, 0.d0, devi, 1)
    call r8inir(3, 0.d0, omp, 1)
!
    nbfsys=nbcomm(nmat,2)
!
!     NSFA : debut de la famille IFA dans DY et YD
    nsfa=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
!
    if (nbcomm(nmat,1) .gt. 0) then
!        ROTATION RESEAU
        ir=1
        do i = 1, 3
            do j = 1, 3
                qm(i,j)=vind(nvi-19+3*(i-1)+j)+iden(i,j)
            end do
        end do
    else
        ir=0
    endif
!
    if (gdef .eq. 1) then
        if (materf(nmat) .eq. 0) then
            call lcopil('ISOTROPE', mod, materf(1), fkooh)
        else if (materf(nmat).eq.1) then
            call lcopil('ORTHOTRO', mod, materf(1), fkooh)
        endif
        call lcprmv(fkooh, sigf, fetfe6)
        call dscal(6, 2.d0, fetfe6, 1)
        call daxpy(6, 1.d0, id6, 1, fetfe6,&
                   1)
        call r8inir(9, 0.d0, gamsns, 1)
    endif
    indtau=0
    do ifa = 1, nbfsys
!
        ifl=nbcomm(ifa,1)
        nuecou=nint(materf(nmat+ifl))
        nomfam=cpmono(5*(ifa-1)+1)(1:16)
        necoul=cpmono(5*(ifa-1)+3)(1:16)
!
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ng, lg, 0, qm)
!
        if (necoul .eq. 'MONO_DD_CC_IRRA') then
            call dcopy(12, vind(nsfv+3*nbsys+1), 1, rhoirr, 1)
            irr=1
            xi=materf(nmat+ifl+23)
        else if (necoul.eq.'MONO_DD_CFC_IRRA') then
            call dcopy(12, vind(nsfv+3*nbsys+1), 1, roloop, 1)
            call dcopy(12, vind(nsfv+3*nbsys+13), 1, fivoid, 1)
            irr=2
            iei =nbcomm(ifa,3)
            rhosat=materf(nmat+iei+8)
            phisat=materf(nmat+iei+9)
            xi = materf(nmat+iei+10)
            dz = materf(nmat+iei+11)
        else
            irr=0
        endif
!
        do is = 1, nbsys
!
            call caltau(ifa, is, sigf, fkooh,&
                        nfs, nsg, toutms, taus, mus,&
                        msns)
!
            call lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                        hsr, nsfv, nsfa, ifa, nbsys,&
                        is, dt, nvi, vind, yd,&
                        dy, itmax, toler, materf, expbp,&
                        taus, dalpha, dgamma, dp, crit,&
                        sgns, rp, iret)
!
            if (iret .gt. 0) goto 999
!
            if (gdef .eq. 0) then
                do i = 1, 6
                    devi(i)=devi(i)+mus(i)*dgamma
                end do
            else
                call daxpy(9, dgamma, msns, 1, gamsns,&
                           1)
            endif
!
! STOCKAGE DES VARIABLES INTERNES PAR SYSTEME DE GLISSEMENT
!
            nuvi=nsfv+3*(is-1)+3
            vinf(nuvi-2)=vind(nuvi-2)+dalpha
            vinf(nuvi-1)=vind(nuvi-1)+dgamma
            vinf(nuvi ) =vind(nuvi)+dp
            dps(is)=dp
            if ((nuecou.eq.4) .or. (nuecou.eq.5)) then
                if (vinf(nuvi-2) .lt. 0.d0) codret=1
            endif
!
! CONTRAINTE DE CLIVAGE
            call lcmcli(nomfam, nbsys, is, pgl,&
                        sigf, sicl)
!
            call lcmmsg(nomfam, nbsys, is, pgl, mus,&
                        ng, lg, ir, qm)
            if (ir .eq. 1) then
!              ROTATION RESEAU - CALCUL DE OMEGAP
                omp(1)=omp(1)+dgamma*0.5d0*(ng(2)*lg(3)-ng(3)*lg(2))
                omp(2)=omp(2)+dgamma*0.5d0*(ng(3)*lg(1)-ng(1)*lg(3))
                omp(3)=omp(3)+dgamma*0.5d0*(ng(1)*lg(2)-ng(2)*lg(1))
            endif
!
            if (irr .eq. 1) then
                rhoirr(is)=rhoirr(is)*exp(-xi*dp)
            endif
        end do
!
        if (irr .eq. 1) then
            call dcopy(12, rhoirr, 1, vinf(nsfv+3*nbsys+1), 1)
        endif
!
        if (irr .eq. 2) then
            do is = 1, nbsys
!              SOMME SUR COPLA(S)
                sdp=0.d0
                do iv = 1, 12
                    is3=(is-1)/3
                    iv3=(iv-1)/3
!                 PARTIE POSITIVE DE ALPHA
                    if (is3 .eq. iv3) then
                        sdp=sdp+dps(iv)
                    endif
                end do
                roloop(is)=rhosat+(roloop(is)-rhosat)*exp(-xi*sdp)
                fivoid(is)=phisat+(fivoid(is)-phisat)*exp(-dz*sdp)
            end do
            call dcopy(12, roloop, 1, vinf(nsfv+3*nbsys+1), 1)
            call dcopy(12, fivoid, 1, vinf(nsfv+3*nbsys+13), 1)
        endif
        nsfa=nsfa+nbsys
        nsfv=nsfv+nbsys*3
    end do
!
    indtau=nsfv
    if (irr .eq. 1) indtau=indtau+12
    if (irr .eq. 2) indtau=indtau+24
!     CISSIONS TAU_S
    ns=0
!     NSFA : debut de la famille IFA dans DY et YD
    nsfa=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
    do ifa = 1, nbfsys
        ifl=nbcomm(ifa,1)
        nomfam=cpmono(5*(ifa-1)+1)(1:16)
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ng, lg, 0, qm)
        do is = 1, nbsys
!           CALCUL DE LA SCISSION REDUITE =
!           PROJECTION DE SIG SUR LE SYSTEME DE GLISSEMENT
!           TAU      : SCISSION REDUITE TAU=SIG:MUS
            call caltau(ifa, is, sigf, fkooh,&
                        nfs, nsg, toutms, tau(ns+is), mus,&
                        msns)
            call lcmmlc(nmat, nbcomm, cpmono, nfs, nsg,&
                        hsr, nsfv, nsfa, ifa, nbsys,&
                        is, dt, nvi, vind, yd,&
                        dy, itmax, toler, materf, expbp,&
                        tau(ns+is), dalpha, dgamma, dp, crit,&
                        sgns, rp, iret)
        end do
        ns=ns+nbsys
        nsfa=nsfa+nbsys
        nsfv=nsfv+nbsys*3
    end do
!
    call dcopy(ns, tau, 1, vinf(indtau+1), 1)
!
!     ROTATION RESEAU DEBUT
    if (ir .eq. 1) then
        call lcmmro(tampon, omp, nvi, vind, vinf)
    endif
! ROTATION RESEAU FIN
!
    if (gdef .eq. 1) then
        call calcfe(nr, ndt, nvi, vind, df,&
                    gamsns, fe, fp, iret)
        if (iret .gt. 0) goto 999
!
!        CALCUL DES CONTRAINTES DE KIRCHOFF
        call dcopy(6, sigf, 1, pk2, 1)
        call dscal(3, sqrt(2.d0), pk2(4), 1)
        call pk2sig(3, fe, 1.d0, pk2, sigf, 1)
!
! les racine(2) attendues par NMCOMP :-)
        call dscal(3, sqrt(2.d0), sigf(4), 1)
!
        call daxpy(9, -1.d0, iden, 1, fe, 1)
        call dcopy(9, fe, 1, vinf(nvi-3-18+10), 1)
!
        call lcgrla(fp, devi)
        call dcopy(6, devi, 1, vinf, 1)
        call dscal(3, sqrt(2.d0), devi(4), 1)
!
        call daxpy(9, -1.d0, iden, 1, fp, 1)
        call dcopy(9, fp, 1, vinf(nvi-3-18+1), 1)
!
        epseq = lcnrte(devi)
        vinf (nvi-1) = epseq
!
    else
        do i = 1, 6
            vinf(i)=vind(i)+devi(i)
        end do
        epseq = lcnrte(devi)
        vinf (nvi-1) = vind (nvi-1) + epseq
    endif
!
    vinf(nvi-2) = sicl
!
    vinf (nvi) = iter
!
999 continue
    codret=max(codret,iret)
end subroutine
