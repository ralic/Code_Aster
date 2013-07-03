subroutine lcmmja(comp, typmod, nmat, materf, timed,&
                  timef, itmax, toler, nbcomm, cpmono,&
                  pgl, nfs, nsg, toutms, hsr,&
                  nr, nvi, vind, df, yf,&
                  yd, dy, drdy, iret)
! aslint: disable=W1306,W1504
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
! person_in_charge: jean-michel.proix at edf.fr
!       ----------------------------------------------------------------
!       MONOCRISTAL : CALCUL DU JACOBIEN DU SYSTEME NL A RESOUDRE = DRDY
!                    DY    = ( DSIG + DGAMMA PAR SYST )
!                    Y     = ( SIG   GAMMA P par syst. gliss)
!       IN  COMP   :  NOM COMPORTEMENT
!           TYPMOD :  TYPE DE MODELISATION
!           NMAT   :  DIMENSION MATER
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!           TIMED  :  ISTANT PRECEDENT
!           TIMEF  :  INSTANT ACTUEL
!           ITMAX  :  ITER_INTE_MAXI
!           TOLER  :  RESI_INTE_RELA
!           NBCOMM :  INCIDES DES COEF MATERIAU
!           CPMONO :  NOM DES COMPORTEMENTS
!           PGL    :  MATRICE DE PASSAGE
!           TOUTMS :  TENSEURS D'ORIENTATION
!           HSR    :  MATRICE D'INTERACTION
!           NR     :  DIMENSION DECLAREE DRDY
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           VIND   :  VARIABLES INTERNES A L'INSTANT PRECEDENT
!           DF     :  Increment de Gradient de deformation
!           YD     :  VARIABLES A T
!           YF     :  VARIABLES A T + DT
!           DY     :  SOLUTION
!       OUT DRDY   :  JACOBIEN DU SYSTEME NON LINEAIRE
!           IRET   :  CODE RETOUR
!       ----------------------------------------------------------------
#include "asterfort/calcfe.h"
#include "asterfort/caldfe.h"
#include "asterfort/caldto.h"
#include "asterfort/caltau.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcicma.h"
#include "asterfort/lcmmjb.h"
#include "asterfort/lcmmjg.h"
#include "asterfort/lcmmsg.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcsoma.h"
#include "asterfort/r8inir.h"
    integer :: nmat, nr, nbfsys, ndt, ndi, nsfa, nsfv, nbsys, is, ir
    integer :: nbcomm(nmat, 3), ifa, i, j, k, l, iret, ifl, itmax, nuvr, nuvs
    integer :: nuecou, ind(3, 3), nvi, nfs, nsg, iexp
    real(kind=8) :: vind(*), yf(*), dy(*), drdy(nr, nr), materf(nmat*2)
    real(kind=8) :: pgl(3, 3), toutms(nfs, nsg, 6), hsr(nsg, nsg), gamsns(3, 3)
    real(kind=8) :: timed, timef, msdgdt(6, 6), dt, fkooh(6, 6), sigf(6)
    real(kind=8) :: toler, dgsdts, dksdts, dgrdbs, dkrdbs, taus, taur
    real(kind=8) :: msns(3, 3)
    real(kind=8) :: q(3, 3), mus(6), ns(3), ms(3), mur(6), dtods(3, 3)
    real(kind=8) :: dfpds(3, 3, 3, 3), yd(*), msnst(3, 3, nsg), fp(3, 3)
    real(kind=8) :: mrnr(3, 3), df(3, 3), fe(3, 3), expbp(nsg)
    real(kind=8) :: dfpdbs(3, 3, nsg), dfpdga(3, 3, nsg)
    character(len=16) :: nomfam, comp(*)
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: typmod
!     ----------------------------------------------------------------
    common /tdim/   ndt , ndi
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!     ----------------------------------------------------------------
    data ind/1,4,5,4,2,6,5,6,3/
!     ----------------------------------------------------------------
!
    iret=0
    dt=timef-timed
!
    call r8inir(nr*nr, 0.d0, drdy, 1)
    call r8inir(36, 0.d0, msdgdt, 1)
!
    call lceqvn(ndt, yf(1), sigf)
!
!     Inverse de la matrice de Hooke
    if (materf(nmat) .eq. 0) then
        call lcopil('ISOTROPE', typmod, materf(1), fkooh)
    else if (materf(nmat).eq.1) then
        call lcopil('ORTHOTRO', typmod, materf(1), fkooh)
    endif
!
    if (gdef .eq. 1) then
        call r8inir(81, 0.d0, dfpds, 1)
        call r8inir(3*3*nsg, 0.d0, dfpdbs, 1)
!        calcul de DFPDGA : dFp / dGamma_S pour tous les systemes S
        call lcmmjg(comp, nmat, nbcomm, cpmono, hsr,&
                    dt, nvi, vind, yd, dy,&
                    itmax, toler, materf, sigf, fkooh,&
                    nfs, nsg, toutms, pgl, msnst,&
                    gamsns, dfpdga, iret)
    endif
!
!     NSFA : debut de la famille IFA dans DY et YD, YF
    nsfa=6
!     NSFV : debut de la famille IFA dans les variables internes
    nsfv=6
!     LE NUMERO GLOBAL DU SYSTEME IS DANS Y EST NUVS
    nbfsys=nbcomm(nmat,2)
!
    do 6 ifa = 1, nbfsys
!
        nomfam=cpmono(5*(ifa-1)+1)(1:16)
        ifl=nbcomm(ifa,1)
        nuecou=nint(materf(nmat+ifl))
!
        call lcmmsg(nomfam, nbsys, 0, pgl, mus,&
                    ns, ms, 0, q)
!
        do 7 is = 1, nbsys
!
!           calcul de Tau_s HPP ou GDEF
!
            call caltau(comp, ifa, is, sigf, fkooh,&
                        nfs, nsg, toutms, taus, mus,&
                        msns)
!
            nuvs=nsfa+is
!
!           CALCUL DES DERIVEES :
!           DGSDTS=dGamma_S/dTau_S,  DKSDTS=dK_s/dTau_S,
!           DGRDBS=dGamma_R/dBeta_S, DKRDBS=dK_S/dBeta_R
!
            iexp=0
            if (is .eq. 1) iexp=1
            call lcmmjb(taus, materf, cpmono, ifa, nmat,&
                        nbcomm, dt, nuecou, nsfv, nsfa,&
                        is, is, nbsys, nfs, nsg,&
                        hsr, vind, dy, iexp, expbp,&
                        itmax, toler, dgsdts, dksdts, dgrdbs,&
                        dkrdbs, iret)
!           ici  DGRDBS,DKRDBS sont inutiles
            if (iret .gt. 0) goto 9999
!
            if (abs(dgsdts) .gt. 0.d0) then
!              Cas ou Delta-Gamma_S est non nul
                if (gdef .eq. 0) then
!                 dR1/dS
                    do 1002 i = 1, 6
                        do 1002 j = 1, 6
                            msdgdt(i,j)=msdgdt(i,j)+mus(i)*mus(j)*&
                            dgsdts
1002                      continue
!                 dR2/dS
                    do 29 i = 1, 6
                        drdy(nuvs,i)=-mus(i)*dksdts
29                  continue
                else
                    call caldto(sigf, fkooh, msns, dtods)
!                 dR1/dS
                    do 1003 i = 1, 3
                        do 1003 j = 1, 3
                            do 1003 k = 1, 3
                                do 1003 l = 1, 3
                                    dfpds(i,j,k,l)=dfpds(i,j,k,l)+&
                                    dfpdga(i,j,is)*dgsdts*dtods(k,l)
1003                              continue
!                 dR2/dS
                    do 30 i = 1, 3
                        do 30 j = 1, 3
                            drdy(nuvs,ind(i,j))=-dksdts*dtods(i,j)
30                      continue
!
                endif
            endif
!
!------------------------
!           calcul des ns termes dR1_i/dBeta_s
!           et     des ns termes dR2_r/dBeta_s
!------------------------
            do 22 ir = 1, nbsys
                call caltau(comp, ifa, ir, sigf, fkooh,&
                            nfs, nsg, toutms, taur, mur,&
                            mrnr)
!
                nuvr=nsfa+ir
!
                call lcmmjb(taur, materf, cpmono, ifa, nmat,&
                            nbcomm, dt, nuecou, nsfv, nsfa,&
                            ir, is, nbsys, nfs, nsg,&
                            hsr, vind, dy, iexp, expbp,&
                            itmax, toler, dgsdts, dksdts, dgrdbs,&
                            dkrdbs, iret)
!              ici DGSDTS,DKSDTS sont inutiles
                if (iret .gt. 0) goto 9999
!
                if (abs(dgrdbs) .gt. 0.d0) then
                    if (gdef .eq. 0) then
!                    terme dR1/dAlpha_s
                        do 193 i = 1, 6
                            drdy(i,nuvs)=drdy(i,nuvs)+mur(i)*dgrdbs
193                      continue
                    else
                        do 1006 i = 1, 3
                            do 1006 j = 1, 3
                                dfpdbs(i,j,is)=dfpdbs(i,j,is)+&
                                dfpdga(i,j,ir)*dgrdbs
1006                          continue
                    endif
!                 terme dR2r/dGammas
                    drdy(nuvr,nuvs)=-dkrdbs
                endif
!
22          continue
!
            drdy(nuvs,nuvs)=drdy(nuvs,nuvs)+1.d0
!
 7      continue
!
        nsfa=nsfa+nbsys
        nsfv=nsfv+nbsys*3
!
 6  end do
!
    if (gdef .eq. 1) then
        call calcfe(nr, ndt, nvi, vind, df,&
                    gamsns, fe, fp, iret)
        call caldfe(df, nr, nvi, vind, dfpds,&
                    fe, dfpdbs, msdgdt, drdy)
    endif
!
    call lcsoma(msdgdt, fkooh, msdgdt)
    call lcicma(msdgdt, 6, 6, ndt, ndt,&
                1, 1, drdy, nr, nr,&
                1, 1)
9999  continue
end subroutine
