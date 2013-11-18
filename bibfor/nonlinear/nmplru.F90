subroutine nmplru(fami, kpg, ksp, poum, ndim,&
                  typmod, imate, compor, ppg, eps,&
                  epsp, rp, ener)
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
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rcvad2.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
!
    integer :: kpg, ksp, ndim, imate
    character(len=*) :: fami, poum
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*)
    real(kind=8) :: ppg, eps(6), epsp(6), ener(2)
!.......................................................................
!
!     REALISE LE CALCUL DE L'ENERGIE LIBRE ET DE LA DERIVEE DE L'ENERGIE
!             LIBRE PAR RAPPORT A LA TEMPERATURE (POUR LE CALCUL DE G)
!             EN PLASTICITE
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  COMPOR  : COMPORTEMENT
! IN  PPG     : DEFORMATION PLASTIQUE CUMULEE
! IN  EPS     : DEFORMATION TOTALE
! IN  EPSP    : DEFORMATION PLASTIQUE
!
! OUT RP      :
! OUT ENER(1) : ENRGIE LIBRE
! OUT ENER(1) : DERIVEE DE L'ENERGIE LIBRE / A LA TEMPERATURE
!.......................................................................
!
    integer :: icodre(3)
    character(len=8) :: nomres(3)
!
    real(kind=8) :: e, nu, demu, k, k3, alpha
    real(kind=8) :: de, dnu, demudt, dk, dalpha
    real(kind=8) :: dsde, sigy, rprim, rp, airep
    real(kind=8) :: dsdedt, dsigy, drprim, drp, dairep
    real(kind=8) :: nrj, dnrj, valres(3), devres(3)
    real(kind=8) :: epsth(6), epsdv(6), epseq, kron(6)
    real(kind=8) :: ther, rbid, divu, epsmo, temp, tref
!
    integer :: i, jprol, jvale, nbval
!
    logical :: cp, trac, line, elas
!
!
!-----------------------------------------------------------------------
    integer :: iret1, iret2
!-----------------------------------------------------------------------
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
    cp = typmod(1) .eq. 'C_PLAN'
    trac = compor(1)(1:14).eq.'VMIS_ISOT_TRAC'
    line = compor(1)(1:14).eq.'VMIS_ISOT_LINE'
    elas = compor(1)(1:16).eq.'ELAS            '
!
! -  LECTURE DE E, NU, ALPHA ET DERIVEES / TEMPERATRURE
!
    call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                ksp, temp, iret1)
    call rcvarc(' ', 'TEMP', 'REF', fami, 1,&
                1, tref, iret2)
    if (iret1 .eq. 1) temp = 0.d0
    if (iret2 .eq. 1) tref = 0.d0
!
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
    call rcvad2(fami, kpg, ksp, poum, imate,&
                'ELAS', 3, nomres, valres, devres,&
                icodre)
!
    if (iret1 .eq. 0) then
        if ((iret2.ge.1) .or. (icodre(3).ne.0)) then
            call utmess('F', 'CALCULEL_15')
        else
            alpha = valres(3)
            dalpha = devres(3)
!          CALL RCVAD2 (FAMI,KPG,KSP,POUM,IMATE,'ELAS',3,
!     &             NOMRES,VALRES,DEVRES,ICODRE)
!
        endif
    else
        if (icodre(3) .eq. 0) then
            alpha = valres(3)
            dalpha =devres(3)
        else
            alpha =0
            dalpha =0
        endif
    endif
!
    e = valres(1)
    nu = valres(2)
!
    de = devres(1)
    dnu = devres(2)
!
    demu = e/(1.d0+nu)
    demudt= ((1.d0+nu)*de-e*dnu)/(1.d0+nu)**2
!
    k = e/(1.d0-2.d0*nu)/3.d0
    dk = (de+2.d0*k*dnu)/(1.d0-2.d0*nu)/3.d0
!
    k3 = 3.d0*k
!
! - LECTURE DES CARACTERISTIQUES DE NON LINEARITE DU MATERIAU
!
    airep=0.d0
    dairep=0.d0
!
    if (line) then
        nomres(1)='D_SIGM_EPSI'
        nomres(2)='SY'
        call rcvad2(fami, kpg, ksp, poum, imate,&
                    'ECRO_LINE', 2, nomres, valres, devres,&
                    icodre)
        if (icodre(1) .ne. 0) then
            call utmess('F', 'ALGORITH7_74')
        endif
        if (icodre(2) .ne. 0) then
            call utmess('F', 'ALGORITH7_75')
        endif
        dsde = valres(1)
        sigy = valres(2)
        dsdedt= devres(1)
        dsigy = devres(2)
!
        rprim = e*dsde/(e-dsde)
        drprim = (de*dsde+e*dsdedt+rprim*(dsdedt-de))/(e-dsde)
!
        rp = sigy +rprim*ppg
        drp = dsigy+drprim*ppg
!
        airep = 0.5d0*(sigy+rp)*ppg
        dairep = 0.5d0*(dsigy+drp)*ppg
!
    else if (trac) then
        call rctrac(imate, 1, 'SIGM', temp, jprol,&
                    jvale, nbval, e)
        call rcfonc('V', 1, jprol, jvale, nbval,&
                    rbid, rbid, rbid, ppg, rp,&
                    rprim, airep, rbid, rbid)
        dairep = 0.d0

    else if (elas) then
        rp = 0.d0

    else
        ASSERT(.false.)

    endif
!
! - CALCUL DE EPSMO ET EPSDV
    if ((iret1+iret2) .eq. 0) then
        ther = alpha*(temp-tref)
    else
        ther = 0.d0
    endif
!
    if (cp) eps(3)=-nu/(1.d0-nu)*(eps(1)+eps(2)) +(1.d0+nu)/(1.d0-nu)*ther
    divu = 0.d0
    do 10 i = 1, 3
        epsth(i) = eps(i)-epsp(i)-ther
        epsth(i+3) = eps(i+3)-epsp(i+3)
        divu = divu + epsth(i)
10  end do
    epsmo = divu/3.d0
    do 20 i = 1, 2*ndim
        epsdv(i) = epsth(i) - epsmo * kron(i)
20  end do
!
! - CALCUL DE LA CONTRAINTE ELASTIQUE EQUIVALENTE
    epseq = 0.d0
    do 30 i = 1, 2*ndim
        epseq = epseq + epsdv(i)*epsdv(i)
30  end do
    epseq = sqrt(1.5d0*epseq)
!
!  CALCUL DE L'ENERGIE LIBRE ET DE LA DERIVEE /TEMPERATURE
!
    nrj = 0.5d0*k*divu*divu+demu*epseq*epseq/3.d0
    dnrj = 0.5d0*dk*divu*divu-k3*divu*(alpha+dalpha*(temp-tref) ) +demudt*epseq*epseq/3.d0
!
    ener(1) = nrj + airep
    ener(2) = dnrj+ dairep
!
end subroutine
