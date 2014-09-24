subroutine lcsrgi(fami, kpg, ksp, ndim, imate,&
                  compor, crit, instam, instap, epsm,&
                  deps, sigm, vim, option, angmas,&
                  sigp, vip, tampon, typmod, icomp,&
                  nvi, dsidep, codret)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      Ficher de base de RGI_BETON
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/rcvarc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rgilin3d.h"
#include "asterfort/matini.h"
#include "asterc/r8prem.h"
#include "asterfort/hydr_xmat.h"

    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, iret
    real(kind=8) :: crit(*), angmas(*)
    real(kind=8) :: instam, instap, tampon(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: sigm(6), sigp(6)
    real(kind=8) :: vim(*), vip(*), tm, tp, tref
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
!
! DECLARATIONS LOCALES
    character(len=8) :: nomres(23)
    real(kind=8) :: valres(23), xmat(30), rbid
    integer :: nmat, nvari, nstrs, mfr, erreur, i, j
    integer :: retour(23), ifour, istep
    real(kind=8) :: dt, d(6, 6), e, nu, coef, coef1, coef2, coef3
    real(kind=8) :: zero, un, deux, rac2, sechp, sechm, sref
    real(kind=8) :: hydrm, hydrp, nam, nap, somme
    real(kind=8) :: hydras,e1
    aster_logical :: fl3d
!
    parameter       (nmat=30)
    parameter       (nvari=26)
!
! APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
!
      call rcvarc('f','TEMP','-',fami,kpg,ksp,tm,iret)
      call rcvarc('f','TEMP','+',fami,kpg,ksp,tp,iret)
      call rcvarc('f','TEMP','REF',fami,kpg,ksp,tref,iret)
!
! ------------------------------------------------
!       recuperation de l hydratation et du sechage
         call rcvarc(' ','SECH','+',fami,kpg,ksp,sechp,iret)
    if (iret .ne. 0) sechp=0.d0
         call rcvarc(' ','SECH','-',fami,kpg,ksp,sechm,iret)
    if (iret .ne. 0) sechm=0.d0
         call rcvarc(' ','SECH','REF',fami,kpg,ksp,sref,iret)
    if (iret .ne. 0) sref=0.d0
!
! ------------------------------------------------
!     recuperation de l hydratation debut de pas
       call rcvarc(' ','HYDR','-',fami,kpg,ksp,hydrm,codret)
    if (codret .ne. 0) then
        hydrm=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!     RECUPERATION DE L HYDRATATION FIN DE PAS
       call rcvarc(' ','HYDR','+',fami,kpg,ksp,hydrp,codret)
    if (codret .ne. 0) then
        hydrp=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!     RECUPERATION DE LA CONCENTRATION EN NA DEBUT DE PAS
       call rcvarc(' ','X1','-',fami,kpg,ksp,nam,codret)
    if (codret .ne. 0) then
        nam=0.d0
        codret = 0
    endif
!
    vim(24) = nam
!
! ------------------------------------------------
!     RECUPERATION DE LA CONCENTRATION EN NA FIN DE PAS
       call rcvarc(' ','X1','+',fami,kpg,ksp,nap,codret)
    if (codret .ne. 0) then
        nap=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!
    nomres(1)='E'
    nomres(2)='NU'
       call rcvalb(fami,kpg,ksp,'-',imate,' ', 'ELAS', 0,' ', [0.d0],&
                   2, nomres, valres, retour, 2)
!
!        MODULES INSTANTANES ISOTROPES
    xmat(1) = valres(1)
    xmat(2) = valres(2)
!      XMAT(3) = 0.d0
!      XMAT(4) = 0.d0
!
    nomres(1) = 'ALUC'
    nomres(2) = 'SULC'
    nomres(3) = 'SILC'
    nomres(4) = 'TDEF'
    nomres(5) = 'PORO'
    nomres(6) = 'HYDS'
    nomres(7) = 'TAAR'
    nomres(8) = 'SSAR'
    nomres(9) = 'SSDE'
    nomres(10) = 'VAAR'
    nomres(11) = 'VETT'
    nomres(12) = 'VVAR'
    nomres(13) = 'VVDE'
    nomres(14) = 'BAAR'
    nomres(15) = 'BDEF'
    nomres(16) = 'MAAR'
    nomres(17) = 'MDEF'
    nomres(18) = 'DT80'
    nomres(19) = 'COTH'
    nomres(20) = 'CORG'
    nomres(21) = 'ID0'
    nomres(22) = 'ID1'
    nomres(23) = 'ID2'
!
    rbid = 0.d0
    rac2 = sqrt(2.d0)
!
       call rcvalb(fami,kpg,ksp,'-',imate,' ', 'PORO_BETON', 0,' ',&
                  [rbid],23, nomres, valres, retour, 2)
!
    do 10 i = 1, 5
        xmat(4+i) = valres(i)
10  end do
!
    xmat(10) = sechp
    xmat(11) = hydrp
    xmat(12) = valres(6)
    xmat(13) = nap

    do 20 i = 14,30
        xmat(i) = valres(i-7)
20  continue
!
    dt = instap - instam
    if (ndim .eq. 2) then
        nstrs = 4
    else
        nstrs = 6
    endif
! ----------------------------------------------------
! ---- initialisation des variables internes si besoin
    somme = 0.d0
    do 30 i = 1, nvari
        somme = somme + abs(vim(i))
30  continue

    if(somme.lt.r8prem())then
        do 40 i = 1, 3 
            vim(i) = sigm(i)
40      continue
        do 50 i = 4, nstrs 
            vim(3+i) = sigm(i)/rac2
50      continue
    endif

    vim(7)  = hydrm
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
!
!-----VALEUR FIXEE PROVISOIREMENT POUR MFR
        mfr = 1
!-----------------------------------------
!
!-----MISE AU FORMAT ASTER --> CASTEM DE
!     L'INCREMENT DE DEFORMATION EN ENTREE
        do 60 i = 4, nstrs
            deps(i) = deps(i) * rac2
60      continue
!
        call rgilin3d(xmat,nmat,vim,vip,nvari,dt,deps,&
                     nstrs,sigp,mfr,erreur,tm,tp,fl3d,ifour,istep)
!
!-----MISE AU FORMAT CASTEM --> ASTER DE
!     L'INCREMENT DE DEFORMATION ET DES
!     CONTRAINTES EN SORTIE
        do 70 i = 4, nstrs
            deps(i) = deps(i) / rac2
            sigp(i) = sigp(i) * rac2
70      continue
!
    endif
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
!
        zero = 0.d0
        un = 1.d0
        deux = 2.d0
!
         call matini(6,6,zero,D)
!
        e = xmat(1)
        nu = xmat(2)
        hydras=xmat(12)
        erreur = 0
        call hydr_xmat(e,e1,hydrp,hydras,0.5d0,erreur)
!
        coef = un/ ((un+nu)* (un-deux*nu))
        coef1 = e1* (un-nu)*coef
        coef2 = e1*nu*coef
        coef3 = e1/ (un+nu)
!
        d(1,1) = coef1
        d(1,2) = coef2
        d(1,3) = coef2
!
        d(2,1) = coef2
        d(2,2) = coef1
        d(2,3) = coef2
!
        d(3,1) = coef2
        d(3,2) = coef2
        d(3,3) = coef1
!
        d(4,4) = 0.5d0*coef3
        d(5,5) = 0.5d0*coef3
        d(6,6) = 0.5d0*coef3
!
        do 80 i = 1, nstrs
            do 90 j = 1, nstrs
                dsidep(i,j) = d(i,j)
90          continue
80      continue
!
    endif
!
end subroutine
