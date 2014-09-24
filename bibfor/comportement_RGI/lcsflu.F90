subroutine lcsflu(fami, kpg, ksp, ndim, imate,&
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
!      Ficher de base de FLUA_PORO_BETON
!=====================================================================
    implicit none
#include "asterfort/rcvarc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/fluag3d.h"
#include "asterfort/matini.h"
#include "asterc/r8prem.h"
#include "asterfort/utmess.h"
!
!
    integer :: imate, ndim, kpg, ksp, codret, icomp, nvi, iret
    real(kind=8) :: crit(*), angmas(*)
    real(kind=8) :: instam, instap, tampon(*)
    real(kind=8) :: epsm(6), deps(6)
    real(kind=8) :: epsmc(6), depsc(6)
    real(kind=8) :: sigm(6), sigp(6)
    real(kind=8) :: vim(*), vip(*), tm, tp, tref
    real(kind=8) :: dsidep(6, 6)
    character(len=16) :: compor(*), option
    character(len=8) :: typmod(*)
    character(len=*) :: fami
!
! DECLARATIONS LOCALES
    character(len=8) :: nomres(28)
    real(kind=8) :: valres(28), xmat(33), rbid
    integer :: nred, nmat, nvari, nstrs, mfr, erreur, i, j
    integer :: retour(27)
    real(kind=8) :: dt, d(6, 6), e, nu, coef, coef1, coef2, coef3
    real(kind=8) :: zero, un, deux, rac2
!
    real(kind=8) :: hydrm, hydrp, sechp, sechm, sref, vgm, vgp
    real(kind=8) :: alpham, alphap, somme
! ATTENTION NRED DOIT ETRE SUPERIEUR OU EGAL A 3
    parameter       (nred=28)
    parameter       (nmat=nred+5)
    parameter       (nvari=77)
!
! APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
!
    call rcvarc('f', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret)
    call rcvarc('f', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret)
    call rcvarc('f', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret)
!
! ------------------------------------------------
!     RECUPERATION DE L HYDRATATION DEBUT DE PAS
    call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                ksp, hydrm, codret)
    if (codret .ne. 0) then
        hydrm=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!     RECUPERATION DE L HYDRATATION FIN DE PAS
    call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                ksp, hydrp, codret)
    if (codret .ne. 0) then
        hydrp=0.d0
        codret = 0
    endif
!      PRINT*,'HYDRP',HYDRP
!      WRITE(6,*)'HYDR OK = ',HYDRM,HYDRP
! ------------------------------------------------
!     RECUPERATION DU SECHAGE
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sechp, iret)
    if (iret .ne. 0) sechp=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sechm, iret)
    if (iret .ne. 0) sechm=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
!      PRINT*,'SECHP',SECHP
!      WRITE(6,*)'SECH OK = ',SECHM,SECHP,SREF
! -----------------------------------------------
!     RECUPERATION DU VOLUME DE GEL DEBUT DE PAS
    call rcvarc(' ', 'X1', '-', fami, kpg,&
                ksp, vgm, codret)
    if (codret .ne. 0) then
        vgm=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!     RECUPERATION DU VOLUME DE GEL FIN DE PAS
    call rcvarc(' ', 'X1', '+', fami, kpg,&
                ksp, vgp, codret)
    if (codret .ne. 0) then
        vgp=0.d0
        codret = 0
    endif
!      PRINT*,'VGP',VGP
!      WRITE(6,*)'NEUT1 OK = ',VGM,VGP
! ------------------------------------------------
!
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='ALPHA'
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                3, nomres, valres, retour, 2)
!
!
!        MODULES INSTANTANES ISOTROPES
    xmat(1) = valres(1)
    xmat(2) = valres(2)
    alpham = valres(3)
!      XMAT(3) = 0.D0
!      XMAT(4) = 0.D0
!
! --- EVALUATION PARAMETERES MATERIAU ELASTIQUES A T+
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                3, nomres, valres, retour, 2)
!
    alphap = valres(3)
!
! ------------------------------------------------------------------
! --  RETRAIT INCREMENT DE DEFORMATION DUE A LA DILATATION THERMIQUE
! ------------------------------------------------------------------
    if (ndim .eq. 2) then
        nstrs = 4
    else
        nstrs = 6
    endif
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        do 10 i = 1, 3
            depsc(i) = deps(i) - (alphap*(tp-tref)-alpham*(tm-tref))
            epsmc(i) = epsm(i) - alpham*(tm-tref)
 10     end do
        do 20 i = 4, nstrs
            depsc(i) = deps(i)
            epsmc(i) = epsm(i)
 20     continue
    endif
!
! ------------------------------------------------------------------
! --  RECUPERATION PARAMETRES MATERIAU DU MODELE FLUA3D
! ------------------------------------------------------------------
!
    nomres(1) = 'HYDS'
    nomres(2) = 'F_C'
    nomres(3) = 'F_T'
    nomres(4) = 'EPS_COMP'
    nomres(5) = 'EPS_TRAC'
    nomres(6) = 'EKVP'
    nomres(7) = 'CBIO'
    nomres(8) = 'MODU_EAU'
    nomres(9) = 'SFLD'
    nomres(10)= 'MODU_GEL'
    nomres(11)= 'VOL_GEL'
    nomres(12)= 'PORO'
    nomres(13)= 'TKVP'
    nomres(14)= 'NRJA'
    nomres(15)= 'MSHR'
    nomres(16)= 'KD'
    nomres(17)= 'MU'
    nomres(18)= 'DT80'
    nomres(19)= 'STMP'
    nomres(20)= 'KTMP'
    nomres(21)= 'TREF'
    nomres(22)= 'Y1SY'
    nomres(23)= 'TAU1'
    nomres(24)= 'TAU2'
    nomres(25)= 'EKFL'
    nomres(26)= 'DFMX'
!
    rbid = 0.d0
    rac2 = sqrt(2.d0)
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'PORO_BETON', 0, ' ', [rbid],&
                26, nomres, valres, retour, 2)
!
!
! --- AFFECTATION DES VALEURS D'HYDRATATION
    xmat(5) = hydrp
!
! --- ON REMPLIT XMAT DE 6 A 12
    do 30 i = 6, 12
        xmat(i) = valres(i-5)
 30 end do
!
! --- ON REMPLIT XMAT DE 6 A 12
    do 40 i = 17, 33
        xmat(i) = valres(i-7)
 40 end do
!
! --- TENEUR EN EAU
!
    xmat(13) = sechp
!
! --- COMPRESSIBILITE DE L'EAU
!
    xmat(14) = valres(8)
!
! --- HETEROGENEITE DE LA CONTRAINTE HYDRIQUE
!
    xmat(15) = valres(9)
!
! --- VOLUME DE GEL
!
    xmat(16) = vgp
!
! --- INCREMENT TEMPOREL
!
    dt = instap - instam
!
! ----------------------------------------------------
! ---- INITIALISATION DES VARIABLES INTERNES SI BESOIN
    somme = 0.d0
    do 50 i = 1, nvari
        somme = somme + abs(vim(i))
 50 end do
!
    if (somme .lt. r8prem()) then
        do 60 i = 1, nstrs
            vim(66+i) = sigm(i)
 60     continue
    endif
!
    vim(1) = hydrm
!
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
!
!-----VALEUR FIXEE PROVISOIREMENT POUR MFR
        mfr = 1
!-----------------------------------------
!
!-----MISE AU FORMAT ASTER --> CASTEM DE
!     L'INCREMENT DE DEFORMATION EN ENTREE
        do 70 i = 4, nstrs
            depsc(i) = depsc(i) * rac2
 70     continue
!
        call fluag3d(xmat, nmat, vim, vip, nvari,&
                     dt, depsc, nstrs, sigp, mfr,&
                     erreur, tm, tp)
!
!-----MISE AU FORMAT CASTEM --> ASTER DE
!     L'INCREMENT DE DEFORMATION ET DES
!     CONTRAINTES EN SORTIE
        do 80 i = 4, nstrs
            sigp(i) = sigp(i) * rac2
 80     continue
!
    endif
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
!
        zero = 0.d0
        un = 1.d0
        deux = 2.d0
!
        call matini(6, 6, zero, d)
!
        e = xmat(1)
        nu = xmat(2)
!
        coef = un/ ((un+nu)* (un-deux*nu))
        coef1 = e* (un-nu)*coef
        coef2 = e*nu*coef
        coef3 = e/ (un+nu)
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
        do 90 i = 1, nstrs
            do 100 j = 1, nstrs
                dsidep(i,j) = d(i,j)
100         continue
 90     continue
!
    endif
!
end subroutine
