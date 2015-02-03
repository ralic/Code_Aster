subroutine lcsend(fami, kpg, ksp, ndim, imate,&
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
!  Gestion du module ENDO_PORO_BETON
!=====================================================================
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterc/r8prem.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/endo3d.h"
#include "asterfort/matini.h"
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
    character(len=8) :: typmod(*), elem, hexa8
    character(len=*) :: fami
!
! DECLARATIONS LOCALES
    character(len=8) :: nomres(31), nomflu(26)
    integer :: nmat, nvari, nstrs, mfr, erreur, i, j
    integer :: retour(31), ifour, istep
    integer :: nvcom, nvflu, nvendo, nvtail, iadzi, iazk24, ind
    integer :: nmelas, nmhydr, nmflu, nmendo, nmtail, nbno
    real(kind=8) :: valres(31), xmat(56), rbid, valflu(26)
    real(kind=8) :: dt, d(6, 6), e, nu, coef, coef1, coef2, coef3
    real(kind=8) :: zero, un, deux, rac2
    real(kind=8) :: hydrm, hydrp, sechp, sechm, sref, vgm, vgp
    real(kind=8) :: alpham, alphap, somme
    aster_logical :: fl3d
!
        do 21 i = 1, 6
            depsc(i)=0.
21      continue
! -----------------------------------
! --- Limitation aux éléments He8
! -----------------------------------
!
    call tecael(iadzi, iazk24, noms=0)
    nbno=zi(iadzi-1+2)
    ind=iazk24+3-1+nbno+3
    elem=zk24(ind)(1:8)
    hexa8="HEXA8"
!
    if (elem .ne. hexa8) call utmess('A', 'COMPOR1_89')
!
! -----------------------------------
! --- TRAITEMENT COMMUN DES SOURCES
! -----------------------------------
!
!
    dt = instap - instam
    if (ndim .eq. 2) then
        nstrs = 4
    else
        nstrs = 6
    endif
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
!
! ------------------------------------------------
!     recuperation de l hydratation fin de pas
    call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                ksp, hydrp, codret)
    if (codret .ne. 0) then
        hydrp=0.d0
        codret = 0
    endif
!
!
!      WRITE(6,*)'HYDR OK = ',HYDRM,HYDRP
! ------------------------------------------------
!     recuperation du sechage
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sechp, iret)
    if (iret .ne. 0) sechp=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sechm, iret)
    if (iret .ne. 0) sechm=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
!
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
!     recuperation du volume de gel fin de pas
    call rcvarc(' ', 'X1', '+', fami, kpg,&
                ksp, vgp, codret)
    if (codret .ne. 0) then
        vgp=0.d0
        codret = 0
    endif
!
! ------------------------------------------------
!
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='ALPHA'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                2, nomres, valres, retour, 2)
!
!        MODULES INSTANTANES ISOTROPES
    xmat(1) = valres(1)
    xmat(2) = valres(2)
!
! --- COEFFICIENT DE DILATATION THERMIQUE A T-
    alpham = valres(3)
!
    rbid = 0.d0
! ----------------------------------------------------------------
! --- TEST POUR SAVOIR S'IL Y A COUPLAGE AVEC LE MODULE DE FLUAGE
! --- TERMES INTERVENANT POUR DIMENSION VECTEUR VIN
! ----------------------------------------------------------------
    nvcom = 29
    nvflu = 48
    nvendo = 77
    nvtail = 10
! --- TERMES INTERVENANT POUR DIMENSION VECTEUR XMAT
    nmelas = 4
    nmhydr = 24
    nmflu = 5
    nmendo = 10
    nmtail = 12
!
! --- TRAITEMENT DIFFERENT SI COUPLAGE FLUA3D-ENDO3D
    if (compor(1) .eq. 'KIT_DDI') then
        fl3d = .true.
        nvari = nvcom+nvflu+nvendo+nvtail
        nmat = nmelas+nmhydr+nmflu+nmendo+nmtail
        nomflu(1) = 'HYDS'
        nomflu(2) = 'F_C'
        nomflu(3) = 'F_T'
        nomflu(4) = 'EPS_COMP'
        nomflu(5) = 'EPS_TRAC'
        nomflu(6) = 'EKVP'
        nomflu(7) = 'CBIO'
        nomflu(8) = 'MODU_EAU'
        nomflu(9) = 'SFLD'
        nomflu(10)= 'MODU_GEL'
        nomflu(11)= 'VOL_GEL'
        nomflu(12)= 'PORO'
        nomflu(13)= 'TKVP'
        nomflu(14)= 'NRJA'
        nomflu(15)= 'MSHR'
        nomflu(16)= 'KD'
        nomflu(17)= 'MU'
        nomflu(18)= 'DT80'
        nomflu(19)= 'STMP'
        nomflu(20)= 'KTMP'
        nomflu(21)= 'TREF'
        nomflu(22)= 'Y1SY'
        nomflu(23)= 'TAU1'
        nomflu(24)= 'TAU2'
        nomflu(25)= 'EKFL'
        nomflu(26)= 'DFMX'
!
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'PORO_BETON', 0, ' ', [rbid],&
                    26, nomflu, valflu, retour, 2)
!
        if ((hydrp.lt.0) .and. (hydrp.ge.-0.0000001)) hydrp=0.0
        if ((hydrp.gt.1) .and. (hydrp.le.1.000001)) hydrp=1.0
!
        xmat(5) = hydrp
!
!
! --- ON REMPLIT XMAT DE 6 A 12
        do 10 i = 1, 7
            xmat(5+i) = valflu(i)
 10     continue
!
! --- ON REMPLIT XMAT DE 17 A 33
        do 20 i = 1, 17
            xmat(16+i) = valflu(9+i)
 20     continue
!
! --- TENEUR EN EAU
!
        xmat(13) = sechp
!
! --- COMPRESSIBILITE DE L'EAU
!
        xmat(14) = valflu(8)
!
! --- HETEROGENEITE DE LA CONTRAINTE HYDRIQUE
!
        xmat(15) = valflu(9)
!
! --- VOLUME DE GEL
!
        xmat(16) = vgp
!
! --- RECUPERATION PARAMETRES DE ENDO3D
        nomres(1)= 'GFTL'
        nomres(2)= 'GFCL'
        nomres(3)= 'WREF'
        nomres(4)= 'TPHI'
        nomres(5)= 'ANG_CRIT'
        nomres(6)= 'SREF'
        nomres(7)= 'VREF'
        nomres(8)= 'VMAX'
        nomres(9)= 'KWB'
        nomres(10)= 'COVS'
!
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'PORO_BETON', 0, ' ', [rbid],&
                    nmendo, nomres, valres, retour, 2)
!
        do 30 i = 1, nmendo
            xmat(nmelas+nmflu+nmhydr+i) = valres(i)
 30     continue
!
        do 40 i = 1, 9
            xmat(nmelas+nmflu+nmhydr+nmendo+i) = tampon(i)
 40     continue
        xmat(nmat-2) = 0.d0
        xmat(nmat-1) = 0.d0
        xmat(nmat) = 0.d0
!
    else
        fl3d = .false.
        nvari = nvcom+nvendo+nvtail
        nmat = nmelas+nmhydr+nmendo+nmtail
!
        nomres(1) = 'HYDS'
        nomres(2) = 'F_C'
        nomres(3) = 'F_T'
        nomres(4) = 'EPS_COMP'
        nomres(5) = 'EPS_TRAC'
        nomres(6) = 'EKVP'
        nomres(7) = 'CBIO'
        nomres(8) = 'MODU_EAU'
        nomres(9)= 'SFLD'
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
        nomres(22)= 'GFTL'
        nomres(23)= 'GFCL'
        nomres(24)= 'WREF'
        nomres(25)= 'TPHI'
        nomres(26)= 'ANG_CRIT'
        nomres(27)= 'SREF'
        nomres(28)= 'VREF'
        nomres(29)= 'VMAX'
        nomres(30)= 'KWB'
        nomres(31)= 'COVS'
!
        call rcvalb(fami, kpg, ksp, '-', imate,&
                    ' ', 'PORO_BETON', 0, ' ', [rbid],&
                    31, nomres, valres, retour, 2)
!
        if ((hydrp.lt.0) .and. (hydrp.ge.-0.0000001)) hydrp=0.0
        if ((hydrp.gt.1) .and. (hydrp.le.1.000001)) hydrp=1.0
!
        xmat(5) = hydrp
!
! --- ON REMPLIT XMAT DE 6 A 12
        do 50 i = 1, 7
            xmat(5+i) = valres(i)
 50     continue
!
! --- ON REMPLIT XMAT DE 17 A 38
        do 60 i = 1, 22
            xmat(16+i) = valres(9+i)
 60     continue
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
        do 70 i = 1, 9
            xmat(38+i) = tampon(i)
 70     continue
        xmat(48) = 0.d0
        xmat(49) = 0.d0
        xmat(50) = 0.d0
!
! ----------------------------------------------------
! ---- INITIALISATION DES VARIABLES INTERNES SI BESOIN
        somme = 0.d0
        do 80 i = 1, nvari
            somme = somme + abs(vim(i))
 80     continue
!
        if (somme .lt. r8prem()) then
            do 90 i = 1, nstrs
                vim(29+i) = sigm(i)
 90         continue
        endif
!
        vim(1) = hydrm
!
    endif
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
!
! --- CALCUL DU RETRAIT DU A LA DILATATION THERMIQUE
        nomres(1)='ALPHA'
        call rcvalb(fami, kpg, ksp, '+', imate,&
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, nomres, valres, retour, 2)
!
        alphap = valres(1)
!
        do 100 i = 1, 3
            depsc(i) = deps(i) - (alphap*(tp-tref)- alpham*(tm-tref))
            epsmc(i) = epsm(i) - alpham*(tm-tref)
100     continue
        do 110 i = 4, nstrs
            depsc(i) = deps(i)
            epsmc(i) = epsm(i)
110     continue
!
!-----VALEUR FIXEE PROVISOIREMENT POUR MFR
        mfr = 1
!-----------------------------------------
!
        if (typmod(1)(1:2) .eq. '3D') then
            ifour = 2
        else
            write(*,*)'Seul le 3D est developpe actuellement'
            write(*,*)'pour la loi SELLIER_ENDO'
            call utmess('F', 'COMPOR1_90')
        endif
!
        istep = 0
!
!-----MISE AU FORMAT ASTER --> CASTEM DE
!     L'INCREMENT DE DEFORMATION EN ENTREE
        rac2 = sqrt(2.d0)
        do 120 i = 4, nstrs
            depsc(i) = depsc(i) * rac2
120     continue
!
        call endo3d(xmat, nmat, vim, vip, nvari,&
                    dt, depsc, nstrs, sigp, mfr,&
                    erreur, tm, tp, fl3d, ifour,&
                    istep)
!
        vip(29)=1.d0
!
!-----MISE AU FORMAT CASTEM --> ASTER DE
!     L'INCREMENT DE DEFORMATION ET DES
!     CONTRAINTES EN SORTIE
        do 130 i = 4, nstrs
            sigp(i) = sigp(i) * rac2
130     continue
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
        do 140 i = 1, nstrs
            do 150 j = 1, nstrs
                dsidep(i,j) = d(i,j)
150         continue
140     continue
!
    endif
end subroutine
