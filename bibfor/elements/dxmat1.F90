subroutine dxmat1(fami, epais, df, dm, dmf, pgl, indith, npg)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/moyte2.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: indith, npg
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dmc(3, 2), dfc(3, 2)
    real(kind=8) :: pgl(3, 3)
    character(len=4) :: fami
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE FLEXION,
!  MEMBRANE, COUPLAGE MEMBRANE-FLEXION POUR LE DKTG (MATERIAU ISOTROPE)
!     LA VARIABLE INDITH EST INITIALISEE A 0
!     DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
!     PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
!     CONTRAINTES THERMIQUES
!     ------------------------------------------------------------------
    integer :: jcoqu, jmate, iret
    integer :: nbpar
    real(kind=8) :: cdf, cdm, valres(21)
    real(kind=8) :: young, nu, epais, valpar
    real(kind=8) :: dh(3, 3)
    real(kind=8) :: dx, dy, dz, norm
    real(kind=8) :: ps, pjdx, pjdy, pjdz, alphat
    real(kind=8) :: alpha, beta
    real(kind=8) :: em, ef, num, nuf
    integer :: icodre(21)
    character(len=8) :: nomres(21), nompar
    character(len=10) :: phenom
!     ------------------------------------------------------------------
!
    call r8inir(9, 0.d0, dm, 1)
    call r8inir(9, 0.d0, df, 1)
    call r8inir(9, 0.d0, dh, 1)
    call r8inir(9, 0.d0, dmf, 1)
    call r8inir(6, 0.d0, dmc, 1)
    call r8inir(6, 0.d0, dfc, 1)
!
    call jevech('PCACOQU', 'L', jcoqu)
    epais = zr(jcoqu)
    alpha = zr(jcoqu+1)*r8dgrd()
    beta = zr(jcoqu+2)*r8dgrd()
!
    dx = cos(beta)*cos(alpha)
    dy = cos(beta)*sin(alpha)
    dz = sin(beta)
    norm = sqrt(dx*dx+dy*dy+dz*dz)
    dx = dx/norm
    dy = dy/norm
    dz = dz/norm
    ps = dx*pgl(3,1) + dy*pgl(3,2) + dz*pgl(3,3)
    pjdx = dx - ps*pgl(3,1)
    pjdy = dy - ps*pgl(3,2)
    pjdz = dz - ps*pgl(3,3)
    norm = sqrt(pjdx*pjdx+pjdy*pjdy+pjdz*pjdz)
!     ------------------------------------------------
    indith = 0
    call jevech('PMATERC', 'L', jmate)
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS') then
        if (norm .le. r8prem()) then
            call utmess('A', 'ELEMENTS_40')
        endif
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
    else if (phenom .eq. 'ELAS_GLRC') then
        if (norm .le. r8prem()) then
            call utmess('A', 'ELEMENTS_40')
        endif
        nomres(1) = 'E_M'
        nomres(2) = 'NU_M'
        nomres(3) = 'E_F'
        nomres(4) = 'NU_F'
        nomres(5) = 'ALPHA'
    else if (phenom .eq. 'ELAS_DHRC') then
        indith = -1
        goto 90
    else
        call utmess('F', 'ELEMENTS_44', sk=phenom)
    endif
!
!===============================================================
!     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
!
    call moyte2(fami, npg, '+', valpar, iret)
    nbpar = 1
    nompar = 'TEMP'
!===============================================================
!
    if (phenom .eq. 'ELAS') then
!        ------ MATERIAU ISOTROPE ------------------------------------
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    1, nomres(3), valres(3), icodre(3), 0)
        if ((icodre(3).ne.0) .or. (valres(3).eq.0.d0)) then
            indith = -1
            goto 90
        endif
        young = valres(1)
        nu = valres(2)
        alphat = valres(3)
        young = young*alphat
!
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        cdf = young*epais*epais*epais/12.d0/ (1.d0-nu*nu)
        df(1,1) = cdf
        df(1,2) = cdf*nu
        df(2,1) = df(1,2)
        df(2,2) = df(1,1)
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        cdm = epais*young/ (1.d0-nu*nu)
        dm(1,1) = cdm
        dm(1,2) = cdm*nu
        dm(2,1) = dm(1,2)
        dm(2,2) = dm(1,1)
!
    else if (phenom .eq. 'ELAS_GLRC') then
!        ------ MATERIAU GLRC ------------------------------------
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
!
        em = valres(1)
        num = valres(2)
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    3, nomres(3), valres(3), icodre(3), 0)
        if ((icodre(5).ne.0) .or. (valres(5).eq.0.d0)) then
            indith = -1
            goto 90
        endif
!
        if (icodre(3) .eq. 0) then
            ef = valres(3)
        else
            ef = em
        endif
!
        if (icodre(4) .eq. 0) then
            nuf = valres(4)
        else
            nuf = num
        endif
!
        alphat = valres(5)
        em = em*alphat
        ef = ef*alphat
!
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        cdf = ef*epais*epais*epais/12.d0/ (1.d0-nuf*nuf)
        df(1,1) = cdf
        df(1,2) = cdf*nuf
        df(2,1) = df(1,2)
        df(2,2) = df(1,1)
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        cdm = epais*em/ (1.d0-num*num)
        dm(1,1) = cdm
        dm(1,2) = cdm*num
        dm(2,1) = dm(1,2)
        dm(2,2) = dm(1,1)
!
    endif
!
90  continue
end subroutine
