subroutine dxmath(fami, epais, df, dm, dmf, pgl, multic, indith, t2iu, t2ui, t1ve, npg)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/codent.h"
#include "asterfort/coqrep.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
    integer :: multic, indith, npg, npgh
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dmc(3, 2), dfc(3, 2)
    real(kind=8) :: pgl(3, 3), t2iu(4), t2ui(4), t1ve(9)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE FLEXION,
!     MEMBRANE, COUPLAGE MEMBRANE-FLEXION POUR UN MATERIAU ISOTROPE OU
!     MULTICOUCHE
!     OUT MULTIC :
!        1 POUR UN MATERIAU MULTICOUCHE SANS COUPLAGE MEMBRANE-FLEXION
!        2 POUR UN MATERIAU MULTICOUCHE AVEC COUPLAGE MEMBRANE-FLEXION
!        0 DANS LES AUTRES CAS
!     LA VARIABLE INDITH EST INITIALISEE A 0
!     DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
!     PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
!     CONTRAINTES THERMIQUES
!     ------------------------------------------------------------------
    integer :: jcoqu, jmate, iret
    integer :: nbv, i, j, k, nbpar, elasco, indalf
    real(kind=8) :: cdf, cdm, valres(56)
    real(kind=8) :: young, nu, epais, valpar, excent
    real(kind=8) :: xab1(3, 3), dh(3, 3)
    real(kind=8) :: s, c
    real(kind=8) :: alphat
    real(kind=8) :: alpha, beta
    real(kind=8) :: em, ef, num, nuf
    real(kind=8) :: deux
    integer :: icodre(56)
    character(len=3) :: nume
    character(len=8) :: nomres(56), nompar
    character(len=10) :: phenom
!     ------------------------------------------------------------------
!
    deux = 2.0d0
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
    excent= zr(jcoqu+4)
!
!
!     ------------------------------------------------
    indith = 0
    call jevech('PMATERC', 'L', jmate)
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
    if (phenom .eq. 'ELAS_COQMU') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
!       CALCUL DE LA MATRICE T1VE DE PASSAGE D'UNE MATRICE
!       (3,3) DU REPERE DE LA VARIETE AU REPERE ELEMENT
        t1ve(1) = c*c
        t1ve(4) = s*s
        t1ve(7) = c*s
        t1ve(2) = t1ve(4)
        t1ve(5) = t1ve(1)
        t1ve(8) = -t1ve(7)
        t1ve(3) = -t1ve(7) - t1ve(7)
        t1ve(6) = t1ve(7) + t1ve(7)
        t1ve(9) = t1ve(1) - t1ve(4)
        nbv = 56
        do i = 1, nbv
            call codent(i, 'G', nume)
            nomres(i) = 'HOM_'//nume
        end do
!
    else if (phenom.eq.'ELAS') then
        nbv = 3
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
    else if (phenom.eq.'ELAS_GLRC') then
        nbv = 5
        nomres(1) = 'E_M'
        nomres(2) = 'NU_M'
        nomres(3) = 'E_F'
        nomres(4) = 'NU_F'
        nomres(5) = 'ALPHA'
    else if (phenom.eq.'ELAS_COQUE') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
!       CALCUL DE LA MATRICE T1VE DE PASSAGE D'UNE MATRICE
!       (3,3) DU REPERE DE LA VARIETE AU REPERE ELEMENT
!
        t1ve(1) = c*c
        t1ve(4) = s*s
        t1ve(7) = c*s
        t1ve(2) = t1ve(4)
        t1ve(5) = t1ve(1)
        t1ve(8) = -t1ve(7)
        t1ve(3) = -t1ve(7) - t1ve(7)
        t1ve(6) = t1ve(7) + t1ve(7)
        t1ve(9) = t1ve(1) - t1ve(4)
!
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, 0, ' ', [0.0d0],&
                    1, 'MEMB_L  ', valres(1), icodre, 0)
        if (icodre(1) .eq. 1) then
            call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, 0, ' ', [0.0d0],&
                        1, 'M_LLLL  ', valres(1), icodre, 0)
            if (icodre(1) .eq. 1) then
                call utmess('F', 'ELEMENTS_41')
            else
                elasco = 2
            endif
        else
            elasco = 1
        endif
        if (elasco .eq. 1) then
            nbv = 10
            nomres(1) = 'MEMB_L  '
            nomres(2) = 'MEMB_LT '
            nomres(3) = 'MEMB_T  '
            nomres(4) = 'MEMB_G_L'
            nomres(5) = 'FLEX_L  '
            nomres(6) = 'FLEX_LT '
            nomres(7) = 'FLEX_T  '
            nomres(8) = 'FLEX_G_L'
            nomres(9) = 'CISA_L  '
            nomres(10) = 'CISA_T  '
            nomres(11) = 'ALPHA   '
        else if (elasco.eq.2) then
            nbv = 33
            multic = 2
            nomres(1) = 'M_LLLL  '
            nomres(2) = 'M_LLTT  '
            nomres(3) = 'M_LLLT  '
            nomres(4) = 'M_TTTT  '
            nomres(5) = 'M_TTLT  '
            nomres(6) = 'M_LTLT  '
            nomres(7) = 'F_LLLL  '
            nomres(8) = 'F_LLTT  '
            nomres(9) = 'F_LLLT  '
            nomres(10) = 'F_TTTT  '
            nomres(11) = 'F_TTLT  '
            nomres(12) = 'F_LTLT  '
            nomres(13) = 'MF_LLLL '
            nomres(14) = 'MF_LLTT '
            nomres(15) = 'MF_LLLT '
            nomres(16) = 'MF_TTTT '
            nomres(17) = 'MF_TTLT '
            nomres(18) = 'MF_LTLT '
            nomres(19) = 'MC_LLLZ '
            nomres(20) = 'MC_LLTZ '
            nomres(21) = 'MC_TTLZ '
            nomres(22) = 'MC_TTTZ '
            nomres(23) = 'MC_LTLZ '
            nomres(24) = 'MC_LTTZ '
            nomres(25) = 'FC_LLLZ '
            nomres(26) = 'FC_LLTZ '
            nomres(27) = 'FC_TTLZ '
            nomres(28) = 'FC_TTTZ '
            nomres(29) = 'FC_LTLZ '
            nomres(30) = 'FC_LTTZ '
            nomres(31) = 'C_LZLZ  '
            nomres(32) = 'C_LZTZ  '
            nomres(33) = 'C_TZTZ  '
            nomres(34) = 'ALPHA   '
        endif
    else
        call utmess('F', 'ELEMENTS_42', sk=phenom)
    endif
!
!===============================================================
!     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
!
    npgh=3
    call moytem(fami, npg, npgh, '+', valpar, iret)
    nbpar = 1
    nompar = 'TEMP'
!===============================================================
!
    if (phenom .eq. 'ELAS') then
!        ------ MATERIAU ISOTROPE ------------------------------------
!
        multic = 0
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
        call rcvalb(fami, 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, [valpar], 1, nomres(3), valres(3), icodre(3), 0)
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
!      --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!      --- ET REACTUALISATION DE LA MATRICE DE FLEXION       --------
!      --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                dmf(i,j) = excent*dm(i,j)
                df (i,j) = df(i,j) + excent*excent*dm(i,j)
            end do
        end do
    else if (phenom .eq. 'ELAS_GLRC') then
!        ------ MATERIAU ISOTROPE ------------------------------------
!
        multic = 0
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
        cdm = em*em/ (1.d0-num*num)
        dm(1,1) = cdm
        dm(1,2) = cdm*num
        dm(2,1) = dm(1,2)
        dm(2,2) = dm(1,1)
!      --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!      --- ET REACTUALISATION DE LA MATRICE DE FLEXION       --------
!      --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                dmf(i,j) = excent*dm(i,j)
                df (i,j) = df(i,j) + excent*excent*dm(i,j)
            end do
        end do
    else if (phenom.eq.'ELAS_COQUE') then
        multic = 0
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 1)
        if (elasco .eq. 1) then
            indalf = 11
        else if (elasco.eq.2) then
            indalf = 34
        endif
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    1, nomres(indalf), valres(indalf), icodre(indalf), 0)
        if ((icodre(indalf).ne.0) .or. (valres(indalf).eq.0.d0)) then
            indith = -1
            goto 90
        endif
        alphat = valres(indalf)
!
        if (elasco .eq. 1) then
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
            dm(1,1) = valres(1)*alphat
            dm(1,2) = valres(2)*alphat
            dm(2,1) = dm(1,2)
            dm(2,2) = valres(3)*alphat
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
            df(1,1) = valres(5)*alphat
            df(1,2) = valres(6)*alphat
            df(2,1) = df(1,2)
            df(2,2) = valres(7)*alphat
!
        else if (elasco.eq.2) then
!
            multic = 2
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
            dm(1,1) = valres(1)*alphat
            dm(1,2) = valres(2)*alphat
            dm(1,3) = valres(3)*alphat
            dm(2,1) = dm(1,2)
            dm(3,1) = dm(1,3)
            dm(2,2) = valres(4)*alphat
            dm(2,3) = valres(5)*alphat
            dm(3,3) = valres(6)*alphat
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
            df(1,1) = valres(7)*alphat
            df(1,2) = valres(8)*alphat
            df(1,3) = valres(9)*alphat
            df(2,1) = df(1,2)
            df(3,1) = df(1,3)
            df(2,2) = valres(10)*alphat
            df(2,3) = valres(11)*alphat
            df(3,2) = df(2,3)
            df(3,3) = valres(12)*alphat
!        --- COUPLAGE  MEMBRANE FLEXION --------------------------------
            dmf(1,1) = valres(13)*alphat
            dmf(1,2) = valres(14)*alphat
            dmf(1,3) = valres(15)*alphat
            dmf(2,1) = dmf(1,2)
            dmf(3,1) = dmf(1,3)
            dmf(2,2) = valres(16)*alphat
            dmf(2,3) = valres(17)*alphat
            dmf(3,2) = dmf(2,3)
            dmf(3,3) = valres(18)*alphat
!
        endif
!        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!        --- REACTUALISATION DE LA MATRICE DE FLEXION          --------
!        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                df(i,j) = df(i,j)+deux*excent*dmf(i,j)+excent*excent* dm(i,j)
                dmf(i,j)= dmf(i,j) + excent*dm(i,j)
            end do
        end do
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
!
        call utbtab('ZERO', 3, 3, dm, t1ve, xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve, xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve, xab1, dmf)
!
    else if (phenom.eq.'ELAS_COQMU') then
!        ------ MATERIAU MULTICOUCHE -----------------------------------
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    1, nomres(19), valres(19), icodre(19), 0)
        epais = valres(19)
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar],&
                    27, nomres(30), valres(30), icodre(30), 0)
        dm(1,1) = valres(30)
        dm(1,2) = valres(31)
        dm(1,3) = valres(32)
        dm(2,1) = valres(33)
        dm(2,2) = valres(34)
        dm(2,3) = valres(35)
        dm(3,1) = valres(36)
        dm(3,2) = valres(37)
        dm(3,3) = valres(38)
        dmf(1,1) = valres(39)
        dmf(1,2) = valres(40)
        dmf(1,3) = valres(41)
        dmf(2,1) = valres(42)
        dmf(2,2) = valres(43)
        dmf(2,3) = valres(44)
        dmf(3,1) = valres(45)
        dmf(3,2) = valres(46)
        dmf(3,3) = valres(47)
        df(1,1) = valres(48)
        df(1,2) = valres(49)
        df(1,3) = valres(50)
        df(2,1) = valres(51)
        df(2,2) = valres(52)
        df(2,3) = valres(53)
        df(3,1) = valres(54)
        df(3,2) = valres(55)
        df(3,3) = valres(56)
!
!        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!        --- REACTUALISATION DE LA MATRICE DE FLEXION          --------
!        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                df(i,j) = df(i,j)+deux*excent*dmf(i,j)+excent*excent* dm(i,j)
                dmf(i,j)= dmf(i,j) + excent*dm(i,j)
            end do
        end do
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
!
        call utbtab('ZERO', 3, 3, dm, t1ve, xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve, xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve, xab1, dmf)
!
        multic = 1
!
    endif
!
    do k = 1, 9
        if (abs(dmf(k,1)) .gt. 1.d-10) multic = 2
    end do
!
90  continue
end subroutine
