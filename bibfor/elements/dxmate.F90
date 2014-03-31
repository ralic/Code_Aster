subroutine dxmate(fami, df, dm, dmf, dc, dci, dmc, dfc, nno, pgl, multic, coupmf, t2iu, t2ui, t1ve)
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/codent.h"
#include "asterfort/coqrep.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/moyte2.h"
#include "asterfort/moytem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/tecach.h"
#include "asterfort/tecael.h"
#include "asterfort/utbtab.h"
#include "asterfort/utdtab.h"
#include "asterfort/utmess.h"
    logical :: coupmf
    integer :: nno, multic
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: pgl(3, 3), t2iu(4), t2ui(4), t1ve(9)
    character(len=4) :: fami
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
!     CALCUL DES MATRICES DE RIGIDITE DE FLEXION, MEMBRANE , COUPLAGE
!     MEMBRANE-FLEXION ET CISAILLEMENT POUR UN MATERIAU ISOTROPE OU
!     MULTICOUCHE
!     OUT MULTIC :
!        1 POUR UN MATERIAU MULTICOUCHE SANS COUPLAGE MEMBRANE-FLEXION
!        0 DANS LES AUTRES CAS
!     OUT COUPMF :
!        .TRUE. POUR UN MATERIAU AVEC COUPLAGE MEMBRANE-FLEXION
!  ------------------------------------------------------------------
! aslint: disable=W1501
    integer :: jcoqu, jmate, nbv, i, j, k, nbpar, elasco
    integer :: iazi, iazk24, npg, jcou, ncou, iret, npgh, iret1
    integer :: ndim, nnos, ipoids, ivf, idfde, jgano
    integer :: icodre(33)
    real(kind=8) :: kcis, cdf, cdm, cdc, gcis, valres(33)
    real(kind=8) :: young, nu, epais, valpar, excent
    real(kind=8) :: xab1(3, 3), xab2(2, 2), xab3(3, 2)
    real(kind=8) :: s, c
    real(kind=8) :: alpha, beta, det
    real(kind=8) :: zero, deux
    real(kind=8) :: em, ef, num, nuf
    character(len=3) :: nume
    character(len=8) :: nomres(33), nompar
    character(len=10) :: phenom
    character(len=16) :: nomte
!
!     ------------------------------------------------------------------
!
    zero = 0.0d0
    deux = 2.0d0
    elasco = 0
    coupmf = .false.
    call r8inir(9, zero, dmf, 1)
    call r8inir(6, zero, dmc, 1)
    call r8inir(6, zero, dfc, 1)
!
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,npg=npg,jpoids=ipoids,&
                    jvf=ivf,jdfde=idfde,jgano=jgano)
    call tecael(iazi, iazk24)
    nomte = zk24(iazk24-1+3+nno+1)(1:16)
!
    call jevech('PCACOQU', 'L', jcoqu)
    epais = zr(jcoqu)
    alpha = zr(jcoqu+1)*r8dgrd()
    beta = zr(jcoqu+2)*r8dgrd()
    excent= zr(jcoqu+4)
    call tecach('NNN', 'PNBSP_I', 'L', iret, iad=jcou)
    if (iret .eq. 0) then
        ncou=zi(jcou)
        npgh=3
    else
        npgh=1
        ncou=1
    endif
!
!     ------------------------------------------------
    call tecach('NNN', 'PMATERC', 'L', iret, iad=jmate)
    if (iret .ne. 0) then
        multic = 0
        goto 999
    endif
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS_COQMU') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
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
        nbv = 26
        do i = 1, nbv
            call codent(i, 'G', nume)
            nomres(i) = 'HOM_'//nume
        end do
!
    else if (phenom.eq.'ELAS') then
        nbv = 2
        nomres(1) = 'E'
        nomres(2) = 'NU'
    else if (phenom.eq.'ELAS_GLRC') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
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
        nbv = 26
        do i = 1, nbv
            call codent(i, 'G', nume)
            nomres(i) = 'HOM_'//nume
        end do
!
        nbv = 6
        nomres(1) = 'E_M'
        nomres(2) = 'NU_M'
        nomres(3) = 'E_F'
        nomres(4) = 'NU_F'
        nomres(5) = 'BT1'
        nomres(6) = 'BT2'
    else if (phenom.eq.'ELAS_COQUE') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
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
!
        call rcvala(zi(jmate), ' ', phenom, 0, ' ', [zero], 1, 'MEMB_L  ', valres(1), icodre, 0)
        if (icodre(1) .eq. 1) then
            call rcvala(zi(jmate), ' ', phenom, 0, ' ', [zero], 1, 'M_LLLL  ', valres(1), icodre, 0)
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
            nomres(1)  = 'MEMB_L  '
            nomres(2)  = 'MEMB_LT '
            nomres(3)  = 'MEMB_T  '
            nomres(4)  = 'MEMB_G_L'
            nomres(5)  = 'FLEX_L  '
            nomres(6)  = 'FLEX_LT '
            nomres(7)  = 'FLEX_T  '
            nomres(8)  = 'FLEX_G_L'
            nomres(9)  = 'CISA_L  '
            nomres(10) = 'CISA_T  '
        else if (elasco.eq.2) then
            nbv = 33
            coupmf = .true.
            nomres(1) =  'M_LLLL  '
            nomres(2) =  'M_LLTT  '
            nomres(3) =  'M_LLLT  '
            nomres(4) =  'M_TTTT  '
            nomres(5) =  'M_TTLT  '
            nomres(6) =  'M_LTLT  '
            nomres(7) =  'F_LLLL  '
            nomres(8) =  'F_LLTT  '
            nomres(9) =  'F_LLLT  '
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
        endif
    else if (phenom.eq.'ELAS_DHRC') then
!
        call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
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
        nbv = 26
        do i = 1, nbv
            call codent(i, 'G', nume)
            nomres(i) = 'HOM_'//nume
        end do
!
        nbv = 21
        nomres(1)  = 'A011'
        nomres(2)  = 'A012'
        nomres(3)  = 'A013'
        nomres(4)  = 'A014'
        nomres(5)  = 'A015'
        nomres(6)  = 'A016'
        nomres(7)  = 'A022'
        nomres(8)  = 'A023'
        nomres(9)  = 'A024'
        nomres(10) = 'A025'
        nomres(11) = 'A026'
        nomres(12) = 'A033'
        nomres(13) = 'A034'
        nomres(14) = 'A035'
        nomres(15) = 'A036'
        nomres(16) = 'A044'
        nomres(17) = 'A045'
        nomres(18) = 'A046'
        nomres(19) = 'A055'
        nomres(20) = 'A056'
        nomres(21) = 'A066'
    else
        call utmess('F', 'ELEMENTS_42', sk=phenom)
    endif
!
    if (nomte .eq. 'MEDKQG4' .or. nomte .eq. 'MEDKTG3') then
        call moyte2(fami, npg, '+', valpar, iret1)
    else
        call moytem(fami, npg, npgh*ncou, '+', valpar, iret1)
    endif
    nbpar = 1
    nompar = 'TEMP'
!
    if (phenom .eq. 'ELAS') then
!
!        ------ MATERIAU ISOTROPE AVEC DECOUPLAGE MEMBRANE FLEXION----
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], nbv,&
                    nomres, valres, icodre, 1)
!
        young = valres(1)
        nu = valres(2)
!
        multic = 0
        kcis = 5.d0/6.d0
!
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        cdf = young*epais*epais*epais/12.d0/ (1.d0-nu*nu)
        do k = 1, 9
            df(k,1) = 0.d0
            dmf(k,1) = 0.d0
        end do
        df(1,1) = cdf
        df(1,2) = cdf*nu
        df(2,1) = df(1,2)
        df(2,2) = df(1,1)
        df(3,3) = cdf* (1.d0-nu)/2.d0
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        cdm = epais*young/ (1.d0-nu*nu)
        do k = 1, 9
            dm(k,1) = 0.d0
        end do
        dm(1,1) = cdm
        dm(1,2) = cdm*nu
        dm(2,1) = dm(1,2)
        dm(2,2) = dm(1,1)
        dm(3,3) = cdm* (1.d0-nu)/2.d0
!      --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
        gcis = young/2.d0/ (1.d0+nu)
        cdc = gcis*kcis*epais
        dc(1,1) = cdc
        dc(2,2) = dc(1,1)
        dc(1,2) = 0.d0
        dc(2,1) = 0.d0
!      --- CALCUL DE SON INVERSE ------------------------------------
        dci(1,1) = 1.d0/dc(1,1)
        dci(2,2) = dci(1,1)
        dci(1,2) = 0.d0
        dci(2,1) = 0.d0
!      --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!      --- ET REACTUALISATION DE LA MATRICE DE FLEXION       --------
!      --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                dmf(i,j) = excent*dm(i,j)
                df (i,j) = df(i,j) + excent*excent*dm(i,j)
            end do
        end do
!
    else if (phenom .eq. 'ELAS_GLRC') then
!
!        ------ MATERIAU ISOTROPE --------------------------------------
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 2,&
                    nomres, valres, icodre, 1)
!
        em = valres(1)
        num = valres(2)
!
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 2,&
                    nomres(3), valres, icodre, 0)
!
        if (icodre(1) .eq. 0) then
            ef = valres(1)
        else
            ef = em
        endif
!
        if (icodre(2) .eq. 0) then
            nuf = valres(2)
        else
            nuf = num
        endif
!
        multic = 0
        kcis = 5.d0/6.d0
!
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
        cdf = ef*epais*epais*epais/12.d0/ (1.d0-nuf*nuf)
        do k = 1, 9
            df(k,1) = 0.d0
            dmf(k,1) = 0.d0
        end do
        df(1,1) = cdf
        df(1,2) = cdf*nuf
        df(2,1) = df(1,2)
        df(2,2) = df(1,1)
        df(3,3) = cdf* (1.d0-nuf)/2.d0
!      ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
        cdm = epais*em/ (1.d0-num*num)
        do k = 1, 9
            dm(k,1) = 0.d0
        end do
        dm(1,1) = cdm
        dm(1,2) = cdm*num
        dm(2,1) = dm(1,2)
        dm(2,2) = dm(1,1)
        dm(3,3) = cdm* (1.d0-num)/2.d0
!      --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 2,&
                    nomres(5), valres, icodre, 0)
!
        if (icodre(1) .eq. 0) then
            dc(1,1) = valres(1)
            dc(2,2) = valres(2)
        else
            gcis = em/2.d0/ (1.d0+num)
            cdc = gcis*kcis*epais
            dc(1,1) = cdc
            dc(2,2) = dc(1,1)
        endif
!
        dc(1,2) = 0.d0
        dc(2,1) = 0.d0
!      --- CALCUL DE SON INVERSE ------------------------------------
        dci(1,1) = 1.d0/dc(1,1)
        dci(2,2) = dci(1,1)
        dci(1,2) = 0.d0
        dci(2,1) = 0.d0
!      --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!      --- ET REACTUALISATION DE LA MATRICE DE FLEXION       --------
!      --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                dmf(i,j) = excent*dm(i,j)
                df (i,j) = df(i,j) + excent*excent*dm(i,j)
            end do
        end do
!
    else if (phenom.eq.'ELAS_COQUE') then
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], nbv,&
                    nomres, valres, icodre, 1)
        if (elasco .eq. 1) then
            multic = 0
!
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
            dm(1,1) = valres(1)
            dm(1,2) = valres(2)
            dm(1,3) = 0.d0
            dm(2,1) = dm(1,2)
            dm(2,2) = valres(3)
            dm(2,3) = 0.d0
            dm(3,1) = 0.d0
            dm(3,2) = 0.d0
            dm(3,3) = valres(4)
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
            df(1,1) = valres(5)
            df(1,2) = valres(6)
            df(1,3) = 0.d0
            df(2,1) = df(1,2)
            df(2,2) = valres(7)
            df(2,3) = 0.d0
            df(3,1) = 0.d0
            df(3,2) = 0.d0
            df(3,3) = valres(8)
!        --- COUPLAGE  MEMBRANE FLEXION --------------------------------
            dmf(1,1) = 0.d0
            dmf(1,2) = 0.d0
            dmf(1,3) = 0.d0
            dmf(2,1) = 0.d0
            dmf(2,2) = 0.d0
            dmf(2,3) = 0.d0
            dmf(3,1) = 0.d0
            dmf(3,2) = 0.d0
            dmf(3,3) = 0.d0
!        --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
            dc(1,1) = valres(9)
            dc(1,2) = 0.d0
            dc(2,1) = 0.d0
            dc(2,2) = valres(10)
!        --- CALCUL DE SON INVERSE -------------------------------------
            dci(1,1) = 1/valres(9)
            dci(1,2) = 0.d0
            dci(2,1) = 0.d0
            dci(2,2) = 1/valres(10)
!
        else if (elasco.eq.2) then
            multic = 0
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN MEMBRANE -------------
            dm(1,1) = valres(1)
            dm(1,2) = valres(2)
            dm(1,3) = valres(3)
            dm(2,1) = dm(1,2)
            dm(2,2) = valres(4)
            dm(2,3) = valres(5)
            dm(3,1) = dm(1,3)
            dm(3,2) = dm(2,3)
            dm(3,3) = valres(6)
!        ---- CALCUL DE LA MATRICE DE RIGIDITE EN FLEXION --------------
            df(1,1) = valres(7)
            df(1,2) = valres(8)
            df(1,3) = valres(9)
            df(2,1) = df(1,2)
            df(2,2) = valres(10)
            df(2,3) = valres(11)
            df(3,1) = df(1,3)
            df(3,2) = df(2,3)
            df(3,3) = valres(12)
!        --- COUPLAGE  MEMBRANE FLEXION --------------------------------
            dmf(1,1) = valres(13)
            dmf(1,2) = valres(14)
            dmf(1,3) = valres(15)
            dmf(2,1) = dmf(1,2)
            dmf(2,2) = valres(16)
            dmf(2,3) = valres(17)
            dmf(3,1) = dmf(1,3)
            dmf(3,2) = dmf(2,3)
            dmf(3,3) = valres(18)
!        --- COUPLAGE  MEMBRANE CISAILLEMENT ---------------------------
            dmc(1,1) = valres(19)
            dmc(1,2) = valres(20)
            dmc(2,1) = valres(21)
            dmc(2,2) = valres(22)
            dmc(3,1) = valres(23)
            dmc(3,2) = valres(24)
!        --- COUPLAGE  FLEXION CISAILLEMENT ---------------------------
            dfc(1,1) = valres(25)
            dfc(1,2) = valres(26)
            dfc(2,1) = valres(27)
            dfc(2,2) = valres(28)
            dfc(3,1) = valres(29)
            dfc(3,2) = valres(30)
!        --- CALCUL DE LA MATRICE DE RIGIDITE EN CISAILLEMENT ----------
            dc(1,1) = valres(31)
            dc(1,2) = valres(32)
            dc(2,1) = dc(1,2)
            dc(2,2) = valres(33)
!        --- CALCUL DE SON INVERSE -------------------------------------
            det = dc(1,1)*dc(2,2) - dc(1,2)*dc(2,1)
            if (det .gt. r8prem()) then
                dci(1,1) = dc(2,2)/det
                dci(1,2) = -dc(1,2)/det
                dci(2,1) = -dc(2,1)/det
                dci(2,2) = dc(1,1)/det
            else
                call utmess('F', 'ELEMENTS_43')
            endif
        endif
!        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!        --- REACTUALISATION DE LA MATRICE DE FLEXION ET DE LA --------
!        --- MATRICE DE COUPLAGE FLEXION-CISAILLEMENT          --------
!        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                df(i,j) = df(i,j)+deux*excent*dmf(i,j)+excent*excent* dm(i,j)
                dmf(i,j)= dmf(i,j) + excent*dm(i,j)
            end do
        end do
!
        do i = 1, 3
            do j = 1, 2
                dfc(i,j) = dfc(i,j) + excent*dmc(i,j)
            end do
        end do
!
        if (nomte .ne. 'MEDKQG4' .and. nomte .ne. 'MEDKTG3') then
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
            call utbtab('ZERO', 3, 3, dm, t1ve, xab1, dm)
            call utbtab('ZERO', 3, 3, df, t1ve, xab1, df)
            call utbtab('ZERO', 3, 3, dmf, t1ve, xab1, dmf)
            call utbtab('ZERO', 2, 2, dc, t2ui, xab2, dc)
            call utbtab('ZERO', 2, 2, dci, t2ui, xab2, dci)
            if (elasco .eq. 2) then
                call utdtab('ZERO', 3, 2, 2, 3, dmc, t2ui, t1ve, xab3, dmc)
                call utdtab('ZERO', 3, 2, 2, 3, dfc, t2ui, t1ve, xab3, dfc)
            endif
        endif
!
    else if (phenom.eq.'ELAS_COQMU') then
!        ------ MATERIAU MULTICOUCHE -----------------------------------
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 18,&
                    nomres, valres, icodre, 1)
        dm(1,1) = valres(1)
        dm(1,2) = valres(2)
        dm(1,3) = valres(3)
        dm(2,2) = valres(4)
        dm(2,3) = valres(5)
        dm(3,3) = valres(6)
        dm(2,1) = dm(1,2)
        dm(3,1) = dm(1,3)
        dm(3,2) = dm(2,3)
        dmf(1,1) = valres(7)
        dmf(1,2) = valres(8)
        dmf(1,3) = valres(9)
        dmf(2,2) = valres(10)
        dmf(2,3) = valres(11)
        dmf(3,3) = valres(12)
        dmf(2,1) = dmf(1,2)
        dmf(3,1) = dmf(1,3)
        dmf(3,2) = dmf(2,3)
        df(1,1) = valres(13)
        df(1,2) = valres(14)
        df(1,3) = valres(15)
        df(2,2) = valres(16)
        df(2,3) = valres(17)
        df(3,3) = valres(18)
        df(2,1) = df(1,2)
        df(3,1) = df(1,3)
        df(3,2) = df(2,3)
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 6,&
                    nomres(21), valres(21), icodre(21), 1)
        dci(1,1) = valres(21)
        dci(2,2) = valres(22)
        dci(1,2) = valres(23)
        dci(2,1) = dci(1,2)
        dc(1,1) = valres(24)
        dc(2,2) = valres(25)
        dc(1,2) = valres(26)
        dc(2,1) = dc(1,2)
!
!        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!        --- REACTUALISATION DE LA MATRICE DE FLEXION          --------
!        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                df(i,j) = df(i,j)+deux*excent*dmf(i,j)+excent*excent* dm(i,j)
                dmf(i,j)= dmf(i,j) +excent*dm(i,j)
            end do
        end do
!
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        call utbtab('ZERO', 3, 3, dm, t1ve, xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve, xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve, xab1, dmf)
        call utbtab('ZERO', 2, 2, dc, t2ui, xab2, dc)
        call utbtab('ZERO', 2, 2, dci, t2ui, xab2, dci)
!
        multic = 1
!
    else if (phenom.eq.'ELAS_DHRC') then
        multic = 0
        coupmf = .true.
!        ------ MATERIAU DHRC -----------------------------------
        call rcvalb(fami, 1, 1, '+', zi(jmate), ' ', phenom, nbpar, nompar, [valpar], 21,&
                    nomres, valres, icodre, 1)
        dm(1,1) = valres(1)
        dm(1,2) = valres(2)
        dm(1,3) = valres(3)
        dm(2,2) = valres(7)
        dm(2,3) = valres(8)
        dm(3,3) = valres(12)
        dm(2,1) = dm(1,2)
        dm(3,1) = dm(1,3)
        dm(3,2) = dm(2,3)
        dmf(1,1) = valres(4)
        dmf(1,2) = valres(5)
        dmf(1,3) = valres(6)
        dmf(2,1) = valres(9)
        dmf(2,2) = valres(10)
        dmf(2,3) = valres(11)
        dmf(3,1) = valres(13)
        dmf(3,2) = valres(14)
        dmf(3,3) = valres(15)
        df(1,1) = valres(16)
        df(1,2) = valres(17)
        df(1,3) = valres(18)
        df(2,2) = valres(19)
        df(2,3) = valres(20)
        df(3,3) = valres(21)
        df(2,1) = df(1,2)
        df(3,1) = df(1,3)
        df(3,2) = df(2,3)
!
!        --- CALCUL DE LA MATRICE DE COUPLAGE MEMBRANE-FLEXION --------
!        --- REACTUALISATION DE LA MATRICE DE FLEXION          --------
!        --- DANS LE CAS D'UN EXCENTREMENT                     --------
        do i = 1, 3
            do j = 1, 3
                df(i,j) = df(i,j)+deux*excent*dmf(i,j)+excent*excent* dm(i,j)
                dmf(i,j)= dmf(i,j) +excent*dm(i,j)
            end do
        end do
!
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        call utbtab('ZERO', 3, 3, dm, t1ve, xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve, xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve, xab1, dmf)
    endif
!
    do k = 1, 9
        if (abs(dmf(k,1)) .gt. 1.d-10) coupmf = .true.
    end do
!
999 continue
!
end subroutine
