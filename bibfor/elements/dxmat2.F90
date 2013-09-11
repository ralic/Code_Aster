subroutine dxmat2(pgl, icou, npg, ordi, epi,&
                  epais, dm, indith)
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
#include "asterfort/u2mess.h"
#include "asterfort/utbtab.h"
    integer :: icou, npg, indith
    real(kind=8) :: pgl(3, 3), ordi, epi, epais, dm(3, 3)
! ----------------------------------------------------------------------
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
!
!     BUT:
!       CALCUL DES MATRICES DE COEFFCIENTS THERMOELASTIQUES DE MEMBRANE,
!       POUR UN MATERIAU ISOTROPE OU MULTICOUCHE
!       LA VARIABLE INDITH EST INITIALISEE A 0
!       DANS LE CAS OU LE COEFFICIENT DE DILATATION ALPHA N'A
!       PAS ETE DONNE, INDITH VAUT -1 ET ON  NE CALCULE PAS LES
!       CONTRAINTES THERMIQUES
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   PGL(3,3) : MATRICE DE PASSAGE DU REPERE GLOBAL AU REPERE LOCAL
! IN   ICOU     : NUMERO DE LA COUCHE
! IN   NPG      : NOMBRE DE POINT DE GAUSS
!
!      SORTIE :
!-------------
! OUT  ORDI     :
! OUT  EPI      :
! OUT  EPAIS    :
! OUT  DM       :
! OUT  INDITH   :
!
! ......................................................................
!
!
!
!
    integer :: jcoqu, jmate, iret
    integer :: nbv, i, nbpar, nbcou, jcou
    integer :: icodre(134)
!
    real(kind=8) :: cdf, cdm, valres(134), df(3, 3), dmf(3, 3), t1ve(9)
    real(kind=8) :: young, nu, valpar
    real(kind=8) :: xab1(3, 3), dh(3, 3)
    real(kind=8) :: s, c
    real(kind=8) :: alphat
    real(kind=8) :: alpha, beta, r8bid4(4)
!
    character(len=2) :: val
    character(len=3) :: num
    character(len=8) :: nomres(134), nompar
    character(len=10) :: phenom
!
!     ------------------------------------------------------------------
!
    call r8inir(9, 0.d0, dm, 1)
    call r8inir(9, 0.d0, df, 1)
    call r8inir(9, 0.d0, dh, 1)
    call r8inir(9, 0.d0, dmf, 1)
!
    call jevech('PCACOQU', 'L', jcoqu)
    epais = zr(jcoqu)
    epi = epais
    ordi = 0.d0
    alpha = zr(jcoqu+1)*r8dgrd()
    beta = zr(jcoqu+2)*r8dgrd()
!
!     ------------------------------------------------
    indith = 0
    call jevech('PMATERC', 'L', jmate)
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS_COQMU') then
!
        call coqrep(pgl, alpha, beta, r8bid4, r8bid4,&
                    c, s)
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
        do 10 i = 1, nbv
            call codent(i, 'G', num)
            nomres(i) = 'HOM_'//num
10      continue
        call codent(icou, 'G', num)
        do 20 i = 1, 78
            call codent(i, 'G', val)
            nomres(56+i) = 'C'//num//'_V'//val
20      continue
!
    else if (phenom.eq.'ELAS') then
        nbv = 3
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nomres(3) = 'ALPHA'
!
    else if (phenom.eq.'ELAS_COQUE') then
!
        call coqrep(pgl, alpha, beta, r8bid4, r8bid4,&
                    c, s)
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
    else
        call u2mess('F', 'ELEMENTS_42')
    endif
!
!===============================================================
!     -- RECUPERATION DE LA TEMPERATURE POUR LE MATERIAU:
    call jevech('PNBSP_I', 'L', jcou)
    nbcou=zi(jcou)
    call moytem('RIGI', npg, 3*nbcou, '+', valpar,&
                iret)
    nbpar = 1
    nompar = 'TEMP'
!===============================================================
!
    if (phenom .eq. 'ELAS') then
!        ------ MATERIAU ISOTROPE ------------------------------------
!
!
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    2, nomres, valres, icodre, 1)
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres(3), valres(3), icodre(3), 0)
        if ((icodre(3).ne.0) .or. (valres(3).eq.0.d0)) then
            indith = -1
            goto 70
        else if ((iret.eq.1).and.(icodre(3).ne.0)) then
            call u2mess('F', 'CALCULEL_15')
        endif
        young = valres(1)
        nu = valres(2)
        alphat = valres(3)
        young = young*alphat
!      ------------------------------------------------------------
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
!        ---------------------------------------------------------------
!
    else if (phenom.eq.'ELAS_COQUE') then
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres(11), valres(11), icodre(11), 0)
        if ((icodre(11).ne.0) .or. (valres(11).eq.0.d0)) then
            indith = -1
            goto 70
        else if ((iret.eq.1).and.(icodre(11).ne.0)) then
            call u2mess('F', 'CALCULEL_15')
        endif
        alphat = valres(11)
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
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
        call utbtab('ZERO', 3, 3, dm, t1ve,&
                    xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve,&
                    xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve,&
                    xab1, dmf)
!
    else if (phenom.eq.'ELAS_COQMU') then
!        ------ MATERIAU MULTICOUCHE -----------------------------------
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres(19), valres(19), icodre(19), 1)
        epais = valres(19)
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres(57), valres(57), icodre(57), 1)
        epi = valres(57)
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    1, nomres(59), valres(59), icodre(59), 1)
        ordi = valres(59)
        call rcvalb('RIGI', 1, 1, '+', zi(jmate),&
                    ' ', phenom, nbpar, nompar, valpar,&
                    27, nomres(102), valres(102), icodre(102), 1)
        dm(1,1) = valres(102)
        dm(1,2) = valres(103)
        dm(1,3) = valres(104)
        dm(2,1) = valres(105)
        dm(2,2) = valres(106)
        dm(2,3) = valres(107)
        dm(3,1) = valres(108)
        dm(3,2) = valres(109)
        dm(3,3) = valres(110)
        dmf(1,1) = valres(111)
        dmf(1,2) = valres(112)
        dmf(1,3) = valres(113)
        dmf(2,1) = valres(114)
        dmf(2,2) = valres(115)
        dmf(2,3) = valres(116)
        dmf(3,1) = valres(117)
        dmf(3,2) = valres(118)
        dmf(3,3) = valres(119)
        df(1,1) = valres(120)
        df(1,2) = valres(121)
        df(1,3) = valres(122)
        df(2,1) = valres(123)
        df(2,2) = valres(124)
        df(2,3) = valres(125)
        df(3,1) = valres(126)
        df(3,2) = valres(127)
        df(3,3) = valres(128)
!
!        ----------- MATRICES DANS LE REPERE INTRINSEQUE DE L'ELEMENT --
!
        call utbtab('ZERO', 3, 3, dm, t1ve,&
                    xab1, dm)
        call utbtab('ZERO', 3, 3, df, t1ve,&
                    xab1, df)
        call utbtab('ZERO', 3, 3, dmf, t1ve,&
                    xab1, dmf)
!
    endif
70  continue
end subroutine
