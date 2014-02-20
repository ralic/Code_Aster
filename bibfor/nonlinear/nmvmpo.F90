subroutine nmvmpo(fami, npg, option, nomte, nc,&
                  xl, icodma, sect, carcri, compor,&
                  u, du, contm, hoel, hota,&
                  d1b, work, rg0, contp, fl,&
                  klv)
!
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
! aslint: disable=W1504
    implicit none
#include "asterfort/jpd1ff.h"
#include "asterfort/jsd1ff.h"
#include "asterfort/mavec.h"
#include "asterfort/moytem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utbtab.h"
#include "asterfort/utlcal.h"
#include "asterfort/verifm.h"
#include "blas/dscal.h"
    character(len=*) :: fami, option, nomte
    character(len=16) :: compor(*)
    integer :: npg, nc, icodma, itemp, iret
    real(kind=8) :: xl, sect(*), carcri(*), u(2*nc), du(2*nc), fl(2*nc), klv(*)
    real(kind=8) :: contm(3*nc), contp(3*nc)
    real(kind=8) :: temp
    real(kind=8) :: hoel(nc, nc), hota(nc, nc), d1b(nc, 2*nc)
    real(kind=8) :: work(nc, 2*nc), rg0(2*nc, 2*nc)
!
!    - FONCTION REALISEE: COMPORTEMENT VMIS_POU_* REPERE LOCAL
!    - CRITERE GLOBAL DE PLASTICITE, MATERIAU VMIS_POUTRE(_FO)
!    - INTEGRATION IMPLICITE OU RUNGE-KUTTA
!
!    - ARGUMENTS IN:
!         OPTION   : RAPH_MECA, FULL_MECA OU RIGI_MECA_TANG
!         NOMTE    : MECA_POU_D_TG OU MECA_POU_D_T OU MECA_POU_D_E
!         NC       : NOMBRE DE COMPOSANTES DE CONTRAINTES
!         XL       : LONGUEUR DE L ELEMENT
!         ICODMA   : ADRESSE DU MATERIAU CODE
!         SECT     : CARACTERISTIQUES DE LA SECTION
!         CARCRI   : PARAMETRES UTILISATEUR DE CONVERGENCE
!         COMPOR   : RELATION DE COMPORTEMENT
!         U        : VECTEUR DEPLACEMENT A L'INSTANT PRECEDENT
!         DU       : VECTEUR ACCROISSEMENT DE DEPLACEMENT
!         CONTM    : CONTRAINTES A L'INSTANT PRECEDENT
!         ITEMP    : INDICATEUR DE PRESENCE DE TEMPERATURES
!         TEMPM    : TEMPERATURES NODALES A L'INSTANT PRECEDENT
!         TEMPP    : TEMPERATURES NODALES A L'INSTANT ACTUEL
!         HOEL     : MATRICE ELASTIQUE
!         VECTEU   : INDIQUE SI ON DOIT CALCULER SIGP ET VIP
!         MATRIC   : INDIQUE SI ON DOIT CALCULER HOTA
!    - ARGUMENTS OUT:
!         CONTP    : CONTRAINTES A L'INSTANT ACTUEL
!         HOTA     : MATRICE DE COMPORTEMENT TANGENT
!         FL       : FORCE NODALE = BT*CONTP
!         KLV      : MATRICE DE RIGIDITE TANGENTE
!    - TABLEAUX DE TRAVAIL : D1B,WORK,RG0
!         D1B      : MATRICE DE COMPORTEMENT TANGENT
!
!
    integer :: codres(2)
    character(len=2) :: nomres(2)
    character(len=16) :: algo
    logical :: vecteu, matric
    integer :: dimklv, kp, kk, i, j, k
    real(kind=8) :: eps(7), deps(7), fg(14), sigp(7), sigm(7), vip(9)
    real(kind=8) :: e, nu, g, phiy, phiz, xls2, epsthf(1), epsthd(1)
    real(kind=8) :: co(3), aa, xiy, xiz, alfay, alfaz, xjx, xjg
    real(kind=8) :: valres(2)
!
!     POUR LA THERMIQUE
    real(kind=8) :: temm, em, num, f, df
!
!     RECUP DU NOM DE L'ALGORITHME D'INTEGRATION LOCAL
    call utlcal('VALE_NOM', algo, carcri(6))
!
    if (npg .eq. 3) then
        co(1) = 5.d0/9.d0
        co(2) = 8.d0/9.d0
        co(3) = 5.d0/9.d0
    else
        co(1) = 1.d0
        co(2) = 1.d0
    endif
    xls2 = xl / 2.d0
    dimklv = 2*nc*(2*nc+1)/2
!
!     -- 1- 2 BOOLEENS PRATIQUES :
!     ----------------------------
    matric = option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG'
    vecteu = option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA'
!
    call r8inir(nc*nc, 0.d0, hoel, 1)
    call r8inir(2*nc, 0.d0, fl, 1)
    call r8inir(2*nc, 0.d0, fg, 1)
    call r8inir(nc*nc, 0.d0, hota, 1)
    call r8inir(nc*2*nc, 0.d0, d1b, 1)
    call r8inir(2*nc*2*nc, 0.d0, rg0, 1)
!
!     MATERIAU: MOT CLE ELAS, TEMPERATURE MOYENNE
!     INTEGRATION EXPLICITE PAR RUNGE-KUTTA
!     LES COEFFICIENTS DE VMIS_POUTRE SONT EVALUES A TEMP(TMOINS)
!     UTILISE UNIQUEMENT POUR VMIS_POU_LINE ET PAS VMIS_POU_FLEJOU
!
    call verifm(fami, npg, 1, '-', icodma,&
                'ELAS', 1, epsthf, iret)
    call verifm(fami, npg, 1, 'T', icodma,&
                'ELAS', 1, epsthd, iret)
    itemp = 0
    if (iret .eq. 0) itemp = 1
    nomres(1) = 'E'
    nomres(2) = 'NU'
!
!   THERMIQUE À T+
    call moytem(fami, npg, 1, '+', temp,&
                iret)
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'ELAS', 1, 'TEMP', [temp],&
                2, nomres, valres, codres, 1)
    e = valres(1)
    nu = valres(2)
    g = e / (2.d0*(1.d0+nu))
!
!   THERMIQUE À T-
    call moytem(fami, npg, 1, '-', temm,&
                iret)
    call rcvalb(fami, 1, 1, '-', icodma,&
                ' ', 'ELAS', 1, 'TEMP', [temm],&
                2, nomres, valres, codres, 1)
    em = valres(1)
    num = valres(2)
!
!   CARACTERISTIQUES DE LA SECTION :
    aa = sect(1)
    xiy = sect(2)
    xiz = sect(3)
    alfay = sect(4)
    alfaz = sect(5)
    xjx = sect(8)
    xjg = sect(12)
!
!     MATERIAU INTEGRE SUR LA SECTION
!
    hoel(1,1) = e*aa
    if (nomte .eq. 'MECA_POU_D_E') then
        hoel(2,2) = g*aa
        hoel(3,3) = g*aa
        phiy = 0.d0
        phiz = 0.d0
    else
        hoel(2,2) = g*aa/alfay
        hoel(3,3) = g*aa/alfaz
        phiy = e*xiz*12.d0*alfay/ (xl*xl*g*aa)
        phiz = e*xiy*12.d0*alfaz/ (xl*xl*g*aa)
    endif
    hoel(4,4) = g*xjx
    hoel(5,5) = e*xiy
    hoel(6,6) = e*xiz
    if (nomte .eq. 'MECA_POU_D_TG') then
        hoel(7,7) = e*xjg
    endif
!
!     BOUCLE SUR LES POINTS DE GAUSS
    do kp = 1, npg
!        CALCUL DE D1B ( EPSI = D1B * U ) :
        if (nomte .eq. 'MECA_POU_D_TG') then
            call jsd1ff(kp, xl, phiy, phiz, d1b)
        else
            call jpd1ff(kp, xl, phiy, phiz, d1b)
        endif
!        CALCUL DE EPS ET DEPS ET SIGM (EFFORT AU PT DE GAUSS)
!        ET DE DSIGM = INCREMENT D'EFFORT ELASTIQUE
        call r8inir(nc, 0.d0, eps, 1)
        call r8inir(nc, 0.d0, deps, 1)
        call r8inir(nc, 0.d0, sigm, 1)
!
        do i = 1, nc
            do j = 1, 2*nc
                eps(i) = eps(i) + d1b(i,j)* u(j)
                deps(i) = deps(i) + d1b(i,j)*du(j)
            enddo
            sigm(i) = contm(nc*(kp-1)+i)*e/em
        enddo
        if ((epsthd(1).ne.0.d0) .and. (itemp.ne.0)) then
            f = epsthf(1)
            df= epsthd(1)
            eps(1) = eps(1)- f
            deps(1)=deps(1)-df
        endif
!
!       CAS ELASTIQUE
        do i = 1, nc
            hota(i,i) = hoel(i,i)
        enddo
        do i = 1, nc
            sigp(i)=sigm(i)+hoel(i,i)*deps(i)
        enddo
        call r8inir(9, 0.d0, vip, 1)
!
!       CALCUL DE BT*H*B :
        if (matric) then
            call dscal(nc*nc, xls2, hota, 1)
            call dscal(nc*nc, co(kp), hota, 1)
            call utbtab('CUMU', nc, 2*nc, hota, d1b,&
                        work, rg0)
        endif
!
!       ON STOCKE   LES  VARIABLES INTERNES "+"
!                   LES CONTRAINTES "+" ET LE FL :
        if (vecteu) then
            do i = 1, nc
                contp(nc*(kp-1)+i) = sigp(i)
            enddo
            do k = 1, 2*nc
                do kk = 1, nc
                    fl(k)=fl(k) + xls2*sigp(kk)*d1b(kk,k)*co(kp)
                enddo
            enddo
        endif
!
    enddo
!
    if (matric) then
        call mavec(rg0, 2*nc, klv, dimklv)
    endif
!
end subroutine
