subroutine recpar(neq, dti, dtmax, vmin, vvar,&
                  cmp, cdp, dtmin, nper, nrmax)
!
    implicit none
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/u2mesg.h'
    real(kind=8) :: vmin(*), cmp, cpmin, dtmin, dtmax, dti, cdp
    real(kind=8) :: valr(4)
    character(len=8) :: vvar
    character(len=24) :: valk
    integer :: neq, nper, nrmax, i, n1, nv
    integer :: vali(2)
    integer :: iarg
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     RECUPERATION DES PARAMETRES D'ADAPTATION DU PAS
!-----------------------------------------------------------------------
! IN  : NEQ    : NOMBRE D'EQUATION
! IN  : DTI    : PAS INITIAL
! IN  : DTMAX  : PAS MAXIMAL
! OUT : VMIN   : TABLEAU DES VITESSES MINIMALES (PAR DDL)
! OUT : VVAR   : METHODE POUR CALCUL DE LA VITESSE MINIMALE
!                ('NORM' OU 'MAXI')
! OUT : CMP    : COEFFICIENT DE REMONTEE DU PAS DE TEMPS
! OUT : CDP    : COEFFICIENT DE DIVISION DU PAS DE TEMPS
! OUT : DTMIN  : PAS MINIMAL
! OUT : NPER   : NOMBRE DE POINTS PAR PERIODE
! OUT : NRMAX  : NOMBRE MAXIMAL DE REDUCTION DU PAS DE TEMPS
!                PAR PAS DE CALCUL
!
!
!     --- VITESSE DE REFERENCE ---
!
    call getvtx('INCREMENT', 'VITE_MIN', 1, iarg, 1,&
                vvar, nv)
    do 10 i = 1, neq
        vmin(i) = 1.d-15
10  end do
!
!     --- COEFFICIENT DE REMONTEE DU PAS DE TEMPS ---
!
    call getvr8('INCREMENT', 'COEF_MULT_PAS', 1, iarg, 1,&
                cmp, n1)
!
!     --- COEFFICIENT DE DIVISION DU PAS DE TEMPS ---
!
    call getvr8('INCREMENT', 'COEF_DIVI_PAS', 1, iarg, 1,&
                cdp, n1)
!
!     --- COEFFICIENT DETERMINANT DT MIN (=DT INIT * CPMIN) --
!
    call getvr8('INCREMENT', 'PAS_MINI', 1, iarg, 1,&
                dtmin, n1)
!
!     --- COEFFICIENT DETERMINANT DT MIN (=DT INIT * CPMIN) --
!
    if (n1 .eq. 0) then
        call getvr8('INCREMENT', 'PAS_LIMI_RELA', 1, iarg, 1,&
                    cpmin, n1)
        dtmin = dti * cpmin
    endif
!
!     --- NOMBRE DE POINTS PAR PERIODE
!
    call getvis('INCREMENT', 'NB_POIN_PERIODE', 1, iarg, 1,&
                nper, n1)
!
!     --- NMAX_REDUC_PAS
!
    call getvis('INCREMENT', 'NMAX_ITER_PAS', 1, iarg, 1,&
                nrmax, n1)
!
!
    vali (1) = nper
    vali (2) = nrmax
    valr (1) = cmp
    valr (2) = cdp
    valr (3) = dtmin
    valr (4) = dtmax
    valk = vvar
    call u2mesg('I', 'ALGORITH15_68', 1, valk, 2,&
                vali, 4, valr)
end subroutine
