subroutine recpar(neq, dti, dtmax, vmin, vvar,&
                  cmp, cdp, dtmin, nper, nrmax)
!
    implicit none
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/utmess.h"
    real(kind=8) :: vmin(*), cmp, cpmin, dtmin, dtmax, dti, cdp
    real(kind=8) :: valr(4)
    character(len=8) :: vvar
    character(len=24) :: valk
    integer :: neq, nper, nrmax, i, n1, nv
    integer :: vali(2)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    call getvtx('INCREMENT', 'VITE_MIN', iocc=1, scal=vvar, nbret=nv)
    do 10 i = 1, neq
        vmin(i) = 1.d-15
10  end do
!
!     --- COEFFICIENT DE REMONTEE DU PAS DE TEMPS ---
!
    call getvr8('INCREMENT', 'COEF_MULT_PAS', iocc=1, scal=cmp, nbret=n1)
!
!     --- COEFFICIENT DE DIVISION DU PAS DE TEMPS ---
!
    call getvr8('INCREMENT', 'COEF_DIVI_PAS', iocc=1, scal=cdp, nbret=n1)
!
!     --- COEFFICIENT DETERMINANT DT MIN (=DT INIT * CPMIN) --
!
    call getvr8('INCREMENT', 'PAS_MINI', iocc=1, scal=dtmin, nbret=n1)
!
!     --- COEFFICIENT DETERMINANT DT MIN (=DT INIT * CPMIN) --
!
    if (n1 .eq. 0) then
        call getvr8('INCREMENT', 'PAS_LIMI_RELA', iocc=1, scal=cpmin, nbret=n1)
        dtmin = dti * cpmin
    endif
!
!     --- NOMBRE DE POINTS PAR PERIODE
!
    call getvis('INCREMENT', 'NB_POIN_PERIODE', iocc=1, scal=nper, nbret=n1)
!
!     --- NMAX_REDUC_PAS
!
    call getvis('INCREMENT', 'NMAX_ITER_PAS', iocc=1, scal=nrmax, nbret=n1)
!
!
    vali (1) = nper
    vali (2) = nrmax
    valr (1) = cmp
    valr (2) = cdp
    valr (3) = dtmin
    valr (4) = dtmax
    valk = vvar
    call utmess('I', 'ALGORITH15_68', sk=valk, ni=2, vali=vali,&
                nr=4, valr=valr)
end subroutine
