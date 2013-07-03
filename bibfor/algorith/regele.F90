subroutine regele(option, typmod, npi, ndim, dimuel,&
                  nddls, nddlm, nno, nnos, nnom,&
                  axi, regula, dimcon, ipoids, ipoid2,&
                  ivf, ivf2, idfde, idfde2, compor,&
                  geom, deplp, contp, imate, dimdef,&
                  matuu, vectu)
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! aslint: disable=W1306,W1504
    implicit      none
#include "asterfort/cabr2g.h"
#include "asterfort/dilopt.h"
#include "asterfort/dilsga.h"
#include "asterfort/equreg.h"
    logical :: axi
    integer :: npi, ipoids, ipoid2, ivf, ivf2, idfde, idfde2, nddls, nddlm
    integer :: imate, dimdef, ndim, nno, nnom, nnos, dimuel, dimcon
    integer :: regula(6)
    real(kind=8) :: vectu(dimuel), matuu(dimuel*dimuel), contp(dimcon*npi)
    real(kind=8) :: geom(ndim, *), deplp(dimuel)
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*)
! ======================================================================
! --- BUT : CALCUL ELEMENTAIRE AUX POINTS D'INTEGRATION ----------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, n, kpi
    real(kind=8) :: b(dimdef, dimuel), poids, poids2, r(dimdef)
    real(kind=8) :: defgep(dimdef), drde(dimdef, dimdef)
! ======================================================================
! --- INITIALISATION DE VECTU, MATUU A 0 SUIVANT OPTION ----------------
! ======================================================================
    if (option(1:9) .eq. 'RIGI_MECA') then
        do 30 i = 1, dimuel*dimuel
            matuu(i)=0.0d0
30      continue
    else if (option(1:9).eq.'RAPH_MECA') then
        do 40 i = 1, dimuel
            vectu(i)=0.0d0
40      continue
    else if (option(1:9).eq.'FULL_MECA') then
        do 50 i = 1, dimuel*dimuel
            matuu(i)=0.0d0
50      continue
        do 60 i = 1, dimuel
            vectu(i)=0.0d0
60      continue
    endif
! ======================================================================
! --- BOUCLE SUR LES POINTS D'INTEGRATION ------------------------------
! ======================================================================
    do 100 kpi = 1, npi
! ======================================================================
! --- DEFINITION DE L'OPERATEUR B (DEFINI PAR E=B.U) -------------------
! ======================================================================
        call cabr2g(kpi, ipoids, ipoid2, ivf, ivf2,&
                    idfde, idfde2, geom, dimdef, dimuel,&
                    ndim, nddls, nddlm, nno, nnos,&
                    nnom, axi, regula, b, poids,&
                    poids2)
! ======================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
! ======================================================================
        do 10 i = 1, dimdef
            defgep(i)=0.0d0
            do 20 n = 1, dimuel
                defgep(i)=defgep(i)+b(i,n)*deplp(n)
20          continue
10      continue
! ======================================================================
! --- CALCUL DES CONTRAINTES VIRTUELLES ET GENERALISEES ----------------
! --- ET DE LEURS DERIVEES ---------------------------------------------
! ======================================================================
        call equreg(imate, option, compor, regula, dimdef,&
                    dimcon, defgep, ndim, contp((kpi-1)*dimcon+1), r,&
                    drde)
! ======================================================================
        if (option(1:9) .eq. 'RIGI_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
! ======================================================================
! --- CALCUL DE SOM_PG(POIDS_PG*BT_PG*DRDE_PG*B_PG) --------------------
! ======================================================================
            call dilopt(dimdef, dimuel, poids, poids2, b,&
                        drde, matuu)
        endif
! ======================================================================
        if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
! ======================================================================
! --- CALCUL DE SOM_PG(POIDS_PG*BT_PG*R_PG) ----------------------------
! ======================================================================
            call dilsga(dimdef, dimuel, poids, poids2, b,&
                        r, vectu)
        endif
! ======================================================================
100  end do
! ======================================================================
end subroutine
