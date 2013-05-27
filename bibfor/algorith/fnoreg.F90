subroutine fnoreg(dimuel, dimdef, nno, nnos, nnom,&
                  ndim, npi, dimcon, geom, ipoids,&
                  ipoid2, ivf, ivf2, idfde, idfde2,&
                  nddls, nddlm, axi, regula, deplm,&
                  contm, imate, vectu)
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRP_21 CRS_1404
! ======================================================================
    implicit      none
    include 'asterfort/cabr2g.h'
    include 'asterfort/dilsga.h'
    include 'asterfort/regcge.h'
    logical :: axi
    integer :: dimuel, dimdef, nno, nnos, nnom, ndim, npi, dimcon, ipoids
    integer :: ipoid2, ivf, ivf2, idfde, idfde2, nddls, nddlm, imate
    integer :: regula(6)
    real(kind=8) :: geom(ndim, *), deplm(dimuel), vectu(dimuel)
    real(kind=8) :: contm(dimcon*npi)
! ======================================================================
! --- BUT : CALCUL DES FORCES NODALES A PARTIR DE RIGI_MECA ------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, kpi, n
    real(kind=8) :: b(dimdef, dimuel), poids, poids2, defgem(dimdef), r(dimdef)
! ======================================================================
    do 10 i = 1, dimuel
        vectu(i)=0.0d0
10  end do
!
    do 100 kpi = 1, npi
! ======================================================================
! --- INITIALISATION DE R ----------------------------------------------
! ======================================================================
        do 22 i = 1, dimdef
            r(i) = 0.0d0
22      continue
! ======================================================================
        call cabr2g(kpi, ipoids, ipoid2, ivf, ivf2,&
                    idfde, idfde2, geom, dimdef, dimuel,&
                    ndim, nddls, nddlm, nno, nnos,&
                    nnom, axi, regula, b, poids,&
                    poids2)
! ======================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
! ======================================================================
        do 110 i = 1, dimdef
            defgem(i)=0.0d0
            do 120 n = 1, dimuel
                defgem(i)=defgem(i)+b(i,n)*deplm(n)
120          continue
110      continue
! ======================================================================
! --- CALCUL DES CONTRAINTES GENERALISEES FINALES ----------------------
! ======================================================================
        call regcge(dimdef, dimcon, regula, ndim, defgem,&
                    contm((kpi-1)* dimcon+1), r)
! ======================================================================
        call dilsga(dimdef, dimuel, poids, poids2, b,&
                    r, vectu)
! ======================================================================
100  end do
! ======================================================================
end subroutine
