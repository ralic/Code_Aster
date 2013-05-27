subroutine epsdil(npi, ipoids, ipoid2, ivf, ivf2,&
                  idfde, idfde2, geom, dimdef, dimuel,&
                  ndim, nddls, nddlm, nno, nnos,&
                  nnom, interp, axi, regula, deplp,&
                  defgep)
! ======================================================================
! person_in_charge: romeo.fernandes at edf.fr
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! TOLE CRP_21 CRS_1404
! ======================================================================
    implicit     none
    include 'asterfort/cabrp0.h'
    include 'asterfort/cabrp1.h'
    include 'asterfort/cabrsl.h'
    logical :: axi
    integer :: npi, ipoids, ipoid2, ivf, ivf2, idfde, idfde2, dimdef, dimuel
    integer :: ndim, nddls, nddlm, nno, nnos, nnom, regula(6)
    real(kind=8) :: geom(ndim, *), deplp(dimuel), defgep(npi*dimdef)
    character(len=2) :: interp
! ======================================================================
! --- BUT : CALCUL DE EPSI_ELGA -----------------------------------
! ======================================================================
    integer :: kpi, i, n
    real(kind=8) :: poids, poids2, b(dimdef, dimuel)
! ======================================================================
! --- BOUCLE SUR LES POINTS D'INTEGRATION ------------------------------
! ======================================================================
    do 100 kpi = 1, npi
! ======================================================================
! --- DEFINITION DE L'OPERATEUR B (DEFINI PAR E=B.U) -------------------
! ======================================================================
        if (interp .eq. 'P0') then
            call cabrp0(kpi, ipoids, ipoid2, ivf, ivf2,&
                        idfde, idfde2, geom, dimdef, dimuel,&
                        ndim, nddls, nddlm, nno, nnos,&
                        nnom, axi, regula, b, poids,&
                        poids2)
        else if (interp.eq.'SL') then
            call cabrsl(kpi, ipoids, ipoid2, ivf, ivf2,&
                        idfde, idfde2, geom, dimdef, dimuel,&
                        ndim, nddls, nddlm, nno, nnos,&
                        nnom, axi, regula, b, poids,&
                        poids2)
        else if (interp.eq.'P1') then
            call cabrp1(kpi, ipoids, ipoid2, ivf, ivf2,&
                        idfde, idfde2, geom, dimdef, dimuel,&
                        ndim, nddls, nddlm, nno, nnos,&
                        nnom, axi, regula, b, poids,&
                        poids2)
        endif
! ======================================================================
! --- CALCUL DES DEFORMATIONS GENERALISEES E=B.U -----------------------
! ======================================================================
        do 10 i = 1, dimdef
            defgep((kpi-1)*dimdef+i)=0.0d0
            do 20 n = 1, dimuel
                defgep((kpi-1)*dimdef+i) = defgep( (kpi-1)*dimdef+i)+ b(i,n)*deplp(n)
20          continue
10      continue
100  end do
! ======================================================================
end subroutine
