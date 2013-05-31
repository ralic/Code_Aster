subroutine equreg(imate, option, compor, regula, dimdef,&
                  dimcon, defgep, ndim, contp, r,&
                  drde)
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
! aslint: disable=W1306
    implicit      none
    include 'asterfort/reg2gr.h'
    include 'asterfort/regcge.h'
    include 'asterfort/regder.h'
    integer :: imate, dimdef, dimcon, regula(6), ndim
    real(kind=8) :: defgep(dimdef), contp(dimcon), r(dimcon)
    real(kind=8) :: drde(dimcon, dimdef)
    character(len=16) :: option, compor(*)
! ======================================================================
! --- BUT : ROUTINE POUR LA RESOLUTION DES LOI DE COMPORTEMENTS --------
! ---       SECOND GRADIENT --------------------------------------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i
    real(kind=8) :: sigp(ndim*ndim*ndim)
    real(kind=8) :: dsde2g(ndim*ndim*ndim, ndim*ndim*ndim)
! ======================================================================
! --- APPEL A LA RESOLUTION MECANIQUE DE LA LOI REGULARISANTE ----------
! ======================================================================
    call reg2gr(imate, compor, ndim, regula, dimdef,&
                defgep, sigp, dsde2g)
    call regcge(dimdef, dimcon, regula, ndim, defgep,&
                sigp, r)
! ======================================================================
! --- CALCUL DES DERIVEES DES CONTRAINTES GENERALISEES -----------------
! ======================================================================
    call regder(dimdef, dimcon, ndim, regula, dsde2g,&
                drde)
! ======================================================================
! --- RECUPERATION DU VECTEUR CONTRAINTES ------------------------------
! ======================================================================
    if (option(1:9) .eq. 'RAPH_MECA' .or. option(1:9) .eq. 'FULL_MECA') then
        do 40 i = 1, dimcon
            contp(i)=r(i)
40      continue
    endif
! ======================================================================
end subroutine
