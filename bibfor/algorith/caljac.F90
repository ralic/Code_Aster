subroutine caljac(np3, ic, typch, nbseg, chockc,&
                  rc, theta, vloc, xloc, vloc0,&
                  xloc0, tetaj, jacobc, jacobk)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
! DESCRIPTION : CALCUL DES MATRICES JACOBIENNES LIEES A LA FORCE
! -----------   NON-LINEAIRE DE CHOC F(X,DX)
!
!               APPELANTS : MDCHOE, MDCHOF
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterfort/jacbci.h'
    include 'asterfort/jacbpy.h'
    include 'asterfort/jacbpz.h'
    include 'asterfort/jacbqu.h'
    integer :: np3, ic, typch(*), nbseg(*)
    real(kind=8) :: chockc(*), rc(np3, *), theta(np3, *), vloc(*), xloc(*)
    real(kind=8) :: vloc0(*), xloc0(*), tetaj, jacobc(3, *), jacobk(3, *)
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: cn, kn, vlocj(3), xlocj(3)
!
! ROUTINES EXTERNES
! -----------------
!     EXTERNAL  JACBCI, JACBPY, JACBPZ, JACBQU
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    kn = chockc(1)
    cn = chockc(2)
!
    if (typch(ic) .eq. 0) then
!
! ------ BUTEE PLANE SUIVANT Y
        call jacbpy(kn, cn, jacobc, jacobk)
!
    else if (typch(ic).eq.1) then
!
! ------ BUTEE PLANE SUIVANT Z
        call jacbpz(kn, cn, jacobc, jacobk)
!
    else if (typch(ic).eq.2) then
!
! ------ BUTEE CIRCULAIRE
        do 20 i = 1, 3
            vlocj(i) = (tetaj*vloc(i)) + ((1.0d0-tetaj)*vloc0(i))
            xlocj(i) = (tetaj*xloc(i)) + ((1.0d0-tetaj)*xloc0(i))
20      continue
        call jacbci(np3, rc, vlocj, xlocj, kn,&
                    cn, ic, jacobc, jacobk)
!
    else if (typch(ic).eq.3) then
!
! ------ BUTEE QUELCONQUE
        do 30 i = 1, 3
            xlocj(i) = (tetaj*xloc(i)) + ((1.0d0-tetaj)*xloc0(i))
30      continue
        call jacbqu(np3, nbseg, rc, theta, xlocj,&
                    kn, cn, ic, jacobc, jacobk)
!
    endif
!
! --- FIN DE CALJAC.
end subroutine
