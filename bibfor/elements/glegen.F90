subroutine glegen(nbre, lobj2, xl, absgam, legen)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!
!     ------------------------------------------------------------------
!
! FONCTION REALISEE:
!
!      CALCUL DES VALEURS DES POLYNOMES DE LEGENDRE
!
!     ------------------------------------------------------------------
! ENTREE:
!        NBRE       : DEGRE DES POLYNOMES DE LEGENDRE
!        LOBJ2      : NOMBRE DE NOEUDS SUR GAMMA0
!        XL         : LONGUEUR DE LA FISSURE
!        ABSGAM     : ABSCISSES CURVILIGNES
!
! SORTIE:
!        LEGEN      : VALEURS DES POLYNOMES DE LEGENDRE
!     ------------------------------------------------------------------
!
!
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: lobj2, nbre, iadabs, iadpo
!
    real(kind=8) :: xl, s1, coef
!
    character(len=24) :: absgam
    real(kind=8) :: legen(1)
    integer :: i, j
    real(kind=8) :: cof1, cof2, x, pleg2, pleg3, pleg4, pleg5, pleg6, pleg7
!
!   POLYNOMES DE LEGENDRE
!
    pleg2(x) = (3.d0*x*x-1.d0)/2.d0
    pleg3(x) = x*(5.d0*x*x-3.d0)/2.d0
    pleg4(x) = (35.d0*x**4-30.d0*x*x+3.d0)/8.d0
    pleg5(x) = x*(63.d0*x**4-70.d0*x*x+15.d0)/8.d0
    pleg6(x) = (231.d0*x**6-315.d0*x**4+105.d0*x*x-5.d0)/16.d0
    pleg7(x) = x*(429.d0*x**6-693.d0*x**4+315.d0*x*x-35.d0)/16.d0
!
!-----------------------------------------------------------------------
    call jemarq()
    call jeveuo(absgam, 'L', iadabs)
!
! CALCUL DES MODULES DES CHAMPS THETA = POLYNOMES DE LEGENDRE
!
    call wkvect('&&LEGEND.VALPOL', 'V V R8', (nbre+1)*lobj2, iadpo)
!
! COEFFICIENTS POUR LES 2 PREMIERS POLYNOMES
!
    cof1 = sqrt(1.d0/xl)
    cof2 = sqrt(3.d0/xl)
!
    do 1 i = 1, lobj2
        zr(iadpo + i - 1) = 1.d0
        legen(i) = cof1*zr(iadpo + i - 1)
        if (nbre .ne. 0) then
            zr(iadpo + lobj2 + i - 1) = -1+2*zr(iadabs + i - 1)/xl
            legen(lobj2 + i) = cof2*zr(iadpo + lobj2 + i - 1)
        endif
 1  end do
!
    if (nbre .ge. 2) then
        coef = sqrt(5.d0/xl)
        do 3 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 2*lobj2 + j ) = coef*pleg2(s1)
 3      continue
    endif
!
    if (nbre .ge. 3) then
        coef = sqrt(7.d0/xl)
        do 4 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 3*lobj2 + j ) = coef*pleg3(s1)
 4      continue
    endif
!
    if (nbre .ge. 4) then
        coef = sqrt(9.d0/xl)
        do 5 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 4*lobj2 + j ) = coef*pleg4(s1)
 5      continue
    endif
!
    if (nbre .ge. 5) then
        coef = sqrt(11.d0/xl)
        do 6 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 5*lobj2 + j ) = coef*pleg5(s1)
 6      continue
    endif
!
    if (nbre .ge. 6) then
        coef = sqrt(13.d0/xl)
        do 7 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 6*lobj2 + j ) = coef*pleg6(s1)
 7      continue
    endif
!
    if (nbre .ge. 7) then
        coef = sqrt(15.d0/xl)
        do 8 j = 1, lobj2
            s1 = -1+2*zr(iadabs + j - 1)/xl
            legen( 7*lobj2 + j ) = coef*pleg7(s1)
 8      continue
    endif
!
    call jedetr('&&LEGEND.VALPOL')
    call jedema()
end subroutine
