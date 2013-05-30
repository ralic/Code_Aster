subroutine cfresb(ndim, lag2d, typlia, fctf, tang,&
                  rtx, rty, rtz)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit     none
    include 'jeveux.h'
    integer :: ndim
    logical :: lag2d
    character(len=2) :: typlia
    real(kind=8) :: fctf(3)
    real(kind=8) :: tang(6)
    real(kind=8) :: rtx
    real(kind=8) :: rty
    real(kind=8) :: rtz
!
!
! ======================================================================
! ROUTINE APPELEE PAR : CFRESU
! ======================================================================
!
! CALCUL DES FORCES TANGENTIELLES DE CONTACT/FROTTEMENT
!
! IN  NDIM   : DIMENSION DU MODELE
! IN  LAG2D  : VAUT .TRUE. SI LAGRANGIEN 2D
! IN  TYPLIA : TYPE DE LIAISON (F0/F1/F2/GL)
!                'F0': FROTTEMENT (2D) OU FROTTEMENT SUIVANT LES DEUX
!                  DIRECTIONS SIMULTANEES (3D)
!                'F1': FROTTEMENT SUIVANT LA PREMIERE DIRECTION (3D)
!                'F2': FROTTEMENT SUIVANT LA SECONDE DIRECTION (3D)
!                'GL': POUR LE CALCUL DES FORCES DE GLISSEMENT
! IN  FCTF   : FORCES DE FROTTEMENT NODALES
! IN  TANG   : TANGENTES
! OUT RTX    : FORCE TANGENTIELLE PROJETEE SUR X
! OUT RTY    : FORCE TANGENTIELLE PROJETEE SUR Y
! OUT RTZ    : FORCE TANGENTIELLE PROJETEE SUR Z
!
!
!
!
    real(kind=8) :: proj1, proj2
!
! ----------------------------------------------------------------------
!
    if (lag2d) then
        proj1 = fctf(1) * tang(1) + fctf(2) * tang(2)
        rtx = proj1 * tang(1)
        rty = proj1 * tang(2)
        goto 100
    endif
!
    if ((typlia.eq.'F0') .or. (typlia.eq.'GL')) then
        proj1 = fctf(1) * tang(1) + fctf(2) * tang(2)
        proj2 = fctf(1) * tang(4) + fctf(2) * tang(5)
        if (ndim .eq. 3) then
            proj1 = proj1 + fctf(3) * tang(3)
            proj2 = proj2 + fctf(3) * tang(6)
        endif
    else if (typlia.eq.'F1') then
        proj1 = fctf(1) * tang(1) + fctf(2) * tang(2)
        proj2 = 0.d0
        if (ndim .eq. 3) then
            proj1 = proj1 + fctf(3) * tang(3)
        endif
    else if (typlia.eq.'F2') then
        proj1 = 0.d0
        proj2 = fctf(1) * tang(4) + fctf(2) * tang(5)
        if (ndim .eq. 3) then
            proj1 = 0.d0
            proj2 = proj2 + fctf(3) * tang(6)
        endif
    endif
!
    rtx = proj1 * tang(1) + proj2 * tang(4)
    rty = proj1 * tang(2) + proj2 * tang(5)
    rtz = 0.d0
    if (ndim .eq. 3) then
        rtz = proj1 * tang(3) + proj2 * tang(6)
    endif
!
100  continue
!
end subroutine
