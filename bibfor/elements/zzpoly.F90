subroutine zzpoly(nno, ino, xino, yino, sig,&
                  b)
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
!    ESTIMATEUR ZZ2 (VERSION 92)
!
!  CETTE ROUTINE CALCULE LA VALEUR DES CONTRAINTES LISSEES AU NOEUD INO
!
!  ENTREE :
!       NNO   :  NOMBRE DE NOEUDS DE L'ELEMENT
!       INO   :  NUMERO GLOBAL DU NOEUD OU ON CALCULE LES CONTRAINTES
!  XINO, YINO :  COORDONNEES DU NOEUD INO
!       B     :  COEFFICIENTS DES MONOMES DU POLYNOME DE BASE
!
!  SORTIE :
!       SIG   :  CONTRAINTES LISSEES
!
    include 'asterfort/u2mess.h'
    real(kind=8) :: xino, yino, sig(1), b(9, 4)
!-----------------------------------------------------------------------
    integer :: ic, ino, nno
!-----------------------------------------------------------------------
    if (nno .eq. 3) then
        do 3 ic = 1, 4
            sig(4*(ino-1)+ic) = sig( 4*(ino-1)+ic) + b(1,ic)+b(2,ic)* xino+b(3,ic )*yino
 3      continue
    else if (nno.eq.6) then
        do 4 ic = 1, 4
            sig(4*(ino-1)+ic) = sig(&
                                4*(ino-1)+ic) + b(1, ic)+b(2, ic)* xino+b(3, ic)*yino + b(4,&
                                ic)*xino*yino + b(5, ic)*xino*xino + b(6, ic&
                                )*yino*yino
 4      continue
    else if (nno.eq.4) then
        do 5 ic = 1, 4
            sig(4*(ino-1)+ic) = sig(&
                                4*(ino-1)+ic) + b(1, ic)+b(2, ic)* xino+b(3, ic)*yino + b(4, ic&
                                )*xino*yino
 5      continue
    else if (nno.eq.8) then
        do 6 ic = 1, 4
            sig(4*(ino-1)+ic) = sig(&
                                4*(ino-1)+ic) + b(1, ic)+b(2, ic)* xino+b(3, ic)*yino + b(4,&
                                ic)*xino*yino + b(5, ic)*xino*xino + b(6, ic)*yino*yino + b(7,&
                                ic)*xino*xino*yino + b(8, ic&
                                )* xino*yino*yino
 6      continue
    else if (nno.eq.9) then
        do 7 ic = 1, 4
            sig(4*(ino-1)+ic) = sig(&
                                4*(ino-1)+ic) + b(1, ic)+b(2, ic)* xino+b(3, ic)*yino + b(4,&
                                ic)*xino*yino + b(5, ic)*xino*xino + b(6, ic)*yino*yino + b(7,&
                                ic)*xino*xino*yino + b(8, ic)* xino*yino*yino + b(9, ic&
                                )*xino*xino*yino*yino
 7      continue
    else
        call u2mess('F', 'ELEMENTS4_62')
    endif
end subroutine
