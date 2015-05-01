subroutine splin2(x, d2y, n, ptx, d2yptx,&
                  iret)
    implicit none
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
!-----------------------------------------------------------------------
! DESCRIPTION : INTERPOLATION SPLINE CUBIQUE
! -----------
!               ETANT DONNEE LA TABULATION DE LA FONCTION Y(I) = F(X(I))
!               EN N POINTS DE DISCRETISATION X(I)
!               ETANT DONNEE LA TABULATION DE LA DERIVEE SECONDE DE LA
!               FONCTION INTERPOLEE D2Y(I), CALCULEE EN AMONT PAR LA
!               ROUTINE SPLINE
!               ETANT DONNE UN POINT PTX
!               CETTE ROUTINE CALCULE LA VALEUR D2YPTX DE
!               L'INTERPOLATION SPLINE CUBIQUE DE LA DERIVEE
!               SECONDE DE LA FONCTION AU POINT PTX
!
! IN     : X      : REAL*8 , VECTEUR DE DIMENSION N
!                   CONTIENT LES POINTS DE DISCRETISATION X(I)
! IN     : D2Y    : REAL*8 , VECTEUR DE DIMENSION N
!                   CONTIENT LES VALEURS DE LA DERIVEE SECONDE DE LA
!                   FONCTION INTERPOLEE AUX POINTS X(I)
! IN     : N      : INTEGER , SCALAIRE
!                   NOMBRE DE POINTS DE DISCRETISATION
! IN     : PTX    : REAL*8 , SCALAIRE
!                   VALEUR DU POINT OU L'ON SOUHAITE CALCULER LA DERIVEE
!                   SECONDE DE LA FONCTION INTERPOLEE
! OUT    : D2YPTX : REAL*8 , SCALAIRE
!                   VALEUR DE LA DERIVEE SECONDE DE LA FONCTION
!                   INTERPOLEE AU POINT PTX
! OUT    : IRET   : INTEGER , SCALAIRE , CODE RETOUR
!                   IRET = 0  OK
!                   IRET = 1  DEUX POINTS CONSECUTIFS DE LA
!                             DISCRETISATION X(I) SONT EGAUX
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
!
    real(kind=8) :: x(*), d2y(*), ptx, d2yptx
    integer :: n, iret
!
! VARIABLES LOCALES
! -----------------
    integer :: k, kinf, ksup
    real(kind=8) :: a, b, h
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    iret = 0
!
    kinf = 1
    ksup = n
10  continue
    if (ksup-kinf .gt. 1) then
        k = (ksup+kinf)/2
        if (x(k) .gt. ptx) then
            ksup = k
        else
            kinf = k
        endif
        goto 10
    endif
!
    h = x(ksup)-x(kinf)
    if (h .eq. 0.0d0) then
        iret = 1
        goto 9999
    endif
    a = (x(ksup)-ptx)/h
    b = (ptx-x(kinf))/h
    d2yptx = a * d2y(kinf) + b * d2y(ksup)
!
9999  continue
!
! --- FIN DE SPLIN2.
end subroutine
