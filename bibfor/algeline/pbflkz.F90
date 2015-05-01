function pbflkz(i, z, long, ln, kcalcu)
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
! COUPLAGE FLUIDELASTIQUE, CONFIGURATIONS DU TYPE "COQUE_COAX"
! RESOLUTION DU PROBLEME FLUIDE INSTATIONNAIRE : CALCUL DE LA VALEUR EN
! Z DE LA SOLUTION PARTICULIERE CORRESPONDANT A LA IEME LIGNE DE LA
! MATRICE KCALCU(3,4), DANS LE CAS OU UMOY <> 0
! APPELANT : PBFLGA, PBFLSO
!-----------------------------------------------------------------------
!  IN : I      : INDICE DE LIGNE DE LA MATRICE KCALCU
!  IN : Z      : COTE
!  IN : LONG   : LONGUEUR DU DOMAINE DE RECOUVREMENT DES DEUX COQUES
!  IN : LN     : NOMBRE D'ONDES
!  IN : KCALCU : MATRICE RECTANGULAIRE A COEFFICIENTS CONSTANTS
!                PERMETTANT DE CALCULER UNE SOLUTION PARTICULIERE DU
!                PROBLEME FLUIDE INSTATIONNAIRE, LORSQUE UMOY <> 0
!-----------------------------------------------------------------------
!
    complex(kind=8) :: pbflkz
    integer :: i
    real(kind=8) :: z, long, ln
    complex(kind=8) :: kcalcu(3, 4)
!
    complex(kind=8) :: j, ju
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    real(kind=8) :: u
!-----------------------------------------------------------------------
    j = dcmplx(0.d0,1.d0)
    u = z*ln/long
    ju = j*u
    pbflkz = kcalcu(i,1) * dcmplx(exp(ju)) + kcalcu(i,2) * dcmplx(exp(-1.d0*ju)) + kcalcu(i,3) * &
             &dcmplx(exp(u)) + kcalcu(i,4) * dcmplx(exp(-1.d0*u))
!
end function
