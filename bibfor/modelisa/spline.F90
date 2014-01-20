subroutine spline(x, y, n, dy1, dyn,&
                  d2y, iret)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
! DESCRIPTION : INTERPOLATION SPLINE CUBIQUE
! -----------
!
!               ETANT DONNEE LA TABULATION DE LA FONCTION Y(I) = F(X(I))
!               EN N POINTS DE DISCRETISATION X(I) TELS QUE
!                               X(1) < X(2) < ... < X(N)
!               ETANT DONNEES LES VALEURS DE LA DERIVEE PREMIERE DY1 ET
!               DYN AUX PREMIER ET DERNIER POINTS X(1) ET X(N)
!               CETTE ROUTINE CALCULE LES VALEURS DE LA DERIVEE SECONDE
!               D2Y(I) DE LA FONCTION INTERPOLEE AUX N POINTS X(I)
!
!               SI DY1 ET/OU DYN DEPASSENT EN VALEUR ABSOLUE LE PLUS
!               GRAND NOMBRE ACCESSIBLE PAR LA MACHINE, LA SOLUTION
!               CALCULEE EST TELLE QUE LA DERIVEE SECONDE EST NULLE
!               AUX BORNES DE L'INTERVALLE
!
! IN     : X    : REAL*8 , VECTEUR DE DIMENSION N
!                 CONTIENT LES POINTS DE DISCRETISATION X(I)
! IN     : Y    : REAL*8 , VECTEUR DE DIMENSION N
!                 CONTIENT LES VALEURS DE LA FONCTION AUX POINTS X(I)
! IN     : N    : INTEGER , SCALAIRE
!                 NOMBRE DE POINTS DE DISCRETISATION
! IN     : DY1  : REAL*8 , SCALAIRE
!                 VALEUR DE LA DERIVEE PREMIERE DE LA FONCTION
!                 AU POINT X1
! IN     : DYN  : REAL*8 , SCALAIRE
!                 VALEUR DE LA DERIVEE PREMIERE DE LA FONCTION
!                 AU POINT XN
! OUT    : D2Y  : REAL*8 , VECTEUR DE DIMENSION N
!                 CONTIENT LES VALEURS DE LA DERIVEE SECONDE
!                 DE LA FONCTION INTERPOLEE AUX POINTS X(I)
! OUT    : IRET : INTEGER , SCALAIRE , CODE RETOUR
!                 IRET = 0  OK
!                 IRET = 1  VALEUR DE N INVALIDE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    real(kind=8) :: x(*), y(*), dy1, dyn, d2y(*)
    integer :: n, iret
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: bignum, p, qn, sig, un
    real(kind=8), pointer :: work(:) => null()
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    AS_ALLOCATE(vr=work, size=n)
!
    iret = 0
    if (n .lt. 3) then
        iret = 1
        goto 9999
    endif
!
    bignum = 0.99d0 / r8miem()
!
    if (dble(abs(dy1)) .gt. bignum) then
        d2y(1) = 0.0d0
        work(1) = 0.0d0
    else
        d2y(1) = -0.5d0
        work(1) = 3.0d0/(x(2)-x(1)) * ( (y(2)-y(1))/(x(2)-x(1)) - dy1 )
    endif
!
    do 10 i = 2, n-1
        sig = (x(i)-x(i-1))/(x(i+1)-x(i-1))
        p = sig * d2y(i-1) + 2.0d0
        d2y(i) = (sig-1.0d0) / p
        work(i) = (&
                     6.0d0 * (&
                     (&
                     y(i+1)-y(i))/(x(i+1)-x(i)) - (y(i)- y(i-1))/(x(i)-x(i-1)) ) / (x(i+1)-x(i-1)&
                     ) - sig * work(1+i-2&
                     )&
                     ) / p
10  end do
!
    if (dble(abs(dyn)) .gt. bignum) then
        qn = 0.0d0
        un = 0.0d0
    else
        qn = 0.5d0
        un = 3.0d0/(x(n)-x(n-1)) * ( dyn - (y(n)-y(n-1))/(x(n)-x(n-1)) )
    endif
!
    d2y(n) = (un-qn*work(1+n-2))/(qn*d2y(n-1)+1.0d0)
    do 20 i = n-1, 1, -1
        d2y(i) = d2y(i) * d2y(i+1) + work(i)
20  end do
!
9999  continue
    AS_DEALLOCATE(vr=work)
    call jedema()
!
! --- FIN DE SPLINE.
end subroutine
