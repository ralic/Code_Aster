subroutine arcseg34(nbno,coor,abscur)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "blas/ddot.h"
#include "asterfort/mgauss.h"
#include "asterfort/utmess.h"
#include "asterfort/provec.h"
!
              integer, intent(in) :: nbno
              real(kind=8), intent(in) :: coor(3,nbno)
              real(kind=8), intent(out) :: abscur(nbno)
! ......................................................................
!  but:  calcul de l'abscisse curviligne de 3 ou 4 points situés sur
!        un arc de cercle
!
!  arguments :
!     nbno : nombre de noeuds de l'arc (SEG3 ou SEG4)
!     coor : coordonnees des noeuds de l'arc
!     abscur : abscisse curviligne des noeuds sur l'arc
!
!  remarques importantes :
!     1) l'arc est suppose etre proche d'un arc de cercle.
!     2) l'ordre des noeuds est celui des SEG3/4 :
!              1 - 3 - 2       (SEG3)
!        ou    1 - 3 - 4 - 2   (SEG4)
!     3) => abscur(1)=0, abscur(2)=longueur totale de l'arc
! ......................................................................
!
    real(kind=8) :: a(3), b(3), c(3), ab(3), bc(3), ce(3),mab(3),mbc(3)
    real(kind=8) :: n(3),mat(3,3), r, r2 ,ra(3), rk(3),x(3),det
    real(kind=8) :: sintheta, costheta, theta, valr(6)
    integer :: k,iret
!   ----------------------------------------------------------------------------
    ASSERT(nbno.ge.1 .and. nbno.le.4)

!   Soit A, B et C 3 noeuds de l'arc (A et C sont les extremites)
    a(:)=coor(:,1)
    c(:)=coor(:,2)
    b(:)=coor(:,3)
    ab(:)=b(:)-a(:)
    bc(:)=c(:)-b(:)
    call provec(ab, bc, n)


!   -- 1. Cas des SEG2 (forcement droit) :
!   ---------------------------------------
    if (nbno.eq.2) then
        abscur(1)=0.d0
        x(:)=coor(:,2)-coor(:,1)
        abscur(2)=abscur(1)+sqrt(ddot(3,x,1,x,1))
        goto 999
    endif


!   -- 1. Cas des points presque alignes :
!   ---------------------------------------
!   -- les points sont presqu'alignes si l'angle (ab,bc) < 1 degre :
    sintheta=sqrt(ddot(3,n,1,n,1))/(sqrt(ddot(3,ab,1,ab,1))*sqrt(ddot(3,bc,1,bc,1)))
    theta=abs(asin(sintheta))
    if ((theta*180./3.14) .lt. 1.) then
!       -- calcul des abscisses curvilignes :
        abscur(1)=0.d0
        x(:)=coor(:,3)-coor(:,1)
        abscur(3)=abscur(1)+sqrt(ddot(3,x,1,x,1))
        if (nbno.eq.3) then
            x(:)=coor(:,2)-coor(:,3)
            abscur(2)=abscur(3)+sqrt(ddot(3,x,1,x,1))
        else
            x(:)=coor(:,4)-coor(:,3)
            abscur(4)=abscur(3)+sqrt(ddot(3,x,1,x,1))
            x(:)=coor(:,2)-coor(:,4)
            abscur(2)=abscur(4)+sqrt(ddot(3,x,1,x,1))
        endif
        goto 999
    endif


!   -- 2. Cas des points formant un arc de cercle :
!   ------------------------------------------------

!   -- 2.1 Determination du centre du cercle avec les 3 premiers noeuds (CE) :
!   Le centre CE est l'intersection des 3 plans :
!     * P1 : le plan mediateur de AB
!     * P2 : le plan mediateur de BC
!     * P3 : le plan contenant A, B et C
    mab(:)=(a(:)+b(:))/2.d0
    mbc(:)=(b(:)+c(:))/2.d0

!   -- mat est la matrice (3,3) contenant les coefs. des equations des 3 plans :
!   -- ce  est le vecteur (3) contenant les termes constants des 3 plans :

!   -- plan P1 :
    mat(1,:)=ab(:)
    ce(1)= ddot(3,mab,1,ab,1)

!   -- plan P2 :
    mat(2,:)=bc(:)
    ce(2)= ddot(3,mbc,1,bc,1)

!   -- plan P3 :
    mat(3,:)=n(:)
    ce(3)= ddot(3,n,1,a,1)

!   -- resolution de mat*ce=ce pour trouver CE :
    call mgauss('NFSP', mat, ce, 3, 3, 1, det, iret)
    ASSERT(iret.eq.0)

!   -- on verifie que les points sont bien sur le cercle trouve (a 1% pres):
    r=sqrt( (ce(1)-a(1))**2 + (ce(2)-a(2))**2 + (ce(3)-a(3))**2)
    ASSERT(r.gt.0.d0)
    do k=2,nbno
        r2=sqrt( (ce(1)-coor(1,k))**2 + (ce(2)-coor(2,k))**2 + (ce(3)-coor(3,k))**2)
        if (abs((r2-r)/r) .gt. 1.d-2) then
            valr(1:3)=a(:)
            valr(4:6)=c(:)
            call utmess('F','MODELISA_4',nr=6, si=nbno, valr=valr)
        endif
    enddo

!   -- calcul des abscisses curvilignes :
    abscur(1)=0.d0
    ra(:)=ce(:)-a(:)
    do k=2,nbno
        rk(:)=ce(:)-coor(:,k)
        costheta=ddot(3,ra,1,rk,1)/(r*r)
        theta=acos(costheta)
        abscur(k)=r*theta
    enddo

999 continue

end subroutine
