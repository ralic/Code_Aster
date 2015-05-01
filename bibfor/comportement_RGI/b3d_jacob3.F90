subroutine b3d_jacob3(a, idim, d, x, control,&
                      epsv)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!     version modifiee par A.Sellier le sam. 28 août 2010 18:16:10 CEST
!     pour corriger le pb (rencontre sous linux)  de non detection de
!     deux valeurs propres petites considerees a tort comme non double :
!     1)on definit deps en valeur absolue pour eviter les pb liees aux vp
!     negative
!     2)dans le cas ou les deux valeur propre sont egales et de
!     signes opposes, la difference est superieure au test ce qui est correct
!     mais difficile a traiter numeriquement lorsque les valeurs sont toutes deux
!     petites par rapport à la troisieme ( elle pourrait alors etre toute
!     deux considerees comme nulle), il faut donc tester si la somme des
!     valeurs absolue peut etre considere comme negligeable par rapport
!     à la troisieme, si c est le cas on impose a ces deux valeurs considere
!     d etre egales a leur moyenne...
!
!======================================================================
!     OBJET
!     -----
!     DIAGONALISATION D UNE MATRICE 3*3 SYMETRIQUE
!
!     ENTREES
!     -------
!     A(3,3) = MATRICE SYMETRIQUE
!     IDIM   = 2 OU 3  SI 2 ON NE S OCCUPE QUE DE A(2,2)
!                      SI 3                    DE A(3,3)
!     SORTIES
!     -------
!     D(3)   = VALEURS PROPRES ORDONNEES D(1)>D(2)>D(3)
!
!     S(3,3) = VECTEURS PROPRES ( S(IP,2) EST LE VECTEUR
!                                 ASSOCIE A D(2) )
!
!===============================================================
    implicit none
#include "asterfort/jacob2.h"
#include "asterfort/b3d_degre3.h"
#include "asterfort/b3d_vectp.h"
!     varibles supplementaires...
    real(kind=8) :: a(3, 3)
    integer :: idim
    real(kind=8) :: d(3)
    real(kind=8) :: x(3, 3)
    logical :: control
    real(kind=8) :: epsv, xmaxi
    real(kind=8) :: ad(3), c1, c2, c0, deps, aux, d1, d2, d3, xi1, xi2, xi3
!
    if (idim .ne. 3) then
        call jacob2(a, d, x)
    endif
    c2=-a(1,1)-a(2,2)-a(3,3)
    c1= (a(1,1)*a(2,2)+a(2,2)*a(3,3)+a(3,3)*a(1,1))&
        - a(1,3)**2 - a(1,2)**2 - a(2,3)**2
    c0=-2.d0*a(1,2)*a(1,3)*a(2,3) + a(1,1)*a(2,3)**2&
        + a(2,2)*a(1,3)**2 + a(3,3)*a(1,2)**2&
        - a(1,1)*a(2,2)*a(3,3)
    call b3d_degre3(c0, c1, c2, d1, xi1,&
                    d2, xi2, d3, xi3)
    d(1)=max(d1,d2,d3)
    d(3)=min(d1,d2,d3)
    d(2)=d1+d2+d3-d(1)-d(3)
!     on impose aux petites valeurs propres d etres exactement egale
!     pour eviter les pb de test de valeurs double
    ad(1)=abs(d(1))
    ad(2)=abs(d(2))
    ad(3)=abs(d(3))
    xmaxi=max(ad(1),ad(2),ad(3))
    deps=xmaxi*epsv
    if ((ad(2)+ad(3)) .le. deps) then
        aux=0.5d0*(d(2)+d(3))
!       print*,'PASSAGE 1 ds jacob3'
!       print*,d(1),d(2),d(3)
        d1=d(1)
        d2=aux
        d3=aux
    else
        if (ad(1)+ad(3) .le. deps) then
            aux=0.5d0*(d(1)+d(3))
!        print*,'PASSAGE 2 ds jacob3'
!        print*,d(1),d(2),d(3)
            d1=aux
            d2=d(2)
            d3=aux
        else
            if (ad(1)+ad(2) .le. deps) then
                aux=0.5d0*(d(1)+d(2))
!         print*,'PASSAGE 3 ds jacob3'
!         print*,d(1),d(2),d(3)
                d1=aux
                d2=aux
                d3=d(3)
            end if
        end if
    end if
!     on reclasse les valeurs propres
    d(1)=max(d1,d2,d3)
    d(3)=min(d1,d2,d3)
    d(2)=d1+d2+d3-d(1)-d(3)
!      if(d(2).lt.d(3))then
!       print*,'PB 2 ds jacob3'
!       print*,d(1),d(2),d(3)
!       stop
!      end if
!     rajout du abs ds le deps
    deps=abs(d(1)*epsv)
    if (d(1)-d(2) .le. deps) then
!      valeur propre double
        control=.true.
        if (d(2)-d(3) .le. deps) then
!       valeur propre triple
            call b3d_vectp(a, d(1), x(1, 1), 3)
        else
            call b3d_vectp(a, d(1), x(1, 1), 2)
            call b3d_vectp(a, d(3), x(1, 3), 1)
        endif
    else
!      rajout du abs ds le deps
        deps=abs(d(2)*epsv)
        if (d(2)-d(3) .le. deps) then
!       valeur propre double
            control=.true.
            call b3d_vectp(a, d(1), x(1, 1), 1)
            call b3d_vectp(a, d(2), x(1, 2), 2)
        else
!       cas normal
            control=.false.
            call b3d_vectp(a, d(1), x(1, 1), 1)
            call b3d_vectp(a, d(2), x(1, 2), 1)
            call b3d_vectp(a, d(3), x(1, 3), 1)
        endif
    endif
end subroutine
