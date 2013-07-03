subroutine xproj(p, a, b, c, m,&
                 mp, d, vn, eps, in)
    implicit none
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/normev.h"
#include "asterfort/padist.h"
#include "asterfort/provec.h"
#include "blas/ddot.h"
    real(kind=8) :: p(3), a(3), b(3), c(3), m(3), mp(3), vn(3), eps(3), d
    logical :: in
!
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
! person_in_charge: patrick.massin at edf.fr
!     ------------------------------------------------------------------
!
!       XPROJ   : X-FEM : PROJECTION D'UN POINT SUR UN TRIANGLE EN 3D
!       -----     -       ----
!    DANS LE CADRE DE XFEM, ON PROJETE LE POINT P SUR LE TRIANGLE ABC,ON
!     RENVOIE LE POINT PROJETE ET SES COORDONNES PARAMETRIQUE DANS ABC.
!     LA METHODE UTILISEE EST CELLE DECRTIE DANS LA DOC R5.03.50-B
!
!    ENTREE
!        P(3)        :  COORDONNEES DU POINT P A PROJETER
!     A(3),B(3),C(3) :  COORDONNEES DU TRIANGLE ABC
!    SORTIE
!        M(3)   : COORDONNEES DE M PROJETE DE P SUR LE TRIANGLE ABC
!                 APRES CORRECTION (VOIR SORTIE "IN" CI-DESSOUS)
!        MP(3)  : COORDONNEES DE MP PROJETE DE P SUR LE TRIANGLE ABC
!        D      : DISTANCE P-M
!        VN(3)  : NORMALE AU TRIANGLE ABC
!        EPS(3) : COORDONNEES PARAMETRIQUES DE M DANS ABC
!                 (APRES PROJECTION & CORRECTION)
!        IN     : VARIABLE LOGIQUE INDIQUANT SI LE POINT M SE TROUVE A
!                 L'INTERIEUR DU TRIANGLE ABC AVANT CORRECTION
!
!     ------------------------------------------------------------------
!
    integer :: i
    real(kind=8) :: ab(3), bc(3), ac(3), ap(3), norme, vnt(3), ps, ps1, ps2
!
!-----------------------------------------------------------------------
!     DEBUT
!-----------------------------------------------------------------------
    call jemarq()
!
    do 100 i = 1, 3
        ab(i)=b(i)-a(i)
        bc(i)=c(i)-b(i)
        ac(i)=c(i)-a(i)
        ap(i)=p(i)-a(i)
100  end do
!
!  CALCUL DE LA NORMALE A L'ISOZERO
!  PROJECTION DE P SUR LE TRIANGLE VOIR R5.03.50-B
    call provec(ab, ac, vn)
    call normev(vn, norme)
    call provec(ap, vn, vnt)
    ps=ddot(3,vnt,1,ac,1)
    eps(1)=-1*ps/norme
    ps=ddot(3,vnt,1,ab,1)
    eps(2)=ps/norme
    eps(3)=1-eps(1)-eps(2)
!
!  ON REPERE AVANT RECTIFICATION SI LA PROJECTION EST DANS LE TRIANGLE
    in = .false.
    if (eps(1) .ge. 0.d0 .and. eps(2) .ge. 0.d0 .and. eps(3) .ge. 0.d0) in = .true.
!
!  CALCULATE THE COORDINATES OF THE PROJECTED POINT
    do 150 i = 1, 3
        mp(i)=a(i)+eps(1)*ab(i)+eps(2)*ac(i)
150  end do
!
!  SI M EST DS LE SECTEUR 1
    if (eps(1) .lt. 0.d0) then
        ps=ddot(3,ac,1,ac,1)
        ps1=ddot(3,ab,1,ac,1)
        eps(2)=eps(2)+eps(1)*ps1/ps
        eps(1)=0.d0
    endif
!  SI M EST DS LE SECTEUR 2
    if (eps(2) .lt. 0.d0) then
        ps=ddot(3,ab,1,ab,1)
        ps1=ddot(3,ab,1,ac,1)
        eps(1)=eps(1)+eps(2)*ps1/ps
        eps(2)=0.d0
    endif
!  SI M EST DS LE SECTEUR 3
    if (eps(3) .lt. 0.d0) then
        ps=ddot(3,bc,1,bc,1)
        ps1=ddot(3,ab,1,bc,1)
        ps2=ddot(3,ac,1,bc,1)
        eps(1)=(-1.d0*eps(1)*ps1+(1.d0-eps(2))*ps2)/ps
        eps(2)=1.d0-eps(1)
    endif
!
!  ON FINIT DE RAMENER LES POINTS ENCORE DEHORS
    if (eps(1) .lt. 0.d0) eps(1)=0.d0
    if (eps(2) .lt. 0.d0) eps(2)=0.d0
    if (eps(1) .gt. 1.d0) eps(1)=1.d0
    if (eps(2) .gt. 1.d0) eps(2)=1.d0
!
    eps(3) = 1-eps(1)-eps(2)
!
    do 200 i = 1, 3
        m(i)=a(i)+eps(1)*ab(i)+eps(2)*ac(i)
200  end do
!
!  CALCUL DE LA DISTANCE PM
    d=padist(3,p,m)
!
!-----------------------------------------------------------------------
!     FIN
!-----------------------------------------------------------------------
    call jedema()
end subroutine
