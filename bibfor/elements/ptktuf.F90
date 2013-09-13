subroutine ptktuf(sk, e, rof, c, a,&
                  ai, xl, xiy, xiz, xjx,&
                  g, alfay, alfaz, ey, ez)
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/utmess.h"
    real(kind=8) :: sk(*)
    real(kind=8) :: e, rof, c, a, ai, xl, xiy, xiz, xjx, g, alfay, alfaz, ey, ez
!    -------------------------------------------------------------------
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
!    * CE SOUS PROGRAMME CALCULE LA MATRICE DE RAIDEUR DES ELEMENTS DE
!      TUYAU DROIT A SECTION CONSTANTE.
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
!      DEPLACEMENTS).
!      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
!      UNICOLONNE
!    -------------------------------------------------------------------
!  DONNEES NON MODIFIEES
!
! IN TYPE ! NOM    !       SIGNIFICATION
! IN -------------------------------------------------------------------
! IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
! IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
! IN R*8  ! C      ! CELERITE DU SON DANS LE FLUIDE
! IN R*8  ! A      ! AIRE DE LA SECTION DE STRUCTURE
! IN R*8  ! AI     ! AIRE DE LA SECTION DE FLUIDE
! IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
! IN R*8  ! XIY    ! MOMENT D INERTIE / Y PRINCIPAL
! IN R*8  ! XIZ    ! MOMENT D INERTIE / Z PRINCIPAL
! IN R*8  ! XJX    ! CONSTANTE DE TORSION
! IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
! IN R*8  ! ALFAY  ! COEFFICIENT DE CISAILLEMENT AXE Y (+)
! IN R*8  ! ALFAZ  ! COEFFICIENT DE CISAILLEMENT AXE Z (+)
! IN R*8  ! EY     ! TORSION / Y
! IN R*8  ! EZ     ! TORSION / Z
!
! (+) REMARQUES :
!  -  LE COEFFICIENT DE CISAILLEMENT EST L'INVERSE DU COEFFICIENT DE
!     FORME ( IL EST DONC SUPERIEUR A 1)
!  -  SI ALFAY OU ALFAZ EST NUL ALORS ON CONSIDERE L'ELEMENT DE TYPE
!     EULER-BERNOULLI (I.E.  SANS EFFORT TRANCHANT)
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 !   SK   ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
! LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
! LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!     ------------------------------------------------------------------
    integer :: ip(16), i
    real(kind=8) :: zero
    real(kind=8) :: c1, c2, c3, c4, c6, c12
    real(kind=8) :: xl2, xl3, phiy, phiz, eiy, eiz, asy, asz
!-----------------------------------------------------------------------
    data ip/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
! ---------------------------------------------------------------------
!
    zero = 0.d0
    c1 = 1.d0
    c2 = 2.d0
    c3 = 3.d0
    c4 = 4.d0
    c6 = 6.d0
    c12  =12.d0
    do 1,i=1,136
    sk(i) =zero
    1 end do
!
!     -- SI G  ET E SONT NULS : K=0
    if (abs(g) .lt. 1.d0/r8gaem()) then
        if (abs(e) .lt. 1.d0/r8gaem()) goto 9999
        call utmess('F', 'ELEMENTS2_54')
    endif
!
!     1/ TRACTION - COMPRESSION
    sk(ip( 1)+ 1) = e * a / xl
    sk(ip( 9)+ 1) = -sk(ip( 1)+ 1)
    sk(ip( 9)+ 9) = sk(ip( 1)+ 1)
!
!     2/ FLEXION
!     2.1) CALCUL DES CONSTANTES
    xl2 = xl * xl
    xl3 = xl * xl2
    eiy = e * xiy
    eiz = e * xiz
    if (alfay .ne. zero) then
        asz = a / alfay
        phiy = (c12 * eiz) / (g * asz * xl2)
    else
        phiy = zero
    endif
    if (alfaz .ne. zero) then
        asy = a / alfaz
        phiz = (c12 * eiy) / (g * asy * xl2)
    else
        phiz = zero
    endif
!
!     2.2) REMPLISSAGE DE LA MATRICE
!     2/ FLEXION
!
!     FLEXION DANS LE PLAN XOY
    sk(ip( 2)+ 2) = c12 * eiz / ((c1 + phiy) * xl3)
    sk(ip( 6)+ 2) = c6 * eiz / ((c1 + phiy) * xl2)
    sk(ip(10)+ 2) = -sk(ip( 2)+ 2)
    sk(ip(14)+ 2) = sk(ip( 6)+ 2)
    sk(ip( 6)+ 6) = (c4 + phiy) * eiz / ((c1 + phiy) * xl)
    sk(ip(10)+ 6) = -sk(ip( 6)+ 2)
    sk(ip(14)+ 6) = (c2 - phiy) * eiz / ((c1 + phiy) * xl)
    sk(ip(10)+10) = sk(ip( 2)+ 2)
    sk(ip(14)+10) = -sk(ip( 6)+ 2)
    sk(ip(14)+14) = sk(ip( 6)+ 6)
!
!     FLEXION DANS LE PLAN XOZ
    sk(ip( 3)+ 3) = c12 * eiy / ((c1 + phiz) * xl3)
    sk(ip( 5)+ 3) = -c6 * eiy / ((c1 + phiz) * xl2)
    sk(ip(11)+ 3) = -sk(ip( 3)+ 3)
    sk(ip(13)+ 3) = sk(ip( 5)+ 3)
    sk(ip( 5)+ 5) = (c4 + phiz) * eiy / ((c1 + phiz) * xl)
    sk(ip(11)+ 5) = -sk(ip( 5)+ 3)
    sk(ip(13)+ 5) = (c2 - phiz) * eiy / ((c1 + phiz) * xl)
    sk(ip(11)+11) = sk(ip( 3)+ 3)
    sk(ip(13)+11) = -sk(ip( 5)+ 3)
    sk(ip(13)+13) = sk(ip( 5)+ 5)
!
!  3/ TORSION
    sk(ip(4) + 4)= g * xjx / xl
    sk(ip(12)+ 4)=-sk(ip( 4)+ 4)
    sk(ip(12)+12)= sk(ip( 4)+ 4)
!
!  4/ CAS OU IL EXISTE UNE EXCENTRICITE
    if (ez .ne. zero .or. ey .ne. zero) then
!        RECTIFICATION DES TERMES DE TORSION
        sk(ip(4) + 4) = sk( ip(4)+4) + ez * ez * sk(ip( 2)+ 2) + ey * ey * sk(ip( 3)+ 3 )
        sk(ip(12)+ 4) = -sk(ip(4)+ 4)
        sk(ip(12)+12) = sk(ip(4)+ 4)
!
!        TERMES INDUITS PAR L'EXCENTRICITE
        sk(ip( 4)+ 2) = -ez * sk(ip(2)+ 2)
        sk(ip(12)+ 2) = -sk(ip(4)+ 2)
        sk(ip( 4)+ 3) = ey * sk(ip(3)+ 3)
        sk(ip(12)+ 3) = -sk(ip( 4)+ 3)
        sk(ip( 5)+ 4) = ey * sk(ip( 5)+ 3)
        sk(ip( 6)+ 4) = -ez * sk(ip( 6)+ 2)
        sk(ip(10)+ 4) = sk(ip(12)+ 2)
        sk(ip(11)+ 4) = sk(ip(12)+ 3)
        sk(ip(13)+ 4) = sk(ip( 5)+ 4)
        sk(ip(14)+ 4) = sk(ip( 6)+ 4)
        sk(ip(12)+ 5) = -sk(ip( 5)+ 4)
        sk(ip(12)+ 6) = -sk(ip( 6)+ 4)
        sk(ip(12)+10) = sk(ip( 4)+ 2)
        sk(ip(12)+11) = sk(ip( 4)+ 3)
        sk(ip(13)+12) = sk(ip(12)+ 5)
        sk(ip(14)+12) = sk(ip(12)+ 6)
    endif
!
!     5/ CONTRIBUTION DU FLUIDE
    sk(ip( 7)+ 7) = xl * ai / (rof * c * c * c3)
    sk(ip(15)+15) = sk(ip(7)+ 7)
    sk(ip(15)+ 7) = sk(ip(7)+ 7) / c2
9999  continue
end subroutine
