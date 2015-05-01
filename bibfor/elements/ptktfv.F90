subroutine ptktfv(itype, sk, e, rof, ce,&
                  a1, ai1, a2, ai2, xl,&
                  xiy1, xiy2, xiz1, xiz2, xjx1,&
                  xjx2, g, alfay1, alfay2, alfaz1,&
                  alfaz2, ey, ez)
! aslint: disable=W1504
    implicit none
#include "asterc/r8gaem.h"
#include "asterfort/fun1.h"
#include "asterfort/fun2.h"
#include "asterfort/utmess.h"
    integer :: itype
    real(kind=8) :: sk(*)
    real(kind=8) :: e, rof, ce, a1, ai1, a2, ai2, xl, xiy1, xiy2, xiz1, xiz2
    real(kind=8) :: xjx1, xjx2
    real(kind=8) :: g, alfay1, alfay2, alfaz1, alfaz2, ey, ez
!    -------------------------------------------------------------------
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
!    * CE SOUS PROGRAMME CALCULERA LA MATRICE DE RAIDEUR DES ELEMENTS
!      DE TUYAU DROIT A SECTION VARIABLE .
!
!    * DESCRIPTION DE L'ELEMENT:
!      C'EST UN ELEMENT A DEUX NOEUDS ET A HUIT DEGRES DE LIBERTES PAR
!      NOEUDS (3 DEPLACEMENTS, 3 ROTATIONS, PRESSION, POTENTIEL DES
!      DEPLACEMENTS).
!      IL PEUT PRENDRE EN COMPTE L'EFFORT TRANCHANT
!    -------------------------------------------------------------------
! IN TYPE ! NOM    !        SIGNIFICATION
! IN -------------------------------------------------------------------
! IN  I   ! ITYPE  ! TYPE DE VARIATION SECTION DROITE
! IN      !        !    ITYPE = 1 : SECTIONS AFFINES
! IN      !        !    ITYPE = 2 : SECTIONS HOMOTHETIQUES
! IN R*8  ! E      ! MODULE D'ELASTICITE DU MATERIAU
! IN R*8  ! ROF    ! MASSE VOLUMIQUE DU FLUIDE
! IN R*8  ! C      ! CELERITE DU SON DANS LE FLUIDE
! IN R*8  ! A1     ! AIRE DE LA SECTION DROITE DE TUYAU INITIALE
! IN R*8  ! AI1    ! AIRE DE LA SECTION DE FLUIDE INITIALE
! IN R*8  ! A2     ! AIRE DE LA SECTION DROITE DE TUYAU IFINALEE
! IN R*8  ! AI2    ! AIRE DE LA SECTION DE FLUIDE FINALE
! IN R*8  ! XL     ! LONGUEUR DE L ELEMENT
! IN R*8  ! XIY1   ! MOMENT D INERTIE / Y PRINCIPAL SECTION INITIALE
! IN R*8  ! XIY2   ! MOMENT D INERTIE / Y PRINCIPAL SECTION FINALE
! IN R*8  ! XIZ1   ! MOMENT D INERTIE / Z PRINCIPAL SECTION INITIALE
! IN R*8  ! XIZ2   ! MOMENT D INERTIE / Z PRINCIPAL SECTION FINALE
! IN R*8  ! XJX1   ! CONSTANTE DE TORSION SECTION INITIALE
! IN R*8  ! XJX2   ! CONSTANTE DE TORSION SECTION FINALE
! IN R*8  ! G      ! MODULE DE CISAILLEMENT DU MATERIAU
! IN R*8  ! ALFAY1 ! COEFFICIENT DE CISAILLEMENT AXE Y SECTION INITIALE
! IN R*8  ! ALFAY2 ! COEFFICIENT DE CISAILLEMENT AXE Y SECTION FINALE
! IN R*8  ! ALFAZ1 ! COEFFICIENT DE CISAILLEMENT AXE Z SECTION INITIALE
! IN R*8  ! ALFAZ2 ! COEFFICIENT DE CISAILLEMENT AXE Z SECTION FINALE
! IN R*8  ! EY     ! COMPOSANTE TG SUR Y PRINCIPAL
! IN R*8  ! EZ     ! COMPOSANTE TG SUR Z PRINCIPAL
!
! REMARQUE :
! ALFAY OU ALFAZ EST NUL ALORS ON CONSIDERE L'ELEMENT DE TYPE
! EULER-BERNOULLI (I.E.  SANS EFFORT TRANCHANT)
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 ! SK     ! (136)   ! MATRICE ELEMENTAIRE UNICOLONNE
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC R*8 ! ASY    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Y
! LOC R*8 ! ASZ    !   -     ! AIRE REDUITE CISAILLEE SUIVANT Z
! LOC R*8 ! PHIY   !   -     ! COEFFICIENT DU A L'EFFORT TRANCHANT SUR Z
! LOC R*8 ! PHIZ   !   -     ! COEFFICIENT DU A L'EFFORT TRANCHANT SUR Y
! LOC I   ! IP     !   16    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!     ------------------------------------------------------------------
!
!     SOUS - PROGRAMMES UTILISES
! FUN1     - AIRES ET CONSTANTE DE TORSION EQUIVALENTES
! FUN2     - MOMENTS D INERTIE EQUIVALENTS
!     ------------------------------------------------------------------
    integer :: ip(16), i, k
    real(kind=8) :: zero
    real(kind=8) :: c2, c4, c8, c9, c12, c60
    real(kind=8) :: exl, xl2, xl3, phiy, phiz, asy, asz
    real(kind=8) :: aa, asy1, asy2, asz1, asz2, xjx, vt, q, xkk
    real(kind=8) :: se, ce2
!
    zero = 0.d0
    c2 = 2.d0
    c4 = 4.d0
    c8 = 8.d0
    c9 = 9.d0
    c12 = 12.d0
    c60 = 60.d0
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data ip/ 0,1,3,6,10,15,21,28,36,45,55,66,78,91,105,120/
! ---------------------------------------------------------------------
    do 1,i=1,136
    sk(i) = zero
    1 end do
!
!     -- SI G  ET E SONT NULS : K=0
    if (abs(g) .lt. 1.d0/r8gaem()) then
        if (abs(e) .lt. 1.d0/r8gaem()) goto 9999
        call utmess('F', 'ELEMENTS2_54')
    endif
!
!     1/ TRACTION-COMPRESSION
    call fun1(aa, a1, a2, itype)
!
    sk(ip( 1)+ 1) = e * aa / xl
    sk(ip( 9)+ 1) = - sk(ip(1)+1)
    sk(ip( 9)+ 9) = sk(ip(1)+1)
!
!     2/ FLEXION
!     2.1) CALCUL DES CONSTANTES
    xl2 = xl * xl
    xl3 = xl * xl2
    exl = e / xl
    if (alfaz1 .ne. zero) then
        asy1 = a1 / alfaz1
    else
        asy1 = a1
    endif
    if (alfaz2 .ne. zero) then
        asy2 = a2 / alfaz2
    else
        asy2 = a2
    endif
    if (alfaz1 .eq. zero .and. alfaz2 .eq. zero) then
        phiz = zero
    else
        call fun1(asy, asy1, asy2, itype)
        phiz = e / (g * asy * xl2)
    endif
!
    if (alfay1 .ne. zero) then
        asz1 = a1 / alfay1
    else
        asz1 = a1
    endif
    if (alfay2 .ne. zero) then
        asz2 = a2 / alfay2
    else
        asz2 = a2
    endif
    if (alfay1 .eq. zero .and. alfay2 .eq. zero) then
        phiy = zero
    else
        call fun1(asz, asz1, asz2, itype)
        phiy = e / (g * asz * xl2)
    endif
!
!     2.1) REMPLISSAGE DE LA MATRICE
!
!     2/  FLEXION DANS LE PLAN X0Y
    k = itype + 2
    call fun2(xiz1, xiz2, phiy, xkk, q,&
              vt, k)
    sk(ip( 2)+ 2) = e * xkk / xl3
    sk(ip( 6)+ 2) = xl * q * sk(ip( 2)+ 2)
    sk(ip(10)+ 2) = - sk(ip( 2)+ 2)
    sk(ip(14)+ 2) = (1.d0/q - 1.d0) * sk(ip( 6)+ 2)
    sk(ip( 6)+ 6) = exl * (vt + q * q * xkk)
    sk(ip(10)+ 6) = - sk(ip( 6)+ 2)
    sk(ip(14)+ 6) = exl * (xkk * q * (1.d0- q) - vt)
!
    call fun2(xiz2, xiz1, phiy, xkk, q,&
              vt, k)
    sk(ip(10)+10) = sk(ip( 2)+ 2)
    sk(ip(14)+10) = - sk(ip(14)+ 2)
    sk(ip(14)+14) = exl * (vt + q * q * xkk)
!
!     3/  FLEXION DANS LE PLAN X0Z
    if (itype .eq. 2) then
        k = 4
    else
        k = 1
    endif
!
    call fun2(xiy1, xiy2, phiz, xkk, q,&
              vt, k)
    sk(ip( 3)+ 3) = e * xkk / xl3
    sk(ip( 5)+ 3) = - xl * q * sk(ip( 3)+ 3)
    sk(ip(11)+ 3) = - sk(ip( 3)+ 3)
    sk(ip(13)+ 3) = (1.d0/q - 1.d0) * sk(ip( 5)+ 3)
    sk(ip( 5)+ 5) = exl * (vt + q * q * xkk)
    sk(ip(11)+ 5) = - sk(ip( 5)+ 3)
    sk(ip(13)+ 5) = exl * (xkk * q * (1.d0 - q) - vt)
!
    call fun2(xiy2, xiy1, phiz, xkk, q,&
              vt, k)
    sk(ip(11)+11) = sk(ip( 3)+ 3)
    sk(ip(13)+11) = - sk(ip(13)+ 3)
    sk(ip(13)+13) = exl * (vt + q * q * xkk)
!
!     4/  TORSION
    call fun1(xjx, xjx1, xjx2, itype+2)
    sk(ip( 4)+ 4) = g * xjx / xl
    sk(ip(12)+ 4) = - sk(ip( 4)+ 4)
    sk(ip(12)+12) = sk(ip( 4)+ 4)
!
!     5/  CAS OU IL EXISTE UNE EXCENTICITE
!
    if (ez .eq. zero .and. ey .eq. zero) goto 10
!
!        CORRECTION DES TERMES DE TORSION
!
    sk(ip( 4)+ 4) = sk(ip( 4)+ 4) + ez*ez*sk(ip( 2)+ 2) + ey*ey*sk(ip( 3)+ 3)
    sk(ip(12)+ 4) = - sk(ip( 4)+ 4)
    sk(ip(12)+12) = sk(ip( 4)+ 4)
    sk(ip( 4)+ 2) = - ez * sk(ip( 2)+ 2)
    sk(ip(12)+2) = - sk(ip( 4)+ 2)
    sk(ip( 4)+ 3) = ey * sk(ip(3)+ 3)
    sk(ip(12)+ 3) = - sk(ip( 4)+ 3)
    sk(ip( 5)+ 4) = ey * sk(ip( 5)+ 3)
    sk(ip( 6)+ 4) = - ez * sk(ip( 6)+ 2)
    sk(ip(10)+ 4) = sk(ip(12)+ 2)
    sk(ip(11)+ 4) = sk(ip(12)+ 3)
    sk(ip(13)+ 4) = ey * sk(ip(13)+ 3)
    sk(ip(14)+ 4) = - ez * sk(ip(14)+ 2)
    sk(ip(12)+ 5) = - sk(ip( 5)+ 4)
    sk(ip(12)+ 6) = - sk(ip( 6)+ 4)
    sk(ip(12)+10) = sk(ip( 4)+ 2)
    sk(ip(12)+11) = sk(ip( 4)+ 3)
    sk(ip(13)+12) = - sk(ip(13)+ 4)
    sk(ip(14)+12) = - sk(ip(14)+ 4)
10  continue
!
!     CONTRIBUTION DU FLUIDE
!
    ce2 = ce * ce
    if (itype .eq. 2) then
        se = (ai1 + ai2 + c2 * sqrt(ai1 * ai2)) / c4
    else
        se = (ai1 + ai2) / c2
    endif
    sk(ip( 7)+ 7) = xl * (c9*ai1 -ai2 + c12*se) / (rof * ce2 * c60)
    sk(ip(15)+ 7) = xl * (ai1 +ai2 + c8*se) / (rof * ce2 * c60)
    sk(ip(15)+15) = xl * (-ai1 +c9*ai2 + c12*se) / (rof * ce2 * c60)
9999  continue
end subroutine
