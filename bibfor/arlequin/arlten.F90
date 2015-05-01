subroutine arlten(coorc1,coorc2,npgs  , ndim , poijcs, &
                  ndml1,ndml2,fcpig1,dfdx1,dfdy1,dfdz1,mcpln1)

! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/arlpff.h"
#include "asterfort/utpvgl.h"
#include "asterfort/matrot.h"
#include "asterfort/jedema.h"

    integer :: npgs,ndim
    integer :: ndml1,ndml2
    real(kind=8) ::  poijcs(npgs),coorc1(ndim*ndml1),coorc2(6)
    real(kind=8) ::  fcpig1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdx1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdy1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdz1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  mcpln1(2*ndim*ndml2,ndim*ndml1)

! ----------------------------------------------------------------------

! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN
! OPTION ARLQ_MATR


! CALCUL DES INTEGRALES DE COUPLAGE ENTRE MAILLE 1 ET MAILLE 2

! TERME CONSTANT (N1)T.N2  (INTEGRALE SUR S)

! ----------------------------------------------------------------------


! IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE S
! IN  POIJCS : PRODUIT POIDS DE GAUSS*JACOBIEN  DE LA MAILLE S
! IN  NDML1  : NOMBRE DE NOEUDS DE LA MAILLE 1
! IN  FCPIG1 : FCT. FORME DE MAILLE 1 AU POINT DE GAUSS KPGS
!               DE LA MAILLE S
! IN  NDML2  : NOMBRE DE NOEUDS DE LA MAILLE 2
! OUT MCPLN1  : MATRICE DES TERMES DE COUPLAGE (N1)T.N2
!              MATRICE RECTANGULAIRE (NDML1xNDML2)

! NB: SI MAILLE 1 == MAILLE 2 ALORS MCPLN1 EQUIVALENTE A UNE MATRICE
!     MASSE DE DENSITE 1 (ET DONC MATRICE CARREE)

! ----------------------------------------------------------------------

    real(kind=8) ::  poids,xl,un,deux,c012,e,rho,xnu,g,a
    real(kind=8) ::  phiy,phiz,alfay,alfaz,xiy,xiz
    integer :: kpgs,jaux,jinfor
    real(kind=8) :: xg(3),xgl(3),pgl(3,3),alpha,beta,gamma,angle(3)
    real(kind=8) :: B3d(ndim,ndim*ndml1),B1d(ndim,2*ndim*ndml2)
    real(kind=8) :: B1dnrj(2*ndim,2*ndim*ndml2)
    real(kind=8) :: hooke(2*ndim,2*ndim),matrix(2*ndim*ndml2,2*ndim)

! ----------------------------------------------------------------------

    call jemarq()

! --- CALCUL DES TERMES DE COUPLAGE - MATRICE STOCKAGE LINEAIRE

    un   = 1.d0
    deux = 2.d0
    c012 = 12.d0
    call jevech('PINFORR','L',jinfor)
    e   = zr(jinfor+6-1)
    rho = zr(jinfor+7-1)
    xnu = zr(jinfor+8-1)
    a   = zr(jinfor+9-1)
    g   = e/(deux*(un+xnu))
    xiy = zr(jinfor+10-1)
    xiz = zr(jinfor+11-1)
    alfay = zr(jinfor+12-1)
    alfaz = zr(jinfor+13-1)
    alpha = zr(jinfor+24-1)
    beta  = zr(jinfor+25-1)
    gamma = zr(jinfor+26-1)
    angle(1) = alpha
    angle(2) = beta
    angle(3) = gamma
    mcpln1 = 0.0
    hooke = 0.0
    hooke(1,1) = e
    hooke(4,4) = g
    hooke(5,5) = g
    hooke(6,6) = g

! --- CONSTRUCTION DU BARYCENTRE DE LA MAILLE 3D

    do 30 kpgs = 1,npgs
        xg(1) = 0.d0
        xg(2) = 0.d0
        xg(3) = 0.d0
        poids = 0.d0
        matrix = 0.0
        B3d    = 0.0

! --- POINT DE GAUSS DANS LE REPERE GLOBAL

        do 40 jaux = 1,ndml1
          xg(1) = xg(1) + fcpig1(ndim*ndim*ndml1*(kpgs-1)+jaux*3-2)*coorc1(jaux*3-2)
          xg(2) = xg(2) + fcpig1(ndim*ndim*ndml1*(kpgs-1)+jaux*3-2)*coorc1(jaux*3-1)
          xg(3) = xg(3) + fcpig1(ndim*ndim*ndml1*(kpgs-1)+jaux*3-2)*coorc1(jaux*3-0)
        40 end do

! --- APPEL AU CHAMP DES FONCTIONS DE FORME POUTRE
    
        xl = 0.d0
        xl = sqrt((coorc2(4)-coorc2(1))**2+(coorc2(5)-coorc2(2))**2 &
                + (coorc2(6)-coorc2(3))**2)

! --- POINT DE GAUSS DANS LE REPERE LOCAL

        call matrot(angle,pgl)
        xg(1) = xg(1)-coorc2(1)
        xg(2) = xg(2)-coorc2(2)
        xg(3) = xg(3)-coorc2(3)
        call utpvgl(1,3,pgl,xg,xgl)
        xgl(1) = xgl(1)/xl
        phiy = (c012*e*xiy)/(g*a*alfaz*xl**2)
        phiz = (c012*e*xiz)/(g*a*alfay*xl**2)
        call arlpff(xgl(1),xgl(2),xgl(3),xl,phiy,phiz,B1d,B1dnrj)
        poids = poijcs(kpgs)
        B3d(1,:) = fcpig1(ndim*ndim*ndml1*(kpgs-1)+1:&
                          ndim*ndim*ndml1*(kpgs-1)+ndim*ndml1)
        B3d(2,:) = fcpig1(ndim*ndim*ndml1*(kpgs-1)+1+ndim*ndml1:&
                          ndim*ndim*ndml1*(kpgs-1)+2*ndim*ndml1)
        B3d(3,:) = fcpig1(ndim*ndim*ndml1*(kpgs-1)+1+2*ndim*ndml1:&
                          ndim*ndim*ndml1*(kpgs-1)+3*ndim*ndml1)
        mcpln1 = mcpln1 + matmul(transpose(B1d),B3d)*poids
    30 end do

    call jedema()

end subroutine
