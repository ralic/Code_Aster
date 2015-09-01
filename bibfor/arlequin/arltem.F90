subroutine arltem(ndim  ,nomte, &
    nns   ,jcoors, &
    npgs  ,ivfs  ,idfdes,ipoids, &
    elref1,ndml1   ,jcoor1, &
    elref2,ndml2   ,jcoor2, &
    mcpln1,mcpln2)



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
#include "asterfort/jevech.h"
#include "asterfort/arlmas.h"
#include "asterfort/arlt1d.h"
#include "asterfort/arlten.h"
#include "asterfort/arlted.h"


    integer :: ndim
    character(len=16) :: nomte
    integer :: nns,npgs
    integer :: ivfs,ipoids,idfdes
    character(len=8) :: elref1,elref2
    integer :: ndml1,jcoor1,jcoors
    integer :: ndml2,jcoor2
    real(kind=8) ::  mcpln1(2*ndim*ndml2,ndim*ndml1)
    real(kind=8) ::  mcpln2(2*ndim*ndml2,2*ndim*ndml2),mlv(78)

! ----------------------------------------------------------------------
!
! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN (OPTION ARLQ_MATR)
! CALCUL DES INTEGRALES DE COUPLAGE ENTRE MAILLE 1 ET MAILLE 2
! SUR MAILLE SUPPORT S
!
! ----------------------------------------------------------------------


! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NOMTE  : NOM DU TYPE_ELEMENT MAILLE SUPPORT S
! IN  NNS    : NOMBRE DE NOEUDS DE LA MAILLE SUPPORT S
! IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE SUPPORT S
! IN  IVFS   : POINTEUR VERS FONCTIONS DE FORME DE LA MAILLE SUPPORT S
! IN  IDFDES : POINTEUR VERS DER. FONCTIONS DE FORME DE LA MAILLE S
! IN  IPOIDS : POINTEUR VERS POIDS DE GAUSS DE LA MAILLE SUPPORT S
! IN  ELREFC : ELREFE DE LA MAILLE C
! IN  NNC    : NOMBRE DE NOEUDS DE LA MAILLE C
! IN  JCOORC : POINTEUR VERS COORD. NOEUDS DE LA MAILLE C
! OUT MCPLN1 : MATRICE DES TERMES DE COUPLAGE NST.NS
!              MATRICE CARREE (NNSxNNS)
! OUT MCPLN2 : MATRICE DES TERMES DE COUPLAGE NST.NC
!              MATRICE RECTANGULAIRE (NNSxNDML1)
! ----------------------------------------------------------------------

    real(kind=8) ::  poijcs(npgs)
    real(kind=8) ::  fcpig1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdx1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdy1(npgs*ndim*ndim*ndml1)
    real(kind=8) ::  dfdz1(npgs*ndim*ndim*ndml1)
    integer :: jinfor
    real(kind=8) ::  e, rho, xnu

! ----------------------------------------------------------------------

! --- CALCUL DES FF ET DES DERIVEES DES FF DES MAILLES COUPLEES


    call jevech('PINFORR','L',jinfor)

    e   = zr(jinfor+6-1)
    rho = 1.d0
    xnu = zr(jinfor+8-1)
    call arlmas('MECA_POU_D_T',e,xnu,rho,1,mlv)
    call arlt1d(mlv,ndim,ndml2,mcpln2)

    if ((nomte(1:9) == 'MECA_HEXA').or.(nomte(1:10) == 'MECA_PENTA') & 
                                   .or.(nomte(1:10) == 'MECA_TETRA')) then
        call arlted(ndim  , &
                    nns   ,jcoors, &
                    npgs  ,ivfs  , idfdes, ipoids, &
                    elref1, ndml1   ,jcoor1, &
                    fcpig1, poijcs, &
                    dfdx1, dfdy1 , dfdz1 )
        call arlten(zr(jcoor1)     ,zr(jcoor2), npgs, ndim , poijcs , &
                    ndml1 , ndml2 , fcpig1 , dfdx1 , dfdy1 , dfdz1 , &
                    mcpln1)
    endif

end subroutine
