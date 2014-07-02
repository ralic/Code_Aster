subroutine nmholi(ndim, axi, nno, npg, ipoids,&
                  ivf, idfde, imate, inst, geom,&
                  depl, chlim)
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
    implicit none
#include "asterf_types.h"
#include "asterc/matfpe.h"
#include "asterfort/nmgeom.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "blas/dnrm2.h"
    aster_logical :: axi
    integer :: ndim, nno, npg, imate, ipoids, ivf, idfde
    real(kind=8) :: geom(ndim, nno), depl(ndim, nno), inst, chlim(3)
!
! --------------------------------------------------------------------
!        CALCUL DES TERMES POUR LE POST TRAITEMENT CHARGE_LIMITE
! -------------------------------------------------------------------
! IN  NDIM   DIMENSION
! IN  AXI    .TRUE. SI AXISYMETRIQUE
! IN  NNO    NOMBRE DE NOEUDS PORTANT LE DEPLACEMENT
! IN  NPG    NOMBRE DE POINTS DE GAUSS DE MECANIQUE
! IN  VFF    VALEUR DES FOCNTIONS DE FORME
! IN  DFDE   DERIVEES DES FONCTIONS DE FORME (REFERENCE)
! IN  DFDN   DERIVEES DES FONCTIONS DE FORME (REFERENCE)
! IN  DFDK   DERIVEES DES FONCTIONS DE FORME (REFERENCE)
! IN  POIDSG POIDS DES POINTS DE GAUSS       (REFERENCE)
! IN  IMATE  ADRESSE DU MATERIAU
! IN  INST   INSTANT COURANT
! IN  GEOM   COORDONNEES DES NOEUDS
! IN  DEPL   DEPLACEMENTS NODAUX
! OUT CHLIM  TERMES CALCULES :
!             1 - SOMME( SY *EPSEQ )
!             2 - SOMME( A(M)/M * EPSNO**M )
!             3 - MAX( SIEQ/SY )
! -------------------------------------------------------------------
!
    integer :: kpg, ndimsi, spt
    character(len=8) :: fami, poum
    real(kind=8) :: eps(6), poids, epsno, sy, m, am, epsh
    real(kind=8) :: dfdi(27, 3), fbid(3, 3), r
    real(kind=8) :: rac23, val(1)
    integer :: cod(1)
! ------------------------------------------------------------------
!
!
    call matfpe(-1)
!
! -- INITIALISATION
!
    ndimsi = 2*ndim
    rac23 = sqrt(2.d0/3.d0)
    call r8inir(3, 0.d0, chlim, 1)
!
!
! -- CARACTERISTIQUES
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ECRO_LINE', 0, ' ', [0.d0],&
                1, 'SY', val, cod, 2)
    sy=val(1)            
    m = 1 + 10**(1-inst)
    am = sy * rac23**m
!
    do 10 kpg = 1, npg
!
! -- DEFORMATION
!
        call nmgeom(ndim, nno, axi, .false._1, geom,&
                    kpg, ipoids, ivf, idfde, depl,&
                    .true._1, poids, dfdi, fbid, eps,&
                    r)
        epsh = (eps(1)+eps(2)+eps(3))/3
        eps(1)=eps(1)-epsh
        eps(2)=eps(2)-epsh
        eps(3)=eps(3)-epsh
        epsno = dnrm2(ndimsi, eps,1)
!
! - CALCUL DES TERME ELEMENTAIRES
!
        chlim(1) = chlim(1) + poids * sy*rac23*epsno
        chlim(2) = chlim(2) + poids * am/m * epsno**m
        chlim(3) = max(chlim(3), (rac23*epsno)**(m-1))
!
 10 end do
    call matfpe(1)
!
end subroutine
