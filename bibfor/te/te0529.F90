subroutine te0529(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/epstmc.h"
#include "asterfort/jevech.h"
#include "asterfort/ortrep.h"
#include "asterfort/r8inir.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
!
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
!
!     BUT: CALCUL DES DEFORMATIONS LIEES AUX VARIABLES DE COMMANDE
!          AUX POINTS D'INTEGRATION DES ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTIONS : 'EPVC_ELGA'
!    CINQ COMPOSANTES :
!    EPTHER_L = DILATATION THERMIQUE (LONGI)   : ALPHA_L*(T-TREF)
!    EPTHER_T = DILATATION THERMIQUE (TRANSV)   : ALPHA_T*(T-TREF)
!    EPTHER_N = DILATATION THERMIQUE (NORMLALE)   : ALPHA_N*(T-TREF)
!    EPSECH = RETRAIT DE DESSICCATION : -K_DESSIC(SREF-SECH)
!    EPHYDR = RETRAIT ENDOGENE        : -B_ENDOGE*HYDR
!    EPPTOT = RETRAIT DU A LA PRESSION DE FLUIDE EN THM CHAINEE :
!             -BIOT*PTOT
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    integer :: jgano, ndim, nno, i, nnos, npg, ipoids, ivf, idfde, igau, isig
    integer :: igeom, itemps, idefo, imate, iret, nbcmp, idim
    real(kind=8) :: epvc(162), repere(7)
    real(kind=8) :: instan, epsse(6), epsth(6), epshy(6), epspt(6)
    real(kind=8) :: xyzgau(3), xyz(3)
    character(len=4) :: fami
    character(len=16) :: optio2
! DEB ------------------------------------------------------------------
!
!    NOMBRE DE COMPOSANTES  A  CALCULER
    nbcmp=6
!
! ---- CARACTERISTIQUES DU TYPE D'ELEMENT :
! ---- GEOMETRIE ET INTEGRATION
!      ------------------------
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! ---- RECUPERATION DES COORDONNEES DES CONNECTIVITES :
!      ----------------------------------------------
    call jevech('PGEOMER', 'L', igeom)
!
! ---- RECUPERATION DU MATERIAU :
!      ----------------------------------------------
    call tecach('NNO', 'PMATERC', 'L', iret, iad=imate)
!
! --- RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE :
!     ------------------------------------------------------------
!     COORDONNEES DU BARYCENTRE ( POUR LE REPERE CYLINDRIQUE )
    xyz(1) = 0.d0
    xyz(2) = 0.d0
    xyz(3) = 0.d0
    do 300 i = 1, nno
        do 310 idim = 1, ndim
            xyz(idim) = xyz(idim)+zr(igeom+idim+ndim*(i-1)-1)/nno
310      continue
300  end do
    call ortrep(ndim, xyz, repere)
!
! ---- RECUPERATION DE L'INSTANT DE CALCUL :
!      -----------------------------------
    call tecach('NNO', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .ne. 0) then
        instan = zr(itemps)
    endif
!
!     -----------------
! ---- RECUPERATION DU VECTEUR DES DEFORMATIONS EN SORTIE :
!      --------------------------------------------------
    call jevech('PDEFOPG', 'E', idefo)
    call r8inir(135, 0.d0, epvc, 1)
!
!
    do 200 igau = 1, npg
!
!      CALCUL AU POINT DE GAUSS DE LA TEMPERATURE ET
!       DU REPERE D'ORTHOTROPIE
! ------------------------------------------
        xyzgau(1) = 0.d0
        xyzgau(2) = 0.d0
        xyzgau(3) = 0.d0
        do 55 idim = 1, ndim
            xyzgau(idim) = xyzgau(idim) + zr(ivf+idim-1+nno*(igau-1))* zr(igeom+idim-1+ndim*(idim&
                           &-1))
55      continue
!
!
        optio2 = 'EPVC_ELGA_TEMP'
!
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, zi( imate), optio2,&
                    epsth)
        optio2 = 'EPVC_ELGA_SECH'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, zi( imate), optio2,&
                    epsse)
        optio2 = 'EPVC_ELGA_HYDR'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, zi( imate), optio2,&
                    epshy)
        optio2 = 'EPVC_ELGA_PTOT'
        call epstmc(fami, ndim, instan, '+', igau,&
                    1, xyzgau, repere, zi( imate), optio2,&
                    epspt)
        do 60 i = 1, 3
            epvc(i+nbcmp*(igau-1)) = epsth(i)
60      continue
        epvc(4+nbcmp*(igau-1) )= epsse(1)
        epvc(5+nbcmp*(igau-1) )= epshy(1)
        epvc(6+nbcmp*(igau-1) )= epspt(1)
!
200  continue
!
!         --------------------
! ---- AFFECTATION DU VECTEUR EN SORTIE AVEC LES DEFORMATIONS AUX
! ---- POINTS D'INTEGRATION :
!      --------------------
    do 80 igau = 1, npg
        do 70 isig = 1, nbcmp
            zr(idefo+nbcmp* (igau-1)+isig-1) = epvc(nbcmp* (igau-1)+ isig)
70      continue
80  continue
!
!
end subroutine
