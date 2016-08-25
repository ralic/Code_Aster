subroutine ef0436(nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W0104
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
#include "asterfort/ppgan2.h"
#include "asterfort/tecael.h"
!
    character(len=16) :: nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DE EFGE_ELNO POUR LES MEMBRANES
!    - ARGUMENTS :
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    character(len=4) :: fami
    integer :: nddl, nno, nnos, npg, ndim, ncomp
    integer :: i, n, c, cc, kpg
    integer :: ipoids, ivf, idfde, jgano, jefno
    integer :: igeom, icacoq, imate, idepl
    real(kind=8) :: dff(2, 8), vff(8), b(3, 3, 8), jac
    real(kind=8) :: alpha, beta
    real(kind=8) :: epsm(3), epsthe, sig(3), sigg(3, 9), rig(6, 6)
!----------------------------------------------------------------------------------
!
! - NOMBRE DE COMPOSANTES DES TENSEURS
!
    ncomp = 3
    nddl = 3
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
                      npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)

!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icacoq)
!
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PMATERC', 'L', imate)
!
    call jevech('PEFFORR', 'E', jefno)


    call r8inir(3*9, 0.d0, sigg, 1)
!
! - LE VECTEUR NORME QUI DETERMINE LE REPERE LOCAL DE LA MEMBRANE
!   (COMPORTEMENT ANISOTROPE)
!
    alpha = zr(icacoq+1) * r8dgrd()
    beta = zr(icacoq+2) * r8dgrd()
!
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do kpg = 1, npg
!
         do n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
         end do
!
!        --- CALCUL DE LA MATRICE "B" :
!              DEPL NODAL --> DEFORMATIONS MEMBRANAIRES ET JACOBIEN
!
         call mbcine(nno, zr(igeom), dff, alpha, beta,&
                    b, jac)
!
!        ---  ON CALCULE LA CONTRAINTE AU PG :
!
!        -- CALCUL DE LA DEFORMATION MEMBRANAIRE DANS LE REPERE LOCAL
         call r8inir(3, 0.d0, epsm, 1)
         do n = 1, nno
             do i = 1, nddl
                 do c = 1, ncomp
                     epsm(c)=epsm(c)+b(c,i,n)*zr(idepl+(n-1)*nddl+i-1)
                 end do
             end do
         end do
!
!        -- RETRAIT DE LA DEFORMATION THERMIQUE
         call verift(fami, kpg, 1, '+', zi(imate),&
                     epsth_=epsthe)
         epsm(1) = epsm(1) - epsthe
         epsm(2) = epsm(2) - epsthe

!
!        --  CALCUL DE LA CONTRAINTE AU PG
         call mbrigi(fami, kpg, imate, rig)
!
         call r8inir(3, 0.d0, sig, 1)
         do c = 1, ncomp
             do cc = 1, ncomp
                 sig(c) = sig(c) + epsm(cc)*rig(cc,c)
             end do
         end do
!
         do c = 1, ncomp
             sigg(c,kpg) = sig(c)
         end do

    end do



!   -- passage ELGA -> ELNO :
    call ppgan2(jgano, 1, ncomp, sigg, zr(jefno))


end subroutine
