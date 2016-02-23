subroutine gapint(coores, nbnes, coorma, nbnma, coorin, nbpint,&
                  types, typma, gmoy, poidin, tole, ndim)
    
!
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jedetr.h"
#include "asterfort/wkvect.h"
#include "asterfort/lctria.h"
#include "asterfort/jacsur.h"
#include "asterfort/mmnewd.h"
#include "asterfort/apdist.h"
#include "asterfort/reerel.h"
#include "asterfort/lcptga.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmdonf.h"
#include "asterfort/mmtang.h"
#include "asterf_types.h"
!
    integer :: nbpint, nbnes, nbnma, ndim
    real(kind=8), intent(in) :: coorma(3,nbnma)
    real(kind=8), intent(in) :: coores(3,nbnes)
    real(kind=8) :: coorin(ndim-1,nbpint), gmoy, tole, poidin
    character(len=8) :: typma, types
! ----------------------------------------------------------------------
!         GAP INTEGRE ENTRE LA MAILLE ESCLAVE ET MAITRE
! ----------------------------------------------------------------------
! IN         COORMA       COORDONNEES DE LA MAILLE MAITRE
! IN         NBNMA        NOMBRE DE POINTS MAITRES
! IN         COORES       COORDONNEES DE LA MAILLE ESCLAVE
! IN         NBNES        NOMBRE DE POINTS ESCLAVES
! IN         COORIN       COORDONNEES DE L'INTERSECTION 
!                         (espace paramétrique maitre)
! IN         NBPINT       NOMBRE DE POINTS CONSTITUANTS L'INTERSECTION
! IN         POIDS        AIRE DE L'INTERSECTION
! IN         TYPMA        TYPE DE LA MAILLE MAITRE
! IN         TYPES        TYPE DE LA MAILLE ESCLAVE
! OUT        GMOY         GAPMOYEN MAILLE ESCLAVE MAITRE 
! ----------------------------------------------------------------------
!
    integer :: ind1, ind2, nbtri, nbpg, niverr
    real(kind=8) :: cortri(2,3),pdgp(12), xpgp(2,12)
    real(kind=8) :: pdgr, xpgr(3), dir(3), d, aux(2), auxvec(3)
    real(kind=8) :: tau1(3), tau2(3), ksi1, ksi2, auxp, sig, tabar(27)
    integer :: triin(6,3)
    character(len=8) :: fpgcal

! ----------------------------------------------------------------------
!
! --- Initialisation----------------------------------------------------
!
    gmoy=0.d0
    poidin=0.d0
    do ind1=1,nbnes
        do ind2=1, ndim
            tabar((ind1-1)*ndim+ind2)=coores(ind2,ind1)
        end do
    end do
!
! --- Triangulation ----------------------------------------------------
!
    if ((ndim-1) .eq. 2) then
        call lctria(nbpint,nbtri,triin)
    else
        nbtri = 1
    end if
!
! --- Boucle sur les "triangles" -----------------------------------------
!
    do ind1=1, nbtri
! ------- On récupère les coordonnée du triangle -----------------------
! ------- courant espace paramétrique esclave ---------------------------
        if ((ndim-1) .eq. 2) then
            cortri(1,1)=coorin(1,triin(ind1,1))
            cortri(2,1)=coorin(2,triin(ind1,1))
            cortri(1,2)=coorin(1,triin(ind1,2))
            cortri(2,2)=coorin(2,triin(ind1,2))
            cortri(1,3)=coorin(1,triin(ind1,3))
            cortri(2,3)=coorin(2,triin(ind1,3))
            fpgcal='FPG6'
        else
            cortri(1,1) = coorin(1,1)
            cortri(2,1) = 0.d0
            cortri(1,2) = coorin(1,2)
            cortri(2,2) = 0.d0
            fpgcal='FPG3'
        end if
        call lcptga(ndim-1,cortri,fpgcal ,nbpg,xpgp,&
                    pdgp)           
!
! ------- Boucle sur les points de gauss -------------------------------
!       
        do ind2=1, nbpg
            auxp=0.d0
            d=0.d0
            dir(1:3)=0.d0
! ----------- On retourne dans l'espace réel ---------------------------          
! ----------- Coordonées -----------------------------------------------
            aux(1) = xpgp(1,ind2)
            aux(2) = 0.d0
            if ((ndim-1) .eq. 2) then
                aux(2) = xpgp(2,ind2)
            end if
            xpgr(1) = 0.d0
            xpgr(2) = 0.d0
            xpgr(3) = 0.d0
            call reerel(types, nbnes, ndim, tabar, aux, xpgr)
! ----------- Poids et normale---------------------------------------------
            call jacsur(coores, nbnes, types, ndim, aux(1), aux(2), auxp, dir)
            pdgr=pdgp(ind2)*auxp
! ----------- On évalue la distance Pgr maille esclave -----------------
! ----------- Projection selon la normale ------------------------------    
            call mmnewd(typma,nbnma, ndim, coorma, xpgr, 200,&
                        tole, dir, ksi1, ksi2, tau1,&
                        tau2, niverr)
            if (niverr.eq.1) then
                write(*,*)"mmnewd failed"
                ASSERT(.false.)
            end if
! ----------- Calcul de la distance point projection -----------------
            call apdist(typma, coorma, nbnma, ksi1, ksi2,&
                        xpgr, d, auxvec)
            if (ndim .eq. 3) then
                sig=auxvec(1)*dir(1)+auxvec(2)*dir(2)+auxvec(3)*dir(3)
            elseif (ndim.eq.2) then
                sig=auxvec(1)*dir(1)+auxvec(2)*dir(2)
            end if           
            d=sign(d,sig)               
! ----------- On somme les contributions -------------------------------
                gmoy=gmoy+pdgr*d
                poidin=poidin+pdgr
            
        enddo
    enddo
   
end subroutine                     
