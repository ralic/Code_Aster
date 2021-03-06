subroutine crvloc(dim, adcom0, iatyma, jconnex0, jconnexc, vgeloc,&
                  nvtot, nvoima, nscoma, touvoi)
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
!    CRER LES DONNEES DU VOISINAGE LOCAL VGELOC D UN ELEMENT
!    A PARTIR DE TOUVOI
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexatr.h"
#include "asterfort/nbsomm.h"
#include "asterfort/somloc.h"
#include "asterfort/assert.h"
!
    integer :: adcom0, iatyma, nvtot, nvoima, nscoma
    integer :: touvoi(1:nvoima, 1:nscoma+2)
    integer :: vgeloc(*)
    integer, intent(in) :: dim
    integer, intent(in) :: jconnex0
    integer, intent(in) :: jconnexc
!
    integer :: ptvois, mv, adcomv, nbnomv, nbsomv, nsco, is, iv, tyvoi
    integer :: nusglo, nuslo0, nuslov
    character(len=8) :: typemv
    integer :: tymas=0, nbsomvs
    save tymas, nbsomvs

!
!
!  IATYME  ADRESSE JEVEUX DES TYPES DE MAILLE
!  NSCO NOMBE DE SOMMETS COMMUNS
!  MV   MAILLE VOISINE
!  PTVOIS POITEUR SUR LES DONNEES DE VOISINAGE
!
!  TYVOI
!        3D PAR FACE    : F3 : 1
!        2D PAR FACE    : F2 : 2
!        3D PAR ARRETE  : A3 : 3
!        2D PAR ARRETE  : A2 : 4
!        1D PAR ARRETE  : A1 : 5
!        3D PAR SOMMET  : S3 : 6
!        2D PAR SOMMET  : S2 : 7
!        1D PAR SOMMET  : S1 : 8
!        0D PAR SOMMET  : S0 : 9
!----------------------------------------------------------------------------------

    vgeloc(1)=nvtot
    if (nvtot .ge. 1) then
        vgeloc(2)=2+nvtot
        do iv = 1, nvtot
            ptvois=vgeloc(iv+1)
            mv=touvoi(iv,1)
!
!           -- RECUPERAION DONNEES DE LA MAILLE MV :
!
!           -- SA CONNECTIVITE
            adcomv=jconnex0-1+zi(jconnexc-1+mv)
!           -- SON TYPE
!           -- SON NOMBRE DE NOEUDS
            nbnomv=zi(jconnexc-1+mv+1)-zi(jconnexc-1+mv)
!           -- SON NOMBRE DE SOMMETS
!              On cherche a economiser du CPU :
            if (zi(iatyma-1+mv).ne.tymas) then
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma-1+mv)), typemv)
                call nbsomm(typemv, nbsomv)
                tymas=zi(iatyma-1+mv)
                nbsomvs=nbsomv
            else
                nbsomv=nbsomvs
            endif
!
            nsco=touvoi(iv,2)
!
!           -- DETERMINATION DU TYPE DE VOISINAGE
!
            if (dim .eq. 3) then
                if (nsco .gt. 2) then
!                   -- VOISIN 3D PAR FACE
                    tyvoi=1
                else if (nsco.eq.2) then
!                   -- VOISIN 3D PAR ARETE
                    tyvoi=3
                else if (nsco.eq.1) then
!                   -- VOISIN 3D PAR SOMMET
                    tyvoi=6
                endif
            else
                if (nsco .eq. 2) then
!                   -- VOISIN 2D PAR ARETE
                    tyvoi=4
                else if (nsco.eq.1) then
!                   -- VOISIN 2D PAR SOMMET
                    tyvoi=7
                endif
            endif
            vgeloc(ptvois)=tyvoi
            vgeloc(ptvois+1)=mv
            vgeloc(ptvois+2)=nbnomv
            vgeloc(ptvois+3)=nsco
            do is = 1, nsco
                nuslo0=touvoi(iv,2+is)
                nusglo=zi(adcom0+nuslo0-1)
                vgeloc(ptvois+3+2*is-1)=nuslo0
                call somloc(mv, adcomv, nbsomv, nusglo, nuslov)
                vgeloc(ptvois+3+2*is)=nuslov
            end do
            if (iv .lt. nvtot) then
                vgeloc(iv+2)=ptvois+4+2*nsco
            endif
        end do
    endif
!
end subroutine
