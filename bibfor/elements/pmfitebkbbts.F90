subroutine pmfitebkbbts(typfib, nf, ncarf, vf, ve, b, wi, gxjx, gxjxpou, g, &
                        gg, nbassepou, maxfipoutre, nbfipoutre, vev, yj, zj, &
                        vfv, skp, sk, vv, vvp)

!
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
!
! --------------------------------------------------------------------------------------------------
!
!       INTEGRATIONS SUR LA SECTION (TENANT COMPTE DU MODULE DE CHAQUE FIBRE)
!
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!   IN
!       typfib       : type des fibres : 1 ou 2
!       nf           : nombre de fibres
!       ncarf        : nombre de caracteristiques sur chaque fibre
!       vf(*)        : positions des fibres
!           Types 1 et 2
!               vf(1,*) : Y fibres
!               vf(2,*) : Z fibres
!               vf(3,*) : Aire fibres
!           Types 2
!               vf(4,*) : Yp groupes de fibres
!               vf(5,*) : Zp groupes de fibres
!               vf(6,*) : num du groupe
!       ve(*)        : module des fibres
!       b            : Matrice b pour la position consideree
!       wi           : Poids du point d'intégration
!       gxjx         : Module torsion pour pmfs classiques
!       gxjxpou(*)   : Module de torsion pour multipoutres
!       g            : Module cisaillement element
!       gg           : Modes incompatibles
!       nbassepou    : nombre d assemblages de fibres (pour multipoutres)
!       maxfipou     : nombre maximal de fibres pour un assemblage de fibres
!       nbfipoutre(*): nombre de fibres pour un assemblage de fibres (1:nbassepou)
!       vev(*)       : tableau vide afin de decrire ve(*) sur une sous-poutre
!       yj(*)        : position Y des sous-poutres
!       zj(*)        : position Z des sous-poutres
!       vfv(*)       : tableau vide afin de decrire les proprietes sur une sous-poutre
!       skp(*)       : tableau des matrices de rigidite des sous-poutres
!
!   OUT
!       skp(*)       : tableau des matrices de rigidite des sous-poutres
!       sk           : matrice de rigidite
!       vv           : efforts generalises
!       vvp(*)       : efforts generalises sur sous-poutres

!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterfort/utmess.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/pmfite.h"
#include "asterfort/pmfbkb.h"
#include "asterfort/pmfbts.h"
#include "asterfort/pmpbkb.h"
#include "asterfort/pmpitp.h"
#include "asterfort/r8inir.h"
!
    integer :: typfib, nf, ncarf, maxfipoutre, nbassepou, nbfipoutre(*)

    real(kind=8) :: vf(ncarf, nf), ve(nf), vs(6), b(4), wi, gxjx, gg, vv(12)
    real(kind=8) :: ksg(3), sk(78), skt(78), vvt(12), g
    real(kind=8) :: yj(*), zj(*), gxjxpou(*), skp(78,*)
    real(kind=8) :: vvp(12, *)
    real(kind=8) :: vev(*), vfv(7, *)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, ii, pos, posfib
!
! --------------------------------------------------------------------------------------------------
!
    vs(:) = 0.0d0

!
    if ( typfib.eq. 1 ) then
!       3 caractéristiques utiles par fibre : y z aire
        call pmfite(typfib, nf, ncarf, vf, ve, vs)
        call pmfbkb(vs, b, wi, gxjx, sk)

!       On se sert de pmfbts pour calculer bt,ks,g. g est scalaire
        ksg(1) = vs(1)*gg
        ksg(2) = vs(2)*gg
        ksg(3) = vs(3)*gg
        call pmfbts(b, wi, ksg, vv)

    else if ( typfib.eq. 2 ) then

        call r8inir(nbassepou*78, 0.d0, skp, 1)
        call r8inir(nbassepou*12, 0.d0, skp, 1)
!       6 caractéristiques utiles par fibre : y z aire yp zp numgr
!       Boucle sur les poutres
        pos=1
        posfib=0
        vv(:)=0
        do i = 1, nbassepou
          !Position de la poutre
          yj(i)=vf(4,pos)
          zj(i)=vf(5,pos)
          call r8inir(maxfipoutre*7, 0.d0, vfv, 1)
          call r8inir(maxfipoutre, 0.d0, vev, 1)
          !Boucle sur les fibres de la poutre
          do ii = 1, nbfipoutre(i)
            !Construction des vecteurs corrigés sur une poutre
            posfib=pos+ii-1
            vfv(1,ii)=vf(1,posfib)-vf(4,posfib)
            vfv(2,ii)=vf(2,posfib)-vf(5,posfib)
            vfv(3,ii)=vf(3,posfib)
            vev(ii) = ve(posfib)
          enddo
!         Propriétes de section sur la poutre
          call pmfite(typfib, maxfipoutre, ncarf, vfv, vev, vs)
!         Matrice de rigidite de la poutre
          call pmfbkb(vs, b, wi, gxjxpou(i), skt)
          do  ii = 1, 78
              skp(ii,i) = skt(ii)
          enddo
!         bt,ks,g. g de la poutre
          ksg(1) = vs(1)*gg
          ksg(2) = vs(2)*gg
          ksg(3) = vs(3)*gg
          vvt(:)=0.d0
          call pmfbts(b, wi, ksg, vvt)
          do ii = 1, 12
             vvp(ii,i)=vvt(ii)
          enddo
          pos=pos+nbfipoutre(i)
        enddo
!       Matrice de rigidite de l element
        sk(:)=0.d0
        call pmpbkb(skp, nbassepou, yj, zj, sk)
        call pmpitp(vvp, nbassepou, yj, zj, vv)
    else
        call utmess('F', 'ELEMENTS2_40', si=typfib)
    endif
!
end subroutine
