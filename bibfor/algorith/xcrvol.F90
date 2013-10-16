subroutine xcrvol(nse, ndim, jcnse, nnose, jpint,&
                  igeom, elrefp, inoloc, nbnoma, jcesd3,&
                  jcesl3, jcesv3, numa2, ifiss, vmoin,&
                  vplus, vtot)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/iselli.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
    integer :: nse, ndim, jcnse, nnose, jpint, igeom, inoloc
    character(len=8) :: elrefp
    integer :: nbnoma, jcesd3, jcesl3, jcesv3, numa2, ifiss
    real(kind=8) :: vmoin, vplus, vtot
!
!  BUT: ESTIMATION CRITERE DE RIGIDITE
!
    real(kind=8) :: co(ndim+1, ndim), mat(ndim, ndim), vse, bary(ndim)
    real(kind=8) :: point(ndim)
    real(kind=8) :: rbid, ff(nbnoma), dfdi(nbnoma, ndim), xe(ndim), deriv
    integer :: ise, ino2, i, j, iad, ibid, k
!
! ----------------------------------------------------------------------
!
!     BOUCLE SUR LES SOUS ELEMENTS
    do 70 ise = 1, nse
!       RECUPERATION DES COORDONNEES DES NOEUDS DU SOUS ELEMENT
        do 80 i = 1, ndim+1
            ino2 = zi(jcnse-1+nnose*(ise-1)+i)
            ! on ne recupere pas les noeuds milieux
            ASSERT(ino2 .le. 2000)
            if (ino2 .gt. 1000) then
                do 90 j = 1, ndim
                    co(i,j)=zr(jpint-1+ndim*(ino2-1000-1)+j)
90              continue
            else
                do 100 j = 1, ndim
                    co(i,j)=zr(igeom-1+ndim*(ino2-1)+j)
100              continue
            endif
80      continue
        do 110 i = 1, ndim
            do 120 j = 1, ndim
                mat(i,j)=co(1,j)-co(i+1,j)
120          continue
110      continue
!
!       CALCUL DU VOLUME DU SOUS ELEMENTS (DÉTERMINANT)
!
        vse = 0.d0
        if (ndim .eq. 2) then
            vse = abs(mat(1,1)*mat(2,2)- mat(2,1)*mat(1,2))/2
        else if (ndim.eq.3) then
            vse = abs(&
                  mat(1,1)*mat(2,2)*mat(3,3) + mat(2,1)*mat(3,2)* mat(1,3) + mat(3,1)*mat(1,2)*ma&
                  &t(2,3) - mat(3,1)*mat(2,2)* mat(1,3) - mat(2,1)*mat(1,2)*mat(3,3) - mat(1,1)*m&
                  &at(3,2)* mat(2,3)&
                  )/6
        endif
!
!       CALCUL DU BARYCENTRE
!
        call vecini(ndim, 0.d0, bary)
        do 170 j = 1, ndim
            do 180 i = 1, ndim+1
                bary(j) = bary(j)+co(i,j)
180          continue
            bary(j) = bary(j)/(ndim+1)
170      continue
!
!        CALCUL DES DERIVEES DES FONCTIONS DE FORME
!
        call reeref(elrefp, .false., nbnoma, ibid, zr(igeom),&
                    bary, 1, .false., ndim, rbid,&
                    rbid, rbid, ibid, ibid, ibid,&
                    ibid, ibid, ibid, rbid, rbid,&
                    'DFF', xe, ff, dfdi, rbid,&
                    rbid, rbid)
        deriv =0.d0
        do 190 i = 1, ndim
            deriv = max(abs(dfdi(inoloc,i)),deriv)
190      continue
!       EN QUADRATIQUE : AUGMENTATION DU NOMBRE DE POINTS
        if (.not.iselli(elrefp)) then
            do 200 k = 1, ndim+1
                call vecini(ndim, 0.d0, point)
                do 210 j = 1, ndim
                    point(j) = (bary(j)+co(k,j))/2
210              continue
                call reeref(elrefp, .false., nbnoma, ibid, zr(igeom),&
                            point, ibid, .false., ndim, rbid,&
                            rbid, rbid, ibid, ibid, ibid,&
                            ibid, ibid, ibid, rbid, rbid,&
                            'DFF', xe, ff, dfdi, rbid,&
                            rbid, rbid)
                do 220 i = 1, ndim
                    deriv = max(abs(dfdi(inoloc,i)),deriv)
220              continue
200          continue
        endif
        vse = vse*deriv**2
        call cesexi('S', jcesd3, jcesl3, numa2, 1,&
                    ifiss, ise, iad)
!       ON AJOUTE LE VOLUME SELON LA VALEUR DE LA FONCTION HEAVISIDE
        if (zi(jcesv3-1+iad) .eq. -1) vmoin = vmoin+vse
        if (zi(jcesv3-1+iad) .eq. 1) vplus = vplus+vse
        vtot = vtot + vse
70  end do
!
end subroutine
