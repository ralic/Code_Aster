subroutine xchavi(actpoi, jbasc, jffis, jfon, jvit,&
                  jbeta, ndim, nfonn, sifval)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
    implicit none
#include "asterfort/assert.h"
#include "blas/ddot.h"
#include "asterfort/getvis.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterc/r8maem.h"
#include "asterc/r8pi.h"
#include "asterc/r8prem.h"
#include "jeveux.h"
!
! Propagation XFEM avec éléments cohésifs
! Calculer l'avancée qui produit le nouveau front
!  à partir de l'ancien
!
! In actpoi => Position du premier point du morceau
!              de front à traiter
! In jbasc => base covariante au front de fissure (ancien)
! In jfiss => coordonnées points ancien front
! In jfon => coordonnées points nouveau front
! Out jvit => avancée qui produit le nouveau front
! In ndim => dimension
! In nfonn => nombre de points nouveau front
! In sifval => nombre de points ancien front
!
    integer :: actpoi
    real(kind=8) :: b(3), beta1, ci(3), cosb
    real(kind=8) :: dir(3)
    integer :: i
    real(kind=8) :: poitot, poiav, poids
    integer :: ipt, j, jbasc, jbeta, jffis, jfon, jvit
    real(kind=8) :: loncar, m(3), mi(3), mtast, pi
    integer :: ndim, nfonn
    real(kind=8) :: n(3), t(3)
    real(kind=8) :: p(3)
    integer :: sifval, nbptfo, ibid
    real(kind=8) :: sinb, spint, tast(3), vecv(3)
    real(kind=8) :: vitn, vnor, vpnt, linf, lprop, lcalc
    aster_logical :: linter
! --------------------------------------------------------------
!
!
!       LONGUEUR CARACTERISTIQUE MAILLAGE
!       SERT POUR LA LONGUEUR D INFLUENCE
    loncar = ( zr(jffis-1+4*(actpoi+sifval-1)+4) - zr(jffis-1+4* actpoi+4) ) / sifval
    loncar = abs(loncar)
    pi = r8pi()
    call getvis(' ','NB_POINT_FOND',scal=nbptfo, nbret=ibid)
    linf = ( zr(jffis-1+4*(actpoi+sifval-1)+4) - zr(jffis-1+4* actpoi+4) ) / nbptfo
    linf = abs(linf)
!
    do 400 i = 1, sifval
!
!       RECUP POINT ET BASE ANCIEN FRONT
        do 250 j = 1, ndim
            m(j) = zr(jffis-1+4*(i-1)+j)
            n(j) = zr(jbasc-1+2*ndim*(i-1)+j)
            t(j) = zr(jbasc-1+2*ndim*(i-1)+ndim+j)
250      continue
!
!       ON REORTHOGONALISE (cf XPRVIT)
        call normev(n, mtast)
        ASSERT(mtast.gt.0.d0)
        call provec(n, t, b)
        call normev(b, mtast)
        ASSERT(mtast.gt.0.d0)
        call provec(b, n, tast)
        call normev(tast, mtast)
        ASSERT(mtast.gt.0.d0)
        do 270 j = 1, ndim
            t(j) = tast(j)
270     continue
!
!       LONGUEUR INFLUENCE EN DUR
        lcalc = (1.d-5)*loncar
        lprop = (1.d-3)*loncar
!
!       INITIALISATIONS
        poitot = 0.d0
        vpnt = 0.d0
        beta1 = 0.d0
        linter = .false.
!
!       BOUCLE SUR LES POINTS DU NOUVEAU FOND
!       QUI NE SONT PAS ORDONNES
        do 240 ipt = 1, nfonn
!
!           DISTANCE PT NOUVEAU FRONT/PT ANCIEN FRONT
            do 280 j = 1, ndim
                ci(j)=zr(jfon-1+11*(ipt-1)+j)
                mi(j)=ci(j)-m(j)
280         continue
!
!           DISTANCE PT PLAN (SIGNEE)
            vitn = ddot(3,mi,1,b,1)
!
!           CALCUL DU VECTEUR VITESSE DS LE PLAN VECV
!           ET SA DIRECTION NORMALISEE DIR
            do 290 j = 1, ndim
                vecv(j) = mi(j) - vitn*b(j)
                dir(j) = vecv(j)
290         continue
            call normev(dir, vnor)
!
!           SI DISTANCE < DISTANCE INFLUENCE
!           ON RENTRE CETTE VITESSE DANS LA MOYENNE
!           POIDS GAUSSIENS TRONQUES A [-2*Linf,+2*Linf]
            poids = 1.d0/(sqrt(2*pi)*linf)*(exp(-vitn*vitn/(2*linf*linf))-0.05)
            if (poids .gt. 0.d0) then
                linter = .true.
                poiav = poitot
                poitot = poitot + poids
                if ((ddot(3,vecv,1,t,1)) .le. lcalc) then
                    vpnt = vpnt*poiav/poitot
                else
                    vpnt = vpnt*poiav/poitot + vnor*poids/poitot
                    sinb = ddot(3,dir,1,n,1)
                    cosb = ddot(3,dir,1,t,1)
                    beta1 = beta1*poiav/poitot + (atan2(sinb,cosb))*poids/poitot
                endif
            endif
!
240      continue
!
!       STOCKAGE VITESSE ET ANGLE
!       PROPAGATION MINIMALE LPROP
!
        if(.not.linter) then
            zr(jvit-1+i) = -2.d0
        else if(vpnt.gt.lprop) then
            zr(jvit-1+i) = vpnt
        else
            zr(jvit-1+i) = 0.d0
        endif
        zr(jbeta-1+i) = beta1
!
400  continue
!
! --- SI PAS DE POINT TROUVE, ON PREND LA VITESSE LA PLUS PROCHE
!
     do i=1,sifval
         if(zr(jvit-1+i).lt.-1.d0) then
!            Il faudrait ajouter un message pour l'utilisateur ici
!            pour dire qu'on prend la vitesse la plus proche
             if(i.lt.sifval/2) then
                 do j=i+1,sifval/2
                     if(zr(jvit-1+j).ge.0.d0) zr(jvit-1+i) = zr(jvit-1+j)
                     if(zr(jvit-1+j).ge.0.d0) zr(jbeta-1+i) = zr(jbeta-1+j)
                     if(zr(jvit-1+j).ge.0.d0) goto 500
                 end do
500              continue
             else if(i.gt.sifval/2) then
                 do j=i-1,sifval/2,-1
                     if(zr(jvit-1+j).ge.0.d0) zr(jvit-1+i) = zr(jvit-1+j)
                     if(zr(jvit-1+j).ge.0.d0) zr(jbeta-1+i) = zr(jbeta-1+j)
                     if(zr(jvit-1+j).ge.0.d0) goto 505
                 end do
505              continue
             endif
         endif
     end do
end subroutine
