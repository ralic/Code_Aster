subroutine cabhvf(maxfa, maxdim, ndim, nno, nnos,&
                  nface, axi, geom, vol, mface,&
                  dface, xface, normfa)
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
! =====================================================================
!
!  NB :           LE PARAMETRE PERMETANT DE CONSIDER QUE LE POINT EST
!                 A L INTERIEUR DU CERCLE OU NON EST ECRIT EN DUR
!                 ET VAUT 0.1D0*SQRT(VOL)(VALEUR ARBITRAIREMENT CHOISIE)
!
!
! NDIM :          DIMENSION D ESPACE
! NFACE :         NOMBRE DE FACES
! KINT  :         PERMEABILITE INTRINSEQUE (DIAGONALE)
! MFACE :         MESURE DES FACES
! DFACE :         DISTANCE DU CENTRE DE GRAVITÉ AUX FACES
! DFACE :         N EST PAS XG - XFACE
! XFACE(1:MAXDIM,1:MAXFA) : COORDONNES DES CENTRES DE FACES
! NORMFA(1:MAXDIM,1:MAXFA) :NORMALE SORTANTE
! MAXFA :         NOMBRE MAX DE FACES
! MAXAR :         NOMBRE MAX DE ARETES
! NOSAR(IAR ,1:2):LES DESDUS SOMMETS DE L ARETE IAR
!
! ======== OUT
! NBNOFA(1:NFACE):NOMBRE DE SOMMETS DE LA FACE
! NOSFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME SOMMET DE LA FACE IFA
!                                      (EN NUMEROTATION LOCALE)
! NARFA(IFA :1,NFACE,J : 1,NBNOFA(IFA)) J EME ARETE DE LA FACE IFA
!                                     (EN NUMEROTATION LOCALE)
! XS(1:MAXDIM,J) : COORD SOMMET J
! T(1:MAXDIM,J) :  COORD DU VECTEUR DE L J EME ARETE
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/pdvc2d.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/vfgefa.h"
#include "asterfort/vfnulo.h"
!
    integer :: ndim, nno, nnos
    aster_logical :: axi
    real(kind=8) :: geom(1:ndim, 1:nno)
!
    integer :: maxfa, maxdim, manofa
    integer :: maxfa1, maxdi1, maxar
    parameter   (maxfa1=6,maxdi1=3,manofa=4,maxar=12)
    real(kind=8) :: mface(1:maxfa), dface(1:maxfa), vol
    real(kind=8) :: normfa(1:maxdim, 1:maxfa), xface(1:maxdim, 1:maxfa)
    real(kind=8) :: xg(maxdi1)
    integer :: ifa
    integer :: nface
!
!  ARIS(IS,1) ET FAIS(IS,2) SONT LES DEUX ARETES ISSUTES DU SOMMET IS
!
    integer :: aris(3, 2)
    real(kind=8) :: epsilo, epsrel
    parameter   (epsrel=0.1d0)
!
!
    integer :: nbnofa(1:maxfa1)
    integer :: nosar(1:maxar, 2)
    integer :: nosfa(1:maxfa1, manofa)
    integer :: narfa(1:maxfa1, manofa)
    real(kind=8) :: xs(1:maxdi1, manofa), t(1:maxdi1, maxar)
    integer :: idim, is, iar, ns1, ns2, iret
!
!
    integer :: iadzi, iazk24
    character(len=8) :: nomail
    ASSERT(maxfa1.eq.maxfa)
    ASSERT(maxdi1.eq.maxdim)
    aris(1,1)=1
    aris(1,2)=3
    aris(2,1)=1
    aris(2,2)=2
    aris(3,1)=2
    aris(3,2)=3
    call vfnulo(maxfa1, maxar, ndim, nnos, nface,&
                nbnofa, nosar, nosfa, narfa)
    do 1 idim = 1, ndim
        xg(idim)=geom(idim,nno)
  1 end do
    if (ndim .eq. 2) then
!  ========================================
!  ========================================
!    2D
!  ========================================
!  ========================================
        do 2 ifa = 1, nface
            do 2 idim = 1, ndim
                xface(idim,ifa)=geom(idim,nnos+ifa)
                t(idim,ifa)=geom(idim,nosfa(ifa,2))- geom(idim,nosfa(&
                ifa,1))
  2         continue
!
        if (nface .eq. 3) then
            vol=abs(pdvc2d(t(1,1),t(1,2)))/2.d0
            epsilo=epsrel*sqrt(vol)
        else
            vol=(abs(pdvc2d(t(1,1),t(1,4)))+ abs(pdvc2d(t(1,3),t(1,2))&
            ))/2.d0
        endif
!
        do 3 ifa = 1, nface
            mface(ifa)=sqrt(t(1,ifa)**2+t(2,ifa)**2)
            normfa(1,ifa)=-t(2,ifa)/mface(ifa)
            normfa(2,ifa)= t(1,ifa)/mface(ifa)
  3     continue
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        do 17 ifa = 1, nface
            dface(ifa)=(xface(1,ifa)-xg(1))*normfa(1,ifa)+ (xface(2,&
            ifa)-xg(2))*normfa(2,ifa)
            if (dface(ifa) .lt. 0.d0) then
                dface(ifa)=-dface(ifa)
                normfa(1,ifa)=-normfa(1,ifa)
                normfa(2,ifa)=-normfa(2,ifa)
            endif
 17     continue
    else
!  ========================================
!  ========================================
!    3D
!  ========================================
!  ========================================
        vol = 0.d0
        do 10 idim = 1, ndim
            xg(idim)=0.d0
            do 18 is = 1, nnos
                xg(idim)=xg(idim)+geom(idim,is)
 18         continue
            xg(idim)=xg(idim)/nnos
 10     continue
!
        do 20 ifa = 1, nface
! T(DIM,IAR) : VECTEURS DES ARETES DE LA FACE
            do 22 iar = 1, nbnofa(ifa)
                ns1=nosar(narfa(ifa,iar),1)
                ns2=nosar(narfa(ifa,iar),2)
                do 21 idim = 1, ndim
                    t(idim,iar)=geom(idim,ns2)-geom(idim,ns1)
 21             continue
 22         continue
! XS(DIM,NS) : COORD DES SOMMETS DE LA FACE
            do 23 is = 1, nbnofa(ifa)
                do 24 idim = 1, ndim
                    xs(idim,is)=geom(idim,nosfa(ifa,is))
 24             continue
 23         continue
            call vfgefa(maxdi1, ndim, nbnofa(ifa), xs, t,&
                        xg, mface(ifa), normfa(1, ifa), xface(1, ifa), dface(ifa),&
                        iret)
!
            if (iret .ne. 0) then
                call tecael(iadzi, iazk24)
                nomail = zk24(iazk24-1+3) (1:8)
                call utmess('F', 'VOLUFINI_13', sk=nomail, si=ifa)
            endif
            vol=vol+dface(ifa)*mface(ifa)/3.d0
 20     continue
    endif
end subroutine
