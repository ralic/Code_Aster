subroutine te0587(option, nomte)
    implicit none
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterfort/carcou.h"
#include "asterfort/elref5.h"
#include "asterfort/jevech.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=16) :: option, nomte
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
! ......................................................................
!
!    - FONCTION REALISEE:  CALC_CHAMP POUR LES TUYAUX :
!        - EFGE_ELGA
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nbcoum, nbsecm, jnbspi
    real(kind=8) :: h, a
    parameter(nbsecm=32,nbcoum=10)
    real(kind=8) :: poicou(2*nbcoum+1), poisec(2*nbsecm+1)
    real(kind=8) :: pi, deuxpi, sig(6), fno(4, 6)
    real(kind=8) :: efg(6)
    real(kind=8) :: pgl(3, 3), pgl4(3, 3)
    real(kind=8) :: cosfi, sinfi
    real(kind=8) :: fi, poids, r, omega
    real(kind=8) :: pgl1(3, 3), pgl2(3, 3), pgl3(3, 3), rayon, theta, l
    integer :: nno, nnos, jgano, ndim, npg, nbcou, nbsec, lorien
    integer :: ipoids, ivf, ic, kp, jin, jcoopg, jdfd2
    integer :: icagep, idfdk
    integer :: igau, icou, isect, i, jout
    integer :: indice, icoud2, mmt
    integer :: kpgs
!
    integer :: vali
!
    call elref5(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, jcoopg, ivf, idfdk,&
                jdfd2, jgano)
!
!
    pi=r8pi()
    deuxpi=2.d0*pi
!
!=====RECUPERATION NOMBRE DE COUCHES ET DE SECTEURS ANGULAIRES
!
    call jevech('PNBSP_I', 'L', jnbspi)
    nbcou=zi(jnbspi-1+1)
    nbsec=zi(jnbspi-1+2)
    if (nbcou*nbsec .le. 0) then
        call u2mess('F', 'ELEMENTS4_46')
    endif
    if (nbcou .gt. nbcoum) then
        vali=nbcoum
        call u2mesg('F', 'ELEMENTS5_2', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
    if (nbsec .gt. nbsecm) then
        vali=nbsecm
        call u2mesg('F', 'ELEMENTS5_3', 0, ' ', 1,&
                    vali, 0, 0.d0)
    endif
!
!
!  LES POIDS POUR L'INTEGRATION DANS L'EPAISSEUR
    poicou(1)=1.d0/3.d0
    do 20 i = 1, nbcou-1
        poicou(2*i)=4.d0/3.d0
        poicou(2*i+1)=2.d0/3.d0
20  end do
    poicou(2*nbcou)=4.d0/3.d0
    poicou(2*nbcou+1)=1.d0/3.d0
!
!  LES POIDS POUR L'INTEGRATION SUR LA CIRCONFERENCE
    poisec(1)=1.d0/3.d0
    do 30 i = 1, nbsec-1
        poisec(2*i)=4.d0/3.d0
        poisec(2*i+1)=2.d0/3.d0
30  end do
    poisec(2*nbsec)=4.d0/3.d0
    poisec(2*nbsec+1)=1.d0/3.d0
!
!
    if (option .eq. 'EFGE_ELGA') then
!     ---------------------------------
        call jevech('PCAGEPO', 'L', icagep)
        call jevech('PCAORIE', 'L', lorien)
        call jevech('PSIEFR', 'L', jin)
        call jevech('PEFGER', 'E', jout)
!
!       -- A= RMOY, H = EPAISSEUR
        h=zr(icagep+1)
        a=zr(icagep)-h/2.d0
!
!       -- ORIENTATION :
        call carcou(zr(lorien), l, pgl, rayon, theta,&
                    pgl1, pgl2, pgl3, pgl4, nno,&
                    omega, icoud2)
        if (icoud2 .ge. 10) then
            mmt=0
        else
            mmt=1
        endif
!
!
        kpgs=0
!       -- CALCUL DES EFFORTS SUR LES POINTS DE GAUSS (VFNO)
        do 180 igau = 1, npg
!
            do 140,i=1,6
            efg(i)=0.d0
140          continue
!
!         -- BOUCLE SUR LES POINTS DE SIMPSON DANS L'EPAISSEUR
            do 160 icou = 1, 2*nbcou+1
                if (mmt .eq. 0) then
                    r=a
                else
                    r=a+(icou-1)*h/(2.d0*nbcou)-h/2.d0
                endif
!
!           -- BOUCLE SUR LES POINTS DE SIMPSON SUR LA CIRCONFERENCE
                do 150 isect = 1, 2*nbsec+1
!
                    kpgs=kpgs+1
                    fi=(isect-1)*deuxpi/(2.d0*nbsec)
                    cosfi=cos(fi)
                    sinfi=sin(fi)
!
                    indice=jin-1+6*(kpgs-1)
                    sig(1)=zr(indice+1)
                    sig(2)=zr(indice+2)
                    sig(3)=zr(indice+4)
                    sig(4)=zr(indice+5)
!
                    poids=poicou(icou)*poisec(isect)*h*deuxpi/&
                    (4.d0*nbcou*nbsec)*r
!
!
                    efg(1)=efg(1)+poids*sig(1)
                    efg(2)=efg(2)-poids*(sinfi*sig(4)+cosfi*sig(3))
                    efg(3)=efg(3)+poids*(sinfi*sig(3)-cosfi*sig(4))
!
                    efg(4)=efg(4)-poids*sig(3)*r
                    efg(5)=efg(5)-poids*sig(1)*r*cosfi
                    efg(6)=efg(6)+poids*sig(1)*r*sinfi
150              continue
160          continue
!
            do 170,i=1,6
            fno(igau,i)=efg(i)
170          continue
180      continue
!
!
        do 200 ic = 1, 6
            do 190 kp = 1, npg
                zr(jout+6*(kp-1)+ic-1)=fno(kp,ic)
190          continue
200      continue
    else
        call u2mesk('F', 'ELEMENTS4_49', 1, option)
    endif
!
end subroutine
