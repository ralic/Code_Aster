subroutine mnleng(imat, xcdl, parcho, xus, ninc,&
                  nd, nchoc, h, nbpt, xeng)
! aslint: disable=W1306

    implicit none
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
! ----------------------------------------------------------------------
!
!     MODE_NON_LINE CALCUL DE L'ENERGIE MECANIQUE
!     -    -   -    -           -       -
! ----------------------------------------------------------------------
!
! EFFECTUE LE PRODUIT DE DEUX SIGNAUX FREQUENTIELS X ET Y PAR LA METHODE
! AFT  : IFFT -> FFT -> IFFT
! LES COEFFICIENTS SONT RANGES AINSI : Z = [Z0 ZC1...ZCH ZS1...ZSH]
! X ET Y PEUVENT CONTENIR N VECTEURS, PAR EX : X = [Z1 Z2 ...ZN]
! ----------------------------------------------------------------------
! IN  IMAT   : I(2)          : DESCRIPTEUR DES MATRICES :
!                               - IMAT(1) => MATRICE DE RAIDEUR
!                               - IMAT(2) => MATRICE DE MASSE
! IN  XCDL   : K14           : INDICE DES CONDITIONS AUX LIMITES
! IN  PARCHO : K14           : NOM DE LA SD PARAMETRE DES CONTACTEURS
! IN  XUS    : K14           : BRANCHE SOLUTION
! IN  IND    : I             : INDICE DISCRETISATION
! IN  OMEGA  : R8            : PULSATION OMEGA
! IN  NINC   : I             : NOMBRE D INCONNUES DU SYSTEME
! IN  ND     : I             : NOMBRE DE DDLS ACTIFS
! IN  NCHOC  : I             : NOMBRE DE CONTACTEURS
! IN  H      : I             : NOMBRE D'HARMONIQUES
! IN  NBPT   : I             : NOMBRE DE POINT DE DISCRETISATION DE LA
!                                                                BRANCHE
! OUT XENG   : K14           : ENERGIE MECANIQUE
! ----------------------------------------------------------------------
!
!
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
#include "asterc/iisnan.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterc/r8pi.h"
#include "asterfort/wkvect.h"
    integer :: imat(2), ind, ninc, nd, h, nbpt, nchoc
    character(len=14) :: xcdl, parcho, xus, xeng
    real(kind=8) :: e
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: x(nd*(2*h+1)), pi, y(nd), dy(nd), ky(nd), mdy(nd)
    real(kind=8) :: omega, alpha, jeu, rayon, origx, origy, ratio
    integer :: ius, ieng, k, icdl, neq, iye, idye, ikye, imdye, i
    integer :: ityp, inddl, ireg, ijeu, iraid, iorig, nddl, nddlx, nddly
!
    call jemarq()
!
! ----------------------------------------------------------------------
! --- RECUPERATION POINTEUR ET TAILLE DE LA MATRICE
! ----------------------------------------------------------------------
    call jeveuo(xus, 'L', ius)
    call jeveuo(xeng, 'E', ieng)
    call dscal(nbpt-1, 0.d0, zr(ieng), 1)
    call jeveuo(xcdl, 'L', icdl)
    call jeveuo(parcho//'.TYPE', 'L', ityp)
    call jeveuo(parcho//'.NDDL', 'L', inddl)
    call jeveuo(parcho//'.REG', 'L', ireg)
    call jeveuo(parcho//'.JEU', 'L', ijeu)
    call jeveuo(parcho//'.RAID', 'L', iraid)
    call jeveuo(parcho//'.ORIG', 'L', iorig)
    neq = zi(imat(1)+2)
! ----------------------------------------------------------------------
! --- DECLARATION VECTEURS TEMPORAIRES
! ----------------------------------------------------------------------
    call wkvect('&&MNLENG.YE', 'V V R', neq, iye)
    call wkvect('&&MNLENG.DYE', 'V V R', neq, idye)
    call wkvect('&&MNLENG.KYE', 'V V R', neq, ikye)
    call wkvect('&&MNLENG.MDYE', 'V V R', neq, imdye)
    do 100 ind = 1, nbpt-1
        omega=zr(ius-1+ind*ninc)
        call dcopy(nd*(2*h+1), zr(ius+(ind-1)*ninc), 1, x, 1)
! ----------------------------------------------------------------------
! --- PASSAGE EN TEMPOREL (t=T/4)
! ----------------------------------------------------------------------
! ---   PI
        pi=r8pi()
        call dcopy(nd, x, 1, y, 1)
        dy=0.d0
        ratio=4.d0
        do 10 k = 1, h
! ---     COS
            call daxpy(nd, cos(2*k*pi/ratio), x(nd*k+1), 1, y,&
                       1)
            call daxpy(nd, k*omega*cos(2*k*pi/ratio), x(nd*(h+k)+1), 1, dy,&
                       1)
! ---     SIN
            call daxpy(nd, sin(2*k*pi/ratio), x(nd*(h+k)+1), 1, y,&
                       1)
            call daxpy(nd, -k*omega*sin(2*k*pi/ratio), x(nd*k+1), 1, dy,&
                       1)
10      continue
! ----------------------------------------------------------------------
! --- CALCUL DE K*Y ET M*DY
! ----------------------------------------------------------------------
        call dscal(nd, 0.d0, zr(iye), 1)
        call dscal(nd, 0.d0, zr(idye), 1)
        call dscal(nd, 0.d0, zr(ikye), 1)
        call dscal(nd, 0.d0, zr(imdye), 1)
        i=0
        do 20 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(iye-1+k)=y(i)
                zr(idye-1+k)=dy(i)
            endif
20      continue
        call mrmult('ZERO', imat(1), zr(iye), zr(ikye), 1,&
                    .false.)
        call mrmult('ZERO', imat(2), zr(idye), zr(imdye), 1,&
                    .false.)
        call dscal(nd, 0.d0, ky, 1)
        call dscal(nd, 0.d0, mdy, 1)
        i=0
        do 30 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                ky(i)=zr(ikye-1+k)
                mdy(i)=zr(imdye-1+k)
            endif
30      continue
        e=ddot(nd,y,1,ky,1)/2
        e=e+ddot(nd,dy,1,mdy,1)/2
        do 40 k=1,nchoc
            alpha=zr(iraid-1+k)
            jeu=zr(ijeu-1+k)
            if(zk8(ityp-1+k)(1:4).eq.'PLAN') then
                nddl=zi(inddl-1+6*(k-1)+1)
                if(y(nddl).gt.jeu) then
                    e=e+0.5*alpha*(y(nddl)-jeu)**2
                endif
            else if(zk8(ityp-1+k)(1:7).eq.'BI_PLAN') then
                nddl=zi(inddl-1+6*(k-1)+1)
                if(y(nddl).gt.jeu) then
                    e=e+0.5*alpha*(y(nddl)-jeu)**2
                else if(y(nddl).lt.(-1.d0*jeu)) then
                    e=e+0.5*alpha*(y(nddl)+jeu)**2
                endif
            else if(zk8(ityp-1+k)(1:6).eq.'CERCLE') then
                nddlx=zi(inddl-1+6*(k-1)+1)
                nddly=zi(inddl-1+6*(k-1)+2)
                origx=zr(iorig-1+3*(k-1)+1)
                origy=zr(iorig-1+3*(k-1)+2)
                rayon=sqrt((y(nddlx)-origx)**2+(y(nddly)-origy)**2)
                if(rayon.gt.jeu) then
                    e=e+alpha*(rayon-jeu)**2
                endif
            endif
40      continue
        zr(ieng-1+ind)=e
100  continue
!
    call jedetr('&&MNLENG.YE')
    call jedetr('&&MNLENG.DYE')
    call jedetr('&&MNLENG.KYE')
    call jedetr('&&MNLENG.MDYE')

    call jedema()
!
end subroutine
