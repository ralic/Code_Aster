subroutine mnleng(imat, xcdl, parcho, xus, ninc,&
                  nd, nchoc, h, nbpt, xeng)
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
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: imat(2), ind, ninc, nd, h, nbpt, nchoc
    character(len=14) :: xcdl, parcho, xus, xeng
    real(kind=8) :: e
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: pi
    real(kind=8) :: omega, alpha, jeu, rayon, origx, origy, ratio
    integer :: ix, iy, idy, imdy, iky
    integer :: ius, ieng, k, icdl, neq,     i
    integer ::   ireg,    nddl, nddlx, nddly
    real(kind=8), pointer :: dye(:) => null()
    real(kind=8), pointer :: kye(:) => null()
    real(kind=8), pointer :: mdye(:) => null()
    real(kind=8), pointer :: ye(:) => null()
    real(kind=8), pointer :: vjeu(:) => null()
    real(kind=8), pointer :: raid(:) => null()
    integer, pointer :: vnddl(:) => null()
    character(len=8), pointer :: type(:) => null()
    real(kind=8), pointer :: orig(:) => null()
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
    call jeveuo(parcho//'.TYPE', 'L', vk8=type)
    call jeveuo(parcho//'.NDDL', 'L', vi=vnddl)
    call jeveuo(parcho//'.REG', 'L', ireg)
    call jeveuo(parcho//'.JEU', 'L', vr=vjeu)
    call jeveuo(parcho//'.RAID', 'L', vr=raid)
    call jeveuo(parcho//'.ORIG', 'L', vr=orig)
    neq = zi(imat(1)+2)
! ----------------------------------------------------------------------
! --- DECLARATION VECTEURS TEMPORAIRES
! ----------------------------------------------------------------------
    call wkvect('&&MNLENG.X', 'V V R', nd*(2*h+1), ix)
    call wkvect('&&MNLENG.Y', 'V V R', nd, iy)
    call wkvect('&&MNLENG.DY', 'V V R', nd, idy)
    call wkvect('&&MNLENG.MDY', 'V V R', nd, imdy)
    call wkvect('&&MNLENG.KY', 'V V R', nd, iky)
!
    AS_ALLOCATE(vr=ye, size=neq)
    AS_ALLOCATE(vr=dye, size=neq)
    AS_ALLOCATE(vr=kye, size=neq)
    AS_ALLOCATE(vr=mdye, size=neq)
    do 100 ind = 1, nbpt-1
        call dscal(nd*(2*h+1), 0.d0, zr(ix), 1)
        call dscal(nd, 0.d0, zr(iy), 1)
        call dscal(nd, 0.d0, zr(idy), 1)
        call dscal(nd, 0.d0, zr(imdy), 1)
        call dscal(nd, 0.d0, zr(iky), 1)
!
        omega=zr(ius-1+ind*ninc)
        call dcopy(nd*(2*h+1), zr(ius+(ind-1)*ninc), 1, zr(ix), 1)
! ----------------------------------------------------------------------
! --- PASSAGE EN TEMPOREL (t=T/4)
! ----------------------------------------------------------------------
! ---   PI
        pi=r8pi()
        call dcopy(nd, zr(ix), 1, zr(iy), 1)
        ratio=4.d0
        do 10 k = 1, h
! ---     COS
            call daxpy(nd, dcos(2*k*pi/ratio), zr(ix-1+nd*k+1), 1, zr(iy),1)
            call daxpy(nd, k*omega*dcos(2*k*pi/ratio), zr(ix-1+nd*(h+k)+1), 1, zr(idy),1)
! ---     SIN
            call daxpy(nd, dsin(2*k*pi/ratio), zr(ix-1+nd*(h+k)+1), 1, zr(iy),1)
            call daxpy(nd, -k*omega*dsin(2*k*pi/ratio), zr(ix-1+nd*k+1), 1, zr(idy),1)
10      continue
! ----------------------------------------------------------------------
! --- CALCUL DE K*Y ET M*DY
! ----------------------------------------------------------------------
        call dscal(nd, 0.d0, ye, 1)
        call dscal(nd, 0.d0, dye, 1)
        call dscal(nd, 0.d0, kye, 1)
        call dscal(nd, 0.d0, mdye, 1)
        i=0
        do 20 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                ye(k)=zr(iy-1+i)
                dye(k)=zr(idy-1+i)
            endif
20      continue
        call mrmult('ZERO', imat(1), ye, kye, 1,&
                    .false._1)
        call mrmult('ZERO', imat(2), dye, mdye, 1,&
                    .false._1)
        call dscal(nd, 0.d0, zr(iky), 1)
        call dscal(nd, 0.d0, zr(imdy), 1)
        i=0
        do 30 k = 1, neq
            if (zi(icdl-1+k) .eq. 0) then
                i=i+1
                zr(iky-1+i)=kye(k)
                zr(imdy-1+i)=mdye(k)
            endif
30      continue
        e=ddot(nd, zr(iy),1,zr(iky),1)/2
        e=e+ddot(nd,zr(idy),1,zr(imdy),1)/2
        do 40 k=1,nchoc
            alpha=raid(k)
            jeu=vjeu(k)
            if(type(k)(1:4).eq.'PLAN') then
                nddl=vnddl(6*(k-1)+1)
                if(zr(iy-1+nddl).gt.jeu) then
                    e=e+0.5*alpha*(zr(iy-1+nddl)-jeu)**2
                endif
            else if(type(k)(1:7).eq.'BI_PLAN') then
                nddl=vnddl(6*(k-1)+1)
                if(zr(iy-1+nddl).gt.jeu) then
                    e=e+0.5*alpha*(zr(iy-1+nddl)-jeu)**2
                else if(zr(iy-1+nddl).lt.(-1.d0*jeu)) then
                    e=e+0.5*alpha*(zr(iy-1+nddl)+jeu)**2
                endif
            else if(type(k)(1:6).eq.'CERCLE') then
                nddlx=vnddl(6*(k-1)+1)
                nddly=vnddl(6*(k-1)+2)
                origx=orig(3*(k-1)+1)
                origy=orig(3*(k-1)+2)
                rayon=sqrt((zr(iy-1+nddlx)-origx)**2+(zr(iy-1+nddly)-origy)**2)
                if(rayon.gt.jeu) then
                    e=e+alpha*(rayon-jeu)**2
                endif
            endif
40      continue
        zr(ieng-1+ind)=e
100  continue
!
    call jedetr('&&MNLENG.X')
    call jedetr('&&MNLENG.Y')
    call jedetr('&&MNLENG.DY')
    call jedetr('&&MNLENG.MDY')
    call jedetr('&&MNLENG.KY')
!
    AS_DEALLOCATE(vr=ye)
    AS_DEALLOCATE(vr=dye)
    AS_DEALLOCATE(vr=kye)
    AS_DEALLOCATE(vr=mdye)

    call jedema()
!
end subroutine
