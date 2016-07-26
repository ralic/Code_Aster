subroutine xcalfev(elrefp, ndim, nnop, basloc, stano, he,&
                   lsn, lst, geom, kappa, mu, ff, fk,&
                   dfdi, dkdgl, face,&
                   nnop_lin, ff_lin, dfdi_lin)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/xbasgl.h"
#include "asterfort/xdeffk_wrap.h"
#include "asterfort/xderfk_wrap.h"
#include "asterfort/coor_cyl.h"
#include "asterfort/elrfvf.h"
#include "asterfort/xelrex.h"
#include "asterfort/xcoocy.h"
#include "asterfort/is_enr_line.h"
#include "asterfort/iselli.h"
#include "asterc/r8pi.h"
!
    character(len=8), intent(in) :: elrefp
    integer :: ndim, nnop, stano(*)
    real(kind=8) :: he, lsn(*), basloc(*), fk(27,3,3), lst(*)
    real(kind=8) :: kappa, ff(*), geom(*), mu
    real(kind=8), optional :: dkdgl(27,3,3,3)
    real(kind=8), optional :: dfdi(nnop,ndim)
    character(len=4), optional :: face
    integer, optional :: nnop_lin
    real(kind=8), optional :: ff_lin(:)
    real(kind=8), optional :: dfdi_lin(:,:)
!
!
!
!     BUT:  CALCUL DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> EN UN POINT DE GAUSS
!            DANS LA BASE <GLOBALE>
!
! IN  HE      : VALEUR DE LA FONCTION HEAVYSIDE CSTE LE SS-ELT
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  KA, MU  : PARAMETRES MATERIAU / LES FONCTIONS ASYMPTIQUE POUR UN MATERIAU ELASTIQUE
! IN  FF      : FONCTIONS DE FORMES DE L ELEMENT PARENT
! IN  DFDI    : DERIVEES DES FONCTIONS DE FORMES DE L ELEMENT PARENT
!
! OUT FK      : VALEURS DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> DANS LA BASE <GLOBALE>
! OUT DKDGL   : DERIVEES DES FONCTIONS D'ENRICHISSEMENT <VECTORIEL> DANS LA BASE <GLOBALE>
!
!----------------------------------------------------------------
!
    integer :: i, j, k, ino, l, alp, nnops
    integer :: ndime, nno
    real(kind=8) :: p(ndim,ndim), invp(ndim,ndim), p_g(ndim,ndim), invp_g(ndim,ndim)
    real(kind=8) :: dkdpo(ndim,ndim,2), dkdlo(ndim,ndim,ndim), fkpo(ndim,ndim)
    real(kind=8) :: rr ,th, r_n(nnop), t_n(nnop), fkpo_n(nnop,ndim,ndim)
    real(kind=8) :: pt(ndim), pfon(ndim)
    real(kind=8) :: fkpo_g(ndim,ndim), dkdgl_g(ndim,ndim,ndim), signe
    real(kind=8) :: ff1(nnop), dfdi1(nnop,ndim)
    real(kind=8) :: xref(81), ff_n(nnop), wrap, dwrap(ndim)
    aster_logical :: lderiv, l_not_zero, lshift, lctlin, lbid
    aster_logical :: lcourb
    character(len=8) :: method
    real(kind=8) :: courb(3,3,3)
!----------------------------------------------------------------
!
    lctlin = is_enr_line()
    lshift=.true.
    method='DEFAULT'
    lcourb=.false.
!    if (.not.iselli(elrefp)) lshift=.true.
!  EN 3D PAS DE CONVERGENCE DE LA METHODE SHIFTED
!    ... EN NORME L2
!    ON ABANDONNE LE SHIFT POUR LE MOMENT
    if (ndim.eq.3) then
      if (iselli(elrefp)) then
        lshift=.true.
!        lcourb=.true.
      else
        lcourb=.true.
      endif
    endif
    fk(:,:,:)=0.d0
    if (.not.present(dkdgl)) then
      lderiv=.false.
    else
      lderiv=.true.
      ASSERT(present(dfdi))
      dkdgl(:,:,:,:)=0.d0
    endif
!
    if (present(nnop_lin).and.lctlin) then
      ASSERT(present(ff_lin))
      nnops=nnop_lin
      ff1(1:nnops)=ff_lin(1:nnops)
      if (lderiv) then
        ASSERT(present(dfdi_lin))
      endif
      if (lderiv) dfdi1(1:nnops,1:ndim)=dfdi_lin(1:nnops,1:ndim)
    else
      nnops=nnop
      ff1(1:nnops)=ff(1:nnops)
      if (lderiv) dfdi1(1:nnops,1:ndim)=dfdi(1:nnops,1:ndim)
    endif
    wrap=0.
    dwrap(:)=0.
    do ino=1, nnop
      if (abs(stano(ino)).ge.2) then
        wrap=wrap+ff(ino)
        if (.not.lderiv) goto 111
        do i=1,ndim
          dwrap(i)=dwrap(i)+dfdi(ino,i)
        enddo
111     continue
      endif
    enddo
!  STIFFER WRAPPING
!    dwrap(1:ndim)=2*dwrap(1:ndim)*wrap
!    wrap=wrap**2
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DES COORDONNEES CYLINDRIQUES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    lcourb=lcourb.and.lderiv
    if (lderiv) then
      call coor_cyl(ndim, nnop, basloc, geom, ff,&
                    p_g, invp_g, rr, th, l_not_zero,&
                    courb, dfdi, lcourb)
    else
      call coor_cyl(ndim, nnop, basloc, geom, ff,&
                    p_g, invp_g, rr, th, l_not_zero)
    endif
!
    if (.not.l_not_zero) goto 999
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CORRECTION POUR LE CONTACT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    if (present(face)) then 
       ASSERT(face.eq.'MAIT'.or.face.eq.'ESCL'.or.face.eq.' ')
       if (face.eq.'MAIT') th=+1.d0*r8pi()
       if (face.eq.'ESCL') th=-1.d0*r8pi()
       if (face.eq.' ') th=he*abs(th)
!    else
!       th=he*abs(th)
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CALCUL DU SIGNE POUR L INTERPOLATION FANTOME
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    signe=sign(1.d0,th)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DU SHIFT: INTERPOLATION AUX NOEUDS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    fkpo_g(:,:)=0.d0
    fkpo_n(:,:,:)=0.d0
    pt(:)=0.d0
    pfon(:)=0.d0
    do ino=1, nnop
      call xelrex(elrefp, nno, xref, ndime=ndime)
      call elrfvf(elrefp, xref((ndime*(ino-1)+1):(ndime*(ino-1)+ndime)),&
                  nnop, ff_n, nno)
      call coor_cyl(ndim, nnop, basloc, geom, ff_n,&
                    p, invp, r_n(ino), t_n(ino), lbid)
    enddo
!
    do 5 ino = 1, nnop
!     INTERPOLATION FANTOME DANS LES MAILLES XHT
      if (stano(ino).eq.1 .or. stano(ino).eq.3) then
        call xdeffk_wrap(kappa, mu, r_n(ino), signe*abs(t_n(ino)), ndim, fkpo_n(ino,1:ndim,1:ndim),&
                         method, stano(ino))
!     INTERPOLATION STANDARD DANS LES MAILLES XT
      elseif(stano(ino).eq.0 .or. stano(ino).eq.2) then
        call xdeffk_wrap(kappa, mu, r_n(ino), t_n(ino), ndim, fkpo_n(ino,1:ndim,1:ndim),&
                         method, stano(ino))
      elseif(stano(ino).ne.-2) then
!        print*,' *** KOR : stano=',stano(1:nnop)
        ASSERT(.false.)
      endif
      do alp =1, ndim
        do i =1, ndim
          fkpo_g(alp,i)=fkpo_g(alp,i)+fkpo_n(ino,alp,i)*ff(ino)
        enddo
      enddo
5   continue
!  BLOCAGE TEMPORAIRE DU SHIFT => POUR COMPARAISON AVANT / APRES
    if (.not.lshift) then
      fkpo_g(:,:)=0.d0
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DE LA DERIVATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    lderiv=lderiv.and.l_not_zero
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DE LA DERIVATION DE LA FONCTION SHIFT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    dkdgl_g(:,:,:)=0.d0
    if ( lderiv .and. lshift ) then
      do 6 ino = 1, nnop
        call xbasgl(ndim, basloc, ino, p, invp)
        do alp =1, ndim
          do i =1, ndim
            do j =1, ndim
              do k =1, ndim
                dkdgl_g(alp,i,j)=dkdgl_g(alp,i,j)+p_g(i,k)*fkpo_n(ino,alp,k)*dfdi(ino,j)
              enddo
            enddo
          enddo
        enddo
 6    continue
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  CALCUL DES FONCTIONS VECTORIELLES AU POINT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    do 10 ino = 1, nnop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  POUR LE QUADRATIQUE => BASCULEMENT EN LINEAIRE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
      if (ino.gt.nnops) goto 10
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  BASE LOCALE => BASE GLOBALE AU NOEUD 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   
      call xbasgl(ndim, basloc, ino, p, invp)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FONCTIONS D'ENRICHISSEMENT 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! - AU POINT DE GAUSS
      call xdeffk_wrap(kappa, mu, rr, th, ndim, fkpo, method, stano(ino))
!  *  CONVERSION DANS LA BASE GLOBALE
      do alp =1, ndim
        do i =1, ndim
           do j =1, ndim
             fk(ino,alp,i)=fk(ino,alp,i)+p(i,j)*(fkpo(alp,j)-fkpo_g(alp,j))
           enddo
        enddo
      enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     CALCUL DES DERIVEES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (.not.lderiv) goto 11
!
! - AU POINT DE GAUSS
!  *  DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
      call xderfk_wrap(kappa, mu, rr, th, ndim, dkdpo, method, stano(ino))
!  *  DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE LOCALE
      do alp =1, ndim
        do  i = 1, ndim
          dkdlo(alp,i,1)=dkdpo(alp,i,1)*cos(th)-dkdpo(alp,i,2)*sin(th)/rr
          dkdlo(alp,i,2)=dkdpo(alp,i,1)*sin(th)+dkdpo(alp,i,2)*cos(th)/rr
          if (ndim.eq.3) dkdlo(alp,i,3)=0.d0
        enddo
      enddo
!  *  CONVERSION DANS LA BASE GLOBALE
      do alp = 1, ndim
        do i =1, ndim
           do j =1, ndim
             do k =1, ndim
               do l =1, ndim
                 dkdgl(ino,alp,i,j)=dkdgl(ino,alp,i,j)+p(i,k)*dkdlo(alp,k,l)*invp_g(l,j)
               enddo
             enddo
             dkdgl(ino,alp,i,j)=(dkdgl(ino,alp,i,j)-dkdgl_g(alp,i,j))*ff1(ino)+&
                                fk(ino,alp,i)*dfdi1(ino,j)
             if (lcourb) then
               do k =1, ndim
                 dkdgl(ino,alp,i,j)=dkdgl(ino,alp,i,j)+fk(ino,alp,k)*ff1(ino)*courb(i,k,j)
               enddo
             endif
!             dkdgl(ino,alp,i,j)=dkdgl(ino,alp,i,j)*wrap+fk(ino,alp,i)*ff1(ino)*dwrap(j)
           enddo
        enddo
      enddo
!
11   continue
!  *  MULTIPLICATION DE FK PAR FF
     do alp = 1, ndim
       do i =1, ndim
!         fk(ino,alp,i)=fk(ino,alp,i)*ff1(ino)*wrap       
         fk(ino,alp,i)=fk(ino,alp,i)*ff1(ino)      
       enddo
     enddo
!
10   continue
!
999  continue
!
end subroutine
