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
#include "asterfort/utmess.h"
#include "asterfort/xbasgl.h"
#include "asterfort/xdeffk_wrap.h"
#include "asterfort/xderfk_wrap.h"
#include "asterfort/coor_cyl.h"
#include "asterfort/elrfvf.h"
#include "asterfort/xelrex.h"
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
    real(kind=8) :: p(27,3,3), invp(27,3,3), p_g(ndim,ndim), invp_g(ndim,ndim)
    real(kind=8) :: dkdpo(ndim,ndim,2), dkdlo(3,3,2), fkpo(ndim,ndim), fk_gl(ndim,ndim)
    real(kind=8) :: rr ,th, r_n(27), t_n(27), fkpo_n(27,3,3)
    real(kind=8) :: fkpo_g(3,3), dkdgl_g(3,3,3), signe
    real(kind=8) :: ff1(27), dfdi1(27,3)
    real(kind=8) :: xref(81), ff_n(27)
    aster_logical :: lderiv, l_not_zero, lshift, lctlin, lbid
    aster_logical :: lcourb
    character(len=8) :: method
    real(kind=8) :: courb(3,3,3)
!----------------------------------------------------------------
!
    lctlin = is_enr_line()
    lshift=.not.(count(stano(1:nnop).eq.-2).eq.nnop)
    method='DEFAULT'
    lcourb=.false.
    fk(:,:,:)=0.d0
    if (.not.present(dkdgl)) then
      lderiv=.false.
    else
      lderiv=.true.
      if (.not.present(dfdi)) then
          call utmess('F', 'ELEMENTS6_6', sk='dfdi')
      endif
      dkdgl(:,:,:,:)=0.d0
    endif
!
    if (present(nnop_lin).and.lctlin) then
      if (.not.present(ff_lin)) then
          call utmess('F', 'ELEMENTS6_6', sk='ff_lin')
      endif
      nnops=nnop_lin
      ff1(1:nnops)=ff_lin(1:nnops)
      if (lderiv) then
        if (.not.present(dfdi_lin)) then
          call utmess('F', 'ELEMENTS6_6', sk='dfdi_lin')
        endif
        dfdi1(1:nnops,1:ndim)=dfdi_lin(1:nnops,1:ndim)
      endif
    else
      nnops=nnop
      ff1(1:nnops)=ff(1:nnops)
      if (lderiv) dfdi1(1:nnops,1:ndim)=dfdi(1:nnops,1:ndim)
    endif
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
       if (.not.(face.eq.'MAIT'.or.face.eq.'ESCL'.or.face.eq.' ')) then
          call utmess('F', 'ELEMENTS6_6', sk='face')
       endif
       if (face.eq.'MAIT') th=+1.d0*r8pi()
       if (face.eq.'ESCL') th=-1.d0*r8pi()
       if (face.eq.' ') th=he*abs(th)
    else
       th=he*abs(th)
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
    if (lshift) then
    call xelrex(elrefp, nno, xref, ndime=ndime)
    do ino=1, nnop      
      call elrfvf(elrefp, xref((ndime*(ino-1)+1):(ndime*(ino-1)+ndime)),&
                  nnop, ff_n, nno)
      call coor_cyl(ndim, nnop, basloc, geom, ff_n,&
                    p(ino,1:ndim,1:ndim), invp(ino,1:ndim,1:ndim),&
                    r_n(ino), t_n(ino), lbid)
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
      elseif(stano(ino).eq.-2) then
        goto 5
      else
        call utmess('F', 'ELEMENTS6_6', sk='stano')
      endif
      do alp =1, ndim
        do i =1, ndim
          fkpo_g(alp,i)=fkpo_g(alp,i)+fkpo_n(ino,alp,i)*ff(ino)
        enddo
      enddo
5   continue
    endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FONCTIONS D'ENRICHISSEMENT 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  * AU POINT DE GAUSS
    call xdeffk_wrap(kappa, mu, rr, th, ndim, fkpo, method, 0)
!  * CONVERSION DANS LA BASE GLOBALE
    fk_gl(:,:)=0.d0
    do alp =1, ndim
        do i =1, ndim
           do j =1, ndim
             fk_gl(alp,i)=fk_gl(alp,i)+p_g(i,j)*(fkpo(alp,j)-fkpo_g(alp,j))
           enddo
        enddo
    enddo
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DE LA DERIVATION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
    lderiv=lderiv.and.l_not_zero
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  PREPARATION DE LA DERIVATION DE LA FONCTION SHIFT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    dkdgl_g(:,:,:)=0.d0
    if ( lderiv ) then
      if ( lshift) then
      do 7 ino = 1, nnop
        if (stano(ino).eq.-2) goto 7
        do alp =1, ndim
          do i =1, ndim
            do j =1, ndim
              do k =1, ndim
                dkdgl_g(alp,i,j)=dkdgl_g(alp,i,j)+p_g(i,k)*fkpo_n(ino,alp,k)*dfdi(ino,j)
!                if (lcourb) dkdgl_g(alp,i,j)=dkdgl_g(alp,i,j)+&
!                                                 courb(i,k,j)*fkpo_n(ino,alp,k)*ff(ino)
              enddo           
            enddo
          enddo
        enddo
7     continue
      endif
!  ON ROGNE SUR TOUT / CALCUL DE LA DERIVEE EN AMONT
!  *  DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE
      call xderfk_wrap(kappa, mu, rr, th, ndim, dkdpo, method, 0)
!  *  DERIVEES DES FONCTIONS D'ENRICHISSEMENT DANS LA BASE LOCALE
      do alp =1, ndim
        do  i = 1, ndim
          dkdlo(alp,i,1)=dkdpo(alp,i,1)*cos(th)-dkdpo(alp,i,2)*sin(th)/rr
          dkdlo(alp,i,2)=dkdpo(alp,i,1)*sin(th)+dkdpo(alp,i,2)*cos(th)/rr
        enddo
      enddo
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
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     DERIVEES DES FONCTIONS D'ENRICHISSEMENT 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (.not.lderiv) goto 11
!  *  CONVERSION DANS LA BASE GLOBALE
      do alp = 1, ndim
        do i =1, ndim
           do j =1, ndim
             do k =1, ndim
               do l =1, 2
                 dkdgl(ino,alp,i,j)=dkdgl(ino,alp,i,j)+p_g(i,k)*dkdlo(alp,k,l)*invp_g(l,j)
               enddo
             enddo
             dkdgl(ino,alp,i,j)=(dkdgl(ino,alp,i,j)-dkdgl_g(alp,i,j))*ff1(ino)+&
                                fk_gl(alp,i)*dfdi1(ino,j)
!             if (lcourb) then
!               do k =1, ndim
!                 dkdgl(ino,alp,i,j)=dkdgl(ino,alp,i,j)+fk(ino,alp,k)*ff1(ino)*courb(i,k,j)
!               enddo
!             endif
           enddo
        enddo
      enddo
!
11   continue
!  *  MULTIPLICATION DE FK PAR FF
     do alp = 1, ndim
       do i =1, ndim
         fk(ino,alp,i)=fk_gl(alp,i)*ff1(ino)      
       enddo
     enddo
!
10   continue
!
999  continue
!
end subroutine
