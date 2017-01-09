subroutine dkqrig(nomte, xyzl, option, pgl, rig,&
                  ener)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterc/r8gaem.h"
#include "asterfort/bsthpl.h"
#include "asterfort/dkqbf.h"
#include "asterfort/dkqshp.h"
#include "asterfort/dxqgm.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqbm.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqlocdri1.h"
#include "asterfort/dxqlocdri2.h"
#include "asterfort/dxqlocdri3.h"
#include "asterfort/dxqlocdri4.h"
#include "asterfort/dxqloe.h"
#include "asterfort/dxqloe_NV.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/r8inir.h"
#include "asterfort/utbtab.h"
#include "asterfort/utctab.h"
#include "asterfort/utpvgl.h"
#include "blas/dcopy.h"
#include "blas/dscal.h"
    real(kind=8) :: xyzl(3, *), pgl(*), rig(*), ener(*)
    character(len=16) :: option, nomte
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     MATRICE DE RIGIDITE DE L'ELEMENT DE PLAQUE DKQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT RIG    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_POT (EPOT_ELEM)
!     ------------------------------------------------------------------
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: multic, i, jcoqu, jdepg
    real(kind=8) :: wgt
    real(kind=8) :: df(9)=0.0, dm(9)=0.0, dmf(9)=0.0, dc(4)=0.0, dci(4)=0.0
    real(kind=8) :: df2(9)=0.0, dm2(9)=0.0, dmf2(9)=0.0
    real(kind=8) :: dmc(3, 2)=0.0, dfc(3, 2)=0.0
    real(kind=8) :: bf(3, 12)=0.0, bm(3, 8)=0.0
    real(kind=8) :: xab1(3, 12)=0.0, depl(24)=0.0, caraq4(25)=0.0, jacob(5)=0.0
    real(kind=8) :: qsi=0.0, eta=0.0
    real(kind=8) :: flex(144), memb(64), mefl(96)
    real(kind=8) :: t2iu(4)=0.0, t2ui(4)=0.0, t1ve(9)=0.0
    real(kind=8) :: bsigth(24)=0.0, enerth=0.0, excent=0.0, un, ctor=0.0
    aster_logical :: coupmf=.false., exce=.false., indith=.false.

!
!   LOCAL VARIABLES FOR COEF_RIGI_DRZ

    integer :: j, ii, jj,irot
!    integer :: iishp,jjshp
    integer, parameter :: npgmx=9
    real(kind=8) :: shp(3,4,npgmx), shpr1(3,4,npgmx), shpr2(3,4,npgmx), bb(12,npgmx)
    real(kind=8) :: gshp1(3,4), gshp2(3,4)
    real(kind=8) :: dArea=0.0, gam=0.0, epais=0.0, fact=0.0, gm(3, 4)
    real(kind=8) :: gmemb(4,4), btgmemb(8,4), gmefl(4,12)
!    real(kind=8) :: nm1(8), nm2(8), gm1(4), gm2(4)
    real(kind=8) :: bxb(12,12)

    aster_logical :: dri = .false.


!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    un = 1.0d0
    enerth = 0.0d0

    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
    epais = zr(jcoqu)
    exce = .false.
! COEF_RIGI_DRZ ACTIVE = -1 --> dri = true,  dri =  false sinon
    dri = .false.
    if (ctor .lt. 0.0d0 ) dri = .true.
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
!
!     ----- MISE A ZERO DES MATRICES : FLEX ,MEMB ET MEFL :
    call r8inir(144, 0.d0, flex, 1)
    call r8inir(64, 0.d0, memb, 1)
    call r8inir(96, 0.d0, mefl, 1)
!
!     ----- CALCUL DES MATRICES DE RIGIDITE DU MATERIAU EN FLEXION,
!           MEMBRANE ET CISAILLEMENT INVERSEE --------------------------
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!
  if(dri) then
     call r8inir(12*npgmx, 0.d0, shp, 1)
     call r8inir(12*npgmx, 0.d0, shpr1, 1)
     call r8inir(12*npgmx, 0.d0, shpr2, 1)
     call r8inir(12*npgmx, 0.d0, bb, 1)

     dArea = 0.0d0

     call r8inir(12, 0.d0, gshp1, 1)
     call r8inir(12, 0.d0, gshp2, 1)
     call r8inir(12,  0.d0,   gm, 1)
     call r8inir(16,  0.d0,   gmemb, 1)
     call r8inir(32,  0.d0, btgmemb, 1)
     call r8inir(48,  0.d0, gmefl, 1)
     call r8inir(144, 0.d0, bxb, 1)

      epais = zr(jcoqu)
     gam   = abs(ctor)*dm(1)
     do ii = 1, npg
!
!        ----- COORDINATES :
        qsi = zr(icoopg-1+ndim*(ii-1)+1)
        eta = zr(icoopg-1+ndim*(ii-1)+2)
!
!        ----- JACOBIAN AND WEIGHT :
        call jquad4(xyzl, qsi, eta, jacob)
        wgt = zr(ipoids+ii-1)*jacob(1)

!
!        ----- LOOP FOR SHP FUNCTIONS :
!
!        -- ELEMENT AREA :
        dArea = dArea + wgt
!
!        -- COMPUTE LINEAR AND ROTATIONAL SHAPE FUNCTIONS AND DERIVATIVES :
        call dkqshp(qsi, eta, caraq4, jacob, &
                    shp(1,1,ii), shpr1(1,1,ii), shpr2(1,1,ii))

          do j = 1,4
            do i = 1,3
              gshp1(i,j) = gshp1(i,j) + shpr1(i,j,ii)*wgt
              gshp2(i,j) = gshp2(i,j) + shpr2(i,j,ii)*wgt
            end do
          end do
     enddo

     do ii = 1, npg


      do j = 1,4
       do i = 1,3
          shpr1(i,j,ii) = shpr1(i,j,ii) - gshp1(i,j)/dArea
          shpr2(i,j,ii) = shpr2(i,j,ii) - gshp2(i,j)/dArea
       end do
      end do


      do i = 1,4
        j = 3*(i-1)
        bb(1+j,ii) = bb(1+j,ii) - shp(2,i,ii)
        bb(2+j,ii) = bb(2+j,ii) + shp(1,i,ii)
        bb(3+j,ii) = bb(3+j,ii) - 2.d0*shp(3,i,ii)
        bb(3+j,ii) = bb(3+j,ii) - shpr1(2,i,ii) + shpr2(1,i,ii)
      end do
   enddo

  endif
!
    do i = 1, npg

        qsi = zr(icoopg-1+ndim*(i-1)+1)
        eta = zr(icoopg-1+ndim*(i-1)+2)
!        ----- CALCUL DU JACOBIEN SUR LE QUADRANGLE --------------------
        call jquad4(xyzl, qsi, eta, jacob)
        wgt = zr(ipoids+i-1)*jacob(1)
!
!        -- FLEXION :
        call dkqbf(qsi, eta, jacob(2), caraq4, bf)
!        ----- CALCUL DU PRODUIT BFT.DF.BF -----------------------------
        call dcopy(9, df, 1, df2, 1)
        call dscal(9, wgt, df2, 1)
        call utbtab('CUMU', 3, 12, df2, bf,&
                    xab1, flex)
!
!        -- MEMBRANE :
        call dxqbm(qsi, eta, jacob(2), bm)
!        ----- CALCUL DU PRODUIT BMT.DM.BM -----------------------------
        call dcopy(9, dm, 1, dm2, 1)
        call dscal(9, wgt, dm2, 1)
        call utbtab('CUMU', 3, 8, dm2, bm,&
                    xab1, memb)
!
!   compute rotational part of membrane B matrix
!
    if(dri) then
!=====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
! ---  LES TERMES SONT EN NK*NP                                      =
!=====================================================================
!
!        call dkqnim(shp(1,1,i), shpr1(1,1,i), shpr2(1,1,i), &
!                          nm1, nm2, gm1, gm2)

!        do i = 1, 8
!            do j = 1, 8
!                memb(i,j) = memb(i,j) + nm1(i) * nm1(j) * wgt
!                memb(i,j) = memb(i,j) + nm2(i) * nm2(j) * wgt
!            end do
!        end do

!        do iishp = 1, 4
!            do jjshp = 1, 4
!                gmemb(i,j) = gmemb(i,j) + gm1(i) * gm1(j) * wgt
!                gmemb(i,j) = gmemb(i,j) + gm2(i) * gm2(j) * wgt
!            end do
!        end do


!        do iishp = 1, 8
!            do jjshp = 1, 4
!                btgmemb(i,j) = btgmemb(i,j) + nm1(i) * gm1(j) * wgt
!                btgmemb(i,j) = btgmemb(i,j) + nm2(i) * gm2(j) * wgt
!            end do
!        end do
!        -- MEMBRANE (DRILLING PART) Gm:
        call dxqgm(shpr1(1,1,i), shpr2(1,1,i), gm)

!        ----- CALCUL DU PRODUIT GMT.DM.GM
        call dcopy(9, dm, 1, dm2, 1)
        call dscal(9, wgt, dm2, 1)
        call utbtab('CUMU', 3, 4, dm2, gm,&
                    xab1, gmemb)
!        ----- CALCUL DU PRODUIT BMT.DM.GM
        call dcopy(9, dm, 1, dm2, 1)
        call dscal(9, wgt, dm2, 1)
        call utctab('CUMU', 3, 4, 8, dm2,&
                    gm, bm, xab1, btgmemb)

!        ----- CALCUL DU PRODUIT gam/Omega*b(x)b

        do irot = 1, 12
          fact = wgt*gam/dArea * bb(irot,i)
          do jj = 1, 12
            bxb(irot,jj) = bxb(irot,jj) + fact * bb(jj,i)
          end do
        end do


  endif

!
!
!        -- COUPLAGE :
        if (coupmf .or. exce) then
          if(dri) then
!           ----- CALCUL DU PRODUIT BMT.DMF.BF -------------------------
            call dcopy(9, dmf, 1, dmf2, 1)
            call dscal(9, wgt, dmf2, 1)
            call utctab('CUMU', 3, 12, 8, dmf2,&
                        bf, bm, xab1, mefl)
!
!   compute product Gmt.Dmf.Bf
!           ----- CALCUL DU PRODUIT GMT.DMF.BF -------------------------
            call dcopy(9, dmf, 1, dmf2, 1)
            call dscal(9, wgt, dmf2, 1)
            call utctab('CUMU', 3, 12, 4, dmf2,&
                        bf, gm, xab1, gmefl)
          else if (.not. dri) then
!           ----- CALCUL DU PRODUIT BMT.DMF.BF -------------------------
            call dcopy(9, dmf, 1, dmf2, 1)
            call dscal(9, wgt, dmf2, 1)
            call utctab('CUMU', 3, 12, 8, dmf2,&
                        bf, bm, xab1, mefl)
          else
             ASSERT(.false.)

          endif
        endif
    end do
!
    if (option .eq. 'RIGI_MECA') then
        if(.not. dri) then
          call dxqloc(flex, memb, mefl, ctor, rig)
        elseif (dri) then
!     Add rotational to stiffness matrix
!
          ctor=0.d0
          call dxqloc(flex, memb, mefl, ctor, rig)
         call dxqlocdri1(gmemb, rig)
         call dxqlocdri2(btgmemb, rig)
         call dxqlocdri3(gmefl, rig)
         call dxqlocdri4(bxb, rig)
         else
          ASSERT(.false.)
        endif
!
!

    else if (option.eq.'EPOT_ELEM') then
        call jevech('PDEPLAR', 'L', jdepg)
        call utpvgl(4, 6, pgl, zr(jdepg), depl)
       if(.not. dri) then
        call dxqloe(flex, memb, mefl, ctor, coupmf,&
                    depl, ener)
       elseif  (dri) then
!        call dxqloe(flex, memb, mefl, abs(ctor), coupmf,&
!                    depl, ener)
  !     Add rotational to stiffness matrix

          ctor=0.d0
          call dxqloc(flex, memb, mefl, ctor, rig)
         call dxqlocdri1(gmemb, rig)
         call dxqlocdri2(btgmemb, rig)
         call dxqlocdri3(gmefl, rig)
         call dxqlocdri4(bxb, rig)
       call dxqloe_NV(coupmf,rig,depl, ener)
       else
         ASSERT(.false.)
       endif
        call bsthpl(nomte, bsigth, indith)
        if (indith) then
            do i = 1, 24
                enerth = enerth + depl(i)*bsigth(i)
            end do
            ener(1) = ener(1) - enerth
        endif
    endif
!
end subroutine
