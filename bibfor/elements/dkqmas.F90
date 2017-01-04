subroutine dkqmas(xyzl, option, pgl, mas, ener)
    implicit none
#include "blas/dcopy.h"
#include "blas/dscal.h"
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8gaem.h"
#include "asterfort/utctab.h"
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/dialum.h"
#include "asterfort/dkqnib.h"
#include "asterfort/dkqniw.h"
#include "asterfort/dkqnim.h"
#include "asterfort/dxqloc.h"
#include "asterfort/dxqloe.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqloe_NV.h"
#include "asterfort/dkqshp.h"
#include "asterfort/dxqlocdri1.h"
#include "asterfort/dxqlocdri2.h"
#include "asterfort/dxqlocdri3.h"
#include "asterfort/dxqlocdri4.h"
#include "asterfort/dxqnim.h"
#include "asterfort/dxqgm.h"
#include "asterfort/dxroep.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
#include "asterfort/jquad4.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
#include "asterfort/utpslg.h"
#include "asterfort/utpvgl.h"
    real(kind=8) :: xyzl(3, *), pgl(*), mas(*), ener(*)
    character(len=16) :: option
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
! Contributors    : nunziante.valoroso@uniparthenope.it
!     ------------------------------------------------------------------
!     MATRICE MASSE DE L'ELEMENT DE PLAQUE DKQ
!     ------------------------------------------------------------------
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  OPTION : OPTION RIGI_MECA OU EPOT_ELEM
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL/LOCAL
!     OUT MAS    : MATRICE DE RIGIDITE
!     OUT ENER   : TERMES POUR ENER_CIN (ECIN_ELEM)
!     ------------------------------------------------------------------
    integer :: i, j, k, i1, i2, int, ii(8), jj(8), ll(16)
    integer :: ndim, nno, nnos, npg, ipoids, icoopg, ivf, idfdx, idfd2, jgano
    integer :: jdepg, jcoqu, jvitg, iret
    real(kind=8) :: roe=0.0, rho=0.0, epais=0.0, rof=0.0, zero=0.0, douze=0.0
    real(kind=8) :: qsi=0.0, eta=0.0
    real(kind=8) :: detj=0.0, wgt=0.0
    real(kind=8) :: nfx(12)=0.0, nfy(12)=0.0, nmi(4)=0.0, vite(24)=0.0
    real(kind=8) :: wkq(12)=0.0, depl(24)=0.0
    real(kind=8) :: masloc(300)=0.0, masglo(300)=0.0
    real(kind=8) :: flex(12, 12)=0.0, memb(8, 8)=0.0, mefl(8, 12)=0.0, amemb(64)=0.0
    real(kind=8) :: unquar, undemi, un, neuf, excent=0.0, xinert=0.0
    real(kind=8) :: coefm=0.0, wgtf=0.0, wgtmf=0.0, caraq4(25)=0.0, jacob(5)=0.0
    character(len=3) :: stopz
    aster_logical :: exce=.false., iner=.false.
!     ------------------------------------------------------------------
    real(kind=8) :: ctor=0.0
    data (ii(k),k=1,8)&
     &   / 1, 10, 19, 28, 37, 46, 55, 64 /
    data (jj(k),k=1,8)&
     &   / 5, 14, 23, 32, 33, 42, 51, 60 /
    data (ll(k),k=1,16)&
     &   / 3, 7, 12, 16, 17, 21, 26, 30, 35, 39, 44, 48, 49, 53, 58, 62/
!     ------------------------------------------------------------------
!
!WARNING BB local variable is not used !
!   LOCAL VARIABLES FOR COEF_RIGI_DRZ
    integer, parameter :: npgmx=9
    integer            :: iishp , jjshp
    real(kind=8) :: shp(3,4,npgmx), shpr1(3,4,npgmx), shpr2(3,4,npgmx), bb(12,npgmx)
    real(kind=8) :: gshp1(3,4), gshp2(3,4)
    real(kind=8) :: dArea=0.0
    ! WARNING = BTGMEMB in dkqrig is ntgm in dkqmas
    real(kind=8) :: gmemb(4,4), ntgm(8,4), gmefl(4,12)
    real(kind=8) :: nm1(8), nm2(8), gm1(4), gm2(4), gm(3, 4)
    real(kind=8) :: bxb(12,12),fact=0.0,gam=0.0
    real(kind=8) :: df(9)=0.0, dm(9)=0.0, dmf(9)=0.0, dc(4)=0.0, dci(4)=0.0
    real(kind=8) :: dmf2(9)=0.0
    real(kind=8) :: dmc(3, 2)=0.0, dfc(3, 2)=0.0
    real(kind=8) :: t2iu(4)=0.0, t2ui(4)=0.0, t1ve(9)=0.0
    integer      :: multic,irot
    real(kind=8) :: xab1(3, 12)=0.0,bf(3, 12)=0.0, bm(3, 8)=0.0

    aster_logical :: dri = .false., coupmf = .false.
!
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jcoopg=icoopg, jvf=ivf, jdfde=idfdx, jdfd2=idfd2,&
                     jgano=jgano)
!
    zero = 0.0d0
    unquar = 0.25d0
    undemi = 0.5d0
    un = 1.0d0
    neuf = 9.0d0
    douze = 12.0d0
!
    call dxroep(rho, epais)
    roe = rho * epais
    rof = rho*epais*epais*epais/douze
    excent = zero
!
    call jevech('PCACOQU', 'L', jcoqu)
    ctor = zr(jcoqu+3)
    excent = zr(jcoqu+4)
    xinert = zr(jcoqu+5)
! COEF_RIGI_DRZ ACTIVE = -1 --> dri = true,  dri =  false sinon
    dri = .false.
    if (ctor .lt. 0.0d0 ) dri = .true.
!
    exce = .false.
    iner = .false.
    if (abs(excent) .gt. un/r8gaem()) exce = .true.
    if (abs(xinert) .gt. un/r8gaem()) iner = .true.
    if (.not. iner) rof = 0.0d0
!
! --- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE :
!     ---------------------------------------------------
    call gquad4(xyzl, caraq4)
!    rotational shape functions
    if(dri) then
     call r8inir(12*npg, 0.d0, shp, 1)
     call r8inir(12*npg, 0.d0, shpr1, 1)
     call r8inir(12*npg, 0.d0, shpr2, 1)
     call r8inir(12*npgmx, 0.d0, bb, 1)

     dArea = 0.0d0

     call r8inir(12, 0.d0, gshp1, 1)
     call r8inir(12, 0.d0, gshp2, 1)
     call r8inir(12,  0.d0,   gm, 1)
     call r8inir(16,  0.d0,   gmemb, 1)
     call r8inir(32,  0.d0, ntgm, 1)
     call r8inir(48,  0.d0, gmefl, 1)
     call r8inir(144, 0.d0, bxb, 1)
!
!     ----- CALCUL DES MATRICES DE RIGIDITE : DRILLING ROTATION --------------------------
    call dxmate('RIGI', df, dm, dmf, dc,&
                dci, dmc, dfc, nno, pgl,&
                multic, coupmf, t2iu, t2ui, t1ve)


     gam   = abs(ctor)*dm(1)
     do iishp = 1, npg
!
!        ----- COORDINATES :
        qsi = zr(icoopg-1+ndim*(iishp-1)+1)
        eta = zr(icoopg-1+ndim*(iishp-1)+2)
!
!        ----- JACOBIAN AND WEIGHT :
        call jquad4(xyzl, qsi, eta, jacob)
        wgt = zr(ipoids+iishp-1)*jacob(1)
!
!        -- ELEMENT AREA :
       dArea = dArea + wgt
!
!        -- COMPUTE LINEAR AND ROTATIONAL SHAPE FUNCTIONS AND DERIVATIVES :
        call dkqshp(qsi, eta, caraq4, jacob(2), &
                    shp(1,1,iishp), shpr1(1,1,iishp), shpr2(1,1,iishp))
!
          do j = 1,4
            do i = 1,3
              gshp1(i,j) = gshp1(i,j) + shpr1(i,j,iishp)*wgt
              gshp2(i,j) = gshp2(i,j) + shpr2(i,j,iishp)*wgt
            end do
          end do
!
     end do

!
     do iishp = 1,npg

      do j = 1,4
       do i = 1,3
          shpr1(i,j,iishp) = shpr1(i,j,iishp) - gshp1(i,j)/dArea
          shpr2(i,j,iishp) = shpr2(i,j,iishp) - gshp2(i,j)/dArea
       end do
      end do


      do i = 1,4
        j = 3*(i-1)
        bb(1+j,iishp) = bb(1+j,iishp) - shp(2,i,iishp)
        bb(2+j,iishp) = bb(2+j,iishp) + shp(1,i,iishp)
        bb(3+j,iishp) = bb(3+j,iishp) - 2.d0*shp(3,i,iishp)
        bb(3+j,iishp) = bb(3+j,iishp) - shpr1(2,i,iishp) + shpr2(1,i,iishp)
      end do

     end do

    endif

!
! --- INITIALISATIONS :
!     ---------------
    do k = 1, 96
        mefl(k,1) = zero
    end do
    do k = 1, 144
        flex(k,1) = zero
    end do

!======================================
! ---  CALCUL DE LA MATRICE DE MASSE  =
!======================================
!=====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE CLASSIQUE DE LA MATRICE DE MASSE =
! ---  LES TERMES SONT EN NK*NP                                      =
!=====================================================================
!
    coefm = caraq4(21) * roe / neuf
    do k = 1, 64
        amemb(k) = zero
    end do
    do k = 1, 8
        amemb(ii(k)) = un
        amemb(jj(k)) = unquar
    end do
    do k = 1, 16
        amemb(ll(k)) = undemi
    end do
    do k = 1, 64
        memb(k,1) = coefm * amemb(k)
    end do
!
! --- BOUCLE SUR LES POINTS D'INTEGRATION :
!     ===================================
    do int = 1, npg
        qsi = zr(icoopg-1+ndim*(int-1)+1)
        eta = zr(icoopg-1+ndim*(int-1)+2)
!
!   compute rotational part of membrane B matrix
!
    if(dri) then
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE MEMBRANE
! ---   EST EGALE A RHO_E = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+int-1) * jacob(1) * roe
!
!=====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE DE LA MATRICE DE MASSE =
! ---  LES TERMES SONT EN NK*NP                                      =
!=====================================================================
!
        call dkqnim(shp(1,1,int), shpr1(1,1,int), shpr2(1,1,int), &
                          nm1, nm2, gm1, gm2)

!        do i = 1, 8
!            do j = 1, 8
!                memb(i,j) = memb(i,j) + nm1(i) * nm1(j) * wgt
!                memb(i,j) = memb(i,j) + nm2(i) * nm2(j) * wgt
!            end do
!        end do

        do i = 1, 4
            do j = 1, 4
                gmemb(i,j) = gmemb(i,j) + gm1(i) * gm1(j) * wgt
                gmemb(i,j) = gmemb(i,j) + gm2(i) * gm2(j) * wgt
            end do
        end do


        do i = 1, 8
            do j = 1, 4
                ntgm(i,j) = ntgm(i,j) + nm1(i) * gm1(j) * wgt
                ntgm(i,j) = ntgm(i,j) + nm2(i) * gm2(j) * wgt
            end do
        end do
!
!        -- MEMBRANE (DRILLING PART) Gm:
!        call dxqgm(shpr1(1,1,int), shpr2(1,1,int), gm)

!        ----- CALCUL DU PRODUIT GMT.DM.GM
!        call dcopy(9, dm, 1, dm2, 1)
!        call dscal(9, wgt, dm2, 1)
!        call utbtab('CUMU', 3, 4, dm2, gm,&
!                    xab1, gmemb)
!!        ----- CALCUL DU PRODUIT BMT.DM.GM
!        call dcopy(9, dm, 1, dm2, 1)
!        call dscal(9, wgt, dm2, 1)
!        call utctab('CUMU', 3, 4, 8, dm2,&
!                    gm, bm, xab1, ntgm)

!        ----- CALCUL DU PRODUIT gam/Omega*b(x)b

        do irot = 1, 12
          fact = wgt*gam/dArea * bb(irot,int)
          do jjshp = 1, 12
            bxb(irot,jjshp) = bxb(irot,jjshp) + fact * bb(jjshp,int)
          end do
        end do
  endif
!
! ---    CALCUL DU JACOBIEN SUR LE QUADRANGLE :
!        ------------------------------------
        call jquad4(xyzl, qsi, eta, jacob)
!
!===========================================================
! ---  CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE  =
!===========================================================
!
! ---    CALCUL DES FONCTIONS D'INTERPOLATION DE LA FLECHE :
!        -------------------------------------------------
        call dkqniw(qsi, eta, caraq4, wkq)
!
        detj = jacob(1)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION W
! ---   EST EGALE A RHO_E = RHO*EPAIS :
!       -----------------------------
        wgt = zr(ipoids+int-1) * detj * roe
!
! ---   CALCUL DE LA PARTIE FLEXION DE LA MATRICE DE MASSE
! ---   DUE AUX SEULS TERMES DE LA FLECHE W :
!       -----------------------------------

        do i = 1, 12
            do j = 1, 12
                flex(i,j) = flex(i,j) + wkq(i) * wkq(j) * wgt
            end do
        end do
!
! ---   CALCUL DES FONCTIONS D'INTERPOLATION DES ROTATIONS :
!       --------------------------------------------------
        call dkqnib(qsi, eta, caraq4, nfx, nfy)
!
! ---   LA MASSE VOLUMIQUE RELATIVE AUX TERMES DE FLEXION BETA
! ---   EST EGALE A RHO_F = RHO*EPAIS**3/12 + D**2*EPAIS*RHO :
!       ----------------------------------------------------
        wgtf = zr(ipoids+int-1)*detj*(rof+excent*excent*roe)
!
! ---   PRISE EN COMPTE DES TERMES DE FLEXION DUS AUX ROTATIONS :
!       -------------------------------------------------------
        do i = 1, 12
            do j = 1, 12
                flex(i,j) = flex(i,j)+(nfx(i)*nfx(j)+nfy(i)*nfy(j))* wgtf
            end do
        end do
!
!====================================================================
! ---  CAS OU L'ELEMENT EST EXCENTRE                                =
!====================================================================
!
        if (exce) then
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
!
! ---     FONCTIONS D'INTERPOLATION MEMBRANE :
!         ----------------------------------
            call dxqnim(qsi, eta, nmi)
!
!====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE  =
!====================================================================
!
! ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
! ---     DE LA MASSE VOLUMIQUE
! ---     RHO_MF = D*EPAIS*RHO  :
!         --------------------
            wgtmf = zr(ipoids+int-1)*detj*excent*roe
!
! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA : (8x12)
!         ------------------------------------------
            do k = 1, 4
                i1 = 2*(k-1)+1
                i2 = i1 +1
                do j = 1, 12
                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
                end do
            end do

! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA : (8x12)
!         ------------------------------------------
!            do i = 1, 8
!                do j = 1, 12
!                    mefl(i,j) = mefl(i,j)+nm1(i)*nfx(j)*wgtmf
!                    mefl(i,j) = mefl(i,j)+nm2(i)*nfy(j)*wgtmf
!                end do
!            end do

! ---     TERMES DE COUPLAGE DRILLING-FLEXION U*BETA: (4x12)
!         ------------------------------------------


!     if(dri) then
!            do i = 1, 4
!                do j = 1, 12
!                    gmefl(i,j) = gmefl(i,j)+gm1(i)*nfx(j)*wgtmf
!                    gmefl(i,j) = gmefl(i,j)+gm2(i)*nfy(j)*wgtmf
!                end do
!            end do

!!        ----- CALCUL DU PRODUIT gam/Omega*b(x)b

!        do iishp = 1, 12
!          fact = wgt*gam/dArea * bb(iishp,int)
!          do jjshp = 1, 12
!            bxb(iishp,jjshp) = bxb(iishp,jjshp) + fact * bb(jjshp,int)
!          end do
!        end do
!     endif
          else
             ASSERT(.false.)
        endif
   endif
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
    end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------

!====================================================================
! ---  CAS OU L'ELEMENT EST EXCENTRE                                =
!====================================================================
!
!        if (exce) then
!
! ---     FONCTIONS D'INTERPOLATION MEMBRANE
!         ----------------------------------
!            call dxqnim(qsi, eta, nmi)
!
!====================================================================
! ---  CALCUL DE LA PARTIE MEMBRANE-FLEXION DE LA MATRICE DE MASSE  =
!====================================================================
!
! ---     POUR LE COUPLAGE MEMBRANE-FLEXION, ON DOIT TENIR COMPTE
! ---     DE LA MASSE VOLUMIQUE
! ---     RHO_MF = D*EPAIS*RHO  :
!         --------------------
!            wgtmf = zr(ipoids+int-1)*detj*excent*roe
!
! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA : (8x12)
!         ------------------------------------------
!            do k = 1, 4
!                i1 = 2*(k-1)+1
!                i2 = i1 +1
!                do j = 1, 12
!                    mefl(i1,j) = mefl(i1,j)+nmi(k)*nfx(j)*wgtmf
!                    mefl(i2,j) = mefl(i2,j)+nmi(k)*nfy(j)*wgtmf
!                end do
!            end do

! ---     TERMES DE COUPLAGE MEMBRANE-FLEXION U*BETA : (8x12)
!         ------------------------------------------
!            do i = 1, 8
!                do j = 1, 12
!                    mefl(i,j) = mefl(i,j)+nm1(i)*nfx(j)*wgtmf
!                    mefl(i,j) = mefl(i,j)+nm2(i)*nfy(j)*wgtmf
!                end do
!            end do

! ---     TERMES DE COUPLAGE DRILLING-FLEXION U*BETA: (4x12)
!         ------------------------------------------

!            do i = 1, 4
!                do j = 1, 12
!                    gmefl(i,j) = gmefl(i,j)+gm1(i)*nfx(j)*wgtmf
!                    gmefl(i,j) = gmefl(i,j)+gm2(i)*nfy(j)*wgtmf
!                end do
!            end do
!        endif
! ---   FIN DU TRAITEMENT DU CAS D'UN ELEMENT EXCENTRE
!       ----------------------------------------------
!    end do
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
! --- FIN DE LA BOUCLE SUR LES POINTS D'INTEGRATION
!     ---------------------------------------------
!
!
! --- INSERTION DES DIFFERENTES PARTIES CALCULEES DE LA MATRICE
! --- DE MASSE A LA MATRICE ELLE MEME :
!     ===============================
    if (( option .eq. 'MASS_MECA' ) .or. (option.eq.'M_GAMMA')) then
        if(.not. dri) then
          call dxqloc(flex, memb, mefl, ctor, mas)
        elseif (dri) then
!     Add rotational to stiffness matrix
!
          ctor=0.d0
!
          call dxqloc(flex, memb, mefl, ctor, mas)
!     Add rotational to stiffness matrix
!
          call dxqlocdri1(gmemb, mas)
          call dxqlocdri2(ntgm, mas)
          call dxqlocdri3(gmefl, mas)
          call dxqlocdri4(bxb, mas)
         else
          ASSERT(.false.)
        endif
!
    else if (option.eq.'MASS_MECA_DIAG' .or. option.eq.'MASS_MECA_EXPLI') then
        if(.not. dri) then
          call dxqloc(flex, memb, mefl, ctor, masloc)
          wgt = caraq4(21) * roe
         call utpslg(4, 6, pgl, masloc, masglo)
         call dialum(4, 6, 24, wgt, masglo,&
                    mas)
        elseif (dri) then
           ctor=0.d0
          call dxqloc(flex, memb, mefl, ctor, masloc)
          call dxqlocdri1(gmemb, masloc)
          call dxqlocdri2(ntgm, masloc)
          call dxqlocdri3(gmefl, masloc)
          call dxqlocdri4(bxb, masloc)
          wgt = caraq4(21) * roe
         call utpslg(4, 6, pgl, masloc, masglo)
         call dialum(4, 6, 24, wgt, masglo,&
                    mas)
         else
          ASSERT(.false.)
        endif
!
    else if (option .eq. 'ECIN_ELEM') then
        stopz='ONO'
! IRET NE PEUT VALOIR QUE 0 (TOUT VA BIEN) OU 2 (CHAMP NON FOURNI)
        call tecach(stopz, 'PVITESR', 'L', iret, iad=jvitg)
        if (iret .eq. 0) then
            call utpvgl(4, 6, pgl, zr(jvitg), vite)
            call dxqloe(flex, memb, mefl, ctor, .false._1,&
                        vite, ener)
        else
            call tecach(stopz, 'PDEPLAR', 'L', iret, iad=jdepg)
            if (iret .eq. 0) then
                call utpvgl(4, 6, pgl, zr(jdepg), depl)
                call dxqloe(flex, memb, mefl, ctor, .false._1,&
                            depl, ener)
            else
                call utmess('F', 'ELEMENTS2_1', sk=option)
            endif
        endif
    endif
!
end subroutine
