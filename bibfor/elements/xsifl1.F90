subroutine xsifl1(angl, basloc, coeff, coeff3, ddlm,&
                  ddls, dfdi, ff, he, heavn, idepl,&
                  igthet, ipref, ipres, ithet, jac,&
                  jlst, ka, mu, nd,&
                  ndim, nfh, nnop, nnops, itemps,&
                  nompar, option, singu, xg, igeom)
    implicit none
!
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
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/chauxi.h"
#include "asterfort/fointe.h"
#include "asterfort/indent.h"
#include "asterfort/lteatt.h"
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalc_code.h"
#include "asterfort/xcalc_heav.h"
#include "jeveux.h"
!
! Calcul de G avec forces de pression XFEM sur les levres
!   de la fissure
!
    integer :: igeom
    integer :: nnop, ndim, heavn(nnop,5)
    real(kind=8) :: angl(2), basloc(9*nnop), cisa, coeff, coeff3
    integer :: cpt, ddlm, ddls
    real(kind=8) :: depla(3), dfdi(nnop, ndim), dfor(3), divt
    real(kind=8) :: dtdm(3, 3), e1(3), e2(3), e3(3), ff(27)
    real(kind=8) :: forrep(3, 2), g, he(2)
    integer :: i, idepl, ier, igthet, ilev, indi, ino, ig, hea_fa(2)
    integer :: ipref, ipres, ithet, j
    real(kind=8) :: jac
    integer :: jlst
    real(kind=8) :: jm, var(ndim+1)
    real(kind=8) :: k1, k2, k3, ka, lsn, lst, mu, nd(3)
    integer :: nfh, nnops, itemps
    character(len=8) :: nompar(4)
    real(kind=8) :: norme
    character(len=16) :: option
    real(kind=8) :: p(3, 3), pres, rb9(3, 3), rr(2)
    real(kind=8) :: pres_test, cisa_test, r8pre
    integer :: singu
    real(kind=8) :: theta(3), u1(3), u1l(3), u2(3), u2l(3), u3(3)
    real(kind=8) :: u3l(3), xg(3), rb33(3,3,3), r
    aster_logical :: axi, l_pres_var, l_cisa_var
    call vecini(3, 0.d0, e1)
    call vecini(3, 0.d0, e2)
    lsn=0.d0
    lst=0.d0
    do 100 ino = 1, nnop
        lsn = lsn + zr(jlst-1+ino)*ff(ino)
        lst = lst + zr(jlst-1+ino)*ff(ino)
        do 110 i = 1, ndim
            e1(i) = e1(i) + basloc(3*ndim*(ino-1)+i+ndim) * ff(ino)
            e2(i) = e2(i) + basloc(3*ndim*(ino-1)+i+2*ndim) * ff(ino)
110      continue
100  continue
!
!     NORMALISATION DE LA BASE
    call normev(e1, norme)
    call normev(e2, norme)
    call provec(e1, e2, e3)
!
!     CALCUL DE LA MATRICE DE PASSAGE P TQ 'GLOBAL' = P * 'LOCAL'
    call vecini(9, 0.d0, p)
    do 120 i = 1, ndim
        p(i,1)=e1(i)
        p(i,2)=e2(i)
        p(i,3)=e3(i)
120  continue
!
!     CALCUL DE RR = SQRT(DISTANCE AU FOND DE FISSURE)
    ASSERT(lst.lt.0.d0)
    rr(1)=-sqrt(-lst)
    rr(2)= sqrt(-lst)
!
! CALCUL DE L IDENTIFIANT DE FACETTES MAITRE/ESCLAVE
    do ilev=1,2
      hea_fa(ilev)=xcalc_code(1,he_real=[he(ilev)])
    enddo
!
!     -----------------------------------------------
!     1) CALCUL DES FORCES SUIVANT LES OPTIONS
!     -----------------------------------------------
!
    call vecini(3*2, 0.d0, forrep)
!
    if ((option.eq.'CALC_K_G') .or. (option.eq.'CALC_G')&
        .or. (option .eq. 'CALC_GTP')) then
!
!         CALCUL DE LA PRESSION AUX POINTS DE GAUSS
        pres = 0.d0
        cisa = 0.d0
        do 240 ino = 1, nnop
            if (ndim .eq. 3) pres = pres + zr(ipres-1+ino) * ff(ino)
            if (ndim .eq. 2) then
                pres = pres + zr(ipres-1+2*(ino-1)+1) * ff(ino)
                cisa = cisa + zr(ipres-1+2*(ino-1)+2) * ff(ino)
            endif
240      continue
        do 250 j = 1, ndim
            forrep(j,1) = -pres * nd(j)
            forrep(j,2) = -pres * (-nd(j))
250      continue
        if (ndim .eq. 2) then
            forrep(1,1) = forrep(1,1)- cisa * nd(2)
            forrep(2,1) = forrep(2,1)+ cisa * nd(1)
            forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
            forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
        endif
!
    else if ((option.eq.'CALC_K_G_F') .or. (option.eq.'CALC_G_F')&
            .or. (option .eq. 'CALC_GTP_F')) then
!
!         VALEUR DE LA PRESSION
        call vecini(ndim+1, 0.d0, var)
        do j = 1, ndim
           var(j) = xg(j)
        end do
        var(ndim+1) = zr(itemps)
        call fointe('FM', zk8(ipref), ndim+1, nompar, var,&
                    pres, ier)
        if (ndim .eq. 2) call fointe('FM', zk8(ipref+1), ndim+1, nompar, var,&
                                     cisa, ier)
        do 260 j = 1, ndim
            forrep(j,1) = -pres * nd(j)
            forrep(j,2) = -pres * (-nd(j))
260      continue
        if (ndim .eq. 2) then
            forrep(1,1) = forrep(1,1)- cisa * nd(2)
            forrep(2,1) = forrep(2,1)+ cisa * nd(1)
            forrep(1,2) = forrep(1,2)- cisa * (-nd(2))
            forrep(2,2) = forrep(2,2)+ cisa * (-nd(1))
        endif
    else
        call utmess('F', 'XFEM_15')
    endif
!
!     -----------------------------------
!     2) CALCUL DE THETA ET DE DIV(THETA)
!     -----------------------------------
    divt= 0.d0
    call vecini(9, 0.d0, dtdm)
!
    do 390 i = 1, ndim
        theta(i)=0.d0
        do 301 ino = 1, nnop
            theta(i) = theta(i) + ff(ino) * zr(ithet-1+ndim*(ino-1)+i)
301      continue
!
        do 310 j = 1, ndim
            do 311 ino = 1, nnop
                dtdm(i,j) = dtdm(i,j) + zr(ithet-1+ndim*(ino-1)+i) * dfdi(ino,j)
311          continue
310      continue
!
        divt = divt + dtdm(i,i)
!
390  continue
!
    axi = lteatt('AXIS','OUI')
    if (axi) then
        r = 0.d0
        do ino = 1, nnop
            r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
        end do
        ASSERT(r.gt.0d0)
        divt = divt + theta(1)/r
        jac = jac * r
    endif

!
!     BOUCLE SUR LES DEUX LEVRES
    do 300 ilev = 1, 2
!
!       ---------------------------------------------
!       3) CALCUL DU DEPLACEMENT
!       ---------------------------------------------
        call vecini(ndim, 0.d0, depla)
        do 200 ino = 1, nnop
            call indent(ino, ddls, ddlm, nnops, indi)
            cpt=0
!         DDLS CLASSIQUES
            do 201 i = 1, ndim
                cpt=cpt+1
                depla(i) = depla(i) + ff(ino)*zr(idepl-1+indi+cpt)
201          continue
!         DDLS HEAVISIDE
            do 202 i = 1, ndim
              do ig = 1, nfh              
                cpt=cpt+1
                depla(i) = depla(i) + xcalc_heav(heavn(ino,ig),hea_fa(ilev),heavn(ino,5))& 
                                    * ff(ino) * zr(idepl-1+ indi+cpt)
              enddo
202          continue
!         DDL ENRICHIS EN FOND DE FISSURE
            do 204 i = 1, singu*ndim
                cpt=cpt+1
                depla(i) = depla(i) + rr(ilev) * ff(ino) * zr(idepl-1+ indi+cpt)
204          continue
200      continue
!
!       --------------------------------
!       4) CALCUL DES CHAMPS AUXILIAIRES
!       --------------------------------
!
        if (option(1:8) .eq. 'CALC_K_G') then
!         CHAMPS AUXILIARES DANS LA BASE LOCALE : U1L,U2L,U3L
            call vecini(9, 0.d0, rb9)
            call chauxi(ndim, mu, ka, -lst, angl(ilev),&
                        rb9, .false._1, rb33, rb9, rb9,&
                        rb9, u1l, u2l, u3l)
!
!         CHAMPS AUXILIARES DANS LA BASE GLOBALE : U1,U2,U3
            call vecini(ndim, 0.d0, u1)
            call vecini(ndim, 0.d0, u2)
            call vecini(ndim, 0.d0, u3)
            do 510 i = 1, ndim
                do 511 j = 1, ndim
                    u1(i) = u1(i) + p(i,j) * u1l(j)
                    u2(i) = u2(i) + p(i,j) * u2l(j)
                    if (ndim .eq. 3) u3(i) = u3(i) + p(i,j) * u3l(j)
511              continue
510          continue
        endif
!
!       -----------------------------------------
!       5) CALCUL DE 'DFOR' =  D(PRES)/DI . THETA
!       -----------------------------------------
        call vecini(3, 0.d0, dfor)
!
!       D(PRES)/DI n'etait pas correctement calcule dans cette routine.
!       issue24174 supprime le calcul de cette quantite (DFOR reste nul) 
!       et interdit toute autre chose qu'un chargement constant.
        if ( (option.eq.'CALC_K_G') .or.&
             (option.eq.'CALC_G') ) then
!
!           Tester le nom de l'option (CALC_*G ou CALC_*G_F) ne suffit
!           pas pour detecter le caractere constant du chargement si on
!           a un evol_char. Le test ci-dessous est plus robuste.
            r8pre = r8prem()
!           en 2D on a les composantes PRES, CISA
            if (ndim .eq. 2) then
                pres_test = zr(ipres-1+2*(1-1)+1)
                cisa_test = zr(ipres-1+2*(1-1)+2)
                do ino = 1, nnop
                    l_pres_var = abs( zr(ipres-1+2*(ino-1)+1) - pres_test ) .ge. r8pre
                    l_cisa_var = abs( zr(ipres-1+2*(ino-1)+2) - cisa_test ) .ge. r8pre
                    if ( l_pres_var .or. l_cisa_var ) then
                        ASSERT(.false.)
                    endif
                enddo
            endif
!           en 3D on a uniquement la composante PRES
            if (ndim .eq. 3) then
                pres_test = zr(ipres-1+1)
                do ino = 1, nnop
                    l_pres_var = abs( zr(ipres-1+ino) - pres_test ) .ge. r8pre
                    if ( l_pres_var ) then
                        ASSERT(.false.)
                    endif
                enddo
            endif
!
        elseif ( (option.eq.'CALC_K_G_F') .or.&
                 (option.eq.'CALC_G_F') ) then
!
            call utmess('F', 'XFEM_99')
!
        else
            ASSERT(.false.)
        endif
!
!       -----------------------------------
!       6) CALCUL EFFECTIF DE G, K1, K2, K3
!       -----------------------------------
        g = 0.d0
        k1 = 0.d0
        k2 = 0.d0
        k3 = 0.d0
        do 520 j = 1, ndim
            g = g + (forrep(j,ilev) * divt + dfor(j)) * depla(j)
            if (option(1:8) .eq. 'CALC_K_G') then
                k1 = k1 + (forrep(j,ilev) * divt + dfor(j)) * u1(j)
                k2 = k2 + (forrep(j,ilev) * divt + dfor(j)) * u2(j)
                if (ndim .eq. 3) k3 = k3 + (forrep(j,ilev) * divt + dfor(j)) * u3(j)
            endif
520      continue
!
        jm = jac*0.5d0
!
        if (ndim .eq. 3) then
            zr(igthet-1+1)=zr(igthet-1+1)+g*jac
            if (option(1:8) .eq. 'CALC_K_G') then
                zr(igthet-1+2)=zr(igthet-1+2)+k1*jm*sqrt(coeff)
                zr(igthet-1+3)=zr(igthet-1+3)+k2*jm*sqrt(coeff)
                zr(igthet-1+4)=zr(igthet-1+4)+k3*jm*sqrt(coeff3)
                zr(igthet-1+5)=zr(igthet-1+5)+k1*jm*coeff
                zr(igthet-1+6)=zr(igthet-1+6)+k2*jm*coeff
                zr(igthet-1+7)=zr(igthet-1+7)+k3*jm*coeff3
            endif
        else if (ndim.eq.2) then
!
            zr(igthet-1+1)=zr(igthet-1+1)+g*jac
!
            if (option(1:8) .eq. 'CALC_K_G') then
                zr(igthet-1+2)=zr(igthet-1+2)+k1*jm*sqrt(coeff)
                zr(igthet-1+3)=zr(igthet-1+3)+k2*jm*sqrt(coeff)
                zr(igthet-1+4)=zr(igthet-1+4)+k1*jm*coeff
                zr(igthet-1+5)=zr(igthet-1+5)+k2*jm*coeff
            endif
!
        endif
!
300  continue
!     FIN DE BOUCLE SUR LES DEUX LEVRES
end subroutine
