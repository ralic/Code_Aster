subroutine dis_contact(dimele, nno, nc, icodma,&
                  utl, xg, pgl, force, klv, option,&
                  varmo, varpl)
! ----------------------------------------------------------------------
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "asterfort/ut2vgl.h"
#include "asterfort/utpvgl.h"
#include "asterfort/vdiff.h"
#include "asterfort/assert.h"
#include "blas/dcopy.h"
    character(len=*) :: option
    integer ::      nno, nc, icodma, dimele
    real(kind=8) :: utl(nno*nc)
    real(kind=8) :: klv(nno*nc,nno*nc), xg(nno*3), pgl(3, 3)
    real(kind=8) :: varmo(8), varpl(8), force(3)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
!     RELATION DE COMPORTEMENT "DIS_CHOC"
!
! ----------------------------------------------------------------------
!
! IN  :  NBT    : NOMBRE DE VALEURS POUR LA DEMI-MATRICE
!        NNO    : NOMBRE DE NOEUDS
!        NC     : NOMBRE DE COMPOSANTES PAR NOEUDS
!        ICODMA : ADRESSE DU MATERIAU CODE
!        DUL    : INCREMENT DE DEPLACEMENT REPERE LOCAL
!        UTL    : DEPLACEMENT COURANT REPERE LOCAL
!        XG     : COORDONNEES DES NOEUDS REPERE GLOBAL
!        PGL    : MATRICE DE PASSAGE REPERE GLOBAL -> REPERE LOCALES
!        DIMELE : DIMENSION DE L'ELEMENT
!        VARMO  : VARIABLES INTERNES (TEMPS MOINS)
!
! OUT :  KLV    : MATRICE TANGENTE
!        FORCE  : EFFORTS
!        VARPL  : VARIABLES INTERNES (TEMPS PLUS)
!
! =============== DECLARATION DES VARIABLES LOCALES ====================
!
    integer :: nbre1, nbpar
    parameter     (nbre1=8)
    real(kind=8) :: valre1(nbre1)
    integer :: codre1(nbre1)
    character(len=8) :: nompar, nomre1(nbre1)
!
    real(kind=8) :: xl(6), xd(3), dirl(6), zero, rignor, rigtan
    real(kind=8) :: coulom, dist12, utot, depx, depy, depz
    real(kind=8) :: lambda, valpar, indic, fort, dist0, rtmp
!
    data nomre1 /'RIGI_NOR','RIGI_TAN','AMOR_NOR','AMOR_TAN'&
     &            ,'COULOMB','DIST_1','DIST_2','JEU'/
! ----------------------------------------------------------------------
!
! --- DEFINITION DES PARAMETRES
    zero = 0.d0
    call r8inir(6, zero, xl, 1)
    call r8inir(6, zero, dirl, 1)
    call r8inir(3, zero, xd, 1)
!     COORDONNEES DANS LE REPERE LOCAL
    ASSERT(dimele.eq.3)
    call utpvgl(nno, 3, pgl, xg, xl)

    nbpar = 0
    nompar = ' '
    valpar = 0.d0
    call r8inir(nbre1, zero, valre1, 1)
! ---    CARACTERISTIQUES DU MATERIAU
    call rcvala(icodma, ' ', 'DIS_CONTACT', nbpar, nompar,&
                [valpar], nbre1, nomre1, valre1, codre1, 0)
    ASSERT(codre1(1).eq.0)
    rignor = valre1(1)
    rigtan = valre1(2)
    coulom = valre1(5)
    if (nno .eq. 2) then
        dist12 = valre1(6)+valre1(7)
        utot = utl(1+nc)-utl(1)
        call vdiff(dimele, xl(1+dimele), xl(1), xd)
        depx = xd(1) - dist12 + utot
        depy = xd(2) + utl(2+nc) - utl(2)
        depz = xd(3) + utl(3+nc) - utl(3)
    else 
        dist12 = valre1(6)
        dist0 = valre1(8)
        depx = utl(1) + dist12 - dist0
        depy = utl(2)
        depz = utl(3)
    endif
!       CALCUL DES EFFORTS : seul un effort est remonté
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RAPH_MECA') then
      if (depx .le. zero) then
          force(1) = rignor*depx
          force(2) = rigtan*(depy-varmo(1))
          force(3) = rigtan*(depz-varmo(2))
          fort = (force(2)**2 + force(3)**2)**0.5d0
          if (fort .gt. abs(coulom*force(1))) then
            lambda=1.d0-abs(coulom*force(1))/fort
            varpl(1)=varmo(1)+lambda*force(2)/rigtan
            varpl(2)=varmo(2)+lambda*force(3)/rigtan
            force(2) = rigtan*(depy-varpl(1))
            force(3) = rigtan*(depz-varpl(2))
            varpl(3) = 1.d0
          else 
            varpl(1)=varmo(1)
            varpl(2)=varmo(2)
            varpl(3)= 0.d0
          endif          
      else
          force(1) = zero
          force(2) = zero
          force(3) = zero
          varpl(1) = zero
          varpl(2) = zero
          varpl(3) = -1.d0
      endif
    endif
!       CALCUL DE LA MATRIC : la matrice complete est remontée
    if (option .eq. 'FULL_MECA' .or. option .eq. 'RIGI_MECA_TANG') then
      if (option .eq. 'FULL_MECA') then
        indic=varpl(3)
      else
        indic=varmo(3)
        force(1)=rignor*depx
        force(2) = rigtan*(depy-varmo(1))
        force(3) = rigtan*(depz-varmo(2))
        fort = (force(2)**2 + force(3)**2)**0.5d0
      endif
      if (nno .eq. 2) then
          call r8inir(36, zero, klv, 1)
          if (indic.eq.0.d0) then
              klv(1,1) = rignor
              klv(4,1) = -rignor
              klv(1,4) = -rignor
              klv(4,4) = rignor
              klv(2,2) = rigtan
              klv(3,3) = rigtan
              klv(5,5) = rigtan
              klv(6,6) = rigtan
              klv(5,2) = -rigtan
              klv(2,5) = -rigtan
              klv(6,3) = -rigtan
              klv(3,6) = -rigtan
          elseif (indic.eq.1.d0) then
              klv(1,1) = rignor
              klv(4,1) = -rignor
              klv(1,4) = -rignor
              klv(4,4) = rignor
              rtmp = -rignor*rigtan*coulom/fort*(depy-varmo(1))
              klv(2,1) = rtmp
              klv(5,1) = -rtmp
              klv(2,4) = -rtmp
              klv(5,4) = rtmp
              rtmp = -rignor*rigtan*coulom/fort*(depz-varmo(2))
              klv(3,1) = rtmp
              klv(6,1) = -rtmp
              klv(3,4) = -rtmp
              klv(6,4) = rtmp
              rtmp = -coulom*force(1)*rigtan/fort*&
 &                     (1.d0-rigtan**2*(depy-varmo(1))**2/fort**2)
              klv(2,2) = rtmp
              klv(2,5) = -rtmp
              klv(5,2) = -rtmp
              klv(5,5) = rtmp
              rtmp = -coulom*force(1)*rigtan/fort*&
 &                     (1.d0-rigtan**2*(depz-varmo(2))**2/fort**2)
              klv(3,3) = rtmp
              klv(3,6) = -rtmp
              klv(6,3) = -rtmp
              klv(6,6) = rtmp
              rtmp = rigtan**3*coulom*force(1)/fort**3*&
 &                    (depy-varmo(1))*(depz-varmo(2))
              klv(2,3) = rtmp
              klv(2,6) = -rtmp
              klv(5,3) = -rtmp
              klv(5,6) = rtmp
              klv(3,2) = rtmp
              klv(3,5) = -rtmp
              klv(6,2) = -rtmp
              klv(6,5) = rtmp
          endif
      else
          call r8inir(9, zero, klv, 1)
          if (indic.eq.0.d0) then
              klv(1,1) = rignor
              klv(2,2) = rigtan
              klv(3,3) = rigtan
          elseif (indic.eq.1.d0) then
              klv(1,1) = rignor
              rtmp = -rignor*rigtan*coulom/fort*(depy-varmo(1))
              klv(2,1) = rtmp
              rtmp = -rignor*rigtan*coulom/fort*(depz-varmo(2))
              klv(3,1) = rtmp
              rtmp = -coulom*force(1)*rigtan/fort*&
 &                     (1.d0-rigtan**2*(depy-varmo(1))**2/fort**2)
              klv(2,2) = rtmp
              rtmp = -coulom*force(1)*rigtan/fort*&
 &                     (1.d0-rigtan**2*(depz-varmo(2))**2/fort**2)
              klv(3,3) = rtmp
              rtmp = rigtan**3*coulom*force(1)/fort**3*&
 &                    (depy-varmo(1))*(depz-varmo(2))
              klv(2,3) = rtmp
              klv(3,2) = rtmp
          endif
      endif
    endif
!
end subroutine
