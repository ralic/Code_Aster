subroutine cafves(cont, tange, maxfa, nface, fks,&
                  dfks1, dfks2, mobfa, dmob1, dmob2,&
                  mob1f, mob2f, flux, dflx1, dflx2)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  CETTE SUBROUTINE PERMET DE CALCULER D UNE MANIERE
!  GENERIQUE LE FLUX TOTALVOLUMIQUE
!
! =========================================================
! ****IN :
!     FKS(IFA)          : FLUX SUR IFA
!     DFKS1(1,IFA)      : DERIVEE DE FKS(IFA) PAR RAPPORT A LA PREMIERE
!                         INCONNUE AU CENTRE
!     DFKS1(JFA+1,IFA)  : DERIVEE DE FKS(IFA) PAR RAPPORT A LA PREMIERE
!                         INCONNUE FACE
!     DFKS2(1,IFA)     : DERIVEE DE FKS(IFA) PAR RAPPORT A LA DEUXIEME
!                        INCONNUE AU CENTRE
!     DFKS2(JFA+1,IFA) : DERIVEE DE FKS(IFA) PAR RAPPORT A LA DEUXIEME
!                        INCONNUE FACE
!     MOBFA            : MOBILITE
!     DMOB1            : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        PREMIERE VARIABLE AU CENTRE
!     DMOB2            : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        SECONDE VARIABLE AU CENTRE
!     MOB1F            : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        PREMIERE VARIABLE A L ARETE
!     MOB2F           : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        SECONDE VARIABLE A L ARETE
! ****IN-OUT :
!     FLUX            : SOMME DES FLUX DE DARCY ENTRANT DASN MAILLE
!     DFLX1           : DERIVEE DU FLUX PAR RAPPORT A LA PREMIERE
!                       VARIABLE
!     DFLX1(1)   = DFLUX/ VARIABLE 1 AU CENTRE MAILLE COURANTE
!     DFLX1(I+1) = DFLUX/ VARIABLE 1 SUR FACE I
!
!     DFLX2          :DERIVEE DU FLUX PAR RAPPORT A LA SECONDE
!                      VARIABLE
!     DFLX2(1)   = DFLUX/ VARIABLE 2 AU CENTRE MAILLE COURANTE
!     DFLX2(I+1) = DFLUX/ VARIABLE 2 SUR FACE I
! ================================================
!     FLUX = SOMME ( MOBFA * F_{K,SIGMA})
!
!================================================
    implicit none
#include "asterf_types.h"
    aster_logical :: cont, tange
    integer :: maxfa
    integer :: nface
    real(kind=8) :: flux
    real(kind=8) :: fks(maxfa)
    real(kind=8) :: mobfa(maxfa), dmob1(maxfa), dmob2(maxfa), mob1f(maxfa)
    real(kind=8) :: mob2f(maxfa)
    real(kind=8) :: dflx1(maxfa+1), dflx2(maxfa+1)
    real(kind=8) :: dfks1(maxfa+1, maxfa), dfks2(maxfa+1, maxfa)
    integer :: ifa, jfa
!
    if (cont) then
        do 2 ifa = 1, nface
            flux = flux +mobfa(ifa) * fks(ifa)
  2     continue
    endif
    if (tange) then
        do 3 jfa = 1, nface
            dflx1(1) = dflx1(1) + dmob1(jfa) * fks(jfa) + mobfa(jfa) * dfks1(1,jfa)
            dflx2(1) = dflx2(1) + dmob2(jfa) * fks(jfa) + mobfa(jfa) * dfks2(1,jfa)
  3     continue
        do 4 ifa = 1, nface
            dflx1(1+ifa) = dflx1(1+ifa) + mob1f(ifa)* fks(ifa)
            dflx2(1+ifa) = dflx2(1+ifa) + mob2f(ifa)* fks(ifa)
            do 4 jfa = 1, nface
                dflx1(1+ifa) = dflx1(1+ifa) + mobfa(jfa) * dfks1(ifa+ 1,jfa)
                dflx2(1+ifa) = dflx2(1+ifa) + mobfa(jfa) * dfks2(ifa+ 1,jfa)
  4         continue
    endif
end subroutine
