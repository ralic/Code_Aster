subroutine cafmsu(ifa, cont, tange, maxfa, nface,&
                  fkss, dfks1, dfks2, mobfas, dmob1s,&
                  dmob2s, fmw, fm1w, fm2w)
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
!  CETTE SUBROUTINE PERMET DE CALCULER D UNE MANIERE GENERIQUE LE FLUX
!  MASSIQUE QUI INTERVIENR DANS L EQUATION DE CONTINUITE POUR UNE ARETE
!  EXTERNE
!
!=======================================================================
! ****IN :
!     IFA              : ARETE EXTERNE QUE L ON CONSIDERE DANS ASSVSU
!     FKSS             : FLUX SUR L ARETE EXTERNE QUE
!                        L ON CONSIDERE DANS ASSVSU
!     DFKS1(1,IFA)     : DERIVEE DE FKS(IFA) PAR RAP A
!                        LA PREMIERE INCONNUE AU CENTRE
!     DFKS1(JFA+1,IFA) : DERIVEE DE FKS(IFA) PAR RAP A
!                         LA PREMIERE INCONNUE FACE
!     DFKS2(1,IFA)     : DERIVEE DE FKS(IFA) PAR RAP A
!                         LA DEUXIEME INCONNUE AU CENTRE
!     DFKS2(JFA+1,IFA) : DERIVEE DE FKS(IFA) PAR RAP A
!                         LA DEUXIEME INCONNUE FACE
!     MOBFAS           : MOBILITE SUR  L ARETE EXTERNE
!                        QUE L ON CONSIDERE DANS ASSVSU
!     DMOB1S           : DERIVEE DE MOBFAS PAR RAPPORT A
!                         LA PREMIERE VARIABLE
!     DMOB2S           : DERIVEE DE MOBFAS PAR RAPPORT A
!                         LA SECONDE VARIABLE
! ****IN-OUT :
!     FMW              : FLUX MASSIQUE
!     FM1W             : DERIVEE DU FLUX PAR RAPPORT
!                        A LA PREMIERE VARIABLE
!     FM2W             : DERIVEE DU FLUX PAR RAPPORT
!                        A LA SECONDE VARIABLE
! ================================================
!     FMW =  MOB * F_{K,SIGMA}
!================================================
    implicit none
#include "asterf_types.h"
    aster_logical :: cont, tange
    integer :: maxfa
    integer :: nface
    real(kind=8) :: fmw(nface)
    real(kind=8) :: fkss
    real(kind=8) :: mobfas
    real(kind=8) :: dmob1s, dmob2s
    real(kind=8) :: fm1w(1+maxfa, nface), fm2w(1+maxfa, nface)
    real(kind=8) :: dfks1(1+maxfa, nface), dfks2(1+maxfa, nface)
    integer :: ifa, jfa
    if (cont) then
        fmw(ifa) = mobfas * fkss
    endif
    if (tange) then
        fm1w(1,ifa) = dmob1s * fkss + mobfas * dfks1(1,ifa)
        fm2w(1,ifa) = dmob2s * fkss + mobfas * dfks2(1,ifa)
        do 4 jfa = 2, nface+1
            fm1w(jfa,ifa) = fm1w(jfa,ifa) + mobfas * dfks1(jfa,ifa)
            fm2w(jfa,ifa) = fm2w(jfa,ifa) + mobfas * dfks2(jfa,ifa)
  4     continue
    endif
end subroutine
