subroutine cafmes(ifa, cont, tange, maxfa, nface,&
                  fkss, dfks1, dfks2, mobfas, dmob1,&
                  dmob2, dmob1f, dmob2f, fmw, fm1w,&
                  fm2w)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!=======================================================================
!  CETTE SUBROUTINE PERMET DE CALCULER D UNE MANIERE GENERIQUE LE FLUX
!  MASSIQUE
!=======================================================================
! ****IN :
!     IFA              : ARETE EXTERNE QUE L ON CONSIDERE DANS ASSESU
!     FKSS             : FLUX SUR L ARETE EXTERNE
!                             QUE L ON CONSIDERE DS ASSESU
!     DFKS1(1,IFA)     : DERIVEE DE FKS(IFA) % A LA PREMIERE
!                                              INCONNUE AU CENTRE
!     DFKS1(JFA+1,IFA) : DERIVEE DE FKS(IFA) % A LA PREMIERE
!                                              INCONNUE FACE
!     DFKS2(1,IFA)     : DERIVEE DE FKS(IFA) % A LA DEUXIEME
!                                              INCONNUE AU CENTRE
!     DFKS2(JFA+1,IFA) : DERIVEE DE FKS(IFA) % A LA DEUXIEME
!                                              INCONNUE FACE
!     MOBFAS           : MOBILITE SUR L ARETE EXTERNE QUE L ON
!                                              CONSIDERE DS ASSESU
!     DMOB1            : DERIVEE DE MOBFAS % A LA PREMIERE
!                                              VARIABLE AU CENTRE
!     DMOB2            : DERIVEE DE MOBFAS % A LA SECONDE
!                                              VARIABLE AU CENTRE
!     DMOB1F           : DERIVEE DE MOBFAS % A LA PREMIERE
!                                              VARIABLE A L ARETE
!     DMOB2F           : DERIVEE DE MOBFAS % A LA SECONDE
!                                              VARIABLE A L ARETE
!
! ****IN-OUT :
!     FMW              : FLUX MASSIQUE
!     FM1W             : DERIVEE DU FLUX % A LA PREMIERE VARIABLE
!     FM2W             : DERIVEE DU FLUX % A LA SECONDE VARIABLE
! ================================================
!     FMW =  MOB * F_{K,SIGMA}
!================================================
    implicit none
    logical :: cont, tange
    integer :: maxfa
    integer :: nface
    real(kind=8) :: fmw(1:maxfa)
    real(kind=8) :: fkss
    real(kind=8) :: mobfas
    real(kind=8) :: dmob1(1:maxfa), dmob2(1:maxfa), dmob1f(1:maxfa)
    real(kind=8) :: dmob2f(1:maxfa)
    real(kind=8) :: fm1w(1+maxfa, maxfa), fm2w(1+maxfa, maxfa)
    real(kind=8) :: dfks1(1+maxfa, maxfa), dfks2(1+maxfa, maxfa)
    integer :: ifa, jfa
!
    if (cont) then
        fmw(ifa) = fmw(ifa)+ mobfas * fkss
    endif
    if (tange) then
        fm1w(1,ifa) = fm1w(1,ifa) + dmob1(ifa) * fkss + mobfas * dfks1(1,ifa)
!
        fm2w(1,ifa) = fm2w(1,ifa) + dmob2(ifa) * fkss + mobfas * dfks2(1,ifa)
        do 4 jfa = 2, nface+1
            fm1w(jfa,ifa)= fm1w(jfa,ifa) + mobfas * dfks1(jfa,ifa)
            fm2w(jfa,ifa)= fm2w(jfa,ifa) + mobfas * dfks2(jfa,ifa)
 4      continue
        fm1w(1+ifa,ifa)= fm1w(1+ifa,ifa) + dmob1f(ifa) * fkss
        fm2w(1+ifa,ifa)= fm2w(1+ifa,ifa) + dmob2f(ifa) * fkss
    endif
end subroutine
