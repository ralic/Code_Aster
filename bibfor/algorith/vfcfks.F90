subroutine vfcfks(cont, tange, maxfa, nface, uk,&
                  dukp1, dukp2, ufa, dufa1, dufa2,&
                  c, pesa, rho, drho1, drho2,&
                  xk, xfa, maxdim, ndim, fks,&
                  dfks1, dfks2)
!
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
! TOLE CRP_21 CRS_1404
!
!
!     CETTE SUBROUTINE PERMET DE CALCULER LES FLUX SURFACIQUES
!     (I-E F_K,SIGMA L' APPROXIMATION DU FLUX )
! IN
!     UK              : UNE VALEUR AU CENTRE
!     DUKP1           : DERIVEE DE UK PAR RAP A LA PREMIERE INCONNUE
!                       AU CENTRE
!     DUKP2           : DERIVEE DE UK PAR RAP A LA DEUXIEME INCONNUE
!                       AU CENTRE
!
!     UFA(IFA)        : UNE VALEUR SUR FACE IFA
!     DUFA1(IFA)      : DERIVEE DE U_FA(IFA) PAR RAP A LA PREMIERE
!                      INCONNUE SUR IFA
!     DUFA2S(IFA)     : DERIVEE DE U_FA(IFA) PAR RAP A LA DEUXIEME
!                       INCONNUE SUR IFA
!
! OUT
!     FKS(IFA)        : FLUX SURFACIQUE SUR IFA
!     DFKS1(1,IFA)    : DERIVEE DE FKS(IFA) PAR RAP A LA PREMIERE
!                       INCONNUE AU CENTRE
!     DFKS1(JFA+1,IFA): DERIVEE DE FKS(IFA) PAR RAP A LA PREMIERE
!                      INCONNUE FACE
!     DFKS2(1,IFA)    : DERIVEE DE FKS(IFA) PAR RAP A LA DEUXIEME
!                       INCONNUE AU CENTRE
!     DFKS2(JFA+1,IFA): DERIVEE DE FKS(IFA) PAR RAP A LA DEUXIEME
!                      INCONNUE FACE
    implicit none
!
    logical :: cont, tange
    integer :: maxfa, maxdim
    integer :: nface, ndim
    real(kind=8) :: fks(nface)
    real(kind=8) :: uk, dukp1, dukp2
    real(kind=8) :: dufa1(1:nface), dufa2(1:nface), c(1:maxfa, 1:nface)
    real(kind=8) :: ufa(1:nface)
    real(kind=8) :: dfks1(1+maxfa, nface), dfks2(1+maxfa, nface)
    real(kind=8) :: pesa(ndim), rho, drho1, drho2
    real(kind=8) :: xk(ndim), xfa(1:maxdim, 1:nface)
    real(kind=8) :: gravi(nface)
    integer :: ifa, jfa, kfa, idim
!
    do 4 ifa = 1, nface
        do 41 jfa = 1, nface
            gravi(jfa)=0.d0
            if (cont) then
                do 42 idim = 1, ndim
                    gravi(jfa)=gravi(jfa)+pesa(idim)* (xk(idim)-xfa(&
                    idim,jfa))
42              continue
                fks(ifa) = fks(ifa) + c(ifa,jfa)* (uk-ufa(jfa)-rho* gravi(jfa) )
            endif
            if (tange) then
                kfa=jfa+1
                dfks1(1,ifa)= dfks1(1,ifa) + c(ifa,jfa)*(dukp1&
                -drho1*gravi(jfa))
                dfks1(kfa,ifa)=dfks1(kfa,ifa)-c(ifa,jfa)*dufa1(jfa)
                dfks2(1,ifa)= dfks2(1,ifa) + c(ifa,jfa) *(dukp2&
                -drho2*gravi(jfa))
                dfks2(kfa,ifa)=dfks2(kfa,ifa)-c(ifa,jfa)*dufa2(jfa)
            endif
41      continue
 4  end do
end subroutine
