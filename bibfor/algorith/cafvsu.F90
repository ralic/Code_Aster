subroutine cafvsu(cont, tange, maxfa, nface, fks,&
                  dfks1, dfks2, mobfa, dmob1, dmob2,&
                  dmob1v, dmob2v, flux, dflx1, dflx2,&
                  dflx1v, dflx2v, nbvois, nvoima)
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
!============================================================
!  CETTE SUBROUTINE PERMET DE CALCULER D UNE MANIERE
!  GENERIQUE LE FLUX TOTAL VOLUMIQUE
!============================================================
! ****IN :
!     FKS(IFA)          : FLUX SUR IFA
!     DFKS1(1,IFA)      : DERIVEE DE FKS(IFA) PAR RAPPORT A LA PREMIERE
!                        INCONNUE AU CENTRE
!     DFKS1(JFA+1,IFA)  : DERIVEE DE FKS(IFA) PAR RAPPORT A LA PREMIERE
!                        INCONNUE FACE
!     DFKS2(1,IFA)     : DERIVEE DE FKS(IFA) PAR RAPPORT A LA DEUXIEME
!                        INCONNUE AU CENTRE
!     DFKS2(JFA+1,IFA) : DERIVEE DE FKS(IFA) PAR RAPPORT A LA DEUXIEME
!                        INCONNUE FACE
!     MOBFA            : MOBILITE
!     DMOB1            : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        PREMIERE VARIABLE
!     DMOB2            : DERIVEE DE LA MOBILITE PAR RAPPORT A LA
!                        SECONDE VARIABLE
!
! ****IN-OUT :
!     FLUX            : SOMME DES FLUX DE DARCY ENTRANT DASN MAILLE
!     DFLX1           : DERIVEE DU FLUX PAR RAPPORT A LA PREMIERE
!                       VARIABLE
!            DFLX1(1)   = DFLUX/ VARIABLE 1 AU CENTRE MAILLE COURANTE
!            DFLX1(I+1) = DFLUX/ VARIABLE 1 SUR FACE I
!     DFLX2          :DERIVEE DU FLUX PAR RAPPORT A LA SECONDE
!                      VARIABLE
!            DFLX2(1)   = DFLUX/ VARIABLE 2 AU CENTRE MAILLE COURANTE
!            DFLX2(I+1) = DFLUX/ VARIABLE 2 SUR FACE I
!     DFLX1V(I) = DFLUX / VARIABLE 1 AU CENTRE DU VOISIN DE LA FACE I
!     DFLX2V(I) = DFLUX / VARIABLE 2 AU CENTRE DU VOISIN DE LA FACE I
! ================================================
!     FLUX = SOMME ( MOBFA * F_{K,SIGMA})
!     SI PAS DE DECENTRAGE
!        DFLUX/DPK=DMOBFA/DPK* F_{K,SIGMA} +MOBFA* DF_{K,SIGMA}/DPK
!        DFLUX/DP_{K,SIGMA}=MOBFA* DF_{K,SIGMA}/DP_{K,SIGMA}
!     SI DECENTRAGE
!        DFLUX/DPL=DMOBFA/DPL* F_{K,SIGMA}
!        DFLUX/DP_{L,SIGMA}=0
!================================================
!     REMARQUE :
!        POUR CALCULER LE FLUX ON BOUCLE SUR LES IVOIS CAR ON A PEUT
!        ETRE DECENTRE LA MOBILITÉ ALORS QUE POUR CALCULER DFLX1 ET
!        DFLX2 ON REGARDE UNIQUEMENT LA DERIVEE DE LA MOBILITE SUR
!        L ELEMENT COURANT(0) CAR SI IL Y A DU DECENTRAGE ON VA STOCKER
!        DANS DFLX1V ET DFLX2
    implicit none
    logical :: cont, tange
    integer :: maxfa
    integer :: nface
    integer :: nbvois, nvoima
    real(kind=8) :: flux
    real(kind=8) :: fks(1:nface)
    real(kind=8) :: mobfa(0:nvoima, 1:nface)
    real(kind=8) :: dmob1(0:nvoima, 1:nface), dmob2(0:nvoima, 1:nface)
    real(kind=8) :: dmob1v(0:nvoima, 1:nface), dmob2v(0:nvoima, 1:nface)
    real(kind=8) :: dflx1(1:nface+1), dflx2(1:nface+1)
    real(kind=8) :: dflx1v(1:nface), dflx2v(1:nface)
    real(kind=8) :: dfks1(1:maxfa+1, nface), dfks2(1:maxfa+1, nface)
    integer :: ifa, jfa, ivois
    if (cont) then
        do 1 ivois = 0, nbvois
            do 2 ifa = 1, nface
                flux = flux + mobfa(ivois,ifa) * fks(ifa)
 2          continue
 1      continue
    endif
    if (tange) then
        do 3 jfa = 1, nface
            dflx1(1) = dflx1(1) + dmob1(0,jfa) * fks(jfa)
            dflx2(1) = dflx2(1) + dmob2(0,jfa) * fks(jfa)
            do 31 ivois = 0, nbvois
                dflx1(1) = dflx1(1) + mobfa(ivois,jfa) * dfks1(1,jfa)
                dflx2(1) = dflx2(1) + mobfa(ivois,jfa) * dfks2(1,jfa)
31          continue
 3      continue
        do 4 ifa = 1, nface
            do 4 jfa = 1, nface
                do 41 ivois = 0, nbvois
                    dflx1(1+ifa) = dflx1(1+ifa) + mobfa(ivois,jfa) * dfks1(ifa+1,jfa)
                    dflx2(1+ifa) = dflx2(1+ifa) + mobfa(ivois,jfa) * dfks2(ifa+1,jfa)
41              continue
 4          continue
        do 5 ivois = 1, nbvois
            do 5 jfa = 1, nface
                dflx1v(jfa) = dflx1v(jfa) + dmob1v(ivois,jfa) * fks( jfa)
                dflx2v(jfa) = dflx2v(jfa) + dmob2v(ivois,jfa) * fks( jfa)
 5          continue
    endif
end subroutine
