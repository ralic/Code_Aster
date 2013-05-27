subroutine utin3d(igeom, nsomm, ino, ityp, inst,&
                  insold, k8cart, ltheta, niv, ifm,&
                  option, valfp, valfm, noe)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: olivier.boiteau at edf.fr
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  UTILITAIRE D'INTERPOLATION 3D DE CHARGE
!                          POUR AERER TE0003
!
! IN IGEOM : ADRESSE JEVEUX DE LA GEOMETRIE
! IN NSOMM    : NOMBRE DE SOMMETS DE LA FACE
! IN INO      : NUMERO DE FACE
! IN ITYP     : TYPE DE FACE
! IN INST/INSOLD : INSTANT + / INSTANT -
! IN K8CART   : CHAMP JEVEUX A INTERPOLER
! IN LTHETA   : LOGICAL = TRUE SI THETA DIFFERENT DE 1
! IN IFM/NIV/OPTION  : PARAMETRES D'AFFICHAGE
! IN NOE : TABLEAU NUMEROS NOEUDS PAR FACE ET PAR TYPE D'ELEMENT 3D
! OUT VALFP/M : VALEUR DU CHAMP RESULTAT AU INSTANTS +/- ET
!               AUX POINTS CI-DESSUS
!   -------------------------------------------------------------------
!     SUBROUTINES APPELLEES:
!       FOINTE.
!
!     FONCTIONS INTRINSEQUES:
!       AUCUNE.
!   -------------------------------------------------------------------
!     ASTER INFORMATIONS:
!       24/09/01 (OB): CREATION POUR SIMPLIFIER TE0003.F.
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
    include 'asterfort/fointe.h'
    integer :: igeom, nsomm, ino, ityp, ifm, niv, option, noe(9, 6, 3)
    real(kind=8) :: inst, insold, valfp(9), valfm(9)
    character(len=8) :: k8cart
    logical :: ltheta
!
!
! DECLARATION VARIABLES LOCALES
    integer :: icode, in, iino, iaux, jaux, kaux
    real(kind=8) :: valpar(4)
    character(len=8) :: nompar(4)
!
! INIT
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
    nompar(4) = 'INST'
!
! BOUCLE SUR LES SOMMETS DE LA FACE
    do 100 in = 1, nsomm
!
! NUMEROTATION LOCALE DU NOEUD A INTERPOLER
        iino = noe(in,ino,ityp)
        iaux = igeom + 3*(iino-1)
        jaux = iaux + 1
        kaux = jaux + 1
!
! INTERPOLATION CHAMP K8CART A L'INSTANT +
        valpar(1) = zr(iaux)
        valpar(2) = zr(jaux)
        valpar(3) = zr(kaux)
        valpar(4) = inst
        call fointe('FM', k8cart, 4, nompar, valpar,&
                    valfp(in), icode)
        if (ltheta) then
! INTERPOLATION CHAMP K8CART A L'INSTANT -
            valpar(4) = insold
            call fointe('FM', k8cart, 4, nompar, valpar,&
                        valfm(in), icode)
        endif
!
! AFFICHAGES
        if (niv .eq. 2) then
            write(ifm,*)' X/Y/Z ',valpar(1),valpar(2),valpar(3)
            if (option .eq. 1) then
                write(ifm,*)' VALFP ',valfp(in)
            else if (option.eq.2) then
                write(ifm,*)' VALHP ',valfp(in)
            else
                write(ifm,*)' VALTP ',valfp(in)
            endif
            if (ltheta) write(ifm,*)'     M ',valfm(in)
        endif
!
100  end do
!
end subroutine
