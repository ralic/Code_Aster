subroutine foec2f(iuni, v, nbcoup, n1, n2,&
                  nompar, nomres)
    implicit none
    integer :: iuni, nbcoup, n1, n2
    real(kind=8) :: v(2*nbcoup)
    character(len=*) :: nompar, nomres
!     ------------------------------------------------------------------
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
!     ECRITURE DES COUPLES (PARAMETRE, RESULTAT) D'UNE FONCTION,
!     DU N1-IEME AU N2-IEME
!     ------------------------------------------------------------------
!     ARGUMENTS D'ENTREE:
!        IUNI  : NUMERO D'UNITE LOGIQUE D'ECRITURE
!        VEC   : VECTEUR DES VALEURS (PARAMETRES ET RESULTATS)
!        NBCOUP: NOMBRE DE COUPLES DE VALEURS
!        N1, N2: NUMEROS DE DEBUT ET FIN DE LA LISTE
!        NOMPAR: NOM DU PARAMETRE
!        NOMRES: NOM DU RESULTAT
!     ------------------------------------------------------------------
    character(len=8) :: gva, gfo
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, j
!-----------------------------------------------------------------------
    n1=min(n1,nbcoup)
    n2=min(n2,nbcoup)
!
    gva = nompar
    gfo = nomres
    write(iuni, 100 )&
     &    ( ('<-PARAMETRE-><-RESULTAT->  ')  , j=1,3  ) ,&
     &    ( ('  '//gva//'     '//gfo//'    '), i=1,3  )
    write(iuni,101) (v(i),v(nbcoup+i),i=n1,n2 )
!
    100 format(/,1x,3a,/,1x,3a )
    101 format( 3(1x,1pd12.5,1x,1pd12.5,1x) )
!
end subroutine
