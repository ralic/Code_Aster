subroutine xnorme(indipt, iptbor, vectn, nbfacb, nunoa,&
                  nunob, nunoc, jcoor, coorg)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/provec.h'
    include 'blas/ddot.h'
    integer :: iptbor(2), nbfacb, jcoor
    integer :: nunoa, nunob, nunoc, indipt
    real(kind=8) :: vectn(12), coorg(3)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!             CALCUL ET STOCKAGE DE LA NORMALE D'UNE FACE
!
!     ENTREE
!       INDIPT   : INDICE DU POINT DE BORD
!       VECTN    : VECTEUR CONTENANT LES VECTEURS NORMAUX DES FACES DE
!                  BORD DE LA MAILLE
!       NBFACB   : NOMBRE DE FACES DE BORD DANS LA MAILLE
!       NUNOA    : NOEUD DE LA FACE COURANTE
!       NUNOB    : NOEUD DE LA FACE COURANTE
!       NUNOC    : NOEUD DE LA FACE COURANTE
!       JCOOR    : ADRESSE DES COORDONNEES DES NOEUDS DU MAILLAGE
!       COORG    : COORDONNEES DU CENTRE DE GRAVITE DE LA MAILLE
!
!     SORTIE
!       IPTBOR   : VECTEUR CONTENANT LES INDICES DES POINTS DE BORD DE
!                  LA MAILLE
!     ------------------------------------------------------------------
!
    integer :: k
    real(kind=8) :: ab(3), ac(3), ag(3), normal(3), proj
! ----------------------------------------------------------------------
    call jemarq()
!
!     CALCUL DE LA NORMALE
    do 240 k = 1, 3
!       A,B ET C SONT DES NOEUDS DE LA FACE
        ab(k)=zr(jcoor-1+3*(nunob-1)+k)-zr(jcoor-1+3*(nunoa-1)+k)
        ac(k)=zr(jcoor-1+3*(nunoc-1)+k)-zr(jcoor-1+3*(nunoa-1)+k)
!       G EST LE CENTRE DE GRAVITE DE LA MAILLE
        ag(k)=coorg(k)-zr(jcoor-1+3*(nunoa-1)+k)
240  end do
!
    call provec(ab, ac, normal)
!
!     ORIENTATION DE LA NORMALE VERS L'EXTERIEUR
    proj = ddot(3,normal,1,ag,1)
!
    if (proj .gt. 0) then
        normal(1) = -normal(1)
        normal(2) = -normal(2)
        normal(3) = -normal(3)
    endif
!
!     NOMBRE DE FACES DE BORD DANS LA MAILLE
    nbfacb=nbfacb+1
!
!     STOCKAGE DE LA NORMALE
    vectn(1+3*(nbfacb-1)) = normal(1)
    vectn(2+3*(nbfacb-1)) = normal(2)
    vectn(3+3*(nbfacb-1)) = normal(3)
!
!     IPTBOR(2) SERT SI LA MAILLE POSSEDE 2 POINTS
!     DE FOND QUI SONT SUR UNE FACE DE BORD
!     (CAS TRES SPECIAL OU LE FOND DE FISSURE N'EST QUE DANS UNE SEULE
!      MAILLE)
    if (iptbor(1) .eq. 0) then
        iptbor(1) = indipt
    else
        iptbor(2) = indipt
    endif
!
    call jedema()
end subroutine
