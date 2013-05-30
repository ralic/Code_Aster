subroutine fonfis(noma, nbnoff, fonoeu, fondfi)
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/wkvect.h'
    integer :: nbnoff
    character(len=8) :: noma
    character(len=24) :: fondfi, fonoeu
!
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
!     ----------------------------------------------------------------
! FONCTION REALISEE:
!
!     POUR CHAQUE NOEUD DU FOND DE FISSURE ON RECUPERE SES
!     COORDONNEES ET ON CALCULE SON ABSCISSE CURVILIGNE
!
!     ------------------------------------------------------------------
! ENTREE:
!        NOMA   : NOM DU MAILLAGE
!        NBNOFF : NOMBRE DE NOEUDS AU FOND DE FISSURE
!        FONOEU : NOMS DES NOEUDS DU FOND DE FISSURE
!
! SORTIE:
!        FONDFI : VECTEUR .FONDFISS CONTENANT LES COORDONNEES ET LES
!                 ABSCISSES CURVILIGNES DES NOEUDS DU FOND
!     ------------------------------------------------------------------
!
!
    integer :: i, ifon, jcoor, jnoe, ni, nj
    real(kind=8) :: absci, coori(3), coorj(3), norm, xij, yij, zij
!
!
!
    call jemarq()
!
!     ADRESSES DES COORDONNEES DES NOEUDS DU MAILLAGE
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
!
!     ALLOCATION DU VECTEUR DES COORDONNEES ET DES ABSCISSES CURVILIGNES
!     DES NOEUDS DU FOND
    call wkvect(fondfi, 'G V R', 4*nbnoff, ifon)
!
!     RECUPERATION DES NOMS DES NOEUDS DU FOND DE FISSURE
    call jeveuo(fonoeu, 'L', jnoe)
!
!     RECUPERATION DES COORDONNNES DE NI
    call jenonu(jexnom(noma//'.NOMNOE', zk8(jnoe)), ni)
    coori(1) = zr(jcoor-1 + (ni-1)*3 + 1)
    coori(2) = zr(jcoor-1 + (ni-1)*3 + 2)
    coori(3) = zr(jcoor-1 + (ni-1)*3 + 3)
!
!     REMPLISSAGE DE .FONDFISS DANS LA SD_FOND_FISS :
!     DONNEES DU CAS 2D OU DU PREMIER NOEUD POUR LE CAS 3D
    zr(ifon-1 + 4*(1-1) + 1) = coori(1)
    zr(ifon-1 + 4*(1-1) + 2) = coori(2)
    zr(ifon-1 + 4*(1-1) + 3) = coori(3)
    zr(ifon-1 + 4*(1-1) + 4) = 0.d0
!
!     REMPLISSAGE DE .FONDFISS DANS LA SD_FOND_FISS: CAS 3D
    if (nbnoff .ne. 1) then
        do 10 i = 2, nbnoff
!
!         NUMEROS (ABSOLUS) DES NOEUDS DU FOND: NI ET NJ
            call jenonu(jexnom(noma//'.NOMNOE', zk8(jnoe-1+i-1)), ni)
            call jenonu(jexnom(noma//'.NOMNOE', zk8(jnoe-1+i)), nj)
!
!         COORDONNEES DES NOEUDS I ET J
            coori(1) = zr(jcoor-1 + (ni-1)*3 + 1)
            coori(2) = zr(jcoor-1 + (ni-1)*3 + 2)
            coori(3) = zr(jcoor-1 + (ni-1)*3 + 3)
!
            coorj(1) = zr(jcoor-1 + (nj-1)*3 + 1)
            coorj(2) = zr(jcoor-1 + (nj-1)*3 + 2)
            coorj(3) = zr(jcoor-1 + (nj-1)*3 + 3)
!
!         CALCUL DES ABSCISSES CURVILIGNES
            zr(ifon-1 + 4*(i-1) + 1) = coorj(1)
            zr(ifon-1 + 4*(i-1) + 2) = coorj(2)
            zr(ifon-1 + 4*(i-1) + 3) = coorj(3)
!
            xij = coorj(1) - coori(1)
            yij = coorj(2) - coori(2)
            zij = coorj(3) - coori(3)
            norm = sqrt(xij*xij + yij*yij + zij*zij)
            absci = zr(ifon-1 + 4*(i-2) + 4)
!
            zr(ifon-1 + 4*(i-1) + 4) = absci + norm
10      continue
    endif
!
    call jedema()
end subroutine
