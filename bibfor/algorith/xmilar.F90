subroutine xmilar(ndim, pinter, tabar, areint, milara,&
                  milarb)
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/abscvf.h'
    include 'asterfort/abscvl.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/reerel.h'
    include 'asterfort/xinvac.h'
    integer :: ndim, areint
    real(kind=8) :: milara(ndim), milarb(ndim), pinter(*), tabar(*)
!
! TOLE CRS_1404
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
!                      TROUVER LES PTS MILIEUX ENTRE LES EXTREMITES DE
!                      L'ARETE ET LE POINT D'INTERSECTION
!
!     ENTREE
!       NDIM    : DIMENSION TOPOLOGIQUE DU MAILLAGE
!       PINTER  : COORDONNÉES DES POINTS D'INTERSECTION
!       TABAR   : COORDONNEES DES 3 NOEUDS DE L'ARETE
!       AREINT  : POSITION DU PT INTER DE L'ARETE DANS LA LISTE
!
!     SORTIE
!       MILARA  : COOR DU PT MILIEU ENTRE 1ER PT DE COORSG ET PT INTER
!       MILARB  : COOR DU PT MILIEU ENTRE 2EM PT DE COORSG ET PT INTER
!     ----------------------------------------------------------------
!
    real(kind=8) :: c(ndim)
    real(kind=8) :: s, sc, s1
    real(kind=8) :: ksi1, ksia(ndim), ksib(ndim)
    integer :: k, j, nno
    character(len=8) :: elp
    parameter     (elp='SE3', nno=3)
!
! --------------------------------------------------------------------
    call jemarq()
!
!     RECUPERATION DES COORDONNES DU PT INTER
    do 100 j = 1, 3
        if (areint .eq. j) then
            do 200 k = 1, ndim
                c(k)=pinter((j-1)*ndim+k)
200          continue
        endif
100  end do
!
!     TABAR : KSI2=-1  /  KSI1= 1  /  KSI3= 0
!     KSI2 ETANT LE POINT D'ORIGINE
!
! --- ABSCISSE CURVILIGNE DU PT INTER SUR L'ARETE
    call abscvl(ndim, tabar, c, sc)
!
! --- COORDONNES DU POINT MILARA DANS L'ELEMENT DE REFERENCE
!     TEL QUE ABSCISSE CURVILIGNE(D1)=ABSCISSE CURVILIGNE(I)/2
    s=sc/2
    call xinvac(elp, ndim, tabar, s, ksia)
    call assert(ksia(1).ge.-1 .and. ksia(1).le.1)
!
! --- COORDONNES DU POINT MILARA DANS L'ELEMENT REEL
    call reerel(elp, nno, ndim, tabar, ksia,&
                milara)
!
! --- ABSCISSE CURVILIGNE DE KSI1
    ksi1=1
    call abscvf(ndim, tabar, ksi1, s1)
!
! --- COORDONNES DU POINT MILARB DANS L'ELEMENT DE REFERENCE
!     TEL QUE ABSCURV(D2)=[ABSCURV(A)+ABSCURV(I)]/2
    s=(s1+sc)/2
    call xinvac(elp, ndim, tabar, s, ksib)
    call assert(ksib(1).ge.-1 .and. ksib(1).le.1)
!
! --- COORDONNES DU POINT MILARB DANS L'ELEMENT REEL
    call reerel(elp, nno, ndim, tabar, ksib,&
                milarb)
!
    call jedema()
end subroutine
