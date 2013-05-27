subroutine mefgri(ntypg, nbgtot, zg, hg, itypg,&
                  zmin, zmax)
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
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
!     APPELANT : FLUST3
!     VERIFICATION DE LA REPARTITION GEOMETRIQUE DES GRILLES
!-----------------------------------------------------------------------
!  IN   : NTYPG  : NOMBRE DE TYPES DE GRILLES
!  IN   : NBGTOT : NOMBRE TOTAL DE GRILLES
!  IN   : ZG     : COORDONNEES 'Z' DES POSITIONS DES GRILLES DANS LE
!                  REPERE AXIAL
!  IN   : HG     : VECTEUR DES HAUTEURS DE GRILLE
!  IN   : ITYPG  : VECTEUR DES TYPES DE GRILLES
!  IN   : ZMIN   : COTE MIN DU FAISCEAU DE TUBES
!  IN   : ZMAX   : COTE MAX DU FAISCEAU DE TUBES
!-----------------------------------------------------------------------
    include 'asterfort/u2mesk.h'
    integer :: ntypg, nbgtot, itypg(nbgtot)
    real(kind=8) :: zg(nbgtot), hg(ntypg), zmin, zmax
!
    character(len=3) :: k3ig, k3jg
    character(len=24) :: valk(2)
    logical :: intnul
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ig, jg
    real(kind=8) :: z1, z1ig, z1jg, z2, z2ig, z2jg
!-----------------------------------------------------------------------
    do 10 ig = 1, nbgtot
        z1 = zg(ig) - hg(itypg(ig))/2.0d0
        z2 = zg(ig) + hg(itypg(ig))/2.0d0
        if ((z1.lt.zmin) .or. (z2.gt.zmax)) then
            write(k3ig,'(I3.3)') ig
            call u2mesk('F', 'ALGELINE_83', 1, k3ig)
        endif
10  end do
!
    if (nbgtot .gt. 1) then
        do 20 ig = 1, nbgtot-1
            z1ig = zg(ig) - hg(itypg(ig))/2.0d0
            z2ig = zg(ig) + hg(itypg(ig))/2.0d0
            do 21 jg = ig+1, nbgtot
                z1jg = zg(jg) - hg(itypg(jg))/2.0d0
                z2jg = zg(jg) + hg(itypg(jg))/2.0d0
                intnul = ((z2ig.lt.z1jg).or.(z2jg.lt.z1ig))
                if (.not.intnul) then
                    write(k3ig,'(I3.3)') ig
                    write(k3jg,'(I3.3)') jg
                    valk(1) = k3ig
                    valk(2) = k3jg
                    call u2mesk('F', 'ALGELINE_84', 2, valk)
                endif
21          continue
20      continue
    endif
!
! --- FIN DE MEFGRI.
end subroutine
