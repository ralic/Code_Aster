subroutine oriema(nomail, tpmail, nbnmai, lnmail, typ3d,&
                  lnm3d, ndim, coor, reorie, norien,&
                  ifm, niv)
    implicit   none
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/vdiff.h'
    include 'blas/ddot.h'
    integer :: nbnmai, lnmail(*), lnm3d(*), ndim, norien, ifm, niv
    real(kind=8) :: coor(*)
    logical :: reorie
    character(len=8) :: nomail, tpmail, typ3d
    character(len=24) :: valk(2)
!.======================================================================
! ======================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ORIEMA  --  ORIENTATION DE LA MAILLE DE NOM NOMAIL
!             DE TELLE MANIERE A CE QUE LA NORMALE A CETTE MAILLE
!             SOIT EXTERIEURE AU VOLUME.
!             CETTE MAILLE EST UNE MAILLE DE PEAU :
!               .EN 2D C'EST UNE MAILLE DE TYPE SEG2 OU SEG3
!               .EN 3D C'EST UNE MAILLE DE TYPE TRIA3, TRIA6,
!                                               QUAD4, QUAD8 OU QUAD9
!
! IN  : REORIE : = .FALSE.  ON VERIFIE L'ORIENTATION
!                = .TRUE.   ON REORIENTE
! OUT : NORIEN : = 0  LA MAILLE EST BIEN ORIENTEE
!                = 1  LA MAILLE EST A REORIENTER
!
!.========================= DEBUT DES DECLARATIONS ====================
!
    integer :: nbnomx
    parameter    (nbnomx = 27)
    integer :: i, ic, n1, n2, n3, nbnsm, nbns3d, ino, nbnoe
    integer :: lisnoe(nbnomx)
    real(kind=8) :: coon1(3), coon2(3), coon3(3), n1n2(3), n1n3(3)
    real(kind=8) :: n(3), norme, n1g(3), xg3d(3), xgm(3), xgn, zero
!
! ========================= DEBUT DU CODE EXECUTABLE ==================
!
    norien = 0
    zero = 0.d0
!
    do 10 i = 1, nbnmai
        lisnoe(i) = lnmail(i)
10  end do
    do 12 i = 1, 3
        n(i) = zero
        n1n2(i) = zero
        n1n3(i) = zero
        xgm(i) = zero
        xg3d(i) = zero
12  end do
!
! --- NUMERO DES 2 (3 EN 3D) PREMIERS NOEUDS DE LA MAILLE :
!     ---------------------------------------------------
    n1 = lisnoe(1)
    n2 = lisnoe(2)
    if (ndim .eq. 3) n3 = lisnoe(3)
!
    do 20 ic = 1, 3
        coon1(ic) = coor(3*(n1-1)+ic)
        coon2(ic) = coor(3*(n2-1)+ic)
20  end do
    call vdiff(3, coon2, coon1, n1n2)
!
    if (ndim .eq. 2) then
        n(1) = n1n2(2)
        n(2) = -n1n2(1)
    else if (ndim.eq.3) then
        do 22 ic = 1, 3
            coon3(ic) = coor(3*(n3-1)+ic)
22      continue
        call vdiff(3, coon3, coon1, n1n3)
        call provec(n1n2, n1n3, n)
    endif
    call normev(n, norme)
!
    if (tpmail(1:4) .eq. 'QUAD') then
        nbnsm = 4
    else if (tpmail(1:4).eq.'TRIA') then
        nbnsm = 3
    else if (tpmail(1:3).eq.'SEG') then
        if (ndim .eq. 3) goto 9999
        nbnsm = 2
    else
        valk(1) = nomail
        valk(2) = tpmail
        call u2mesk('F', 'MODELISA5_94', 2, valk)
    endif
!
! --- CENTRE DE GRAVITE DE LA MAILLE DE PEAU
!
    do 30 i = 1, nbnsm
        ino = lisnoe(i)
        xgm(1) = xgm(1) + coor(3*(ino-1)+1)
        xgm(2) = xgm(2) + coor(3*(ino-1)+2)
        xgm(3) = xgm(3) + coor(3*(ino-1)+3)
30  end do
    xgm(1) = xgm(1) / nbnsm
    xgm(2) = xgm(2) / nbnsm
    xgm(3) = xgm(3) / nbnsm
!
    if (typ3d(1:4) .eq. 'HEXA') then
        nbns3d=8
    else if (typ3d(1:4).eq.'PENT') then
        nbns3d=6
    else if (typ3d(1:4).eq.'PYRA') then
        nbns3d=5
    else if (typ3d(1:4).eq.'TETR') then
        nbns3d=4
    else if (typ3d(1:4).eq.'QUAD') then
        nbns3d=4
    else if (typ3d(1:4).eq.'TRIA') then
        nbns3d=3
    endif
!
! --- DETERMINATION DU CENTRE DE GRAVITE DE LA MAILLE 3D
!
    do 40 i = 1, nbns3d
        ino = lnm3d(i)
        xg3d(1) = xg3d(1) + coor(3*(ino-1)+1)
        xg3d(2) = xg3d(2) + coor(3*(ino-1)+2)
        xg3d(3) = xg3d(3) + coor(3*(ino-1)+3)
40  end do
    xg3d(1) = xg3d(1) / nbns3d
    xg3d(2) = xg3d(2) / nbns3d
    xg3d(3) = xg3d(3) / nbns3d
!
    call vdiff(3, xg3d, xgm, n1g)
    call normev(n1g, norme)
    xgn = ddot ( 3, n1g, 1, n, 1 )
!
! --- SI XGN > 0, LA NORMALE A LA MAILLE DE PEAU
! --- EST DIRIGEE VERS L'INTERIEUR DU VOLUME, IL FAUT
! --- LA REORIENTER :
!     -------------
    if (xgn .gt. zero) then
        norien = norien + 1
        if (.not. reorie) goto 9999
        if (tpmail(1:5) .eq. 'QUAD9' .or. tpmail(1:5) .eq. 'TRIA7') then
            nbnoe = nbnmai - 1
        else
            nbnoe = nbnmai
        endif
        ino = 0
        do 50 i = nbnsm, 1, -1
            ino = ino+1
            lnmail(i) = lisnoe(ino)
50      continue
        if (nbnsm .ne. nbnoe) then
            ino = 0
            do 52 i = nbnoe-1, nbnsm+1, -1
                ino = ino+1
                lnmail(i) = lisnoe(ino+nbnsm)
52          continue
            lnmail(nbnoe) = lisnoe(nbnoe)
        endif
        if (niv .eq. 2) then
            write(ifm,*) ' REORIENTATION MAILLE ',nomail, ' NOEUDS ',(&
            lnmail(i),i=1,nbnmai), ' PRODUIT SCALAIRE ',xgn
        endif
    endif
!
9999  continue
!
end subroutine
