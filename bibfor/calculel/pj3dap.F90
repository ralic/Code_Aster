subroutine pj3dap(ino2, geom2, geom1, tetr4,&
                  cobary, itr3, nbtrou, btdi, btvr,&
                  btnb, btlc, btco,&
                  l_dmax, dmax, dala, loin, dmin)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/pj3da1.h"
#include "asterfort/pj3da2.h"
#include "asterfort/pj3dgb.h"
    real(kind=8) :: cobary(4), geom1(*), geom2(*), btvr(*)
    integer :: itr3, nbtrou, btdi(*), btnb(*), btlc(*), btco(*), tetr4(*)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  but :
!    trouver le tetr4 qui servira a interpoler le noeud ino2
!    ainsi que les coordonnees barycentriques de ino2 dans ce tetr4

!  in   ino2       i  : numero du noeud de m2 cherche
!  in   geom2(*)   r  : coordonnees des noeuds du maillage m2
!  in   geom1(*)   r  : coordonnees des noeuds du maillage m1
!  in   tetr4(*)   i  : objet '&&pjxxco.tetr4'
!  in   btdi(*)    i  : objet .bt3ddi de la sd boite_3d
!  in   btvr(*)    r  : objet .bt3dvr de la sd boite_3d
!  in   btnb(*)    i  : objet .bt3dnb de la sd boite_3d
!  in   btlc(*)    i  : objet .bt3dlc de la sd boite_3d
!  in   btco(*)    i  : objet .bt3dco de la sd boite_3d
!  in   l_dmax     l  : .true. : il faut prendre dmax en compte
!  in   dmax       r  : distance au dela de laquelle le noeud ino2
!                       ne sera pas projete.
!  in   dala       r  : distance au dela de laquelle le noeud ino2
!                       sera considere comme lointain
!  out  nbtrou     i  : 2 -> on a trouve 1 tetr4 qui contient ino2
!                     : 1 -> on a trouve 1 tetr4 assez proche de ino2
!                     : 0 -> on n'a pas trouve de tetr4 assez proche
!  out  itr3       i  : numero du tetr4 solution
!  out  cobary(4)  r  : coordonnees barycentriques de ino2 dans itr3
!  out  dmin       r  : distance de ino2 au bord de itr3 si ino2 est
!                       exterieur a itr3.
!  out  loin       l  : .true. si dmin > 10% diametre(itr3) ou si dmin < dala

!  remarque :
!    si nbtrou=0, ino2 ne sera pas projete car il est au dela de dmax
!    alors : dmin=0, loin=.false.
! ----------------------------------------------------------------------


    real(kind=8) :: cobar2(4), dmin, d2, dx, dy, dz, xmin, ymin, zmin, volu
    real(kind=8) :: rtr3
    integer :: p, q, r, p1, q1, p2, q2, r1, r2, ino2, i, k, iposi, nx, ny, ntrbt
    aster_logical :: ok

    aster_logical :: l_dmax, loin
    real(kind=8) :: dmax, dala
! DEB ------------------------------------------------------------------
    nbtrou=0
    loin=.false.
    dmin=0.d0

    nx=btdi(1)
    ny=btdi(2)
    dx=btvr(7)
    dy=btvr(8)
    dz=btvr(9)
    xmin=btvr(1)
    ymin=btvr(3)
    zmin=btvr(5)


!   -- 1. : on cherche un tetr4 itr3 qui contienne ino2 :
!   -------------------------------------------------------
    p=int((geom2(3*(ino2-1)+1)-xmin)/dx)+1
    q=int((geom2(3*(ino2-1)+2)-ymin)/dy)+1
    r=int((geom2(3*(ino2-1)+3)-zmin)/dz)+1
    ntrbt=btnb((r-1)*nx*ny+(q-1)*nx+p)
    iposi=btlc((r-1)*nx*ny+(q-1)*nx+p)
    do 10 k = 1, ntrbt
        i=btco(iposi+k)
        call pj3da1(ino2, geom2, i, geom1, tetr4,&
                    cobar2, ok)
        if (ok) then
            itr3=i
            nbtrou=2
            cobary(1)=cobar2(1)
            cobary(2)=cobar2(2)
            cobary(3)=cobar2(3)
            cobary(4)=cobar2(4)
            goto 9999

        endif
 10 end do


!   -- 2. : si echec de la recherche precedente, on
!        cherche le tetr4 itr3 le plus proche de ino2 :
!  -------------------------------------------------------
    if (l_dmax) then
        dmin=dmax
    else
        dmin=r8maem()
    endif

!   -- on recherche la grosse boite candidate :
    call pj3dgb(ino2, geom2, geom1, tetr4, 6,&
                btdi, btvr, btnb, btlc, btco,&
                p1, q1, r1, p2, q2,&
                r2)
    do 60 p = p1, p2
        do 50 q = q1, q2
            do 40 r = r1, r2
                ntrbt=btnb((r-1)*nx*ny+(q-1)*nx+p)
                iposi=btlc((r-1)*nx*ny+(q-1)*nx+p)
                do 30 k = 1, ntrbt
                    i=btco(iposi+k)
                    call pj3da2(ino2, geom2, i, geom1, tetr4,&
                                cobar2, d2, volu)
                    if (sqrt(d2) .lt. dmin) then
                        rtr3=volu
                        itr3=i
                        dmin=sqrt(d2)
                        nbtrou=1
                        cobary(1)=cobar2(1)
                        cobary(2)=cobar2(2)
                        cobary(3)=cobar2(3)
                        cobary(4)=cobar2(4)
                    endif
 30             continue
 40         continue
 50     continue
 60 end do


!   -- calcul de loin :
    if (nbtrou .eq. 1) then
        if (dala.ge.0.d0) then
            if (dmin.lt.dala) loin=.false.
        else
            if (rtr3 .eq. 0) then
                loin=.true.
            else
                rtr3 = rtr3** (1.d0/3.d0)
                if (dmin/rtr3 .gt. 1.d-1) loin=.true.
            endif
        endif
    else
        dmin=0.d0
    endif

9999 continue

end subroutine
