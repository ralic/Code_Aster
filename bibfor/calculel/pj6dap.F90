subroutine pj6dap(ino2, geom2, geom1, seg2,&
                  cobary, itr3, nbtrou, btdi, btvr,&
                  btnb, btlc, btco,&
                  l_dmax, dmax, dala, loin, dmin)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterfort/pj3dgb.h"
#include "asterfort/pj6da2.h"
    real(kind=8) :: cobary(2), geom1(*), geom2(*), btvr(*)
    integer :: itr3, nbtrou, btdi(*), btnb(*), btlc(*), btco(*), seg2(*)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!  but :
!    trouver le seg2 qui servira a interpoler le noeud ino2
!    ainsi que les coordonnees barycentriques de ino2 dans ce seg2

!  in   ino2       i  : numero du noeud de m2 cherche
!  in   geom2(*)   r  : coordonnees des noeuds du maillage m2
!  in   geom1(*)   r  : coordonnees des noeuds du maillage m1
!  in   seg2(*)    i  : objet '&&pjxxco.seg2'
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
!  out  nbtrou     i  : 1 -> on a trouve 1 seg2 solution
!                     : 0 -> on n'a pas trouve de seg2 solution
!  out  itr3       i  : numero du seg2 solution
!  out  cobary(4)  r  : coordonnees barycentriques de ino2 dans itr3
!  out  dmin       r  : distance de ino2 au bord de itr3 si ino2 est
!                       exterieur a itr3.
!  out  loin       l  : .true. si dmin > 10% diametre(itr3) ou si dmin < dala

!  remarque :
!    si nbtrou=0, ino2 ne sera pas projete car il est au dela de dmax
!    alors : dmin=0, loin=.false.
! ----------------------------------------------------------------------


    real(kind=8) :: cobar2(2), dmin, d2, long, rtr3
    integer :: p, q, r, p1, q1, p2, q2, r1, r2, ino2, i, k, iposi, nx, ny, ntrbt

    aster_logical :: l_dmax, loin
    real(kind=8) :: dmax, dala
! DEB ------------------------------------------------------------------
    nbtrou=0
    loin=.false.
    dmin=0.d0

    nx=btdi(1)
    ny=btdi(2)


!     --  ON CHERCHE LE SEG2 ITR3 LE PLUS PROCHE DE INO2 :
!     ------------------------------------------------------
    if (l_dmax) then
        dmin=dmax
    else
        dmin=r8maem()
    endif

!       -- ON RECHERCHE LA GROSSE BOITE CANDIDATE :
    call pj3dgb(ino2, geom2, geom1, seg2, 3,&
                btdi, btvr, btnb, btlc, btco,&
                p1, q1, r1, p2, q2,&
                r2)
    do 40 p = p1, p2
        do 30 q = q1, q2
            do 20 r = r1, r2
                ntrbt=btnb((r-1)*nx*ny+(q-1)*nx+p)
                iposi=btlc((r-1)*nx*ny+(q-1)*nx+p)
                do 10 k = 1, ntrbt
                    i=btco(iposi+k)
                    call pj6da2(ino2, geom2, i, geom1, seg2,&
                                cobar2, d2, long)
                    if (sqrt(d2) .lt. dmin) then
                        rtr3=long
                        itr3=i
                        dmin=sqrt(d2)
                        nbtrou=1
                        cobary(1)=cobar2(1)
                        cobary(2)=cobar2(2)
                    endif
 10             continue
 20         continue
 30     continue
 40 end do


!   -- calcul de loin :
    if (nbtrou .eq. 1) then
        if (dala.ge.0.d0) then
            if (dmin.lt.dala) loin=.false.
        else
            if (rtr3 .eq. 0) then
                loin=.true.
            else
                if (dmin/rtr3 .gt. 1.d-1) loin=.true.
            endif
        endif
    else
        dmin=0.d0
    endif


end subroutine
