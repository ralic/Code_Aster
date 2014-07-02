subroutine rsbary(lr8, nr8, tous, lexi, x,&
                  i1, i2, iposit)
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
    implicit none
#include "asterf_types.h"
!
!     ARGUMENTS:
!     ----------
    integer :: nr8, i1, i2, iposit
    real(kind=8) :: lr8(*), x
    aster_logical :: tous, lexi(*)
! ----------------------------------------------------------------------
!     BUT:
!      TROUVER DANS UNE LISTE DE R8 QUELS SONT LES 2 REELS LES PLUS
!      PROCHES DU REEL X DONNE. (POUR FAIRE UN BARYCENTRE)
!     (ON PEUT NE PAS PRENDRE EN COMPTE TOUS LES REELS DE LA LISTE GRACE
!       A L'ARGUMENT LEXI)
!     IN:
!     NR8    : NOMBRE DE REELS DANS LA LISTE LR8.
!     LR8    : LISTE DE REELS (PAS FORCEMENT ORDONNEE).
!     TOUS   : INDIQUE QUE TOUS LES REELS DE LA LISTE SONT A CONSIDERER.
!     LEXI   : INDIQUE QUELS SONT LES REELS A CONSIDERER (SI TOUS=FALSE)
!              SI TOUS=.TRUE. CET ARGUMENT EST INUTILISE.
!      X     : REEL DONT ON CHERCHE LES COORDONEES BARYCENTRIQUES.
!
!     OUT:
!     I1,I2  : INDICES DES 2 REELS DE LA LISTE QUI "ENCADRENT" X
!              (LR8(I1) =< LR8(I2))
!              (EVENTUELLEMENT I1 PEUT ETRE EGAL A I2)
!     IPOSIT : CODE LA POSITION DE X PAR RAPPORT A LR8(I1) ET LR8(I2)
!         IPOSIT=0  -->  LR8(I1)  =<   X  =<   LR8(I2)
!         IPOSIT=1  -->  LR8(I1) =<  LR8(I2) =< X  (PROL_DR)
!         IPOSIT=-1 -->  X =< LR8(I1) =<  LR8(I2)  (PROL_GA)
!         IPOSIT=-2 -->  ERREUR : LA LISTE DE REELS EST VIDE.
!
!
! ----------------------------------------------------------------------
    integer :: ipp, ip, is, iss
    real(kind=8) :: xpp, xp, xs, xss, epsi, inter
    aster_logical :: afaire
!-----------------------------------------------------------------------
    integer :: i, imax, imin
    real(kind=8) :: xmax, xmin
!-----------------------------------------------------------------------
    data epsi /1.0d-10/
!
! DEB-------------------------------------------------------------------
!
!     --------XI-----XPP--XP-------X---XS-------XSS-------XJ-->
!
!     ON APPELLE : XP : LE REEL PRECEDENT X DANS LA LISTE
!                XPP: LE REEL PRECEDENT XP DANS LA LISTE
!                XS : LE REEL SUIVANT  X DANS LA LISTE
!                XSS: LE REEL SUIVANT  XS DANS LA LISTE
!               XMAX: LE REEL MAX DE LA LISTE
!               XMIN: LE REEL MIN DE LA LISTE
!
    ip = 0
    ipp = 0
    is = 0
    iss = 0
!
!     -- CAS DE LA LISTE VIDE:
!     ------------------------
    do 100 i = 1, nr8
        if (tous) then
            afaire = .true.
        else
            if (lexi(i)) then
                afaire = .true.
            else
                afaire = .false.
            endif
        endif
        if (afaire) then
            imin = i
            imax = i
            xmax = lr8(i)
            xmin = lr8(i)
            goto 101
        endif
100 end do
    iposit = -2
    goto 9999
101 continue
!
!     RECHERCHE DE XMAX ET XMIN:
    do 1 i = 1, nr8
        if (tous) then
            afaire = .true.
        else
            if (lexi(i)) then
                afaire = .true.
            else
                afaire = .false.
            endif
        endif
        if (afaire) then
            if (lr8(i) .ge. xmax) then
                imax = i
                xmax = lr8(i)
            endif
            if (lr8(i) .le. xmin) then
                imin = i
                xmin = lr8(i)
            endif
        endif
  1 end do
!
!
    inter=epsi*(xmax-xmin)
!
!     -- 1ER CAS X EST INCLU DANS L'INTERVALLE DE LA LISTE:
    if (((x.ge.xmin).or.(abs(x-xmin).lt.inter)) .and.&
        ((x.le.xmax).or.(abs(x-xmax).lt.inter))) then
        iposit = 0
        ip = imin
        is = imax
        xp = xmin
        xs = xmax
        do 2 i = 1, nr8
            if (tous) then
                afaire = .true.
            else
                if (lexi(i)) then
                    afaire = .true.
                else
                    afaire = .false.
                endif
            endif
            if (afaire) then
                if ((lr8(i).ge.x) .and. (lr8(i).le.xs)) then
                    is = i
                    xs = lr8(i)
                endif
                if ((lr8(i).le.x) .and. (lr8(i).ge.xp)) then
                    ip = i
                    xp = lr8(i)
                endif
            endif
  2     continue
        i1 = ip
        i2 = is
        goto 9999
    endif
!
!
!     -- 2EME CAS X EST A DROITE DE L'INTERVALLE DE LA LISTE:
    if (x .gt. xmax) then
        iposit = 1
        ip = imax
        xp = xmax
        ipp = imin
        xpp = xmin
        do 31 i = 1, nr8
            if (tous) then
                afaire = .true.
            else
                if (lexi(i)) then
                    afaire = .true.
                else
                    afaire = .false.
                endif
            endif
            if (afaire) then
                if (i .eq. imax) goto 31
                if (lr8(i) .ge. xpp) then
                    ipp = i
                    xpp = lr8(i)
                endif
            endif
 31     continue
        i1 = ipp
        i2 = ip
        goto 9999
    endif
!
!     -- 3EME CAS X EST A GAUCHE DE L'INTERVALLE DE LA LISTE:
    if (x .lt. xmin) then
        iposit = -1
        is = imin
        xs = xmin
        iss = imax
        xss = xmax
        do 41 i = 1, nr8
            if (tous) then
                afaire = .true.
            else
                if (lexi(i)) then
                    afaire = .true.
                else
                    afaire = .false.
                endif
            endif
            if (afaire) then
                if (i .eq. imin) goto 41
                if (lr8(i) .le. xss) then
                    iss = i
                    xss = lr8(i)
                endif
            endif
 41     continue
        i1 = is
        i2 = iss
        goto 9999
    endif
!
!
!
9999 continue
end subroutine
