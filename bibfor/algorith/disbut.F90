subroutine disbut(np3, ic, xloc, typobs, xjeu,&
                  rayon, theta, nbseg, cost, sint,&
                  dnorm)
    implicit none
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! DESCRIPTION : CALCUL DE LA DISTANCE NORMALE A LA BUTEE
! -----------
!               APPELANTS : CALND2, CALREF, CALRES, CHVERI, COMPTR,
!                           FTEST1, FTEST2, INIALG, JACBQU, MDCHOE,
!                           MDCHOF, TESTCH, TSTCNT
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
! ARGUMENTS
! ---------
    include 'asterc/r8depi.h'
    integer :: np3, ic
    real(kind=8) :: xloc(*)
    integer :: typobs
    real(kind=8) :: xjeu, rayon(np3, *), theta(np3, *)
    integer :: nbseg
    real(kind=8) :: cost, sint, dnorm
!
! VARIABLES LOCALES
! -----------------
    integer :: i
    real(kind=8) :: zero, un, xlg, sintno, costno, tetano
    real(kind=8) :: r1, r2, t1, t2, y1, y2, z1, z2, dy, dz, xls
!
! FONCTIONS INTRINSEQUES
! ----------------------
!     INTRINSIC  ABS, ATAN2, COS, SIGN, SIN, SQRT
!
! FONCTIONS EXTERNES
! ------------------
!     EXTERNAL   R8DEPI
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    zero = 0.0d0
    un = 1.0d0
!
! 0.  OBSTACLE PLAN PARALLELE A YLOCAL
!     --------------------------------
    if (typobs .eq. 0) then
!
        xlg = abs(xloc(2))
        dnorm = xjeu - xlg
        sint = zero
        cost = -sign( un,xloc(2) )
!
! 1.  OBSTACLE PLAN PARALLELE A ZLOCAL
!     --------------------------------
    else if (typobs.eq.1) then
!
        xlg = abs(xloc(3))
        dnorm = xjeu - xlg
        cost = zero
        sint = -sign( un,xloc(3) )
!
! 2.  OBSTACLE CIRCULAIRE
!     -------------------
    else if (typobs.eq.2) then
!
        xlg = sqrt( xloc(2)*xloc(2) + xloc(3)*xloc(3) )
        dnorm = xjeu - xlg
        if (xlg .ne. zero) then
            sint = -xloc(3) / xlg
            cost = -xloc(2) / xlg
        else
            sint = zero
            cost = -un
        endif
!
! 3.  OBSTACLE DISCRETISE
!     -------------------
    else if (typobs.eq.3) then
!
        xlg = sqrt( xloc(2)*xloc(2) + xloc(3)*xloc(3) )
        if (xlg .ne. zero) then
            sintno = xloc(3) / xlg
            costno = xloc(2) / xlg
        else
            sintno = zero
            costno = un
        endif
        tetano = atan2(sintno,costno)
        if (tetano .lt. zero) tetano = tetano + r8depi()
        do 10 i = 1, nbseg-1
            t1 = theta(i,ic)
            t2 = theta(i+1,ic)
            if ((tetano.ge.t1) .and. (tetano.le.t2)) then
                r1 = rayon(i,ic)
                r2 = rayon(i+1,ic)
                y1 = r1*cos(t1)
                y2 = r2*cos(t2)
                z1 = r1*sin(t1)
                z2 = r2*sin(t2)
                dy = y2-y1
                dz = z2-z1
                xls = sqrt( dy*dy + dz*dz )
                if (xls .ne. zero) then
                    cost = -dz / xls
                    sint = dy / xls
                else
                    sint = zero
                    cost = -un
                endif
                dnorm = (xloc(2)-y1)*cost+(xloc(3)-z1)*sint
                goto 999
            endif
10      continue
!
    endif
!
999  continue
!
! --- FIN DE DISBUT.
end subroutine
