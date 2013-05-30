subroutine jjmzat(iclas, idat)
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
!     ==================================================================
    include 'jeveux_private.h'
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!-----------------------------------------------------------------------
    integer :: iadmar, iclas, idat, jcara, jdate, jdocu, jgenr
    integer :: jhcod, jiadd, jiadm, jlong, jlono, jltyp, jluti
    integer :: jmarq, jorig, jrnom, jtype, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
! DEB ------------------------------------------------------------------
    ltyp( jltyp(iclas) + idat ) = 0
    long( jlong(iclas) + idat ) = 0
    date( jdate(iclas) + idat ) = 0
    iadd( jiadd(iclas) + 2*idat-1 ) = 0
    iadd( jiadd(iclas) + 2*idat ) = 0
    iadm( jiadm(iclas) + 2*idat-1 ) = 0
    iadm( jiadm(iclas) + 2*idat ) = 0
    lono( jlono(iclas) + idat ) = 0
    luti( jluti(iclas) + idat ) = 0
    genr( jgenr(iclas) + idat ) = ' '
    type( jtype(iclas) + idat ) = ' '
    docu( jdocu(iclas) + idat ) = '    '
    orig( jorig(iclas) + idat ) = '        '
    imarq(jmarq(iclas) + 2*idat-1 ) = 0
    iadmar = imarq(jmarq(iclas) + 2*idat)
    if (iadmar .gt. 0) then
        iszon(jiszon+kdesma(1)+iadmar-1) = 0
        imarq(jmarq(iclas) + 2*idat) = 0
    endif
! FIN ------------------------------------------------------------------
end subroutine
