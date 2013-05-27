subroutine jenonu(nomlu, numo)
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
    implicit none
    include 'jeveux_private.h'
    include 'asterfort/assert.h'
    include 'asterfort/jjallc.h'
    include 'asterfort/jjcroc.h'
    include 'asterfort/jjlide.h'
    include 'asterfort/jjvern.h'
    include 'asterfort/jxveuo.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomlu
    integer :: numo
!     ==================================================================
!-----------------------------------------------------------------------
    integer :: iadmex, iadmi, ibacol, ipgcex, jcara, jctab, jdate
    integer :: jhcod, jiadd, jiadm, jlong, jlono, jltyp, jluti
    integer :: jmarq, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    integer :: icre, iret, itab
!     ------------------------------------------------------------------
    numo = 0
    ipgcex = ipgc
    ipgc = -2
!
    if (len(nomlu) .ne. 32) call u2mesk('F', 'JEVEUX_24', 1, nomlu)
!
    icre = 0
    noml32 = nomlu
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) call u2mesk('F', 'JEVEUX_23', 1, noml32)
!
    if (iret .eq. 1) then
!       ----- OBJET DE TYPE REPERTOIRE
        iadmi = iadm ( jiadm(iclaos) + 2*idatos-1 )
        iadmex = iadmi
        if (iadmex .eq. 0) then
            call jxveuo('L', itab, iret, jctab)
        endif
        call jjcroc('        ', icre)
        if (iadmex .eq. 0) then
            call jjlide('JENONU', noml32, iret)
        endif
!
    else if (iret .eq. 2) then
!       ----- REPERTOIRE DE COLLECTION --
        call jjallc(iclaco, idatco, 'L', ibacol)
        call jjcroc(noml32(25:32), icre)
        call jjlide('JENONU', noml32(1:24), iret)
    else
        call assert(.false.)
    endif
!
    numo = idatoc
    ipgc = ipgcex
!
end subroutine
