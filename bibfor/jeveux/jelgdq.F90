subroutine jelgdq(nomlu, rlong, nbsv)
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
! ----------------------------------------------------------------------
! BUT:
!   DETERMINER LA LONGUEUR DES SEGMENTS DE VALEURS STOCKES SUR LA BASE
!   ET LE NOMBRE DE SEGMENT DE VALEURS ASSOCIES A L'OBJET NOMLU
!
!  IN    NOMLU : NOM DE L'OBJET
!  OUT   RLONG : CUMUL DES TAILLES EN OCTETS DES OBJETS ASSOCIES
!  OUT   NBSV  : NOMBRE DE SEGMENTS DE VALEURS ASSOCIES
!
!
! ----------------------------------------------------------------------
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjlide.h"
#include "asterfort/jjvern.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: nomlu
    real(kind=8) :: rlong
    integer :: nbsv
!     ------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: n
    parameter  ( n = 5 )
    integer :: jltyp, jlong, jdate, jiadd, jiadm, jlono, jhcod, jcara, jluti
    integer :: jmarq
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    integer :: jgenr, jtype, jdocu, jorig, jrnom
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idlono, idnum
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2  ,&
     &               idlono = 8  ,idnum  = 10 )
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    integer :: ipgcex, icre, iret, ic, id, ibacol, k, ix
    integer :: ltypi, ixlono, ixiadd, iblono, nmax
!
    ipgcex = ipgc
    ipgc = -2
    noml32 = nomlu
    rlong = 0.0d0
    nbsv = 0
    icre = 0
    iret = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
!
    else if (iret .eq. 1) then
        ic = iclaos
        id = idatos
        rlong = lono(jlono(ic)+id)*ltyp(jltyp(ic)+id)
        nbsv = nbsv + 1
!
    else if (iret .eq. 2) then
        ic = iclaco
        call jjallc(iclaco, idatco, 'L', ibacol)
        id = iszon(jiszon + ibacol + iddeso )
        ixlono = iszon(jiszon + ibacol + idlono )
        ixiadd = iszon(jiszon + ibacol + idiadd )
        rlong = idnum*ltyp(jltyp(ic)+id)
        nbsv = nbsv + 1
!
! --------OBJETS ATTRIBUTS DE COLLECTION
!
        do 20 k = 1, idnum
            ix = iszon( jiszon + ibacol + k )
            if (ix .gt. 0) then
                rlong = rlong + lono(jlono(ic)+ix)*ltyp(jltyp(ic)+ix)
                nbsv = nbsv + 1
            endif
20      continue
!
! ------- CAS D'UNE COLLECTION DISPERSEE
!
        if (ixiadd .ne. 0) then
            nmax = iszon(jiszon + ibacol+ivnmax)
            ltypi = ltyp(jltyp(ic)+id)
            if (ixlono .ne. 0) then
                nmax = iszon(jiszon + ibacol+ivnmax)
                iblono = iadm (jiadm(ic) + 2*ixlono-1)
                do 10 k = 1, nmax
                    rlong = rlong + iszon(jiszon+iblono-1+k)*ltypi
                    nbsv = nbsv + 1
10              continue
            else
                rlong = rlong + nmax*lono(jlono(ic)+id)*ltypi
                nbsv = nbsv + 1
            endif
        endif
!
        call jjlide('JEIMPA', noml32(1:24), 2)
    endif
    ipgc = ipgcex
end subroutine
