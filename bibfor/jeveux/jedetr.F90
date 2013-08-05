subroutine jedetr(nomlu)
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
#include "jeveux_private.h"
#include "asterfort/assert.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjcren.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjlidy.h"
#include "asterfort/jjmzat.h"
#include "asterfort/jjvern.h"
#include "asterfort/jxlibd.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: nomlu
! ----------------------------------------------------------------------
! DESTRUCTION D'UN OBJET JEVEUX
!
! IN  NOMLU  : NOM D'OBJET JEVEUX
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
!-----------------------------------------------------------------------
    integer :: iadmar, iadmi, iadmoc, iadyn, iadyoc, ibacol, ibiadd
    integer :: ibiadm, iblong, iblono, ibmarq, ic, ixdeso, ixiadd
    integer :: ixiadm, ixlong, ixlono, ixmarq, ixnom, jcara, jdate
    integer :: jdocu, jgenr, jhcod, jiadd, jiadm, jlong, jlono
    integer :: jltyp, jluti, jmarq, jorig, jrnom, jtype, k
    integer :: lonoi, n, nmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: ifnivo, nivo
    common /jvnivo/  ifnivo, nivo
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idmarq, idnom, idlong, idlono
    integer :: idnum
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idmarq = 4 , idnom  = 5 ,              idlong = 7 ,&
     &               idlono = 8 , idnum  = 10 )
!     ------------------------------------------------------------------
    character(len=32) :: noml32, nom32
    integer :: icre, iret, id(idnum), iaddi(2)
! DEB ------------------------------------------------------------------
    noml32 = nomlu
    icre = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        goto 9999
    else if (iret .eq. 1) then
        ic = iclaos
        iadmi = iadm (jiadm(ic) + 2*idatos-1 )
        iadyn = iadm (jiadm(ic) + 2*idatos )
        call jjlidy(iadyn, iadmi)
        iaddi(1) = iadd (jiadd(ic) + 2*idatos-1 )
        iaddi(2) = iadd (jiadd(ic) + 2*idatos )
        if (iaddi(1) .gt. 0) then
            lonoi = lono(jlono(ic)+idatos)*ltyp(jltyp(ic)+idatos)
            call jxlibd(0, idatos, ic, iaddi, lonoi)
        endif
        if (nivo .ge. 2) then
            call u2mesk('I', 'JEVEUX_07', 1, noml32)
        endif
        call jjcren(noml32, -1, iret)
        nomos = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
        call jjmzat(iclaos, idatos)
    else
        ic = iclaco
        call jjallc(ic, idatco, 'E', ibacol)
        if (noml32(25:32) .ne. '        ') then
            call jjcroc(noml32(25:32), icre)
            iret = 3
        endif
        if (iret .eq. 2) then
            ixiadm = iszon( jiszon + ibacol + idiadm )
            ixiadd = iszon( jiszon + ibacol + idiadd )
            ixlono = iszon( jiszon + ibacol + idlono )
            ixdeso = iszon( jiszon + ibacol + iddeso )
            ixmarq = iszon( jiszon + ibacol + idmarq )
            if (ixiadm .gt. 0) then
                ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                ibmarq = iadm ( jiadm(ic) + 2*ixmarq-1 )
                nmax = iszon(jiszon+ibacol+ivnmax)
                do 10 k = 1, nmax
                    iadmar = iszon( jiszon + ibmarq - 1 + 2*k )
                    if (iadmar .ne. 0) then
                        iszon(jiszon+kdesma(1)+iadmar-1) = 0
                    endif
                    iadmoc = iszon( jiszon + ibiadm - 1 + 2*k-1 )
                    iadyoc = iszon( jiszon + ibiadm - 1 + 2*k )
                    call jjlidy(iadyoc, iadmoc)
                    ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
                    iaddi(1) = iszon( jiszon + ibiadd -1 + 2*k-1 )
                    iaddi(2) = iszon( jiszon + ibiadd -1 + 2*k )
                    if (iaddi(1) .gt. 0) then
                        if (ixlono .gt. 0) then
                            iblono=iadm(jiadm(ic)+2*ixlono-1)
                            lonoi =iszon(jiszon+iblono+k-1)*ltyp(&
                            jltyp(ic)+ixdeso)
                        else
                            lonoi = lono( jlono(ic)+ixdeso)*ltyp(jltyp( ic)+ixdeso)
                        endif
                        call jxlibd(idatco, k, ic, iaddi, lonoi)
                    endif
10              continue
            endif
            do 1 k = 1, idnum
                id (k) = iszon ( jiszon + ibacol + k )
                if (id(k) .gt. 0) then
                    nom32 = rnom ( jrnom(ic) + id(k) )
                    if (nom32(1:24) .eq. noml32(1:24) .or. nom32(25: 26) .eq. '&&') then
                        iadmi = iadm (jiadm(ic) + 2*id(k)-1 )
                        iadyn = iadm (jiadm(ic) + 2*id(k) )
                        call jjlidy(iadyn, iadmi)
                        iaddi(1) = iadd (jiadd(ic) + 2*id(k)-1 )
                        iaddi(2) = iadd (jiadd(ic) + 2*id(k) )
                        if (iaddi(1) .gt. 0) then
                            lonoi = lono(jlono(ic)+id(k))*ltyp(jltyp( ic)+id(k) )
                            call jxlibd(0, id(k), ic, iaddi, lonoi)
                        endif
                    else
                        id(k) = 0
                    endif
                endif
 1          continue
            do 2 k = 1, idnum
                if (id(k) .gt. 0) then
                    nom32 = rnom ( jrnom(ic) + id(k) )
                    if (nivo .ge. 2) then
                        call u2mesk('I', 'JEVEUX_07', 1, noml32(1:24))
                    endif
                    call jjcren(nom32, -2, iret)
                    call jjmzat(ic, id(k))
                endif
 2          continue
            iadyn = iadm(jiadm(ic)+2*idatco)
            call jjlidy(iadyn, ibacol)
            iaddi(1) = iadd (jiadd(ic) + 2*idatco-1)
            iaddi(2) = iadd (jiadd(ic) + 2*idatco )
            if (iaddi(1) .gt. 0) then
                lonoi = lono(jlono(ic)+idatco)*ltyp(jltyp(ic)+idatco)
                call jxlibd(0, idatco, ic, iaddi, lonoi)
            endif
            if (nivo .ge. 2) then
                call u2mesk('I', 'JEVEUX_07', 1, noml32(1:24))
            endif
            call jjcren(noml32(1:24), -2, iret)
            call jjmzat(ic, idatco)
            nomco = '$$$$$$$$$$$$$$$$$$$$$$$$'
        else if (iret .eq. 3) then
            ixiadm = iszon ( jiszon + ibacol + idiadm )
            ixiadd = iszon ( jiszon + ibacol + idiadd )
            ixlong = iszon ( jiszon + ibacol + idlong )
            ixnom = iszon ( jiszon + ibacol + idnom )
            ixdeso = iszon ( jiszon + ibacol + iddeso )
            ixlono = iszon ( jiszon + ibacol + idlono )
            ixmarq = iszon ( jiszon + ibacol + idmarq )
!
!         DESTRUCTION D''UN OBJET DE COLLECTION CONTIGUE REFUSEE
            ASSERT(ixiadd .gt. 0)
!
!         DESTRUCTION DANS UNE COLLECTION NON NOMMEE REFUSEE
            ASSERT(ixnom .gt. 0)
!
            ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
            iaddi(1) = iszon ( jiszon + ibiadd - 1 + 2*idatoc-1 )
            iaddi(2) = iszon ( jiszon + ibiadd - 1 + 2*idatoc )
            if (iaddi(1) .gt. 0) then
                if (ixlono .gt. 0) then
                    iblono = iadm(jiadm(ic)+2*ixlono-1)
                    lonoi = iszon(jiszon+iblono+idatoc-1)* ltyp(jltyp( ic)+ixdeso)
                else
                    lonoi = lono(jlono(ic)+ixdeso)*ltyp(jltyp(ic)+ ixdeso)
                endif
                call jxlibd(idatco, idatoc, ic, iaddi, lonoi)
            endif
            iszon ( jiszon + ibiadd + idatoc - 1 ) = 0
            ibmarq = iadm ( jiadm(ic) + 2*ixmarq-1 )
            iadmar = iszon( jiszon + ibmarq - 1 + 2*idatoc )
            if (iadmar .ne. 0) then
                iszon(jiszon+kdesma(1)+iadmar-1) = 0
            endif
            ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
            iadmi = iszon ( jiszon + ibiadm - 1 + 2*idatoc-1 )
            iadyn = iszon ( jiszon + ibiadm - 1 + 2*idatoc )
            call jjlidy(iadyn, iadmi)
            iszon ( jiszon + ibiadm - 1 + 2*idatoc-1 ) = 0
            iszon ( jiszon + ibiadm - 1 + 2*idatoc ) = 0
            if (ixlong .gt. 0) then
                iblong = iadm ( jiadm(ic) + 2*ixlong-1 )
                iszon ( jiszon + iblong + idatoc - 1 ) = 0
            endif
            if (nivo .ge. 2) then
                call u2mesk('I', 'JEVEUX_07', 1, noml32)
            endif
            call jjcroc(nomlu(25:32), -3)
            nomoc = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
        endif
    endif
9999  continue
! FIN ------------------------------------------------------------------
end subroutine
