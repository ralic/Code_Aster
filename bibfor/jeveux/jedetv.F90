subroutine jedetv()
! person_in_charge: j-pierre.lefebvre at edf.fr
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
!
! DETRUIT TOUS LES OBJETS JEVEUX PRESENTS SUR LA BASE VOLATILE A
! L'EXCEPTION DES OBJETS SYSTEME
!
    implicit none
! ----------------------------------------------------------------------
#include "jeveux_private.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjcren.h"
#include "asterfort/jjlidy.h"
#include "asterfort/jjmzat.h"
#include "asterfort/jxlibd.h"
#include "asterfort/utmess.h"
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!-----------------------------------------------------------------------
    integer :: iadmar, iadmi, iadmoc, iadyn, iadyoc, ibacol, ibiadd
    integer :: ibiadm, iblono, ibmarq, iret, ixdeso, ixiadd
    integer :: ixiadm, ixlono, ixmarq, jcara, jdate, jdocu, jgenr
    integer :: jhcod, jiacce, jiadd, jiadm, jido, jindir, jlong
    integer :: jlono, jltyp, jluti, jmarq, jorig, jrnom, jtype
    integer :: k, lonoi, n, nbacce, nmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
    integer :: ifnivo, nivo
    common /jvnivo/  ifnivo, nivo
    common /jindir/  jindir(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idmarq, idlono, idnum
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idmarq = 4 ,&
     &               idlono = 8 , idnum  = 10 )
!     ------------------------------------------------------------------
    integer :: lidbas
    parameter      ( lidbas = 20 )
    integer :: ic, id(idnum), ido, iaddi(2)
    real(kind=8) :: valr(3)
    character(len=1) :: cgenr
    character(len=32) :: crnom, nom32
! DEB ------------------------------------------------------------------
!
    ic = index ( classe , 'V')
!
    valr(1)= 100.d0
    valr(2)= nbacce(2*ic-1)*longbl(ic)*lois/1024.d0
    valr(3)= nbacce(2*ic  )*longbl(ic)*lois/1024.d0
    if (valr(3) .gt. valr(1)*valr(2) .and. valr(2) .ne. 0) then
        call utmess('A', 'JEVEUX1_64', nr=3, valr=valr)
    endif
!
    do 150 jido = 1, nremax(ic)
        ido = indir(jindir(ic)+jido)
        if (ido .le. lidbas) goto 150
        crnom = rnom(jrnom(ic)+ido)
        if (crnom(1:1) .eq. '?' .or. crnom(25:32) .ne. '     ') goto 150
        cgenr = genr(jgenr(ic)+ido)
        if (cgenr .eq. 'X') then
!
!    ON TRAITE D'ABORD LES COLLECTIONS
!
            call jjallc(ic, ido, 'E', ibacol)
            ixiadm = iszon( jiszon + ibacol + idiadm )
            ixiadd = iszon( jiszon + ibacol + idiadd )
            ixlono = iszon( jiszon + ibacol + idlono )
            ixdeso = iszon( jiszon + ibacol + iddeso )
            ixmarq = iszon( jiszon + ibacol + idmarq )
            if (ixiadm .ne. 0) then
                ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
                ibmarq = iadm ( jiadm(ic) + 2*ixmarq-1 )
                nmax = iszon(jiszon+ibacol+ivnmax )
                do 10 k = 1, nmax
                    iadmar = iszon( jiszon + ibmarq -1 + 2*k )
                    if (iadmar .ne. 0) then
                        iszon(jiszon+kdesma(1)+iadmar-1) = 0
                    endif
                    iadmoc = iszon( jiszon + ibiadm - 1 + 2*k-1 )
                    iadyoc = iszon( jiszon + ibiadm - 1 + 2*k )
                    call jjlidy(iadyoc, iadmoc)
                    iaddi(1) = iszon( jiszon + ibiadd - 1 + 2*k-1 )
                    iaddi(2) = iszon( jiszon + ibiadd - 1 + 2*k )
                    if (iaddi(1) .gt. 0) then
                        if (ixlono .gt. 0) then
                            iblono=iadm(jiadm(ic)+2*ixlono-1)
                            lonoi =iszon(jiszon+iblono+k-1)*ltyp(&
                            jltyp(ic)+ixdeso)
                        else
                            lonoi = lono( jlono(ic)+ixdeso)*ltyp(jltyp( ic)+ixdeso)
                        endif
                        call jxlibd(ido, k, ic, iaddi, lonoi)
                    endif
10              continue
            endif
            do 1 k = 1, idnum
                id(k) = iszon ( jiszon + ibacol + k )
                if (id(k) .gt. 0) then
                    nom32 = rnom ( jrnom(ic) + id(k) )
                    if (nom32(1:24) .eq. crnom(1:24) .or. nom32(25: 26) .eq. '&&') then
                        iadmi = iadm (jiadm(ic) + 2*id(k)-1 )
                        iadyn = iadm (jiadm(ic) + 2*id(k) )
                        call jjlidy(iadyn, iadmi)
                        iaddi(1) = iadd (jiadd(ic) + 2*id(k)-1 )
                        iaddi(2) = iadd (jiadd(ic) + 2*id(k) )
                        if (iaddi(1) .gt. 0) then
                            lonoi=lono(jlono(ic)+id(k))*ltyp(jltyp(ic)&
                            +id(k))
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
                        call utmess('I', 'JEVEUX_07', sk=nom32)
                    endif
                    call jjcren(nom32, -2, iret)
                    call jjmzat(ic, id(k))
                endif
 2          continue
            crnom = rnom ( jrnom(ic) + ido )
            iadyn = iadm (jiadm(ic) + 2*ido)
            call jjlidy(iadyn, ibacol)
            iaddi(1) = iadd (jiadd(ic) + 2*ido-1)
            iaddi(2) = iadd (jiadd(ic) + 2*ido )
            if (iaddi(1) .gt. 0) then
                lonoi = lono(jlono(ic)+ido)*ltyp(jltyp(ic)+ido)
                call jxlibd(0, ido, ic, iaddi, lonoi)
            endif
            if (nivo .ge. 2) then
                call utmess('I', 'JEVEUX_07', sk=crnom(1:24))
            endif
            call jjcren(crnom(1:24), -2, iret)
            call jjmzat(ic, ido)
            nomco = '$$$$$$$$$$$$$$$$$$$$$$$$'
        endif
150  end do
!
    do 250 jido = 1, nremax(ic)
        ido = indir(jindir(ic)+jido)
        if (ido .le. lidbas) goto 250
        crnom = rnom(jrnom(ic)+ido)
        if (crnom(1:1) .eq. '?' .or. crnom(25:32) .ne. '      ') goto 250
        cgenr = genr(jgenr(ic)+ido)
!
!    ON TRAITE LES OBJETS SIMPLES
!
        if (cgenr .ne. 'X') then
            iadmi = iadm (jiadm(ic) + 2*ido-1)
            iadyn = iadm (jiadm(ic) + 2*ido )
            call jjlidy(iadyn, iadmi)
            iaddi(1) = iadd (jiadd(ic) + 2*ido-1)
            iaddi(2) = iadd (jiadd(ic) + 2*ido )
            if (iaddi(1) .gt. 0) then
                lonoi = lono(jlono(ic)+ido)*ltyp(jltyp(ic)+ido)
                call jxlibd(0, ido, ic, iaddi, lonoi)
            endif
            if (nivo .ge. 2) then
                call utmess('I', 'JEVEUX_07', sk=crnom(1:24))
            endif
            call jjcren(crnom, -1, iret)
            nomos = '$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'
            call jjmzat(ic, ido)
        endif
250  end do
! FIN ------------------------------------------------------------------
end subroutine
