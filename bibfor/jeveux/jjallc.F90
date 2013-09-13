subroutine jjallc(iclasi, idatci, cel, ibacol)
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
#include "asterfort/jjalls.h"
#include "asterfort/jjecrs.h"
#include "asterfort/jxliro.h"
#include "asterfort/utmess.h"
    integer :: iclasi, idatci, ibacol
    character(len=*) :: cel
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadml, iadyn, ic, icel, id, ipgcex, is
    integer :: ix, jcara, jctab, jdate, jdocu, jgenr, jhcod
    integer :: jiadd, jiadm, jlong, jlono, jltyp, jluti, jmarq
    integer :: jorig, jrnom, jtype, k, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
! ----------------------------------------------------------------------
    integer :: istat
    common /istaje/  istat(4)
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
! ----------------------------------------------------------------------
    integer :: iddeso, idiadm, idmarq, idnom, idlong, idlono, idluti, idnum
    parameter    (  iddeso = 1  , idiadm = 3 ,&
     &               idmarq = 4 , idnom  = 5 ,              idlong = 7 ,&
     &               idlono = 8 , idluti = 9 , idnum  = 10 )
!     ------------------------------------------------------------------
    character(len=1) :: genri, typei
    integer :: col(1), jcol, itab(1)
    integer :: iadmi, iaddi(2), ltypi, lonoi, ista1, ista2
! DEB ------------------------------------------------------------------
    ipgcex = ipgc
    ic = iclasi
    id = idatci
    genri = genr ( jgenr(ic) + id )
    typei = type ( jtype(ic) + id )
    ltypi = ltyp ( jltyp(ic) + id )
    lonoi = lono ( jlono(ic) + id ) * ltypi
    iadmi = iadm ( jiadm(ic) + 2*id-1 )
    iadyn = iadm ( jiadm(ic) + 2*id )
    iaddi(1) = iadd ( jiadd(ic) + 2*id-1 )
    iaddi(2) = iadd ( jiadd(ic) + 2*id )
! ------- OBJET CONTENANT LES IDENTIFICATEURS DE LA COLLECTION
    if (iadmi .eq. 0) then
        iadml = 0
        if (iaddi(1) .eq. 0) then
            if (cel .eq. 'E') then
                call jjalls(lonoi, ic, genri, typei, ltypi,&
                            'INIT', col, jcol, iadml, iadyn)
            else
                call utmess('F', 'JEVEUX_18', sk=rnom(jrnom(ic)+id))
            endif
        else
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'NOINIT', col, jcol, iadml, iadyn)
            call jxliro(ic, iadml, iaddi, lonoi)
        endif
        iadmi = iadml
        iadm (jiadm(ic)+2*id-1) = iadml
        iadm (jiadm(ic)+2*id ) = iadyn
    else
        ista1 = iszon (jiszon+iadmi - 1)
        is = jiszon+iszon(jiszon+iadmi-4)
        ista2 = iszon (is - 4)
        icel = istat(3)
        if (cel .eq. 'E') icel = istat(4)
        if (ista1 .eq. istat(2) .and. ista2 .eq. icel) then
            if (imarq(jmarq(ic)+2*id-1) .ne. 0) then
                ibacol = iadmi
                goto 100
            endif
        endif
    endif
    ibacol = iadmi
    call jjecrs(iadmi, ic, id, 0, cel,&
                imarq(jmarq(ic)+2*id-1))
100  continue
!
    do 20 k = 2, idnum
!     ----------- OBJETS ATTRIBUTS DE COLLECTION
        ix = iszon( jiszon + ibacol + k )
        if (ix .gt. 0) then
            genri = genr ( jgenr(ic) + ix )
            typei = type ( jtype(ic) + ix )
            ltypi = ltyp ( jltyp(ic) + ix )
            lonoi = lono ( jlono(ic) + ix ) * ltypi
            if (lonoi .eq. 0) then
                call utmess('F', 'JEVEUX_26', sk=rnom(jrnom(ic)+ix))
            endif
            iadmi = iadm ( jiadm(ic) + 2*ix-1 )
            iadyn = iadm ( jiadm(ic) + 2*ix )
            iaddi(1) = iadd ( jiadd(ic) + 2*ix-1 )
            iaddi(2) = iadd ( jiadd(ic) + 2*ix )
            if (iadmi .ne. 0) then
! --------- IL N'Y A RIEN A FAIRE
!
            else if (k .ne. idiadm .and. k .ne. idmarq) then
! --------- MISE EN MEMOIRE AVEC LECTURE DISQUE
                iadml = 0
                if (iaddi(1) .eq. 0) then
                    if (cel .eq. 'E') then
                        call jjalls(lonoi, ic, genri, typei, ltypi,&
                                    'INIT', col, jcol, iadml, iadyn)
                    else
                        call utmess('F', 'JEVEUX_18', sk=rnom(jrnom(ic)+ ix))
                    endif
                else
                    call jjalls(lonoi, ic, genri, typei, ltypi,&
                                'NOINIT', col, jcol, iadml, iadyn)
                    call jxliro(ic, iadml, iaddi, lonoi)
                endif
                iadmi = iadml
                iadm(jiadm(ic)+2*ix-1) = iadml
                iadm(jiadm(ic)+2*ix ) = iadyn
            else
! --------- MISE EN MEMOIRE SANS LECTURE DISQUE
                call jjalls(lonoi, ic, genri, typei, ltypi,&
                            'INIT', itab, jctab, iadmi, iadyn)
                iadm(jiadm(ic)+2*ix-1) = iadmi
                iadm(jiadm(ic)+2*ix ) = iadyn
            endif
            if (( k .eq. idnom .or. k .eq. idlong .or. k .eq. idlono .or. k .eq. idluti )&
                .and. rnom(jrnom(ic)+ix)(25:26) .ne. '$$') then
                ipgc = -1
            endif
            call jjecrs(iadmi, ic, ix, 0, cel,&
                        imarq(jmarq(ic)+2*ix-1))
            ipgc = ipgcex
        endif
20  end do
!
    ix = iszon( jiszon + ibacol + iddeso )
    ltypi = ltyp ( jltyp(ic) + ix )
    lonoi = lono ( jlono(ic) + ix ) * ltypi
    if (lonoi .gt. 0) then
        iadmi = iadm ( jiadm(ic) + 2*ix-1 )
        iadyn = iadm ( jiadm(ic) + 2*ix )
        if (iadmi .ne. 0) then
            call jjecrs(iadmi, ic, ix, 0, cel,&
                        imarq(jmarq(ic)+2*ix-1))
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
