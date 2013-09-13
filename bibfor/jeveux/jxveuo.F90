subroutine jxveuo(cel, itab, inat, jitab)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjalls.h"
#include "asterfort/jjecrs.h"
#include "asterfort/jjlirs.h"
#include "asterfort/jjprem.h"
#include "asterfort/jxliro.h"
#include "asterfort/jxlocs.h"
#include "asterfort/utmess.h"
    integer :: itab(*), inat, jitab
    character(len=*) :: cel
! ----------------------------------------------------------------------
! MISE EN MEMOIRE D'UN SEGMENT DE VALEUR
! ROUTINE AVEC ADHERENCE SYSTEME CRAY : SHIFTR AND
!
! IN  CEL    : ACCES : 'L' OU 'E'
! IN  ITAB   : TABLEAU PAR RAPPORT AUQUEL L'ADRESSE EST CALCULEE
! IN  INAT   : TYPE D'OBJET 1:OS, 2:CO, 3:OC
! OUT JITAB  : ADRESSE PAR RAPPORT A ITAB
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadyn, ibacol, ibiadd, ibiadm, iblong, iblono, ibluti
    integer :: ibmarq, ic, idco, idos, ista, iusa, ixdeso
    integer :: ixiadd, ixiadm, ixlong, ixlono, ixluti, ixmarq, jcara
    integer :: jdate, jdocu, jgenr, jhcod, jiadd, jiadm, jlong
    integer :: jlono, jltyp, jluti, jmarq, jorig, jrnom, jtype
    integer :: k, longj, lonoj, lonok, lonti, lutilo, n
!
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
! ----------------------------------------------------------------------
    integer :: idehc
    parameter      ( idehc=6)
! ----------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    integer :: istat
    common /istaje/  istat(4)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    character(len=1) :: typei, genri
    integer :: ltypi, iaddi(2), iadmi, lonoi, irt
    logical :: ldeps, lconst
! ----------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idmarq, idlong, idlono, idluti
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idmarq = 4  ,              idlong = 7 ,&
     &               idlono = 8 , idluti = 9  )
! DEB ------------------------------------------------------------------
    jitab = 0
    irt = 0
    goto ( 10 , 20 , 30 ) , inat
!
! ----- INAT =  1 : OBJET SIMPLE
!
10  continue
    ic = iclaos
    idos = idatos
    idco = 0
    ixdeso = idatos
    genri = genr ( jgenr(ic) + idos )
    typei = type ( jtype(ic) + idos )
    ltypi = ltyp ( jltyp(ic) + idos )
    lonoi = lono ( jlono(ic) + idos ) * ltypi
    iadmi = iadm ( jiadm(ic) + 2*idos-1 )
    iadyn = iadm ( jiadm(ic) + 2*idos )
    iaddi(1) = iadd ( jiadd(ic) + 2*idos-1 )
    iaddi(2) = iadd ( jiadd(ic) + 2*idos )
    goto 99
!
! ----- INAT = 2 : COLLECTION
!
20  continue
    ic = iclaco
    ibacol = iadm ( jiadm(ic) + 2*idatco-1 )
    ixdeso = iszon ( jiszon + ibacol + iddeso )
    idos = ixdeso
    idco = 0
    genri = genr ( jgenr(ic) + ixdeso )
    typei = type ( jtype(ic) + ixdeso )
    ltypi = ltyp ( jltyp(ic) + ixdeso )
    ixlong = iszon ( jiszon + ibacol + idlong )
    lconst = ( ixlong .eq. 0 )
    ixlono = iszon ( jiszon + ibacol + idlono )
    ixluti = iszon ( jiszon + ibacol + idluti )
    if (lconst) then
        if (long(jlong(ic)+ixdeso) .ne. 0) then
            lonoi = lono ( jlono(ic) + ixdeso ) * ltypi
        else
            call utmess('F', 'JEVEUX1_62')
        endif
    else
        iblono = iadm ( jiadm(ic) + 2*ixlono-1 )
        iblong = iadm ( jiadm(ic) + 2*ixlong-1 )
        lonti = lono ( jlono(ic) + ixdeso )
        lutilo = luti ( jluti(ic) + ixlono )
        if (lutilo .eq. 0) then
            k = 1
            iszon(jiszon+iblono-1+k) = 1
 5          continue
            if (k .le. iszon(jiszon + ibacol + ivnmax)) then
                longj = iszon(jiszon+iblong-1+k)
                if (longj .gt. 0) then
                    if (genri .eq. 'V') then
                        lonoj = longj
                    else if (genri .eq. 'N') then
                        lonok = (idehc+jjprem(longj,irt))*lois+(longj+ 1)*ltypi
                        if (mod(lonok,ltypi) .gt. 0) then
                            lonok = (lonok/ltypi + 1 )
                        else
                            lonok = lonok/ltypi
                        endif
                        lonoj = lonok
                        ibluti = iadm ( jiadm(ic) + 2*ixluti-1 )
                        iszon(jiszon+ibluti-1+k) = 0
                    endif
                else
                    lonoj = 0
                endif
                iszon(jiszon+iblono-1+k+1)=lonoj+iszon(jiszon+iblono-&
                1+k)
                k = k + 1
                lutilo = lutilo + 1
                goto 5
            endif
            luti ( jluti(ic) + ixlono ) = lutilo
        endif
        if (lonti .ne. 0) then
            lonoi = lonti * ltypi
        else
            lonti = iszon(jiszon+ iblono -1 + lutilo + 1 ) - 1
            lonoi = ltypi * lonti
            lono ( jlono(ic) + ixdeso ) = lonoi / ltypi
            luti ( jluti(ic) + ixdeso ) = lutilo
        endif
    endif
    iadmi = iadm ( jiadm(ic) + 2*ixdeso-1 )
    iadyn = iadm ( jiadm(ic) + 2*ixdeso )
    iaddi(1) = iadd ( jiadd(ic) + 2*ixdeso-1 )
    iaddi(2) = iadd ( jiadd(ic) + 2*ixdeso )
    if (iadmi .eq. 0) then
        if (iaddi(1) .ne. 0) then
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'NOINIT', itab, jitab, iadmi, iadyn)
            call jxliro(ic, iadmi, iaddi, lonoi)
        else
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'INIT  ', itab, jitab, iadmi, iadyn)
        endif
        iadm( jiadm(ic) + 2*ixdeso-1 ) = iadmi
        iadm( jiadm(ic) + 2*ixdeso ) = iadyn
        call jjecrs(iadmi, ic, ixdeso, 0, cel,&
                    imarq(jmarq(ic)+2*ixdeso- 1))
    endif
    goto 99
!
! ----- INAT = 3 : OBJET DE COLLECTION
!
30  continue
    ic = iclaco
    idco = idatco
    idos = idatoc
    ibacol = iadm ( jiadm(ic) + 2*idatco-1 )
    ixdeso = iszon ( jiszon + ibacol + iddeso )
    genri = genr(jgenr(ic)+ixdeso)
    typei = type(jtype(ic)+ixdeso)
    ltypi = ltyp(jltyp(ic)+ixdeso)
    ixlono = iszon ( jiszon + ibacol + idlono )
    if (ixlono .eq. 0) then
        lonoi = lono ( jlono(ic) + ixdeso ) * ltypi
    else
        iblono = iadm ( jiadm(ic) + 2*ixlono-1 )
        lonoi = iszon ( jiszon + iblono - 1 + idatoc ) * ltypi
    endif
    ixiadm = iszon ( jiszon + ibacol + idiadm )
    ixiadd = iszon ( jiszon + ibacol + idiadd )
    ixmarq = iszon ( jiszon + ibacol + idmarq )
    ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
    ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
    ibmarq = iadm ( jiadm(ic) + 2*ixmarq-1 )
    iadmi = iszon ( jiszon + ibiadm - 1 + 2*idatoc-1 )
    iadyn = iszon ( jiszon + ibiadm - 1 + 2*idatoc )
    iaddi(1) = iszon ( jiszon + ibiadd - 1 + 2*idatoc-1 )
    iaddi(2) = iszon ( jiszon + ibiadd - 1 + 2*idatoc )
99  continue
!
    if (iadmi .eq. 0) then
!
! ----- PAS DE SEGMENT EN MEMOIRE
!
        if (iaddi(1) .eq. 0) then
!
! ------- PAS D'IMAGE DISQUE
!
            if (cel .eq. 'E') then
                call jjalls(lonoi, ic, genri, typei, ltypi,&
                            'INIT', itab, jitab, iadmi, iadyn)
            else
                call utmess('F', 'JEVEUX1_61')
            endif
        else
!
! ------- AVEC  IMAGE DISQUE
!
            call jjalls(lonoi, ic, genri, typei, ltypi,&
                        'NOINIT', itab, jitab, iadmi, iadyn)
            call jxliro(ic, iadmi, iaddi, lonoi)
        endif
    else
!
! ----- SEGMENT EN MEMOIRE
!
        call jjlirs(iadmi, ic, idos, iusa, ista)
        ldeps = .false.
        if (iusa .ne. istat(2)) ldeps = .true.
        call jxlocs(itab, genri, ltypi, lonoi, iadmi,&
                    ldeps, jitab)
    endif
!
    if (inat .eq. 3) then
        iszon ( jiszon + ibiadm - 1 + 2*idatoc-1 ) = iadmi
        iszon ( jiszon + ibiadm - 1 + 2*idatoc ) = iadyn
        iszon ( jiszon + ibiadd - 1 + 2*idatoc-1 ) = iaddi(1)
        iszon ( jiszon + ibiadd - 1 + 2*idatoc ) = iaddi(2)
        call jjecrs(iadmi, ic, idos, idco, cel,&
                    iszon (jiszon+ibmarq- 1+2*idatoc-1))
    else
        iadm( jiadm(ic) + 2*ixdeso-1 ) = iadmi
        iadm( jiadm(ic) + 2*ixdeso ) = iadyn
        iadd( jiadd(ic) + 2*ixdeso-1 ) = iaddi(1)
        iadd( jiadd(ic) + 2*ixdeso ) = iaddi(2)
        call jjecrs(iadmi, ic, idos, 0, cel,&
                    imarq(jmarq(ic)+2*idos-1))
    endif
! FIN ------------------------------------------------------------------
end subroutine
