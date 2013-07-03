subroutine jxveri()
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
! ----------------------------------------------------------------------
! VERIFIE L'INTEGRITE DU CHAINAGE AVANT DES SEGMENTS DE VALEURS ET DE LA
! ZONE MEMOIRE UTILISEE
! ----------------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjlide.h"
#include "asterfort/jjvern.h"
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: isstat
    common /iconje/  isstat
    integer :: istat
    common /istaje/  istat(4)
!-----------------------------------------------------------------------
    integer :: iadmi, iadmoc, iadyn, iadyoc, ibacol, ibiadm, ic
    integer :: idm, il, iret, isd, isdc, isf
    integer :: ixiadm, j, jcara, jdate, jdocu
    integer :: jgenr, jhcod, jiadd, jiadm, jlong, jlono, jltyp
    integer :: jluti, jmarq, jorig, jrnom, jtype, k, n
    integer :: ncla1, ncla2, nmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: ivnmax, idiadm
    parameter    ( ivnmax = 0   , idiadm = 3 )
! ----------------------------------------------------------------------
    character(len=32) :: nom32
    character(len=1) :: cgenr
! DEB ------------------------------------------------------------------
!
    nom32 = '??'
!
!     ON TRAITE LES OBJETS ALLOUES EN MEMOIRE DYNAMIQUE
!
    if (ldyn .ne. 1 .and. ldyn .ne. 2) goto 300
    ncla1 = 1
    ncla2 = index ( classe , '$' ) - 1
    if (ncla2 .lt. 0) ncla2 = n
    do 200 ic = ncla2, ncla1, - 1
        do 205 j = 1, nremax(ic)
            iadmi = iadm(jiadm(ic)+2*j-1)
            iadyn = iadm(jiadm(ic)+2*j )
            if (iadmi .eq. 0 .or. iadyn .eq. 0) goto 205
            cgenr = genr(jgenr(ic)+j)
            nom32 = rnom(jrnom(ic)+j)
!
            isdc = iszon(jiszon + iadmi - 1) / isstat
            call assert(isdc.eq.1 .or. isdc.eq.2)
            if (cgenr .eq. 'X' .and. isdc .eq. 2) then
                call jjvern(nom32, 0, iret)
                call jjallc(ic, j, 'L', ibacol)
                ixiadm = iszon ( jiszon + ibacol + idiadm )
                nmax = iszon ( jiszon + ibacol + ivnmax )
                if (ixiadm .gt. 0) then
                    ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                    do 210 k = 1, nmax
                        iadmoc = iszon(jiszon + ibiadm - 1 +2*k-1)
                        iadyoc = iszon(jiszon + ibiadm - 1 +2*k )
                        if (iadyoc .ne. 0) then
                            idm = iadmoc - 4
                            isd = iszon(jiszon + idm + 3) / isstat
                            call assert(isd.eq.1 .or. isd.eq.2)
                            isf = iszon(jiszon + iszon(jiszon+idm) - 4 ) / isstat
                            call assert(isf.eq.3 .or. isf.eq.4)
                            il = iszon(jiszon+idm) - 8 - idm
                            call assert(il .gt. 0)
                        endif
210                  continue
                endif
                call jjlide('JEIMPO', nom32(1:24), 2)
                goto 205
            else
                idm = iadmi - 4
                isd = iszon(jiszon + idm + 3) / isstat
                call assert(isd.eq.1 .or. isd.eq.2)
                isf = iszon(jiszon + iszon(jiszon+idm) - 4) / isstat
                call assert(isf.eq.3 .or. isf.eq.4)
                il = iszon(jiszon+idm) - 8 - idm
                call assert(il .gt. 0)
            endif
205      continue
200  end do
!
300  continue
! FIN ------------------------------------------------------------------
end subroutine
