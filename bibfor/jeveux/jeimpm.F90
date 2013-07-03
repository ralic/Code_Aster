subroutine jeimpm(unit)
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
    implicit none
#include "jeveux_private.h"
#include "asterfort/assert.h"
#include "asterfort/jxveri.h"
    integer :: unit
! ----------------------------------------------------------------------
! IMPRIME LA SEGMENTATION DE LA MEMOIRE
!
! IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmi, iadmoc, iadyn, iadyoc, ibacol, ibiadm, ibmarq
    integer :: ic, idco, idm, il, im, isd, isf
    integer :: ixiadm, ixmarq, j, jcara, jdate, jdocu, jgenr
    integer :: jhcod, jiadd, jiadm, jlong, jlono, jltyp, jluti
    integer :: jmarq, jorig, jrnom, jtype, n, ncla1, ncla2
    integer :: nmax
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
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    integer :: isstat
    common /iconje/  isstat
    character(len=4) :: kstat
    common /kstaje/  kstat
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    integer :: ivnmax, idiadm, idmarq
    parameter    ( ivnmax = 0   , idiadm = 3 ,&
     &               idmarq = 4   )
! ----------------------------------------------------------------------
    character(len=32) :: nom32
    character(len=8) :: nom8
    character(len=1) :: cla, cgenr
    integer :: k
    real(kind=8) :: vusta, vudyn, vxsta, vxdyn
! DEB ------------------------------------------------------------------
!
    if (unit .le. 0) goto 9999
    vusta = 0.d0
    vudyn = 0.d0
    vxsta = 0.d0
    vxdyn = 0.d0
!
!     ON LISTE MAINTENANT LES OBJETS ALLOUES DYNAMIQUEMENT
!
    write (unit,'(4A)' ) ('--------------------',k=1,4)
    write (unit,'(A)') 'OBJETS ALLOUES DYNAMIQUEMENT '
    write (unit,'(4A)' ) ('--------------------',k=1,4)
    write (unit,'(/A,A/)')&
     &      ' CL-  --NUM-- -MA-  ---------IADY--------- -U- - LON UA',&
     &      ' -  -S- ------------- NOM --------------'
    ncla1 = 1
    ncla2 = index ( classe , '$' ) - 1
    if (ncla2 .lt. 0) ncla2 = n
    do 200 ic = ncla1, ncla2
        cla = classe(ic:ic)
        do 205 j = 1, nremax(ic)
            idco = 0
            iadmi = iadm(jiadm(ic)+2*j-1)
            iadyn = iadm(jiadm(ic)+2*j )
            cgenr = genr(jgenr(ic)+j)
            nom32 = rnom(jrnom(ic)+j)
            if (nom32 .eq. ' ' .or. nom32 .eq. '?') goto 205
            if (iadyn .ne. 0) then
                idm = iadmi - 4
                im = imarq(jmarq(ic)+2*j-1)
                il = iszon(jiszon+idm) - 8 - idm
                isd = iszon(jiszon + idm + 3) / isstat
                call assert((isd.gt.0) .and. (isd.lt.5))
                isf = iszon(jiszon + iszon(jiszon+idm) - 4) / isstat
                call assert((isf.gt.0) .and. (isf.lt.5))
                write(unit,&
     &        '(''|'',A1,''|'',I4,''|'',I8,''|'',I4,''|'','//&
     &        'I20,''|'',A1,''|'',I11,''| '',A1,''| '',A)')&
     &        cla,idco,j,im,iadyn,kstat(isd:isd),il,kstat(isf:isf),nom32
                if (isd .eq. 2) then
                    vudyn = vudyn + iszon(jiszon+idm) - idm + 1
                else
                    vxdyn = vxdyn + iszon(jiszon+idm) - idm + 1
                endif
            endif
            if (cgenr .eq. 'X' .and. iadmi .ne. 0) then
                nom32 = rnom(jrnom(ic)+j)(1:24)
                ibacol = iadmi
                ixiadm = iszon ( jiszon + ibacol + idiadm )
                ixmarq = iszon ( jiszon + ibacol + idmarq )
                nmax = iszon ( jiszon + ibacol + ivnmax )
                if (ixiadm .gt. 0) then
                    ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                    ibmarq = iadm ( jiadm(ic) + 2*ixmarq-1 )
                    if (ibiadm .ne. 0) then
                        do 210 k = 1, nmax
                            iadmoc = iszon(jiszon + ibiadm - 1 +2*k-1)
                            iadyoc = iszon(jiszon + ibiadm - 1 +2*k )
                            if (iadyoc .ne. 0) then
                                idm = iadmoc - 4
                                im = iszon(jiszon + ibmarq - 1 + 2*k- 1)
                                il = iszon(jiszon+idm) - 8 - idm
                                isd = iszon(jiszon + idm + 3) / isstat
                                isf = iszon(jiszon + iszon(jiszon+idm) - 4 )/isstat
                                write(nom8,'(I8)') k
                                nom32 = rnom(jrnom(ic)+j)(1:24)//nom8
                                write(unit,&
     &              '(''|'',A1,''|'',I4,''|'',I8,''|'',I4,''|'','//&
     &              'I20,''|'',A1,''|'',I11,''| '',A1,''| '',A)')&
     &              cla,j,k,im,iadyoc,kstat(isd:isd),il,&
     &              kstat(isf:isf),nom32
                                if (isd .eq. 2) then
                                    vudyn = vudyn + iszon(jiszon+idm) - idm + 1
                                else
                                    vxdyn = vxdyn + iszon(jiszon+idm) - idm + 1
                                endif
                            endif
210                      continue
                    endif
                endif
            endif
205      continue
200  end do
!
    write(unit,*) '  '
    write(unit,*) ' CUMUL DES LONGUEURS DES SEGMENTS UTILISES UA/UD'
    write(unit,60) ' ALLOCATION STATIQUE  :',vusta*lois/(1024*1024),&
     &                ' Mo'
    write(unit,60) ' ALLOCATION DYNAMIQUE :',vudyn*lois/(1024*1024),&
     &                ' Mo'
    write(unit,60) ' ALLOCATION TOTALE    :',(vusta+vudyn)*lois&
     &                /(1024*1024),' Mo',(vusta+vudyn)*lois,' o '
    write(unit,*) '  '
    write(unit,*) ' CUMUL DES LONGUEURS DES SEGMENTS DECHARGEABLES'&
     &                //' XA/XD'
    write(unit,60) ' ALLOCATION STATIQUE  :',vxsta*lois/(1024*1024),&
     &                ' Mo'
    write(unit,60) ' ALLOCATION DYNAMIQUE :',vxdyn*lois/(1024*1024),&
     &                ' Mo'
    write(unit,60) ' ALLOCATION TOTALE    :',(vxsta+vxdyn)*lois&
     &                /(1024*1024),' Mo',(vxsta+vxdyn)*lois,' o '
    write(unit,*) '  '
    write(unit,60) ' ESPACE MEMOIRE JEVEUX OCCUPE    :',&
     &  (vusta+vudyn+vxsta+vxdyn)*lois/(1024*1024),' Mo',&
     &  (vusta+vudyn+vxsta+vxdyn)*lois,' o '
!
    call jxveri()
!
    60 format(a,2(1pe12.2,a3))
9999  continue
! FIN ------------------------------------------------------------------
end subroutine
