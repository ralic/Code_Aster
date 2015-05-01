subroutine jeimpr(unit, clas, cmess)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: unit
    character(len=*) :: clas, cmess
! ----------------------------------------------------------------------
! IMPRESSION DU REPERTOIRE D'UNE OU PLUSIEURS CLASSES
!
! IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
! IN  CLAS   : NOM DE LA CLASSE ASSOCIEE ( ' ' POUR TOUTES LES CLASSES )
! IN  CMESS  : MESSAGE D'INFORMATION
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iiadd, iibas, iibdy, ilong, ilono, iltyp
    integer :: j, jcara, jdate, jdocu, jgenr, jhcod, jiadd
    integer :: jiadm, jlong, jlono, jltyp, jluti, jmarq, jorig
    integer :: jrnom, jtype, k, n, ncla1, ncla2
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
! ----------------------------------------------------------------------
    character(len=1) :: kclas, cgenr, ctype, clasi
    character(len=32) :: crnom
! DEB ------------------------------------------------------------------
!
    kclas = clas ( 1: min(1,len(clas)))
    if (unit .le. 0) goto 9999
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas)
        ncla2 = ncla1
    endif
    do 10 i = ncla1, ncla2
        clasi = classe(i:i)
        if (clasi .ne. ' ') then
            write (unit,'(4A)' ) ('---------------------',k=1,4)
            write (unit,'(2A)' )&
     &          '------     CATALOGUE CLASSE ',clasi     ,&
     &          '------    ', cmess(1:min(72,len(cmess)))
            write (unit,'(4A)' ) ('---------------------',k=1,4)
            do 5 j = 1, nremax(i)
                crnom = rnom(jrnom(i)+j)
                if (crnom(1:1) .eq. '?') goto 5
                if (mod(j,25) .eq. 1) then
                    write ( unit , '(/,A,A/)' )&
     &      '--- NUM  -------------- NOM ---------------- G T L- --LONG'&
     &      ,'--- -LOTY- -IADD- --------KADM------- --------KDYN-------'
                endif
                cgenr = genr(jgenr(i)+j)
                ctype = type(jtype(i)+j)
                iltyp = ltyp(jltyp(i)+j)
                ilong = long(jlong(i)+j)
                ilono = lono(jlono(i)+j)
                iiadd = iadd(jiadd(i)+2*j-1)
                iibas = iadm(jiadm(i)+2*j-1)
                iibdy = iadm(jiadm(i)+2*j )
                write(unit , 1001) j,crnom,cgenr,ctype,iltyp, ilong,&
                ilono,iiadd,iibas,iibdy
 5          continue
            write ( unit , '(/)' )
        endif
10  end do
9999  continue
    1001 format(i8,2x,a,'  -',2(a,'-'),i2,1x,i8,1x,i7,i7,i20,i20)
! FIN ------------------------------------------------------------------
end subroutine
