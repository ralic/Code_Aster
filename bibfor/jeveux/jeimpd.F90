subroutine jeimpd(unit, clas, cmess)
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
#include "asterfort/jjallc.h"
#include "asterfort/jjlide.h"
    integer :: unit
    character(len=*) :: clas, cmess
! ---------------------------------------------------------------------
! ROUTINE UTILISATEUR D'IMPRESSION DE LA LISTE DES OBJETS PRESENTS SUR
! LE FICHIER D'ACCES DIRECT ASSOCIE A UNE BASE
!
! IN  UNIT  : NUMERO D'UNITE LOGIQUE ASSOCIE AU FICHIER D'IMPRESSION
! IN  CLAS   : CLASSE ASSOCIEE A LA BASE ( ' ' : TOUTES LES CLASSES )
! IN  CMESS  : MESSAGE D'INFORMATION
! ---------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ---------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iacc, ibacol, ibiadd, iblono, iiadd
    integer :: ilono, iltyp, ixdeso, ixiadd, ixlono, j, jcara
    integer :: jdate, jdocu, jgenr, jhcod, jiacce, jiadd, jiadm
    integer :: jlong, jlono, jltyp, jluti, jmarq, jorig, jrnom
    integer :: jtype, k, kiadd, kj, koc, liadd, n
    integer :: nbacce, ncla1, ncla2, nmax
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
    character(len=8) :: nombas
    common /kbasje/  nombas(n)
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ---------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    common /jiacce/  jiacce(n),nbacce(2*n)
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
! ---------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idlono
    parameter    ( ivnmax = 0 , iddeso = 1 ,idiadd = 2  ,&
     &               idlono = 8   )
! ---------------------------------------------------------------------
    character(len=1) :: kclas, cgenr, ctype, clasi, cgen2
    character(len=32) :: crnom
    logical :: lcol, lente
    integer :: ipgcex, lgbl
! DEB -----------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -2
    lente = .true.
    kclas = clas ( 1: min(1,len(clas)))
    if (unit .le. 0) goto 9999
    if (kclas .eq. ' ') then
        ncla1 = 1
        ncla2 = index ( classe , '$' ) - 1
        if (ncla2 .lt. 0) ncla2 = n
    else
        ncla1 = index ( classe , kclas )
        ncla2 = ncla1
    endif
    do 10 i = ncla1, ncla2
        clasi = classe(i:i)
        if (clasi .ne. ' ') then
            write (unit,'(''1'',4A)' ) ('--------------------',k=1,4)
            write(unit,*)'                                  '
            write (unit,'(1X,2A)' )&
     &          '       CONTENU DE LA BASE ',clasi     ,&
     &          '        ', cmess(1:min(72,len(cmess)))
            write(unit,*)' NOM DE LA BASE               : ',nombas(i)
            write(unit,*)' NB D''ENREGISTREMENTS MAXIMUM : ',nblmax(i)
            lgbl=1024*longbl(i)*lois
            write(unit,*) ' LONGUEUR D''ENREGISTREMENT (OCTETS): ',&
            lgbl
            write(unit,*)'                                  '
            write (unit,'(    1X,4A)' ) ('--------------------',k=1,4)
            kj = 1
            do 5 j = 1, nremax(i)
                crnom = rnom(jrnom(i)+j)
                if (crnom(1:1) .eq. '?') goto 5
                if (mod(kj,25) .eq. 1 .and. lente) then
                    write ( unit , '(/,A,A/)' )&
     &     '---- NUM ------------- NOM ---------------- G T -L-'&
     &     ,' -LOTY- -IADD- --LIADD- NB AC'
                    lente = .false.
                endif
                cgenr = genr(jgenr(i)+j)
                ctype = type(jtype(i)+j)
                iltyp = ltyp(jltyp(i)+j)
                ilono = lono(jlono(i)+j)
                iiadd = iadd(jiadd(i)+2*j-1)
                if (iiadd .eq. 0) goto 6
                kj = kj + 1
                lente = .true.
                lcol = .false.
                liadd = iadd(jiadd(i)+2*j)
                iacc = iacce(jiacce(i)+iiadd)
                write(unit , 1001) j,crnom,cgenr,ctype,iltyp, ilono,&
                iiadd,liadd,iacc
 6              continue
                if (cgenr .eq. 'X') then
                    idatco = j
                    iclaco = i
                    lcol = .true.
                    call jjallc(i, j, 'L', ibacol)
                    ixiadd = iszon ( jiszon + ibacol + idiadd )
                    ixdeso = iszon ( jiszon + ibacol + iddeso )
                    if (ixiadd .eq. 0) goto 51
                    ixlono = iszon ( jiszon + ibacol + idlono )
                    nmax = iszon ( jiszon + ibacol + ivnmax )
                    cgen2 = genr(jgenr(i)+ixdeso)
                    ctype = type(jtype(i)+ixdeso)
                    iltyp = ltyp(jltyp(i)+ixdeso)
                    do 50 koc = 1, nmax
                        ibiadd = iadm ( jiadm(i) + 2*ixiadd-1 )
                        kiadd = iszon ( jiszon + ibiadd - 1 + 2*koc-1 )
                        if (kiadd .eq. 0) goto 50
                        if (mod(kj,25) .eq. 1 .and. lente) then
                            write ( unit , '(/,A,A/)' )&
     &            '---- NUM ------------- NOM -------------- E G T -L-'&
     &           ,' -LOTY- -IADD- --LIADD- NB AC'
                            lente = .false.
                        endif
                        iiadd = iszon ( jiszon + ibiadd - 1 + 2*koc-1 )
                        liadd = iszon ( jiszon + ibiadd - 1 + 2*koc )
                        iacc = iacce(jiacce(i)+iiadd)
                        if (ixlono .eq. 0) then
                            ilono = lono(jlono(i)+ixdeso)
                        else
                            iblono = iadm ( jiadm(i) + 2*ixlono-1 )
                            ilono = iszon ( jiszon + iblono - 1 + koc )
                        endif
                        kj = kj + 1
                        lente = .true.
                        write( crnom(25:32) , '(I8)' ) koc
                        write(unit,1001) j,crnom,cgen2,ctype,iltyp,&
                        ilono,iiadd,liadd,iacc
50                  continue
51                  continue
                    if (lcol) then
                        call jjlide('JEIMPO', crnom(1:24), 2)
                    endif
                endif
 5          continue
            write ( unit , '(/)' )
        endif
10  end do
9999  continue
    ipgc = ipgcex
    1001 format(i8,1x,a,'  -',2(a,'-'),i3,i7,i7,i9,i6)
! FIN -----------------------------------------------------------------
end subroutine
