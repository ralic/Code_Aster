subroutine jeimhd(fichdf, clas)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux_private.h"
#include "asterc/gtoptk.h"
#include "asterc/hdfclf.h"
#include "asterc/hdfclg.h"
#include "asterc/hdfcrf.h"
#include "asterc/hdfcrg.h"
#include "asterc/hdfopg.h"
#include "asterc/hdfwat.h"
#include "asterfort/enlird.h"
#include "asterfort/iunifi.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjalty.h"
#include "asterfort/jjimhd.h"
#include "asterfort/jjlide.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: fichdf, clas
! ----------------------------------------------------------------------
! IMPRESSION DE L'ENSEMBLE DES OBJETS JEVEUX AU FORMAT HDF
!
! IN  FICHDF : NOM LOCAL DU FICHIER HDF UTILISE POUR L'IMPRESSION
! IN  CLAS   : NOM DE LA CLASSE ASSOCIEE CR
!
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibacol, ibiadd, ibiadm, iblono, ibnom, ideco, idenom
    integer :: inat, inat0, ipgcex, iret, ix, ixdeso
    integer :: ixiadd, ixiadm, ixlono, ixnom, j, jcara, jdate
    integer :: jdocu, jgenr, jhcod, jiadd, jiadm, jlong, jlono
    integer :: jltyp, jluti, jmarq, jorig, jrnom, jtype, k
    integer :: l, lnom, loua, n
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
!
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
    integer :: numec
    common /inumje/  numec
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: ifnivo, nivo
    common /jvnivo/  ifnivo, nivo
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
! ----------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    character(len=1) :: kclas, genri, typei, clasi
    character(len=8) :: k8
    character(len=16) :: k16
    character(len=24) :: kattr(5), nomatr, kattrc(5), nomatc
    parameter      ( nomatr = 'BASE GLOBALE JEVEUX' )
    parameter      ( nomatc = 'ATTRIBUTS JEVEUX' )
    character(len=32) :: crnom, ngrp, nomcol
    character(len=80) :: nhdf
    real(kind=8) :: rbid
    integer :: ic, julist, ltypi, ilong, lonoi, iaddi(2), iadmi, iadmx
    integer :: idfic, nbmax, jctab, idg, idgc
    integer :: lidbas
    parameter      ( lidbas = 20 )
! ----------------------------------------------------------------------
    integer :: ideno, ilnom
    parameter      ( ideno=2,ilnom=3)
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idnom, idlono, idnum
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idnom  = 5 ,&
     &               idlono = 8 , idnum  = 10 )
! DEB ------------------------------------------------------------------
    ipgcex = ipgc
    ipgc = -2
    kclas = clas
    nhdf = fichdf
    idfic = hdfcrf (nhdf)
    if (idfic .lt. 0) then
        call u2mesg('F', 'JEVEUX_66', 1, nhdf, 1,&
                    idfic, 0, rbid)
    endif
    ngrp ='/'
    idg = hdfopg (idfic,ngrp)
    k8 = ' '
    k16 = ' '
    call gtoptk('date', k16, iret)
    call gtoptk('versionD0', k8, iret)
    kattr(1) = k16//k8
    call gtoptk('hostname', kattr(2), iret)
    call gtoptk('system', kattr(3), iret)
!
    call enlird(kattr(4))
    loua = 1
    write(kattr(5),'(''LBIS='',I2,'' LOIS='',I2,'' LOUA='',I2)')&
     &                   lbis, lois, loua
    iret = hdfwat (idg,nomatr,5,kattr)
    iret = hdfclg (idg)
!
    ic = index ( classe , kclas)
    clasi = classe(ic:ic)
    julist = iunifi ('MESSAGE')
    if (clasi .ne. ' ') then
        if (nivo .ge. 2) then
            write(julist ,*) '<INFO> IMPRESSION DE LA BASE '//kclas//&
            ' AU FORMAT HDF DANS LE FICHIER '//nhdf
        endif
!       ----------- ACTUALISER CARA
!
        cara(jcara(ic)+1) = nreuti(ic)
        cara(jcara(ic)+3) = nblmax(ic)
        do 5 j = 1, nreuti(ic)
            crnom = rnom(jrnom(ic)+j)
            do 1 k = 1, 5
                kattr(k)=' '
 1          continue
            kattr(3)(1:1)=kclas
            if (j .gt. lidbas .and. (crnom(25:26) .eq. '$$' .or. crnom(1:1) .eq. '?')) &
            goto 5
            genri = genr(jgenr(ic)+j)
            typei = type(jtype(ic)+j)
            ltypi = ltyp(jltyp(ic)+j)
            ilong = long(jlong(ic)+j)
            lonoi = lono(jlono(ic)+j)*ltypi
            iaddi(1) = iadd(jiadd(ic)+2*j-1)
            iaddi(2) = iadd(jiadd(ic)+2*j )
            iadmi = iadm(jiadm(ic)+2*j-1)
            iadmx = iadmi
            if (genri .ne. 'X') then
!           ON TRAITE UN OBJET SIMPLE
                idatos = j
                iclaos = ic
                nomos = crnom
                inat = 1
                inat0 = 1
                if (j .le. lidbas) inat0 = 0
                if (iadmx .eq. 0) then
                    if (iaddi(1) .eq. 0 .or. lonoi .eq. 0) then
                        if (nivo .ge. 2) then
                            call u2mesk('A', 'JEVEUX_27', 1, crnom)
                        endif
                        goto 5
                    endif
                    call jjalty(typei, ltypi, 'L', 1, jctab)
                    iadmi = iadm ( jiadm(ic) + 2*j-1 )
                endif
                write(kattr(2),'(16X,I8)') j
                call jjimhd(idfic, inat0, crnom, ngrp, kattr,&
                            iadmi, genri, typei, ltypi, lonoi)
                if (nivo .ge. 2) then
                    if (mod(j,25) .eq. 1) then
                        write ( julist , '(/,A,A/)' )&
     &       ' NUM  ------------- NOM ---------------- G T L- --LONG---'&
     &       ,' -LOTY- -IADD- ------ --KADM--'
                    endif
                    write(julist , 1001) j,crnom,genri,typei,ltypi,&
                    ilong,lonoi,iaddi(1),iaddi(2),iadmi
                endif
                if (iadmx .eq. 0) then
                    call jjlide('JEIMPO', crnom, inat)
                endif
            else
!         ON TRAITE UNE COLLECTION
                idatco = j
                iclaco = ic
                nomco = crnom(1:24)
                nomcol = crnom
                inat = 2
                call jjallc(ic, j, 'L', ibacol)
                typei = type( jtype(ic) + j )
                ltypi = ltyp( jltyp(ic) + j )
                lonoi = lono( jlono(ic) + j ) * ltypi
                write(kattr(2),'(16X,I8)') j
                call jjimhd(idfic, -1, crnom, ngrp, kattr,&
                            ibacol, genri, typei, ltypi, lonoi)
                ixiadd = iszon ( jiszon + ibacol + idiadd )
                ixdeso = iszon ( jiszon + ibacol + iddeso )
                genri = genr( jgenr(ic) + ixdeso )
                ltypi = ltyp( jltyp(ic) + ixdeso )
                if (ixiadd .eq. 0) then
!             ON TRAITE UNE COLLECTION CONTIGUE :
!                TRAITEMENT PARTICULIER DU $$DESO
                    typei = type( jtype(ic) + ixdeso )
                    ltypi = ltyp( jltyp(ic) + ixdeso )
                    lonoi = lono( jlono(ic) + ixdeso ) * ltypi
                    iadmi = iadm ( jiadm(ic) + 2*ixdeso-1 )
                    iaddi(1) = iadd ( jiadd(ic) + 2*ixdeso-1 )
                    iaddi(2) = iadd ( jiadd(ic) + 2*ixdeso )
                    iadmx = iadmi
                    if (iadmx .eq. 0) then
                        if (iaddi(1) .eq. 0) then
                            if (nivo .ge. 2) then
                                call u2mesk('A', 'JEVEUX_28', 1, nomcol( 1:24))
                            endif
                            goto 5
                        endif
                        call jjalty(typei, ltypi, 'L', 2, jctab)
                        iadmi = iadm ( jiadm(ic) + 2*ixdeso-1 )
                    endif
                    crnom = rnom( jrnom(ic) + ixdeso )
                    write(kattr(2),'(16X,I8)') ixdeso
                    call jjimhd(idfic, inat, crnom, ngrp, kattr,&
                                iadmi, genri, typei, ltypi, lonoi)
                else
!             ON TRAITE UNE COLLECTION DISPERSEE
!                TRAITEMENT PARTICULIER DES OBJETS DE COLLECTION
                    inat = 3
                    ixiadm = iszon ( jiszon + ibacol + idiadm )
                    ibiadm = iadm ( jiadm(ic) + 2*ixiadm-1 )
                    ibiadd = iadm ( jiadm(ic) + 2*ixiadd-1 )
                    nbmax= iszon (jiszon + ibacol + ivnmax )
                    if (nbmax .gt. 0) then
                        nomcol(25:32) = '__OBJETS'
                        iret = hdfcrg(idfic,ngrp,nomcol)
                        idgc = hdfopg(idfic,nomcol)
                        kattrc(1)='COLLECTION'
                        kattrc(2)=' '
                        kattrc(3)=' '
                        kattrc(4)=' '
                        kattrc(5)=' '
                        iret = hdfwat(idgc,nomatc,5,kattrc)
                    endif
                    do 10 k = 1, nbmax
                        iadmi = iszon(jiszon + ibiadm - 1 + 2*k-1 )
                        idatoc = k
                        if (iadmi .eq. 0) then
                            iaddi(1) = iszon(jiszon + ibiadd - 1 + 2* k-1 )
                            iaddi(2) = iszon(jiszon + ibiadd - 1 + 2* k )
                            if (iaddi(1) .eq. 0) then
                                if (nivo .ge. 2) then
                                    call u2mesg('A', 'JEVEUX_29', 1, nomcol(1:24), 1,&
                                                k, 0, rbid)
                                endif
                                goto 10
                            endif
                            call jjalty(typei, ltypi, 'L', inat, jctab)
                            iadmi = iszon(jiszon + ibiadm - 1 + 2*k-1 )
                        endif
                        ixlono = iszon ( jiszon + ibacol + idlono )
                        if (ixlono .eq. 0) then
                            lonoi = lono ( jlono(ic) + ixdeso ) * ltypi
                        else
                            iblono = iadm ( jiadm(ic) + 2*ixlono-1 )
                            lonoi = iszon ( jiszon + iblono - 1 + k ) * ltypi
                        endif
                        ixnom = iszon ( jiszon + ibacol + idnom )
                        if (ixnom .gt. 0) then
                            ibnom = iadm ( jiadm(ic) + 2*ixnom-1 )
                            idenom = iszon ( jiszon + ibnom - 1 + ideno )
                            lnom = iszon ( jiszon + ibnom - 1 + ilnom )
                            ideco = (ibnom - 1) * lois + idenom + lnom * (k - 1)
                            do 12 l = 1, min (24, lnom)
                                kattr(2)(l:l) = k1zon ( jk1zon + ideco + l )
12                          continue
                        else
                            write(kattr(2),'(16X,I8)') k
                        endif
                        write(crnom(25:32),'(I8)') k
                        call jjimhd(idfic, inat, crnom, nomcol, kattr,&
                                    iadmi, genri, typei, ltypi, lonoi)
                        numec = k
                        call jjlide('JEIMPO', nomco(1:24)//'$$XNUM  ', 2)
10                  continue
                    if (idgc .gt. 0) iret = hdfclg(idgc)
                endif
!                TRAITEMENT DES OBJETS SYSTEME DE COLLECTION
                inat = 2
                do 20 k = idiadd, idnum
                    ix = iszon( jiszon + ibacol + k )
                    if (ix .gt. 0) then
                        iadmi = iadm (jiadm(ic) + 2*ix-1)
                        if (iadmi .ne. 0) then
                            genri = genr(jgenr(ic)+ix)
                            typei = type(jtype(ic)+ix)
                            ltypi = ltyp(jltyp(ic)+ix)
                            ilong = long(jlong(ic)+ix)
                            lonoi = lono(jlono(ic)+ix)*ltypi
                            crnom = rnom(jrnom(ic)+ix)
!
! LES POINTEURS PARTAGES SONT TRAITES AVEC LES OBJETS SIMPLES
!
                            if (crnom(1:24) .ne. nomcol(1:24)) goto 20
                            iaddi(1) = iadd (jiadd(ic)+2*ix-1 )
                            iaddi(2) = iadd (jiadd(ic)+2*ix )
                            write(kattr(2),'(16X,I8)') ix
                            call jjimhd(idfic, inat, crnom, ngrp, kattr,&
                                        iadmi, genri, typei, ltypi, lonoi)
                            if (nivo .ge. 2) then
                                write(julist , 1001) j,crnom,genri,&
                                typei,ltypi, ilong,lonoi,iaddi(1),&
                                iaddi(2),iadmi
                            endif
                        endif
                    endif
20              continue
                call jjlide('JEIMPO', nomco, 2)
            endif
 5      continue
    endif
    1001 format(i5,1x,a,'  -',2(a,'-'),i2,1x,i8,1x,i7,i7,i7,i9)
    iret = hdfclf (idfic)
    if (iret .ne. 0) then
        call u2mesk('F', 'JEVEUX_55', 1, nhdf)
    endif
    ipgc = ipgcex
! FIN ------------------------------------------------------------------
end subroutine
