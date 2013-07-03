subroutine jjlchd(id, ic, idfic, idts, ngrp)
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
! LECTURE SUR FICHIER HDF D'UNE COLLECTION PUIS LIBERATION
!
! IN  ID    : IDENTIFICATEUR DE COLLECTION
! IN  IC    : CLASSE ASSOCIEE
! IN  IDFIC : IDENTIFICATEUR DU FICHIER HDF
! IN  IDTS  : IDENTIFICATEUR DU DATASET ASSOCIE A LA COLLECTION
! IN  NGRP  : NOM DU GROUPE CONTENANT LE DATASET IDTS
!
! ----------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux_private.h"
#include "asterc/hdfcld.h"
#include "asterc/hdfclg.h"
#include "asterc/hdfnbo.h"
#include "asterc/hdfopd.h"
#include "asterc/hdfopg.h"
#include "asterc/hdfrsv.h"
#include "asterc/hdftsd.h"
#include "asterfort/assert.h"
#include "asterfort/jjalls.h"
#include "asterfort/jjecrs.h"
#include "asterfort/jjhrsv.h"
#include "asterfort/jjlide.h"
#include "asterfort/jjlihd.h"
#include "asterfort/u2mesk.h"
    integer :: id, ic, idfic, idts
    character(len=*) :: ngrp
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
! ----------------------------------------------------------------------
    integer :: n
    parameter  ( n = 5 )
    integer :: jltyp, jlong, jdate, jiadd, jiadm, jlono, jhcod, jcara, jluti
    integer :: jmarq
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
    integer :: jgenr, jtype, jdocu, jorig, jrnom
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
! ----------------------------------------------------------------------
    integer :: istat
    common /istaje/  istat(4)
    integer :: ipgc, kdesma(2), lgd, lgduti, kposma(2), lgp, lgputi
    common /iadmje/  ipgc,kdesma,   lgd,lgduti,kposma,   lgp,lgputi
!
    integer :: numec
    common /inumje/  numec
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
! ----------------------------------------------------------------------
    integer :: iddeso, idiadd, idiadm, idmarq, idlono, idnum
    parameter    (  iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idmarq = 4   ,&
     &               idlono = 8  , idnum  = 10 )
!     ------------------------------------------------------------------
    integer :: ilorep, ideno, idehc
    parameter      ( ilorep=1,ideno=2,idehc=6)
!     ------------------------------------------------------------------
    character(len=32) :: nomo, ngrc, d32
    character(len=8) :: nrep(2)
    character(len=1) :: genri, typei, typeb
    integer :: itab(1), ida, jctab, nbob, ido, idgr, iconv
    integer :: iadmi, ltypi, lonoi, ltypb, lon
    integer :: ibacol, iret, k, ix, ixiadd, ixiadm, ixmarq, ixdeso, idgc
    integer :: ibiadm, ibmarq, iblono, idt1, idt2, nbval, kitab, ixlono
    integer :: iadyn
    data             nrep / 'T_HCOD' , 'T_NOM' /
    data             d32 /'$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$'/
! DEB ------------------------------------------------------------------
    iconv = 0
    iclas = ic
    iclaco = ic
    idatco = id
    nomos = d32
    nomco = rnom(jrnom(ic)+id)(1:24)
    nomoc = d32
    genri = genr (jgenr(ic) + id)
    typei = type (jtype(ic) + id)
    ltypi = ltyp (jltyp(ic) + id)
    lon = lono (jlono(ic) + id)
    lonoi = lon * ltypi
    iadm (jiadm(ic) + 2*id-1) = 0
    iadm (jiadm(ic) + 2*id  ) = 0
    iadd (jiadd(ic) + 2*id-1) = 0
    iadd (jiadd(ic) + 2*id  ) = 0
    ltypb = 0
    typeb = ' '
! ------- OBJET CONTENANT LES IDENTIFICATEURS DE LA COLLECTION
    call jjlihd(idts, lon, lonoi, genri, typei,&
                ltypi, ic, id, 0, imarq(jmarq(ic)+2*id-1),&
                ibacol, iadyn)
    iadm (jiadm(ic)+2*id-1) = ibacol
    iadm (jiadm(ic)+2*id  ) = iadyn
!
    do 20 k = idiadd, idnum
!     ----------- OBJETS ATTRIBUTS DE COLLECTION
        ix = iszon(jiszon + ibacol + k)
        if (ix .gt. 0) then
            genri = genr (jgenr(ic) + ix)
            nomo = rnom (jrnom(ic) + ix)
            typei = type (jtype(ic) + ix)
            ltypi = ltyp (jltyp(ic) + ix)
            lon = lono (jlono(ic) + ix)
            lonoi = lon * ltypi
            iadd (jiadd(ic) + 2*ix-1) = 0
            iadd (jiadd(ic) + 2*ix ) = 0
            if (genri .ne. 'N') then
                ida = hdfopd(idfic,ngrp,nomo)
                if (ida .lt. 0) then
                    call u2mesk('F', 'JEVEUX1_52', 1, nomo)
                endif
                iadmi = 0
                if (k .eq. idiadm .or. k .eq. idmarq .or. k .eq. idiadd) then
! --------- MISE EN MEMOIRE SANS LECTURE SUR FICHIER HDF
                    call jjalls(lonoi, ic, genri, typei, ltypi,&
                                'INIT', itab, jctab, iadmi, iadyn)
                    call jjecrs(iadmi, ic, ix, 0, 'E',&
                                imarq(jmarq(ic)+2* ix-1))
                else
! --------- MISE EN MEMOIRE AVEC LECTURE DISQUE SUR FICHIER HDF
                    call jjlihd(ida, lon, lonoi, genri, typei,&
                                ltypi, ic, ix, 0, imarq(jmarq(ic)+2*ix-1),&
                                iadmi, iadyn)
                endif
                iadm(jiadm(ic)+2*ix-1) = iadmi
                iadm(jiadm(ic)+2*ix ) = iadyn
                iret = hdfcld(ida)
                call assert(iret .eq. 0)
            else
!-------- ON TRAITE UN REPERTOIRE DE NOMS
                idgr=hdfopg(idfic,nomo)
                idt1=hdfopd(idfic,nomo,nrep(1))
                idt2=hdfopd(idfic,nomo,nrep(2))
                call jjalls(lonoi, ic, genri, typei, ltypi,&
                            'INIT', itab, jctab, iadmi, iadyn)
                call jjecrs(iadmi, ic, ix, 0, 'E',&
                            imarq(jmarq(ic)+2*ix-1))
                iret=hdftsd(idt1,typeb,ltypb,nbval)
                call assert(iret .eq. 0)
                call jjhrsv(idt1, nbval, iadmi)
!
!           ON AJUSTE LA POSITION DES NOMS EN FONCTION DU TYPE D'ENTIER
!
                iszon(jiszon+iadmi-1+ideno)= (idehc+iszon(jiszon+&
                iadmi-1+ilorep))*lois
                iret=hdftsd(idt2,typeb,ltypb,nbval)
                call assert(iret .eq. 0)
                kitab=jk1zon+(iadmi-1)*lois+iszon(jiszon+iadmi-1+&
                ideno)+1
                iret=hdfrsv(idt2,nbval,k1zon(kitab),iconv)
                call assert(iret .eq. 0)
                iret=hdfclg(idgr)
                call assert(iret .eq. 0)
                iadm(jiadm(ic)+2*ix-1) = iadmi
                iadm(jiadm(ic)+2*ix ) = iadyn
                iret = hdfcld(idt2)
                call assert(iret .eq. 0)
            endif
        endif
20  end do
    ixiadd = iszon(jiszon + ibacol + idiadd)
    ixiadm = iszon(jiszon + ibacol + idiadm)
    ixmarq = iszon(jiszon + ibacol + idmarq)
    ixdeso = iszon(jiszon + ibacol + iddeso)
    if (ixiadd .eq. 0) then
!       COLLECTION CONTIGUE, ELLE PEUT ETRE LIBEREE IMMEDIATEMENT APRES
!       RELECTURE DU $$DESO
        genri = genr(jgenr(ic) + ixdeso)
        typei = type(jtype(ic) + ixdeso)
        ltypi = ltyp(jltyp(ic) + ixdeso)
        lon = lono(jlono(ic) + ixdeso)
        lonoi = lon * ltypi
        nomo = rnom(jrnom(ic) + ixdeso)
        ida = hdfopd(idfic,ngrp,nomo)
        call jjlihd(ida, lon, lonoi, genri, typei,&
                    ltypi, ic, ixdeso, 0, imarq(jmarq(ic)+2*ixdeso-1),&
                    iadmi, iadyn)
        iadm(jiadm(ic)+2*ixdeso-1) = iadmi
        iadm(jiadm(ic)+2*ixdeso ) = iadyn
        iret = hdfcld(ida)
        call assert(iret .eq. 0)
    else
!       COLLECTION DISPERSEE, IL FAUT RELIRE LES OBJETS STOCKES SUR LE
!       FICHIER HDF DANS LE GROUPE ASSOCIE ET UNIQUEMENT ACTUALISER LES
!       ADRESSES MEMOIRE DANS L'OBJET SYSTEME $$IADM
        ibiadm = iadm(jiadm(ic)+2*ixiadm-1)
        ibmarq = iadm(jiadm(ic)+2*ixmarq-1)
        ixlono = iszon (jiszon + ibacol + idlono)
        genri = genr(jgenr(ic)+ixdeso)
        typei = type(jtype(ic)+ixdeso)
        ltypi = ltyp(jltyp(ic)+ixdeso)
        ngrc = rnom(jrnom(ic)+id)(1:24)//'__OBJETS'
        idgc = hdfopg(idfic,ngrc)
        nbob = hdfnbo(idfic,ngrc)
        nomo = rnom(jrnom(ic)+id)(1:24)
        do 30 k = 1, nbob
            write(nomo(25:32),'(I8)') k
            if (ixlono .eq. 0) then
                lonoi = lono (jlono(ic) + ixdeso) * ltypi
            else
                iblono = iadm (jiadm(ic) + 2*ixlono-1)
                lonoi = iszon (jiszon + iblono - 1 + k) * ltypi
            endif
            if (lonoi .gt. 0) then
                ido=hdfopd(idfic,ngrc,nomo)
                iret=hdftsd(ido,typeb,ltypb,lon)
                call assert(iret .eq. 0)
                call jjlihd(ido, lon, lonoi, genri, typei,&
                            ltypi, ic, k, id, iszon(jiszon+ibmarq-1+2*k-1),&
                            iadmi, iadyn)
                iszon(jiszon+ibiadm-1+2*k-1) = iadmi
                iszon(jiszon+ibiadm-1+2*k ) = iadyn
                numec = k
                call jjlide('JELIBE', rnom(jrnom(ic)+id)//'$$XNUM  ', 2)
                iret = hdfcld(ido)
                call assert(iret .eq. 0)
            endif
30      continue
        iret = hdfclg(idgc)
        call assert(iret .eq. 0)
    endif
    call jjlide('JELIBE', rnom(jrnom(ic)+id), 2)
! FIN ------------------------------------------------------------------
end subroutine
