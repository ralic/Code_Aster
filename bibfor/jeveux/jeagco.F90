subroutine jeagco(schin, schout, nbocnw, lontnw, claout)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
#include "jeveux.h"
#include "jeveux_private.h"
#include "asterfort/assert.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjallt.h"
#include "asterfort/jjcrec.h"
#include "asterfort/jjcren.h"
#include "asterfort/jjecrs.h"
#include "asterfort/jjlide.h"
#include "asterfort/jxdeps.h"
#include "asterfort/jxliro.h"
#include "asterfort/utmess.h"
    character(len=*) :: schin, schout, claout
    integer :: nbocnw, lontnw
! ----------------------------------------------------------------------
!     AGRANDIT LA COLLECTION DE NOM SCHOUT A PARTIR DE SCHIN SUR LA BASE
!     CLAOUT
!
! IN  SCHIN  : SOUS-CHAINE EN ENTREE : NOM DE COLLECTION
! OUT SCHOUT : SOUS-CHAINE EN SORTIE : NOM DE LA COLLECTION A CREER
!              SI L'OBJET SCHOUT EXISTE, IL EST DETRUIT
! IN  NBOCNW : NOMBRE D'OBJET DE LA NOUVELLE COLLECTION
! IN  LONTNW : LONGUEUR TOTALE DE LA NOUVELLE COLLECTION DANS LE CAS
!              COLLECTION CONTIGUE
! IN  CLAOUT : NOM DE LA CLASSE DE LA COLLECTION A CREER
!     POUR UNE COLLECTION DISERSEE ON VERIFIE QUE LOBNTNW > 0
!     POUR UNE COLLECTION CONTIGUEE ON S'ASSURE QUE LONTNW >= LONT SCHIN
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!-----------------------------------------------------------------------
    integer :: iadmi, iadmo1, iadmo2, iadout, iadyn, iadzon, ibacol
    integer :: ibaout, ibiadd, ibiadm, ibiado, iblono, ibmaro, icin
    integer :: icout, idat, idcout, idin, idout, ioc, iret1
    integer :: iret2, iret3, ista1, ista2, ixdeso, ixiadd, ixiadm
    integer :: ixiado, ixlong, ixlono, ixmaro, ixnom, jcara, jdate
    integer :: jdocu, jgenr, jhcod, jiadd, jiadm, jlong, jlono
    integer :: jltyp, jluti, jmarq, jorig, jrnom, jtype, k
    integer :: llect, lonoi, n, nbl, nmax
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
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: istat
    common /istaje/  istat(4)
!     ------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm, idmarq, idnom, idlong, idlono
    integer :: idnum
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 ,&
     &               idmarq = 4 , idnom  = 5 ,              idlong = 7 ,&
     &               idlono = 8 , idnum  = 10 )
    integer :: iv(idnum)
    character(len=8) :: csuffi(idnum)
! ----------------------------------------------------------------------
    integer :: ltypi, iaddi(2)
    character(len=24) :: nom24
    character(len=32) :: nomin, nomout
    character(len=1) :: kclas, genri, typei
    logical :: libcol, x2u, lconst, lnom
    data             iv / 0 , 0 , 0 , 0 , 1 , 0 , 1 , 1 , 1 , 1  /
    data             csuffi / '$$DESO  ','$$IADD  ','$$IADM  ',&
     &                          '$$MARQ  ','$$NOM   ','        ',&
     &                          '$$LONG  ','$$LONO  ','$$LUTI  ',&
     &                          '$$NUM   ' /
! DEB ------------------------------------------------------------------
    kclas = claout
    icout = index ( classe , kclas)
!
    nomin = schin
    nomout = schout
!
    call jjcren(nomin(1:24), 0, iret1)
!
    if (iret1 .eq. 2) then
!
! ----  OBJET DE TYPE COLLECTION
!
        idin = idatco
        icin = iclaco
        ibacol = iadm ( jiadm(icin) + 2*idin-1 )
        libcol = .false.
        if (ibacol .eq. 0) then
            libcol = .true.
        else
            if (iszon(jiszon+ibacol-1) .eq. istat(1)) libcol = .true.
        endif
        call jjallc(icin, idin, 'L', ibacol)
        nmax = iszon(jiszon+ibacol+ivnmax)
        ASSERT(nmax .le. nbocnw)
        iclas = icout
        call jjcren(nomout(1:24), 0, iret2)
        if (iret2 .ne. 0) then
            call jedetr(nomout(1:24))
        endif
        call jjcren(nomout(1:24), 2, iret2)
        idout = idatco
        idcout = idatco
        call jjcrec(icout, idout, 'X', 'I', idnum+1,&
                    iadzon)
        iszon(jiszon+iadzon+ivnmax) = nbocnw
        ibaout = iadm(jiadm(icout)+2*idout-1)
        if (iszon(jiszon+ibacol+idiadd) .eq. 0) then
!
! ----  COLLECTION CONTIGUE
            iv(1) = 1
        else
!
! ----  COLLECTION DISPERSEE
            iv(1) = 0
            ASSERT(lontnw .eq. 0)
        endif
!
        ixlong = iszon (jiszon+ibacol+idlong)
        lconst = (ixlong .eq. 0)
        ixnom = iszon (jiszon+ibacol+idnom)
        lnom = (ixnom .ne. 0)
!
! ----- RECOPIE DES OBJETS ATTRIBUTS DE COLLECTION
!
        do 1 k = 1, idnum
            idat = iszon ( jiszon + ibacol + k )
            if (idat .gt. 0) then
                nomin = rnom(jrnom(icin)+idat)
                nomout = nomout(1:24)//csuffi(k)
                call jjcren(nomout(1:32), 0, iret3)
                if (iret3 .eq. 0) then
                    call jjcren(nomout(1:32), 1, iret3)
                    idout = idatos
                    iadmi = iadm(jiadm(icin)+2*idat-1)
                    iaddi(1) = iadd(jiadd(icin)+2*idat-1)
                    iaddi(2) = iadd(jiadd(icin)+2*idat )
                    genr(jgenr(icout)+idout) = genr(jgenr(icin)+idat)
                    ltyp(jltyp(icout)+idout) = ltyp(jltyp(icin)+idat)
                    type(jtype(icout)+idout) = type(jtype(icin)+idat)
                    llect = lono(jlono(icin)+idat)
!
                    if (k .eq. iddeso .and. iv(1) .eq. 1) then
                        lono(jlono(icout)+idout)= max(lontnw,llect)
                        if (lconst) then
                            long(jlong(icout)+idout)=lontnw/nbocnw
                        endif
                    else if (k.eq.idlono) then
                        lono(jlono(icout)+idout)= nbocnw+1
                        long(jlong(icout)+idout)= nbocnw+1
                    else if (k.eq.idnum) then
                        lono(jlono(icout)+idout)= 2
                        long(jlong(icout)+idout)= 2
                    else
                        lono(jlono(icout)+idout)= nbocnw
                        long(jlong(icout)+idout)= nbocnw
                    endif
                    genri = genr(jgenr(icout)+idout)
                    typei = type(jtype(icout)+idout)
                    ltypi = ltyp(jltyp(icout)+idout)
                    lonoi = lono(jlono(icout)+idout)
                    nbl = lonoi * ltypi
                    if (nbl .gt. 0) then
                        if ((k.eq.1 .and. iv(1).eq.1 ) .or. k .gt. 1) then
                            call jjallt(nbl, icout, genri, typei, ltypi,&
                                        'INIT', iadout, iadyn)
                            call jjecrs(iadout, icout, idout, 0, 'E',&
                                        imarq(jmarq(icout)+2*idout-1))
                            iadm(jiadm(icout)+2*idout-1) = iadout
                            iadm(jiadm(icout)+2*idout ) = iadyn
                        endif
                    endif
!
                    if (k .eq. idnom) then
                        if (lnom) then
!
! -- IL FAUT TRAITER LE REPERTOIRE DE NOM A PART
!
                            do 20 ioc = 1, luti(jluti(icin) + ixnom)
                                call jenuno(jexnum(nomin, ioc), nom24)
                                call jecroc(jexnom(nomout, nom24))
20                          continue
                        endif
!
                    else if (iv(k) .eq. 1) then
                        if (iadmi .ne. 0) then
                            iadmo1 = (iadmi-1)*lois+iszon(jiszon+ iadmi-3)+1
                            iadmo2 = (iadout-1)*lois+iszon(jiszon+ iadout-3)+1
                            call jxdeps(iadmo1, iadmo2, llect*ltypi)
                        else if (iaddi(1) .gt. 0) then
                            call jxliro(icin, iadout, iaddi, llect* ltypi)
                        endif
                    endif
                    docu(jdocu(icout)+idout) = docu(jdocu(icin)+idat)
                    luti(jluti(icout)+idout) = luti(jluti(icin)+idat)
                    if (k .eq. idnum .and. iadmi .ne. 0) then
                        iszon(jiszon+iadout) = nbocnw
                        iszon(jiszon+iadout+1) = iszon(jiszon+iadmi+1)
                    endif
                endif
                iszon(jiszon+ibaout+k) = idatos
            endif
 1      continue
!
! ----- POUR UNE COLLECTION DISPERSEE, RECOPIE DES SEGMENTS DE VALEURS
! ----- ASSOCIES AUX OBJETS DE COLLECTION
!
        if (iv(1) .eq. 0) then
            ixdeso = iszon(jiszon+ibacol+iddeso)
            ixiadm = iszon(jiszon+ibacol+idiadm)
            ibiadm = iadm(jiadm(icin)+2*ixiadm-1)
!
            ixmaro = iszon(jiszon+ibaout+idmarq)
            ibmaro = iadm(jiadm(icout)+2*ixmaro-1)
!
            ixiado = iszon(jiszon+ibaout+idiadm)
            ibiado = iadm(jiadm(icout)+2*ixiado-1)
            ixiadd = iszon(jiszon+ibacol+idiadd)
            ibiadd = iadm(jiadm(icin)+2*ixiadd-1)
            ixlono = iszon(jiszon+ibacol+idlono)
            genri = genr(jgenr(icin)+ixdeso)
            typei = type(jtype(icin)+ixdeso)
            ltypi = ltyp(jltyp(icin)+ixdeso)
            do 2 k = 1, nmax
                iadmi = iszon(jiszon+ibiadm-1+2*k-1)
                iaddi(1) = iszon(jiszon+ibiadd-1+2*k-1)
                iaddi(2) = iszon(jiszon+ibiadd-1+2*k )
                if (iadmi .eq. 0 .and. iaddi(1) .eq. 0) goto 2
                if (ixlono .eq. 0) then
                    nbl = lono(jlono(icin)+ixdeso)*ltypi
                else
                    iblono = iadm(jiadm(icin)+2*ixlono-1)
                    nbl = iszon(jiszon+iblono-1+k)*ltypi
                endif
                if (iadmi .ne. 0) then
                    ista1 = iszon(jiszon+iadmi-1)
                    ista2 = iszon(jiszon+iszon(jiszon+iadmi-4)-4)
                    if (ista1 .eq. istat(1) .and. (ista2.eq.istat(3) .or.ista2.eq.istat(4))) then
                        x2u = .true.
                        iszon(jiszon+iadmi-1) = istat(2)
                    else
                        x2u = .false.
                    endif
                endif
                call jjallt(nbl, icout, genri, typei, ltypi,&
                            'INIT', iadout, iadyn)
                call jjecrs(iadout, icout, k, idcout, 'E',&
                            iszon(jiszon+ ibmaro-1+2*k-1))
                iszon(jiszon+ibiado-1+2*k-1) = iadout
                iszon(jiszon+ibiado-1+2*k ) = iadyn
                if (iadmi .ne. 0) then
                    iadmo1 = (iadmi-1)*lois+iszon(jiszon+iadmi-3)+1
                    iadmo2 = (iadout-1)*lois+iszon(jiszon+iadout-3)+1
                    call jxdeps(iadmo1, iadmo2, nbl)
                    if (x2u) iszon(jiszon+iadmi-1) = istat(1)
                else if (iaddi(1) .gt. 0) then
                    call jxliro(icin, iadout, iaddi, nbl)
                else
                    call utmess('F', 'JEVEUX1_65', sk=nomin, si=k)
                endif
 2          continue
        endif
        if (libcol) call jjlide('JELIBE', nomin(1:24), iret1)
        call jjlide('JELIBE', nomout(1:24), iret2)
    endif
! FIN ------------------------------------------------------------------
end subroutine
