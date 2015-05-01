subroutine jelibd(nomlu, ltot)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
! LIBERE DE LA MEMOIRE LE SEGMENT DE VALEURS ASSOCIES A UN OBJET
! JEVEUX ALLOUE DYNAMIQUEMENT LORSQU'IL EST DANS L'ETAT XA OU XD
!
! IN   NOMLU : NOM DE L'OBJET A LIBERER
! OUT  LTOT  : LONGUEUR EN ENTIERS LIBEREE
!
! ----------------------------------------------------------------------
! person_in_charge: j-pierre.lefebvre at edf.fr
! POUR LES VARS I4ZON, LSZON, R8ZON QUI DANS UN COMMON MAIS UNIQUEMENT
! PAR EQUIVALENCE
! aslint: disable=
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjlbsg.h"
#include "asterfort/jjvern.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomlu
!
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
!     -----------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: iadmi, iadyn, ibacol, ibiadm
    integer :: inat, ixdeso, ixiadd, ixiadm, jcara
    integer :: jdate, jdocu, jgenr, jhcod, jiacce, jiadd, jiadm
    integer :: jindir, jlong, jlono, jltyp, jluti, jmarq, jorig
    integer :: jrnom, jtype, k, ltot, ltypi, n
    integer :: nbacce, nbmax
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jiatje/  jltyp(n), jlong(n), jdate(n), jiadd(n), jiadm(n),&
     &                 jlono(n), jhcod(n), jcara(n), jluti(n), jmarq(n)
!
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!
    integer :: nblmax, nbluti, longbl, kitlec, kitecr, kiadm, iitlec, iitecr
    integer :: nitecr, kmarq
    common /ificje/  nblmax(n) , nbluti(n) , longbl(n) ,&
     &                 kitlec(n) , kitecr(n) ,             kiadm(n) ,&
     &                 iitlec(n) , iitecr(n) , nitecr(n) , kmarq(n)
    character(len=2) :: dn2
    character(len=5) :: classe
    character(len=8) :: nomfic, kstout, kstini
    common /kficje/  classe    , nomfic(n) , kstout(n) , kstini(n) ,&
     &                 dn2(n)
!
    integer :: nrhcod, nremax, nreuti
    common /icodje/  nrhcod(n) , nremax(n) , nreuti(n)
    common /jiacce/  jiacce(n),nbacce(2*n)
    common /jindir/  jindir(n)
    integer :: isstat
    common /iconje/  isstat
    integer :: ldyn, lgdyn, nbdyn, nbfree
    common /idynje/  ldyn , lgdyn , nbdyn , nbfree
    integer :: icdyn, mxltot
    common /xdynje/  icdyn , mxltot
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio, cuvtrav
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2), cuvtrav
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    integer :: datei
    common /iheuje/  datei
! ----------------------------------------------------------------------
    integer :: ivnmax, iddeso, idiadd, idiadm
    parameter    ( ivnmax = 0 , iddeso = 1 , idiadd = 2 , idiadm = 3 )
! ----------------------------------------------------------------------
    character(len=32) :: noml32
    integer :: icre, iret
!
    noml32 = nomlu
    ltot = 0
    icre = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call utmess('A', 'JEVEUX_26', sk=noml32(1:24))
        goto 9999
    else if (iret .eq. 1) then
!
! ----  CAS D'UN OBJET SIMPLE
!
        inat = 1
        iadmi = iadm (jiadm(iclaos)+2*idatos-1)
        ltypi = ltyp (jltyp(iclaos)+idatos)
        if (iadmi .eq. 0) then
            goto 9999
        endif
        iadyn = iadm(jiadm(iclaos)+2*idatos)
!
        call jjlbsg(iclaos, idatos, 0, 0, iadmi,&
                    iadyn, ltot)
!
    else if (iret .eq. 2) then
!
! ----- CAS D'UNE COLLECTION
!
        call jjallc(iclaco, idatco, 'L', ibacol)
        if (noml32(25:32) .eq. '        ') then
            inat = 2
        else
            call jjcroc(noml32(25:32), icre)
            inat = 3
        endif
    endif
    if (inat .eq. 2) then
!
! ----- CAS D'UNE COLLECTION ENTIERE : ON LIBERE TOUS LES OC
!
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixiadm = iszon ( jiszon + ibacol + idiadm )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        ltypi = ltyp( jltyp(iclaco) + ixdeso )
        if (ixiadd .eq. 0) then
!
! ------- COLLECTION CONTIGUE
!
            iadmi = iadm ( jiadm(iclaco) + 2*ixdeso-1 )
            if (iadmi .eq. 0) then
                goto 9999
            endif
            iadyn = iadm ( jiadm(iclaco) + 2*ixdeso )
!
            call jjlbsg(iclaco, ixdeso, 0, 0, iadmi,&
                        iadyn, ltot)
!
        else
!
! ------- COLLECTION DISPERSEE
!
            nbmax = iszon ( jiszon + ibacol + ivnmax )
            ibiadm = iadm ( jiadm(iclaco) + 2*ixiadm-1 )
            do 10 k = 1, nbmax
                iadmi = iszon(jiszon + ibiadm - 1 + 2*k-1 )
                if (iadmi .eq. 0) then
                    goto 10
                endif
                iadyn = iszon(jiszon + ibiadm - 1 + 2*k )
!
                call jjlbsg(iclaco, idatco, k, ibacol, iadmi,&
                            iadyn, ltot)
!
10          continue
        endif
    else if (inat .eq. 3) then
!       ------ CAS D'UN OBJET DE COLLECTION  ------
        ixiadd = iszon ( jiszon + ibacol + idiadd )
        ixiadm = iszon ( jiszon + ibacol + idiadm )
        ixdeso = iszon ( jiszon + ibacol + iddeso )
        ltypi = ltyp( jltyp(iclaco) + ixdeso )
        if (ixiadd .ne. 0) then
!
! -------- COLLECTION DISPERSEE
!
            ibiadm = iadm ( jiadm(iclaco) + 2*ixiadm-1 )
            iadmi = iszon(jiszon + ibiadm - 1 + 2*idatoc-1 )
            if (iadmi .eq. 0) then
                goto 9999
            endif
            iadyn = iszon(jiszon + ibiadm - 1 + 2*idatoc)
!
            call jjlbsg(iclaco, idatco, idatoc, ibacol, iadmi,&
                        iadyn, ltot)
!
        endif
    endif
9999  continue
!
end subroutine
