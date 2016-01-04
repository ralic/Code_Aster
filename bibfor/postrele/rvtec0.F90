subroutine rvtec0(t, co, sp, absc, x,&
                  cmp, nd, sdm, nbpoin, docu,&
                  nbcmp, padr, nomtab, iocc,&
                  xnovar, ncheff, i1)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/codent.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsorac.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbexip.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: co(*), sp(*), nbpoin, nbcmp, padr(*), iocc, i1
    real(kind=8) :: t(*), absc(*), x(*)
    character(len=4) :: docu
    character(len=8) :: cmp(*), nd(*)
    character(len=16) :: ncheff
    character(len=19) :: nomtab
    character(len=24) :: sdm, xnovar
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!     AFFICHAGE CHAM_ELEM DE NBCMP COMPOSANTES
!     COPIE ROUTINE RVIMPK
!     ------------------------------------------------------------------
! IN  T    : R  : VAL DE TOUTES LES CMP
! IN  CO   : I  : TABLE DES NOMBRES DE COUCHES
! IN  SP   : I  : TABLE DES NOMBRES DE SOUS-PT
! IN  S    : R  : TABLE DES ABSCISSES CURVILIGNES
! IN  X    : R  : TABLES DES COORDONNEES
! IN  CMP  : K8 : NOMS DE TOUTES LES CMP
! IN  ND   : K8 : NOMS DES EVENTUELS NOEUDS
! IN  SDM  : K24: NOM OJB SD MAILLES
! IN  NBPOIN : I  : NOMBRE DE POINTS
! IN  DOCU : K4 : TYPE DE LIEU
! IN  NBCMP : I  : NOMBRE TOTAL DE CMP
!     ------------------------------------------------------------------
    integer :: nbpar, ilign, nbsp, i, ikk, l, jam, nbco, lc, is, ic, valei(1052), n1, adrval
    integer :: nbmail, j, adracc, jacc, ik, ir, ii, ivari(1000), nbcmp2, jvari, ico, lm, im
    integer :: nbvari, nbacc, nbpr, jaces, iac, iadr, iord(1)
    real(kind=8) :: prec, valer(1050)
    complex(kind=8) :: c16b
    aster_logical :: exist, erreur
    character(len=3) :: typpar
    character(len=7) :: kii
    character(len=8) :: k8b, acces, nomres, ctype, crit
    character(len=16) :: intitu
    character(len=24) :: nomval, nomacc, nnores, nopara(1053), nomjv
    character(len=80) :: valek(1051)
    character(len=8), pointer :: nom_para(:) => null()
    character(len=8), pointer :: typ_para(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    if (nbcmp .le. 0) goto 9999
!
    if (docu .ne. 'LSTN') goto 9999
!
    call jelira(jexnum(xnovar, iocc), 'LONUTI', nbvari)
    if (nbvari .ne. 0) then
        call jelira(jexnum(xnovar, iocc), 'LONUTI', nbvari)
        call jeveuo(jexnum(xnovar, iocc), 'L', jvari)
        if (nbvari .eq. 1 .and. zi(jvari) .eq. -1) then
            nbcmp2 = sp(1)
        else
            nbcmp2 = nbvari
        endif
    else
        nbcmp2 = nbcmp
        do 2 i = 1, nbcmp2, 1
            ivari(i) = i
  2     continue
    endif
    if (nbcmp2 .gt. 1000) then
        call utmess('F', 'POSTRELE_13')
    endif
!
    call getvtx('ACTION', 'INTITULE', iocc=iocc, scal=intitu, nbret=n1)
!
    call getvr8('ACTION', 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    call getvtx('ACTION', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
    nomval = ncheff//'.VALACCE'
    nomacc = ncheff//'.TYPACCE'
    nnores = ncheff//'.NOMRESU'
    call jeveuo(nomacc, 'L', jacc)
!
    ik = 1
    ii = 0
    ir = 0
    nbpar = 1
    valek(ik) = intitu
    nopara(nbpar) = 'INTITULE'
!
!
    if (zk8(jacc) .eq. 'DIRECT  ') then
        call jeveuo(jexnum(ncheff//'.LSCHEFF', 1), 'L', jacc)
        nbpar = nbpar + 1
        nopara(nbpar) = 'CHAM_GD'
        ik = ik + 1
        valek(ik) = zk24(jacc)(1:8)
    else
        call jeveuo(nnores, 'L', jacc)
        nomres = zk16(jacc)(1:8)
        nbpar = nbpar + 1
        nopara(nbpar) = 'RESU'
        ik = ik + 1
        valek(ik) = nomres
        nbpar = nbpar + 1
        nopara(nbpar) = 'NOM_CHAM'
        ik = ik + 1
        valek(ik) = zk16(jacc+1)
        call jeveuo(nomacc, 'L', adracc)
        call jeveuo(nomval, 'L', adrval)
        acces = zk8(adracc)
        if (acces(1:1) .eq. 'O') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
            nomjv = '&&RVRCCM.NOMS_ACCES'
            call rsnopa(nomres, 0, nomjv, nbacc, nbpr)
            if (nbacc .ne. 0) then
                call jeveuo(nomjv, 'L', jaces)
                do 10 iac = 1, nbacc
                    call rsadpa(nomres, 'L', 1, zk16(jaces-1+iac), zi(adrval+i1-1),&
                                1, sjv=iadr, styp=ctype)
                    call tbexip(nomtab, zk16(jaces-1+iac), exist, typpar)
                    if (.not. exist) then
                        call tbajpa(nomtab, 1, zk16(jaces-1+iac), ctype)
                    endif
                    nbpar = nbpar + 1
                    nopara(nbpar) = zk16(jaces-1+iac)
                    if (ctype(1:1) .eq. 'I') then
                        ii = ii + 1
                        valei(ii) = zi(iadr)
                    else if (ctype(1:1) .eq. 'R') then
                        ir = ir + 1
                        valer(ir) = zr(iadr)
                    else if (ctype(1:3) .eq. 'K80') then
                        ik = ik + 1
                        valek(ik) = zk80(iadr)
                    else if (ctype(1:3) .eq. 'K32') then
                        ik = ik + 1
                        valek(ik) = zk32(iadr)
                    else if (ctype(1:3) .eq. 'K24') then
                        ik = ik + 1
                        valek(ik) = zk24(iadr)
                    else if (ctype(1:3) .eq. 'K16') then
                        ik = ik + 1
                        valek(ik) = zk16(iadr)
                    else if (ctype(1:2) .eq. 'K8') then
                        ik = ik + 1
                        valek(ik) = zk8(iadr)
                    endif
 10             continue
                call jedetr(nomjv)
            endif
        else if (acces(1:1) .eq. 'M') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'NUME_MODE', zi(adrval+i1-1), 0.d0, k8b,&
                        c16b, prec, crit, iord, 1,&
                        n1)
            ii = ii + 1
            valei(ii) = iord(1)
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_MODE'
            ii = ii + 1
            valei(ii) = zi(adrval + i1-1)
        else if (acces(1:1) .eq. 'I') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_ORDRE'
            call rsorac(nomres, 'INST', 0, zr(adrval + i1-1), k8b,&
                        c16b, prec, crit, iord, 1,&
                        n1)
            ii = ii + 1
            valei(ii) = iord(1)
            nbpar = nbpar + 1
            nopara(nbpar) = 'INST'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        else if (acces(1:1) .eq. 'F') then
            nbpar = nbpar + 1
            nopara(nbpar) = 'FREQ'
            ir = ir + 1
            valer(ir) = zr(adrval + i1-1)
        endif
    endif
    if (docu .eq. 'LSTN') then
        call tbexip(nomtab, 'NOEUD', exist, typpar)
        if (.not. exist) then
            call tbajpa(nomtab, 1, 'NOEUD', 'K8')
        endif
        nbpar = nbpar + 1
        nopara(nbpar) = 'NOEUD'
    endif
    call tbexip(nomtab, 'MAILLE', exist, typpar)
    if (.not. exist) then
        call tbajpa(nomtab, 1, 'MAILLE', 'K8')
    endif
    nbpar = nbpar + 1
    nopara(nbpar) = 'MAILLE'
    nbpar = nbpar + 1
    nopara(nbpar) = 'ABSC_CURV'
    nbpar = nbpar + 1
    nopara(nbpar) = 'COOR_X'
    nbpar = nbpar + 1
    nopara(nbpar) = 'COOR_Y'
    nbpar = nbpar + 1
    nopara(nbpar) = 'COOR_Z'
    ic = 0
    is = 0
    do 20 i = 1, nbpoin, 1
        nbco = co(i)
        nbsp = sp(i)
        if (nbco .gt. 1 .and. ic .eq. 0) then
            call tbexip(nomtab, 'NUME_COUCHE', exist, typpar)
            if (.not. exist) then
                call tbajpa(nomtab, 1, 'NUME_COUCHE', 'I')
            endif
            ic = 1
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_COUCHE'
        endif
        if (nbsp .gt. 1 .and. is .eq. 0) then
            call tbexip(nomtab, 'NUME_GAUSS', exist, typpar)
            if (.not. exist) then
                call tbajpa(nomtab, 1, 'NUME_GAUSS', 'I')
            endif
            is = 1
            nbpar = nbpar + 1
            nopara(nbpar) = 'NUME_GAUSS'
        endif
 20 end do
    if (nbvari .eq. 0) then
        do 40 i = 1, nbcmp2, 1
            nbpar = nbpar + 1
            nopara(nbpar) = cmp(i)
 40     continue
    else
        AS_ALLOCATE(vk8=nom_para, size=nbcmp2)
        AS_ALLOCATE(vk8=typ_para, size=nbcmp2)
        if (nbvari .eq. 1 .and. zi(jvari) .eq. -1) then
            do 12 i = 1, nbcmp2, 1
                ivari(i) = i
                call codent(i, 'G', kii)
                nbpar = nbpar + 1
                nopara(nbpar) = 'V'//kii
                nom_para(i) = 'V'//kii
                typ_para(i) = 'R'
 12         continue
        else
            do 14 i = 1, nbcmp2, 1
                ivari(i) = zi(jvari+i-1)
                call codent(zi(jvari+i-1), 'G', kii)
                nbpar = nbpar + 1
                nopara(nbpar) = 'V'//kii
                nom_para(i) = 'V'//kii
                typ_para(i) = 'R'
 14         continue
        endif
        call tbajpa(nomtab, nbcmp2, nom_para, typ_para)
    endif
!
    erreur = .false.
    if (nbpar .gt. 1050) erreur = .true.
    if (ii+2 .gt. 1050) erreur = .true.
    if (ir+4+nbcmp2 .gt. 1050) erreur = .true.
    if (ik .gt. 1050) erreur = .true.
    if (erreur) then
        call utmess('F', 'POSTRELE_12')
    endif
!
    ilign = 0
!
    do 30 i = 1, nbpoin, 1
!
        valer(ir+1) = absc(i)
        valer(ir+2) = x(1+(i-1)*3)
        valer(ir+3) = x(2+(i-1)*3)
        valer(ir+4) = x(3+(i-1)*3)
!
        ikk = 0
        if (docu .eq. 'LSTN') then
            ikk = ikk + 1
            valek(ik+ikk) = nd(i)
        endif
!
        if (docu .eq. 'LSTN') then
            call jeveuo(jexnum(sdm, i), 'L', jam)
            call jelira(jexnum(sdm, i), 'LONMAX', nbmail)
            l = 0
            nbco = co(i)
            nbsp = sp(i)
            j = padr(i)
            lm = nbcmp*nbsp
            lc = lm*nbmail
        else
            if (i .eq. 1) then
                nbsp = sp(i)
                nbco = co(i)
                j = 1
                lc = nbsp*nbcmp
                lm = lc*(2*nbco-1)
                lc = 2*lc
                nbmail = 1
                l = 1
            else if (i .eq. nbpoin) then
                nbsp = sp(i-1)
                nbco = co(i-1)
                lc = nbsp*nbcmp
                lm = lc*(2*nbco-1)
                j = lc*(2*(i-2)*nbco + 1) + 1
                lc = 2*lc
                nbmail = 1
                l = 0
            else
                nbsp = sp(i)
                nbco = co(i)
                lc = nbsp*nbcmp
                lm = lc*(2*nbco-1)
                j = lc*(2*(i-2)*nbco + 1) + 1
                lc = 2*lc
                nbmail = 2
                l = 0
            endif
        endif
!        POUR UN CHAMP DE TYPE "VARI"
!        LES SOUS-POINTS SONT PRIS EN CHARGE PAR LE NBCMP
        if (nbvari .ne. 0) nbsp = 1
!
        do 200 ico = 1, nbco, 1
!
            valei(ii+1) = ico
!
            do 220 is = 1, nbsp, 1
!
                valei(ii+2) = is
!
                do 222 im = 1, nbmail, 1
!
                    if (docu .eq. 'LSTN') then
                        valek(ik+ikk+1) = zk8(jam+l+im-1)
                    else
                        valek(ik+ikk+1) = '   -    '
                    endif
!
                    do 224 ic = 1, nbcmp2, 1
                        valer(ir+4+ic) = t(j-1+(ico-1)*lc+(is-1)* nbcmp+(im-1)*lm+ivari(ic))
224                 continue
!
                    call tbajli(nomtab, nbpar, nopara, valei, valer,&
                                [c16b], valek, ilign)
!
222             continue
!
220         continue
!
200     continue
!
 30 end do
!
    if (nbvari .ne. 0) then
        AS_DEALLOCATE(vk8=nom_para)
        AS_DEALLOCATE(vk8=typ_para)
    endif
!
9999 continue
    call jedema()
!
end subroutine
