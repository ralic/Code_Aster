subroutine op0163()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     INTERFACE ASTER - MISS3D : COMMANDE  LIRE_MISS_3D
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdgeph.h"
#include "asterfort/refdaj.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
#include "asterfort/vtcrem.h"
#include "asterfort/wkvect.h"
!
    integer :: ier, itresr(3), itresi(3)
    character(len=4) :: type(3)
    character(len=8) :: k8b, nomres
    character(len=8) :: mael, basemo
    character(len=14) :: numddl
    character(len=16) :: concep, nomcmd, typres, k16nom
    character(len=19) :: kinst, knume
    character(len=24) :: refe, matrk
    character(len=24) :: matrm, chamno, chamn1, chamn2, numer, matric(3)
    character(len=32) :: fichi
    character(len=64) :: base
    character(len=80) :: titre
    integer :: iarg
    complex(kind=8) :: cbid
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iarch, ibi, ich, idbase, idresi
    integer :: idresr, ie, ifmis, imess, iord, iret
    integer :: j, jinst, jnume, jrefe, linst, lval1
    integer :: lval2, lvale, nbcham, nbinst, nbmodd, nbmode, nbmods
    integer :: nbsauv, nbsto, neq, nf, nmm, nti, nu
!
!-----------------------------------------------------------------------
    data  refe  /'                  _REFE'/
    data  kinst /'&&OP0163.INSTANT'/
    data  knume /'&&OP0163.NUME_RANG'/
    data  chamno/'&&OP0163.CHAMNO'/
    data  chamn1/'&&OP0163.CHAMN1'/
    data  chamn2/'&&OP0163.CHAMN2'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
    imess = iunifi('MESSAGE')
    call getres(nomres, concep, nomcmd)
    write(imess,'(''TYPE DE RESULTAT :'',1X,A16)') concep
!
!     ----- RECUPERATION UNITE DE MISS ---
    call getvis(' ', 'UNITE', scal=ifmis, nbret=nu)
    call getvtx(' ', 'NOM', scal=fichi, nbret=nf)
    if (nf .eq. 0) then
        k16nom = ' '
        if (ulisop ( ifmis, k16nom ) .eq. 0) then
            call ulopen(ifmis, ' ', ' ', 'NEW', 'O')
        endif
    else
        base = './tmp_miss3d/'//fichi
        call ulopen(ifmis, base, ' ', 'NEW', 'O')
    endif
!
    call getvtx(' ', 'TITRE', scal=titre, nbret=nti)
    if (nti .ne. 0) write(imess,'(A80)') titre
!
!     ----- RECUPERATION DES MODES -----
    call getvid(' ', 'MACR_ELEM_DYNA', scal=mael, nbret=nmm)
    refe(1:18) = mael//'.MAEL_RAID'
    call jeveuo(refe, 'L', jrefe)
    matrk = zk24(jrefe+1)
    refe(1:18) = mael//'.MAEL_MASS'
    call jeveuo(refe, 'L', jrefe)
    basemo = zk24(jrefe)
    matrm = zk24(jrefe+1)
!      CALL JEVEUO(BASEMO//'           .REFD','L',IADRIF)
!      INTERF = ZK24(IADRIF+4) (1:8)
!
    call dismoi('F', 'NB_MODES_DYN', basemo, 'RESULTAT', nbmodd,&
                k8b, ier)
    call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmods,&
                k8b, ier)
!
    nbmode = nbmodd + nbmods
    write(imess,'(1X,I6,1X,''MODES DYNAMIQUES'')') nbmodd
    write(imess,'(1X,I6,1X,''MODES STATIQUES'')') nbmods
    read(ifmis,1000) nbinst
    nbsauv = nbinst
    nbsto = nbmode * nbsauv
!     ----- RECUPERATION TYPE DE RESULTAT ---
!
    typres = concep
    nbcham = 3
    type(1) = 'DEPL'
    type(2) = 'VITE'
    type(3) = 'ACCE'
    call dismoi('F', 'NOM_NUME_DDL', matrm, 'MATR_ASSE', ibi,&
                numddl, iret)
    call dismoi('F', 'NB_EQUA', matrm, 'MATR_ASSE', neq,&
                k8b, iret)
    call wkvect('&&OP0163.BASEMO', 'V V R', nbmode*neq, idbase)
    call copmod(basemo, 'DEPL', neq, numddl, nbmode,&
                'R', zr(idbase), [cbid])
    call wkvect('&&OP0163.DEPLR', 'V V R', nbsto, itresr(1))
    call wkvect('&&OP0163.VITER', 'V V R', nbsto, itresr(2))
    call wkvect('&&OP0163.ACCER', 'V V R', nbsto, itresr(3))
    call wkvect('&&OP0163.DEPLI', 'V V R', nbsto, itresi(1))
    call wkvect('&&OP0163.VITEI', 'V V R', nbsto, itresi(2))
    call wkvect('&&OP0163.ACCEI', 'V V R', nbsto, itresi(3))
    call wkvect(knume, 'V V I', nbinst, jnume)
    call wkvect(kinst, 'V V R8', nbinst, jinst)
    do 60 iord = 0, nbinst-1
        zi(jnume+iord) = iord + 1
60  continue
    read(ifmis,1001) (zr(jinst+iord-1),iord=1,nbinst)
    read(ifmis,1001) ((zr(itresr(1)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresr(2)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresr(3)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresr(1)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
    read(ifmis,1001) ((zr(itresr(2)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
    read(ifmis,1001) ((zr(itresr(3)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
    write(imess,'(''PARAMETRES DE CALCUL :'',/6(1X,1PE12.5))')&
     & (zr(jinst+iord-1),iord=1,nbinst)
    if (typres .ne. 'DYNA_HARMO') goto 9998
    read(ifmis,1000) nbinst
    read(ifmis,1001) (zr(jinst+iord-1),iord=1,nbinst)
    read(ifmis,1001) ((zr(itresi(1)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresi(2)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresi(3)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=1,nbmodd),i=1,nbinst)
    read(ifmis,1001) ((zr(itresi(1)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
    read(ifmis,1001) ((zr(itresi(2)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
    read(ifmis,1001) ((zr(itresi(3)+j-1+(zi(jnume+i-1)-1)*nbmode),&
     &             j=nbmodd+1,nbmode),i=1,nbinst)
9998  continue
    iarch = 0
    call rscrsd('G', nomres, typres, nbinst)
    if (typres .eq. 'DYNA_HARMO') then
        call vtcrem(chamn1, matrm, 'V', 'R')
        call vtcrem(chamn2, matrm, 'V', 'R')
    endif
    do 90 i = 0, nbinst-1
        iarch = iarch + 1
        do 92 ich = 1, nbcham
            call rsexch(' ', nomres, type(ich), iarch, chamno,&
                        iret)
            if (iret .eq. 0) then
                call utmess('A', 'ALGORITH2_64', sk=chamno)
            else if (iret .eq. 100) then
                if (typres .eq. 'DYNA_HARMO') then
                    call vtcrem(chamno, matrm, 'G', 'C')
                else
                    call vtcrem(chamno, matrm, 'G', 'R')
                endif
            else
                ASSERT(.false.)
            endif
            idresr = itresr(ich)
            chamno(20:24) = '.VALE'
            call jeveuo(chamno, 'E', lvale)
            if (typres .eq. 'DYNA_HARMO') then
                idresi = itresi(ich)
                chamn1(20:24) = '.VALE'
                call jeveuo(chamn1, 'E', lval1)
                chamn2(20:24) = '.VALE'
                call jeveuo(chamn2, 'E', lval2)
                call mdgeph(neq, nbmode, zr(idbase), zr(idresr+(zi( jnume+i)-1)*nbmode),&
                            zr(lval1))
                call mdgeph(neq, nbmode, zr(idbase), zr(idresi+(zi( jnume+i)-1)*nbmode),&
                            zr(lval2))
                do 93 ie = 1, neq
                    zc(lvale+ie-1) = dcmplx(zr(lval1+ie-1),zr(lval2+ ie-1))
93              continue
            else
                call mdgeph(neq, nbmode, zr(idbase), zr(idresr+(zi( jnume+i)-1)*nbmode),&
                            zr(lvale))
            endif
            call jelibe(chamno)
            call rsnoch(nomres, type(ich), iarch)
92      continue
        if (typres .eq. 'DYNA_HARMO') then
            call rsadpa(nomres, 'E', 1, 'FREQ', iarch,&
                        0, sjv=linst, styp=k8b)
        else
            call rsadpa(nomres, 'E', 1, 'INST', iarch,&
                        0, sjv=linst, styp=k8b)
        endif
        zr(linst) = zr(jinst+i)
90  continue
!
    matric(1) = matrk
    matric(2) = matrm
    matric(3) = ' '
    call dismoi('F', 'NOM_NUME_DDL', matrk, 'MATR_ASSE', iarg,&
                numer, iret)
    call refdaj('F', nomres, nbcham, numer, 'DYNAMIQUE',&
                matric, iret)
!
    1000 format(i6)
    1001 format(6(1pe12.5))
!
    call jedema()
end subroutine
