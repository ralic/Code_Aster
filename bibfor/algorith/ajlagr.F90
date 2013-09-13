subroutine ajlagr(rigid, masse, masinv)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtcmbi.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/pteddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: rigid, masse, masinv
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     AJOUTE LES "LAGRANGE" DANS LA MATRICE DE MASSE A PARTIR DES
!     DONNEES STOCKEES DANS LA MATRICE DE RAIDEUR.
!
! IN  : RIGID  : NOM DE LA MATRICE DE RAIDEUR
! IN  : MASSE  : NOM DE LA MATRICE DE MASSE
! OUT : MASINV : NOM DE LA MATRICE DE MASSE AVEC LES LAGRANGES
!-----------------------------------------------------------------------
    integer :: neq, hbloc, nbbloc, mxddl
    real(kind=8) :: zero, un, mmax, kmax, coef, lcoef(2)
    character(len=1) :: typmat, typma2, typcst(2)
    character(len=8) :: raid, mass, masi, nomddl, matrer
    character(len=14) :: numddl, nu2ddl
    character(len=19) :: rigi2, mass2, matre2, masin2
    complex(kind=8) :: cun, cmmax, ckmax, ccoef
    character(len=24) :: nmat(4), nmati
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, imati, imatm, imatr, imtrer, j, jconl
    integer :: jmass, jraid, jrefa1, jrefa2, jsmde, lddl, nbmat
!
!-----------------------------------------------------------------------
    call jemarq()
    zero = 0.d0
    un = 1.d0
    cun = dcmplx(un,zero)
!
    raid = rigid
    call mtdscr(rigid)
    rigi2=rigid
    call jeveuo(rigi2//'.&INT', 'E', imatr)
    if (zi(imatr+3) .eq. 1) then
        typmat = 'R'
    else if (zi(imatr+3) .eq. 2) then
        typmat = 'C'
    else
        call utmess('F', 'ALGORITH_3')
    endif
    call jeveuo(raid//'           .REFA', 'L', jrefa1)
    numddl = zk24(jrefa1-1+2)(1:14)
!
    mass = masse
    call mtdscr(masse)
    mass2=masse
    nmat(2)=mass2//'.&INT'
    call jeveuo(nmat(2), 'E', imatm)
    if (zi(imatm+3) .eq. 1) then
        typma2 = 'R'
    else if (zi(imatm+3) .eq. 2) then
        typma2 = 'C'
    else
        call utmess('F', 'ALGORITH_3')
    endif
    call jeveuo(mass//'           .REFA', 'L', jrefa2)
    nu2ddl = zk24(jrefa2-1+2)(1:14)
!
    if (typma2 .ne. typmat) then
        valk (1) = typmat
        valk (2) = typma2
        call utmess('F', 'ALGORITH14_77', nk=2, valk=valk)
    endif
    if (nu2ddl .ne. numddl) then
        valk (1) = numddl
        valk (2) = nu2ddl
        call utmess('F', 'ALGORITH14_78', nk=2, valk=valk)
    endif
!
!
    call jeveuo(numddl//'.SMOS.SMDE', 'L', jsmde)
    neq = zi(jsmde-1+1)
    hbloc = zi(jsmde-1+2)
    nbbloc = zi(jsmde-1+3)
    ASSERT(nbbloc.eq.1)
!
!     --- DETERMINATION DU COEFFICIENT DE CONDITIONNEMENT ---
    if (typmat .eq. 'R') then
        mmax = zero
        kmax = zero
        do 10 i = 1, nbbloc
            call jeveuo(jexnum(raid//'           .VALM', i), 'L', jraid)
            call jeveuo(jexnum(mass//'           .VALM', i), 'L', jmass)
            do 12 j = 0, hbloc-1
                mmax = max(zr(jmass+j),mmax)
                kmax = max(zr(jraid+j),kmax)
12          continue
            call jelibe(jexnum(mass//'           .VALM', i))
            call jelibe(jexnum(raid//'           .VALM', i))
10      continue
        coef = mmax / kmax
    else
        cmmax = zero
        ckmax = zero
        do 20 i = 1, nbbloc
            call jeveuo(jexnum(raid//'           .VALM', i), 'L', jraid)
            call jeveuo(jexnum(mass//'           .VALM', i), 'L', jmass)
            do 22 j = 0, hbloc-1
                cmmax = max(abs(zc(jmass+j)),abs(cmmax))
                ckmax = max(abs(zc(jraid+j)),abs(ckmax))
22          continue
            call jelibe(jexnum(mass//'           .VALM', i))
            call jelibe(jexnum(raid//'           .VALM', i))
20      continue
        ccoef = cmmax / ckmax
    endif
!
    matrer = '&&RIGIL'
    call mtdefs(matrer, rigid, 'V', typmat)
    call mtdscr(matrer)
    matre2=matrer
    nmat(1)=matre2//'.&INT'
    call jeveuo(nmat(1), 'E', imtrer)
!
    call mtcmbi(typmat, imatr, coef, ccoef, imtrer)
!
    masi = masinv
    call mtdefs(masinv, rigid, 'V', typmat)
    call mtdscr(masinv)
    masin2=masinv
    nmati=masin2//'.&INT'
    call jeveuo(nmati, 'E', imati)
!
    nbmat = 2
    nomddl = ' '
    lcoef(1) = 1.d0
    lcoef(2) = 1.d0
    typcst(1) = typmat
    typcst(2) = typmat
    call mtcmbl(nbmat, typcst, lcoef, nmat, nmati,&
                nomddl, ' ', 'ELIM=')
!
    nomddl = 'LAGR    '
    mxddl = 1
    call wkvect('&&AJLAGR.LAGR', 'V V I', neq*mxddl, lddl)
    call pteddl('NUME_DDL', numddl, mxddl, nomddl, neq,&
                zi(lddl))
    call jeveuo(masi//'           .CONL', 'E', jconl)
    if (typmat .eq. 'R') then
        do 30 i = 0, neq-1
            if (zi(lddl+i) .ne. 0) then
                zr(jconl+i) = mmax
            else
                zr(jconl+i) = un
            endif
30      continue
    else
        do 32 i = 0, neq-1
            if (zi(lddl+i) .ne. 0) then
                zc(jconl+i) = cmmax
            else
                zc(jconl+i) = cun
            endif
32      continue
    endif
!
!
! --- MENAGE
!
    call jedetr('&&AJLAGR.LAGR')
    call detrsd('MATR_ASSE', '&&RIGIL')
!
    call jedema()
end subroutine
