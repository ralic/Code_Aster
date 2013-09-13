subroutine op0164()
    implicit none
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
!
!     OPERATEUR LIRE_IMPE_MISS
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/r8prem.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/irmifr.h"
#include "asterfort/irmitm.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/ulisop.h"
#include "asterfort/ulopen.h"
#include "asterfort/wkvect.h"
!
    integer :: n1, n2, n4, jscde
    real(kind=8) :: partr, parti, inst
    character(len=8) :: k8b, nomres, basemo, numgen
    character(len=16) :: typres, nomcom, typbas, k16nom, typbin, tissf, tsym
    character(len=19) :: resu, stolci
    character(len=14) :: nugene
    character(len=24) :: tabrig, tabfrq, tabri2, tabrit
    character(len=72) :: texte
    real(kind=8) :: a(3), a2(3)
    integer(kind=8) :: long1, long2, long3
    logical :: lissf, lsym
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, iaconl, iadesc, ialime, ic
    integer :: ier, ifmis, ifr, ifreq, ii, j, jfrq
    integer :: jj, jrefa, jri2, jrig, jrit, ldblo, ldblo2
    integer :: nbmodd, nbmode, nbmods, nfr, nfreq, nit, nsaut
    integer :: nterm, nueq
    real(kind=8) :: freq
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomres, typres, nomcom)
!
! --- RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getvis(' ', 'UNITE_RESU_IMPE', scal=ifmis, nbret=n1)
    call getvr8(' ', 'FREQ_EXTR', scal=freq, nbret=nfr)
    call getvr8(' ', 'INST_EXTR', scal=inst, nbret=nit)
    call getvid(' ', 'BASE', scal=basemo, nbret=n4)
    call getvid(' ', 'NUME_DDL_GENE', scal=numgen, nbret=n2)
    call getvtx(' ', 'TYPE', scal=typbin, nbret=n2)
    lissf = .false.
    call getvtx(' ', 'ISSF', scal=tissf, nbret=n2)
    if (tissf(1:3) .eq. 'OUI') lissf = .true.
    lsym = .false.
    call getvtx(' ', 'SYME', scal=tsym, nbret=n2)
    if (tsym(1:3) .eq. 'OUI') lsym = .true.
!
    call gettco(basemo, typbas)
!
    nugene = numgen
    stolci = numgen//'      .SLCS'
!
!==================================================
!
!
! RECUPERATION DU NOMBRE DE MODES REDUIT,
! NB_VECT DONNE PAR NUME_DDL_GENE
    call jeveuo(stolci//'.SCDE', 'L', jscde)
!
    call dismoi('F', 'NB_MODES_DYN', basemo, 'RESULTAT', nbmodd,&
                k8b, ier)
    call dismoi('F', 'NB_MODES_STA', basemo, 'RESULTAT', nbmods,&
                k8b, ier)
    if (lissf) then
        nbmode = nbmodd + nbmods
    else
        nbmode = nbmods
    endif
!
    tabrig = '&&OP0164.RIGM'
    tabri2 = '&&OP0164.RIG2'
    tabrit = '&&OP0164.RIGT'
    tabfrq = '&&OP0164.FREQ'
    call wkvect(tabrig, 'V V R', 2*nbmode*nbmode, jrig)
    call wkvect(tabri2, 'V V R', 2*nbmode*nbmode, jri2)
    if (nit .ne. 0) then
        call wkvect(tabrit, 'V V R', nbmode*nbmode, jrit)
        call irmitm(nbmode, ifmis, inst, tabrit)
        call jeveuo(tabrit, 'L', jrit)
        do 20 i1 = 1, nbmode
            do 20 i2 = 1, nbmode
                zr(jrig+2*(i2-1)*nbmode+2*i1-2)=zr(jrit+(i2-1)*nbmode+&
                i1-1)
                zr(jrig+2*(i2-1)*nbmode+2*i1-1)=0.d0
20          continue
        call jedetr(tabrit)
        goto 21
    endif
    if (typbin .ne. 'BINAIRE') then
        k16nom = ' '
        if (ulisop ( ifmis, k16nom ) .eq. 0) then
            call ulopen(ifmis, ' ', ' ', 'NEW', 'O')
        endif
        call irmifr(ifmis, freq, ifreq, nfreq, ic)
        call wkvect(tabfrq, 'V V R', nfreq, jfrq)
        rewind ifmis
        read(ifmis,'(A72)') texte
        if (texte(1:4) .eq. 'XXXX') goto 4
        do 1 i2 = 1, nbmode
            do 1 i1 = 1, nbmode
                nsaut = nfreq
                if (ic .ge. 1) nsaut = nfreq-1
                if (i1 .eq. 1 .and. i2 .eq. 1) nsaut = ifreq
                do 2 i = 1, nsaut
                    read(ifmis,'(A72)') texte
 2              continue
                read(ifmis,*) (a(j),j=1,3)
                zr(jrig+2*(i2-1)*nbmode+2*i1-2) = a(2)
                zr(jrig+2*(i2-1)*nbmode+2*i1-1) = -a(3)
                if (ic .ge. 1) then
                    read(ifmis,*) (a2(j),j=1,3)
                    zr(jri2+2*(i2-1)*nbmode+2*i1-2) = a2(2)
                    zr(jri2+2*(i2-1)*nbmode+2*i1-1) = -a2(3)
                    zr(jrig+2*(i2-1)*nbmode+2*i1-2) = zr(&
                                                      jrig+2*(i2-1) *nbmode+2*i1-2) + (freq-a(1))&
                                                      &/(a2(1)-a(1)) * (zr(jri2+2*(i2-1)*nbmode+2&
                                                      &*i1-2) -zr(jrig+2*(i2- 1)*nbmode+2*i1-2)&
                                                      )
                    zr(jrig+2*(i2-1)*nbmode+2*i1-1) = zr(&
                                                      jrig+2*(i2-1) *nbmode+2*i1-1) + (freq-a(1))&
                                                      &/(a2(1)-a(1)) * (zr(jri2+2*(i2-1)*nbmode+2&
                                                      &*i1-1) -zr(jrig+2*(i2- 1)*nbmode+2*i1-1)&
                                                      )
                endif
 1          continue
 4      continue
    else
        rewind ifmis
!
!   Lecture d'entiers INTEGER*8 en binaire venant de MISS3D
!   On convertit ensuite en INTEGER (*4 sur machine 32 bits, sinon *8).
!   Les reels ne posent pas de probleme : ce sont toujours des REAL*8
!
        read(ifmis) long1,long2,long3
        nfreq=long1
        nbmode=long2
        n1=long3
        ic=1
        call wkvect(tabfrq, 'V V R', nfreq, jfrq)
        read(ifmis) (zr(jfrq+ifr-1),ifr=1,nfreq)
        do 3 i = 1, nfreq
            a(1) = zr(jfrq+i-1)
            if (freq .le. (a(1) + 1.d-6)) then
                ifreq = i
                if (i .gt. 1 .and. freq .lt. (a(1) - 1.d-6)) then
                    ifreq = ifreq-1
                endif
                if (freq .le. r8prem( )) ic = 2
                if (i .eq. 1 .and. nfreq .eq. 1) ic = 0
                if (i .eq. nfreq .and. freq .ge. (a(1) - 1.d-6)) then
                    ic = 0
                    ifreq = nfreq
                endif
                goto 7
            endif
 3      continue
        ifreq = nfreq
        ic = 0
 7      continue
        do 5 i = 1, ifreq-1
            read(ifmis) a(1)
 5      continue
        read(ifmis) ((zr(jrig+2*(i2-1)*nbmode+2*i1-2), zr(jrig+2*(i2-&
        1)*nbmode+2*i1-1), i1=1,nbmode),i2=1,nbmode)
        if (ic .ge. 1) then
            read(ifmis) ((zr(jri2+2*(i2-1)*nbmode+2*i1-2), zr(jri2+2*(&
            i2-1)*nbmode+2*i1-1), i1=1,nbmode),i2=1,nbmode)
            do 8 i1 = 1, nbmode
                do 8 i2 = 1, nbmode
                    zr(jrig+2*(i2-1)*nbmode+2*i1-2) = zr(&
                                                      jrig+2*(i2-1) *nbmode+2*i1-2) + (freq-zr(jf&
                                                      &rq+ifreq-1))/(zr( jfrq+ifreq)-zr(jfrq+ifre&
                                                      &q-1)) * (zr(jri2+2*(i2-1)* nbmode+2*i1-2) &
                                                      &-zr(jrig+2*(i2-1)*nbmode+2*i1-2)&
                                                      )
                    zr(jrig+2*(i2-1)*nbmode+2*i1-1) = zr(&
                                                      jrig+2*(i2-1) *nbmode+2*i1-1) + (freq-zr(jf&
                                                      &rq+ifreq-1))/(zr( jfrq+ifreq)-zr(jfrq+ifre&
                                                      &q-1)) * (zr(jri2+2*(i2-1)* nbmode+2*i1-1) &
                                                      &-zr(jrig+2*(i2-1)*nbmode+2*i1-1)&
                                                      )
 8              continue
        endif
        do 6 i1 = 1, nbmode
            do 6 i2 = 1, nbmode
                zr(jrig+2*(i2-1)*nbmode+2*i1-1)= -zr(jrig+2*(i2-1)*&
                nbmode+2*i1-1)
 6          continue
    endif
21  continue
!
! ----- RECUPERATION DU NOMBRE D'EQUATIONS DU SYSTEME PHYSIQUE
!
!
    call jeveuo(stolci//'.SCDE', 'L', jscde)
    nueq = zi(jscde-1+1)
!      NTBLOC = ZI(JSCDE-1+2)
!      NBLOC  = ZI(JSCDE-1+3)
    nterm = nueq*(nueq+1)/2
!
    resu = ' '
    resu(1:8) = nomres
    if (lsym) then
        call jecrec(resu//'.VALM', 'G V C', 'NU', 'DISPERSE', 'CONSTANT',&
                    1)
    else
        call jecrec(resu//'.VALM', 'G V C', 'NU', 'DISPERSE', 'CONSTANT',&
                    2)
    endif
    call jeecra(resu//'.VALM', 'LONMAX', nterm)
!
    call wkvect(resu//'.LIME', 'G V K24', 1, ialime)
    zk24(ialime) = '                        '
!
    call wkvect(resu//'.CONL', 'G V C', nueq, iaconl)
    do 10 i = 1, nueq
        zc(iaconl+i-1) = dcmplx(1.0d0,0.0d0)
10  end do
!
    call wkvect(resu//'.REFA', 'G V K24', 11, jrefa)
    zk24(jrefa-1+11)='MPI_COMPLET'
    zk24(jrefa-1+1) = basemo
    zk24(jrefa-1+2) = nugene
    if (lsym) then
        zk24(jrefa-1+9) = 'MS'
    else
        zk24(jrefa-1+9) = 'MR'
    endif
    zk24(jrefa-1+10) = 'GENE'
!
    call wkvect(resu//'.DESC', 'G V I', 3, iadesc)
    zi(iadesc) = 2
    zi(iadesc+1) = nueq
!   ON TESTE LA HAUTEUR MAXIMALE DES COLONNES DE LA MATRICE
!   SI CETTE HAUTEUR VAUT 1, ON SUPPOSE QUE LE STOCKAGE EST DIAGONAL
    if (zi(jscde-1+4) .eq. 1) then
        zi(iadesc+2) = 1
    else
        zi(iadesc+2) = 2
    endif
!
!
! --- RECUPERATION DE LA STRUCTURE DE LA MATR_ASSE_GENE
!
!
!
    call jecroc(jexnum(resu//'.VALM', 1))
    call jeveuo(jexnum(resu//'.VALM', 1), 'E', ldblo)
    if (.not.lsym) then
        call jecroc(jexnum(resu//'.VALM', 2))
        call jeveuo(jexnum(resu//'.VALM', 2), 'E', ldblo2)
    endif
!
! ------ PROJECTION DE LA MATRICE ASSEMBLEE
!
!        BOUCLE SUR LES COLONNES DE LA MATRICE ASSEMBLEE
!
!
    do 30 i = 1, nueq
!
        ii = i - nbmodd
        if (lissf .and. i .le. nbmodd) ii = i+nbmods
!
! --------- BOUCLE SUR LES INDICES VALIDES DE LA COLONNE I
!
        do 40 j = 1, i
            jj = j - nbmodd
            if (lissf .and. j .le. nbmodd) jj = j+nbmods
!
! ----------- PRODUIT SCALAIRE VECTASS * MODE
!
            if (.not.lissf .and. (i.le.nbmodd.or.j.le.nbmodd)) then
                zc(ldblo+i*(i-1)/2+j-1) = dcmplx(0.d0,0.d0)
                if (.not.lsym) then
                    zc(ldblo2+i*(i-1)/2+j-1) = dcmplx(0.d0,0.d0)
                endif
            else
!
! ----------- STOCKAGE DANS LE .UALF A LA BONNE PLACE (1 BLOC)
!
                partr = zr(jrig+2*(ii-1)*nbmode+2*jj-2)
                parti = zr(jrig+2*(ii-1)*nbmode+2*jj-1)
                if (lsym) then
                    partr = 0.5d0*(zr(jrig+2*(jj-1)*nbmode+2*ii-2) + partr)
                    parti = 0.5d0*(zr(jrig+2*(jj-1)*nbmode+2*ii-1) + parti)
                endif
                zc(ldblo+i*(i-1)/2+j-1) = dcmplx(partr,parti)
                if (.not.lsym) then
                    partr = zr(jrig+2*(jj-1)*nbmode+2*ii-2)
                    parti = zr(jrig+2*(jj-1)*nbmode+2*ii-1)
                    zc(ldblo2+i*(i-1)/2+j-1) = dcmplx(partr,parti)
                endif
            endif
!
40      continue
30  end do
    call jelibe(jexnum(resu//'.VALM', 1))
    if (.not.lsym) call jelibe(jexnum(resu//'.VALM', 2))
    call jedetr(tabrig)
    call jedetr(tabfrq)
!
    call jedema()
end subroutine
