subroutine irmad1(ifi, versio, nbno, prno, nueq,&
                  nec, dg, ncmpmx, itype, nstat,&
                  chamno, nomcmp, nomsym, numnoe)
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/exisdg.h"
#include "asterfort/irgags.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: ifi, nbno, prno(*), nueq(*), nec, dg(*), ncmpmx, numnoe(*)
    integer :: versio, itype, nstat
    character(len=*) :: nomcmp(*), nomsym, chamno(*)
!--------------------------------------------------------------------
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
!        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 252
!        A VALEURS REELLES OU COMPLEXES
!--------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=4) :: nomgds(50), nomchs(50)
    character(len=19) :: chamn
    integer :: nbchs
    integer :: nbcmps(50), ipcmps(50, 50), impre
    logical :: ltabl(50), afaire
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, iavale, ichs, icmp, icms, icmsup, icompt
    integer :: icp, iec, imat, inno, ino, irval, ist
    integer :: ival, k1, k2, mfor, mkey, mtyp, nbcmpt
    integer :: ncmp, ncol, ndim, nrow
!-----------------------------------------------------------------------
    call jemarq()
    do 10 i = 1, ncmpmx
        ltabl(i) = .false.
10  end do
!
    nbcmpt = 0
    do 100 inno = 1, nbno
        ino = numnoe(inno)
        do 110 iec = 1, nec
            dg(iec)=prno((ino-1)*(nec+2)+2+iec)
110      continue
        ncmp = prno((ino-1)* (nec+2)+2)
        if (ncmp .eq. 0) goto 100
        icompt = 0
        do 112 icmp = 1, ncmpmx
            if (exisdg(dg,icmp)) icompt = icompt + 1
112      continue
        nbcmpt = max( nbcmpt , icompt )
100  end do
    nrow = nbcmpt
    ncol = nbno * nstat
    ndim = ncol * nrow
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    call assert((itype.eq.1).or.(itype.eq.2))
    if (itype .eq. 1) then
        call wkvect('&&IRMAD1.VAL', 'V V R', ndim, irval)
    else if (itype .eq. 2) then
        call wkvect('&&IRMAD1.VAL', 'V V C', ndim, irval)
    endif
!
! ---- RECHERCHE DES GRANDEURS SUPERTAB -----
!
    call irgags(ncmpmx, nomcmp, nomsym, nbchs, nomchs,&
                nbcmps, nomgds, ipcmps)
    do 777 ichs = 1, 50
        do 778 ist = 1, 50
            ipcmps(ichs,ist)=-1
778      end do
777  end do
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
    impre = 0
    do 20 ichs = 1, nbchs
        if (ichs .gt. 1) then
            afaire = .false.
            do 22 icp = 1, nbcmps(ichs)
                afaire = (afaire.or.ltabl(ipcmps(ichs,icp)))
22          continue
            if (.not. afaire) goto 20
        endif
        impre = impre + 1
        do 30 ist = 1, nstat
            chamn = chamno(ist)
            call jeveuo(chamn//'.VALE', 'L', iavale)
            do 40 inno = 1, nbno
                ino = numnoe(inno)
                do 42 iec = 1, nec
                    dg(iec) = prno((ino-1)*(nec+2)+2+iec)
42              continue
                ival = prno((ino-1)* (nec+2)+1)
                ncmp = prno((ino-1)* (nec+2)+2)
                if (ncmp .eq. 0) goto 40
                icompt = 0
                do 44 icmp = 1, ncmpmx
                    if (exisdg(dg,icmp)) then
                        if (ichs .eq. 1) ltabl(icmp)= .true.
                        icompt = icompt + 1
                        k1 = nueq(ival-1+icompt)
                        do 46 icms = 1, nbcmps(ichs)
                            icmsup = ipcmps(ichs,icms)
                            if (icmp .eq. icmsup) then
                                k2 = icms + (inno-1)*nbcmpt + (ist-1)* nbcmpt*nbno
                                if (itype .eq. 1) then
                                    zr(irval-1+k2) = zr(iavale-1+k1)
                                else
                                    zc(irval-1+k2) = zc(iavale-1+k1)
                                endif
                                goto 44
                            endif
46                      continue
                    endif
44              continue
40          continue
30      continue
20  end do
!
    call assert(impre .le. 1)
!
    if (versio .eq. 5 .and. impre .eq. 1) then
        imat = 147
        if (itype .eq. 1) then
            mtyp = 4
        else
            mtyp = 6
        endif
        mfor = 3
        mkey = 2
        write (ifi,'(A)') '    -1'
        write (ifi,'(A)') '   252'
        write (ifi,'(I10)') imat
        write (ifi,'(5I10)') mtyp, mfor, nrow, ncol, mkey
        if (itype .eq. 1) then
            write (ifi ,'(1P,4D20.12)') ( zr(irval+i) , i=0,ndim-1 )
        else
            write (ifi ,'(1P,2(2D20.12))') ( zc(irval+i) , i=0,ndim-1&
            )
        endif
        mfor = 3
        write (ifi,'(A)') '    -1'
    endif
!
    call jedetr('&&IRMAD1.VAL')
    call jedema()
end subroutine
