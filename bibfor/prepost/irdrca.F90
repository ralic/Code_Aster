subroutine irdrca(ifi, nbno, desc, nec, dg,&
                  ncmpmx, vale, nomgd, ncmpgd, nomsym,&
                  numnoe, lresu, nbcput, ncmput, nive)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ifi, nbno, desc(*), nec, dg(*), ncmpmx, numnoe(*), nbcput, nive
    real(kind=8) :: vale(*)
    character(len=*) :: nomgd, ncmpgd(*), ncmput(*)
    character(len=*) :: nomsym
    aster_logical :: lresu
!
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
!--------------------------------------------------------------------
!        ECRITURE D'UN CHAM_NO SUR FICHIER CASTEM, DATASET TYPE 55
!        A VALEURS REELLES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER CASTEM
!         NBNO  : NOMBRE DE NOEUDS DU MAILLAGE
!         DESC  :
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR NOMGD
!         VALE  : VALEURS DU CHAM_NO
!         NOMGD : NOM DE LA GRANDEUR  DEPL_R, TEMP_R, SIEF_R, EPSI_R,...
!         NCMPGD: NOMS DES CMP
!         NOMSYM: NOM SYMBOLIQUE
!         NUMNOE: NUMERO DES NOEUDS
!         LRESU : =.TRUE. IMPRESSION D'UN CONCEPT RESULTAT
!         NBCPUT: NOMBRE DE CMP DEMANDE PAR L'UTILISATEUR
!         NCMPUT: NOMS DES CMP DEMANDE PAR L'UTILISATEUR
!         NIVE  : NIVEAU IMPRESSION CASTEM 3 OU 10
!
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
    character(len=8) :: nomvar(30)
    character(len=8) :: blanc, nomco
    integer :: nbcmp, iutil
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, iad, iadr, ibid, ic, icm
    integer :: icmp, iec, inno, ino, inum
    integer :: irval, iun, iva, ival, izero
    integer :: ncmp
    integer, pointer :: bid(:) => null()
    character(len=8), pointer :: nom(:) => null()
    integer, pointer :: pos(:) => null()
    integer, pointer :: last(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    blanc = '      '
    AS_ALLOCATE(vi=pos, size=ncmpmx)
    if (nec .gt. 0) then
        do 16 iec = 1, nec
            dg(iec)=desc(3+iec-1)
 16     continue
    endif
!
    if (.not.lresu) then
        call jeveuo('&&OP0039.LAST', 'E', vi=last)
        inum = last(4) + 1
    else
        inum = 0
    endif
!
    ncmp = -desc(2)
    if (nbcput .ne. 0) then
        do 30 icm = 1, nbcput
            do 32 icmp = 1, ncmpmx
                if (ncmput(icm) .eq. ncmpgd(icmp)) then
                    pos(icmp) = icmp
                    goto 30
                endif
 32         continue
            valk (1) = ncmput(icm)
            valk (2) = nomgd
            call utmess('A', 'PREPOST5_25', nk=2, valk=valk)
 30     continue
    else
        do 2 icmp = 1, ncmpmx
            if (exisdg(dg,icmp)) pos(icmp) = icmp
  2     continue
    endif
!
    iad = 1
    nbcmp = 0
    do 5 i = 1, ncmpmx
        if (pos(i) .ne. 0) then
            if (ncmpgd(i) .eq. 'DX') then
                nbcmp = nbcmp+1
                nomvar(iad)='UX'
                iad = iad+1
            else if (ncmpgd(i).eq.'DY') then
                nbcmp= nbcmp+1
                nomvar(iad)='UY'
                iad = iad+1
            else if (ncmpgd(i).eq.'DZ') then
                nbcmp= nbcmp+1
                nomvar(iad)='UZ'
                iad = iad+1
            else if (ncmpgd(i).eq.'DRX') then
                nbcmp= nbcmp+1
                nomvar(iad)='RX'
                iad = iad+1
            else if (ncmpgd(i).eq.'DRY') then
                nbcmp= nbcmp+1
                nomvar(iad)='RY'
                iad = iad+1
            else if (ncmpgd(i).eq.'DRZ') then
                nbcmp= nbcmp+1
                nomvar(iad)='RZ'
                iad = iad+1
            else
                nbcmp= nbcmp+1
                nomco = ncmpgd(i)
                iutil = lxlgut (nomco)
                if (iutil .le. 4) then
                    nomvar(iad) = ncmpgd(i)
                else
                    nomvar(iad) = nomco (1:2)// nomco((iutil-1):iutil)
                endif
                iad = iad+1
            endif
        endif
  5 end do
!
! ---- ECRITURE DE L'EN-TETE -----
!
    ibid = 2
    iun = 1
    izero = 0
    write (ifi,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ibid
    if (lresu) then
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
        endif
    else
        if (nive .eq. 3) then
            write (ifi,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',iun
! ECRITURE DES OBJETS NOMMES
            write(ifi,'(1X,A8)') nomsym
            write(ifi,'(I5)') inum
        else if (nive.eq.10) then
            write (ifi,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',ibid,&
            'NBRE OBJETS NOMMES',iun,'NBRE OBJETS',iun
! ECRITURE DES OBJETS NOMMES
            write(ifi,'(1X,A8)') nomsym
            write(ifi,'(I8)') inum
        endif
    endif
!
    if (nive .eq. 3) then
        write(ifi,'(I5,I5,I5)') iun,nbcmp,ibid
        write(ifi,'(16(I5))') iun,nbno,nbcmp
    else if (nive.eq.10) then
        write(ifi,'(I8,I8,I8,I8)') iun,nbcmp,ibid,iun
        write(ifi,'(10(I8))') iun,nbno,nbcmp
    endif
!
    AS_ALLOCATE(vi=bid, size=ncmpmx)
    AS_ALLOCATE(vk8=nom, size=nbcmp)
!
    izero = 0
    do 10 i = 1, nbcmp
        bid(i) = izero
        nom(i) = nomvar(i)
 10 end do
    write(ifi,'(16(1X,A4))') (nom(i)(1:4),i=1,nbcmp)
    if (nive .eq. 3) write(ifi,'(16(I5))') (bid(i),i=1,nbcmp)
    if (nive .eq. 10) write(ifi,'(10(I8))') (bid(i),i=1,nbcmp)
    if (lresu) then
        write(ifi,'(1X,A71)') nomsym
    else
        write(ifi,'(1X,A71)') nomgd
    endif
    write(ifi,'(1X,A)') blanc
    if (nive .eq. 10) write (ifi,'(I8)') izero
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ---
!
    call wkvect('&&IRDRCA.VALE', 'V V R', ncmpmx*nbno, irval)
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS CASTEM ----
    do 22 iva = 1, ncmp
        ic = pos(iva)
        iadr = irval-1+(iva-1)*nbno
        do 12 inno = 1, nbno
            ino = numnoe(inno)
            ival = (ino-1)*ncmp
            zr(iadr+inno) = vale(ival+ic)
 12     continue
 22 end do
    write(ifi,'(3(1X,E21.13E3))') (zr(irval-1+i),i=1,nbcmp*nbno)
    if (.not.lresu) last(4)=inum
    call jedetr('&&IRDRCA.VALE')
    AS_DEALLOCATE(vi=bid)
    AS_DEALLOCATE(vk8=nom)
    AS_DEALLOCATE(vi=pos)
    call jedema()
end subroutine
