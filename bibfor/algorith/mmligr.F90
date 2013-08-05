subroutine mmligr(noma, nomo, defico, resoco)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmeltc.h"
#include "asterfort/mmeltf.h"
#include "asterfort/mmeltm.h"
#include "asterfort/mmimp2.h"
#include "asterfort/mminfl.h"
#include "asterfort/mmlige.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma, nomo
    character(len=24) :: defico, resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CREATION OBJETS - LIGREL)
!
! CREATION DU LIGREL POUR LES ELEMENTS TARDIFS DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
!
! ----------------------------------------------------------------------
!
    integer :: nbtyp
    parameter   (nbtyp=40)
!
    integer :: ztabf
    integer :: ico, jco, iptc, ityp, ino, izone
    integer :: jtynma, iacnx1, ilcnx1
    integer :: nummam, nummae
    integer :: jnbno, long, jad, ityte, ityma
    integer :: nndel, ntpc, nbnom, nbnoe, nbgrel, nndtot
    integer :: comptc(nbtyp), comptf(nbtyp)
    character(len=8) :: k8bid
    character(len=16) :: nomtm, nomte
    character(len=24) :: tabfin, crnudd
    integer :: jtabf, jcrnud
    integer :: ifm, niv
    character(len=19) :: ligrcf
    character(len=24) :: typelt
    character(len=24) :: nosdco
    integer :: jnosdc
    logical :: lappar, laxis, lfrot
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    crnudd = resoco(1:14)//'.NUDD'
    nosdco = resoco(1:14)//'.NOSDCO'
    call jeveuo(nosdco, 'L', jnosdc)
    call jeveuo(crnudd, 'L', jcrnud)
    call jeveuo(tabfin, 'L', jtabf)
!
    ztabf = cfmmvd('ZTABF')
!
! --- INITIALISATIONS
!
    ntpc = cfdisi(defico,'NTPC')
    typelt = '&&MMLIGR.TYPNEMA'
    laxis = cfdisl(defico,'AXISYMETRIQUE')
!
! --- LIGREL DES ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
!
    ligrcf = zk24(jnosdc+2-1)(1:19)
!
! --- REAPPARIEMENT OU PAS ?
!
    lappar = zl(jcrnud)
    if (lappar) then
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> CREATION DU LIGREL DES'//&
            ' ELEMENTS DE CONTACT'
        endif
    else
        if (niv .ge. 2) then
            write (ifm,*) '<CONTACT> PAS DE CREATION DU LIGREL DES'//&
            ' ELEMENTS DE CONTACT'
        endif
        goto 999
    endif
!
! --- DESTRUCTION DU LIGREL S'IL EXISTE
!
    call detrsd('LIGREL', ligrcf)
!
! --- ACCES MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
! --- LISTE DES ELEMENTS DE CONTACT
!
    call mmlige(noma, defico, resoco, typelt, nbtyp,&
                comptc, comptf, nndtot, nbgrel)
    call jeveuo(typelt, 'L', jtynma)
!
! --- PAS DE NOEUDS TARDIFS
!
    call wkvect(ligrcf//'.NBNO', 'V V I', 1, jnbno)
    zi(jnbno-1+1) = 0
!
! --- CREATION DE L'OBJET .NEMA
!
    call jecrec(ligrcf//'.NEMA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                ntpc)
    call jeecra(ligrcf//'.NEMA', 'LONT', nndtot, k8bid)
    do 50 iptc = 1, ntpc
!
! --- VERIF NOMBRE DE NOEUDS SUR ELEMENT DE CONTACT
!
        nndel = zi(jtynma-1+2*(iptc-1)+2)
        nummae = nint(zr(jtabf+ztabf*(iptc-1)+1))
        nummam = nint(zr(jtabf+ztabf*(iptc-1)+2))
        nbnoe = zi(ilcnx1+nummae) - zi(ilcnx1-1+nummae)
        nbnom = zi(ilcnx1+nummam) - zi(ilcnx1-1+nummam)
        if (nndel .ne. (nbnom+nbnoe)) then
            ASSERT(.false.)
        endif
!
! --- CREATION DE L'ELEMENT DE CONTACT DANS LE LIGREL
!
        call jecroc(jexnum(ligrcf//'.NEMA', iptc))
        call jeecra(jexnum(ligrcf//'.NEMA', iptc), 'LONMAX', nndel+1, k8bid)
        call jeveuo(jexnum(ligrcf//'.NEMA', iptc), 'E', jad)
        zi(jad-1+nndel+1) = zi(jtynma-1+2*(iptc-1)+1)
!
! --- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE ESCLAVE
!
        do 30 ino = 1, nbnoe
            zi(jad-1+ino) = zi(iacnx1+zi(ilcnx1-1+nummae)-2+ino)
30      continue
!
! --- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE MAITRE
!
        do 40 ino = 1, nbnom
            zi(jad-1+nbnoe+ino) = zi(iacnx1+zi(ilcnx1-1+nummam)-2+ino)
40      continue
!
50  end do
!
! --- LONGUEUR TOTALE DU LIEL
!
    long = nbgrel
    do 70 ityp = 1, nbtyp
        long = long + comptc(ityp) + comptf(ityp)
70  end do
!
! --- CREATION DE L'OBJET .LIEL
!
    if (nbgrel .eq. 0) then
        ASSERT(.false.)
    else
        call jecrec(ligrcf//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbgrel)
    endif
!
    call jeecra(ligrcf//'.LIEL', 'LONT', long, k8bid)
    ico = 0
    do 90 ityp = 1, nbtyp
        if (comptc(ityp) .ne. 0) then
            ico = ico + 1
            call jecroc(jexnum(ligrcf//'.LIEL', ico))
            call jeecra(jexnum(ligrcf//'.LIEL', ico), 'LONMAX', comptc( ityp)+1, k8bid)
            call jeveuo(jexnum(ligrcf//'.LIEL', ico), 'E', jad)
!
            nomte = mmeltc(ityp)
            nomtm = mmeltm(ityp)
!
!         --- MODIF NOM SI AXISYMETRIQUE
!
            if (laxis) then
                if (nomtm(1:3) .eq. 'SEG') then
                    nomte(7:7) = 'A'
                else
                    ASSERT(.false.)
                endif
            endif
!
            call jenonu(jexnom('&CATA.TE.NOMTE', nomte), ityte)
            call jenonu(jexnom('&CATA.TM.NOMTM', nomtm), ityma)
!
            zi(jad-1+comptc(ityp)+1) = ityte
!
            jco = 0
            do 80 iptc = 1, ntpc
                if (zi(jtynma-1+2* (iptc-1)+1) .eq. ityma) then
                    izone = nint(zr(jtabf+ztabf*(iptc-1)+13))
                    lfrot = mminfl(defico,'FROTTEMENT_ZONE',izone)
                    if (.not.lfrot) then
                        jco = jco + 1
                        zi(jad-1+jco) = -iptc
                    endif
                endif
80          continue
            ASSERT(jco.eq.comptc(ityp))
        endif
        if (comptf(ityp) .ne. 0) then
            ico = ico + 1
            call jecroc(jexnum(ligrcf//'.LIEL', ico))
            call jeecra(jexnum(ligrcf//'.LIEL', ico), 'LONMAX', comptf( ityp)+1, k8bid)
            call jeveuo(jexnum(ligrcf//'.LIEL', ico), 'E', jad)
!
            nomte = mmeltf(ityp)
            nomtm = mmeltm(ityp)
!
!         --- MODIF NOM SI AXISYMETRIQUE
!
            if (laxis) then
                if (nomtm(1:3) .eq. 'SEG') then
                    nomte(7:7) = 'A'
                else
                    ASSERT(.false.)
                endif
            endif
!
            call jenonu(jexnom('&CATA.TE.NOMTE', nomte), ityte)
            call jenonu(jexnom('&CATA.TM.NOMTM', nomtm), ityma)
!
            zi(jad-1+comptf(ityp)+1) = ityte
!
            jco = 0
            do 85 iptc = 1, ntpc
                if (zi(jtynma-1+2* (iptc-1)+1) .eq. ityma) then
                    izone = nint(zr(jtabf+ztabf*(iptc-1)+13))
                    lfrot = mminfl(defico,'FROTTEMENT_ZONE',izone)
                    if (lfrot) then
                        jco = jco + 1
                        zi(jad-1+jco) = -iptc
                    endif
                endif
85          continue
            ASSERT(jco.eq.comptf(ityp))
        endif
90  end do
    ASSERT(ico.eq.nbgrel)
!
! --- INITIALISATION DU LIGREL
!
    call jedupo(nomo//'.MODELE    .LGRF', 'V', ligrcf//'.LGRF', .false.)
    call adalig(ligrcf)
    call initel(ligrcf)
!
! --- IMPRESSIONS
!
    if (niv .ge. 2) call mmimp2(ifm, noma, ligrcf, jtabf)
!
! --- MENAGE
!
    call jedetr(typelt)
999  continue
!
    call jedema()
end subroutine
