subroutine xmligr(noma, nomo, resoco)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmimp2.h"
#include "asterfort/wkvect.h"
#include "asterfort/xmelel.h"
!
    character(len=8) :: noma, nomo
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - CREATION OBJETS - LIGREL)
!
! CREATION DU LIGREL POUR LES ELEMENTS TARDIFS DE CONTACT
!
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
!
!
!
    integer :: nbtyp
    parameter (nbtyp=30)
    integer :: ico, jco, nbgrel, ipc, nbpc, k, ino
    integer :: jlgrf, jtymai, jmail, iacnx1, ilcnx1
    integer :: nummam, nummae, jnbno, long, jad, ityte, jtynma
    integer :: nndel, numtyp, compt(nbtyp), jtabf, ztabf
    integer :: jnosdc, ifm, niv
    integer :: imod, iatt(2), imail(2), nno(2), ndim
    character(len=24) :: tabfin, nosdco
    character(len=19) :: ligrxf
    character(len=16) :: nomte, nomtm, nomte2(nbtyp), mail3(2, 8)
    character(len=4) :: mode(3)
    character(len=3) :: mail2(2, 8)
    character(len=2) :: mail(2, 8)
    character(len=1) :: attr(7)
!
    data (mode(k),k=1,3) /'MECP','MEDP','ME3D'/
    data (attr(k),k=1,7) /'H','C','T','2','3','4','H'/
    data (mail(1,k),k=1,8) /'T3','Q4','T6','Q8',&
     &                         ' ',' ',' ',' '/
    data (mail(2,k),k=1,8) /'T4','P5','P6','H8',&
     &                        'TD','PT','PQ','HV'/
    data (mail2(1,k),k=1,8) /'TR3','QU4','TR6','QU8',&
     &                         ' ',' ',' ',' '/
    data (mail2(2,k),k=1,8) /'TE4','PY5','PE6','HE8',&
     &                         'T10','P13','P15','H20'/
    data (mail3(1,k),k=1,8) /'TRIA3','QUAD4','TRIA6','QUAD8',&
     &                         ' ',' ',' ',' '/
    data (mail3(2,k),k=1,8) /'TETRA4','PYRAM5','PENTA6','HEXA8',&
     &                         'TETRA10','PYRAM13','PENTA15','HEXA20'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> CREATION DU LIGREL DES'//&
     &        ' ELEMENTS DE CONTACT'
    endif
!
! --- ACCES OBJETS
!
    tabfin = resoco(1:14)//'.TABFIN'
    nosdco = resoco(1:14)//'.NOSDCO'
    call jeveuo(tabfin, 'E', jtabf)
    call jeveuo(nosdco, 'L', jnosdc)
    ztabf = cfmmvd('ZTABF')
    call jeveuo(noma//'.TYPMAIL', 'L', jtymai)
    call jeveuo(nomo//'.MAILLE', 'L', jmail)
!
! --- INITIALISATIONS
!
    call dismoi('DIM_GEOM', noma, 'MAILLAGE', repi=ndim)
    do 10 k = 1, nbtyp
        compt(k) = 0
 10 end do
    nbpc = nint(zr(jtabf-1+1))
    ligrxf = zk24(jnosdc+3-1)(1:19)
!
! --- DESTRUCTION DU LIGREL S'IL EXISTE
!
    call detrsd('LIGREL', ligrxf)
!
! --- CREATION DE .NOMA
!
    call wkvect(ligrxf//'.LGRF', 'V V K8', 2, jlgrf)
    zk8(jlgrf-1+1) = noma
    zk8(jlgrf-1+2) = nomo
    call jeveuo(noma//'.CONNEX', 'L', iacnx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', ilcnx1)
!
! --- ON COMPTE LE NOMBRE DE NOEUDS A STOCKER AU TOTAL
!
    long = nbpc
    do 20 ipc = 1, nbpc
        nummae = nint(zr(jtabf+ztabf*(ipc-1)+1))
        nummam = nint(zr(jtabf+ztabf*(ipc-1)+2))
        call xmelel(ndim, jmail, jtymai, nummae, nummam,&
                    imod, iatt, imail, nno)
        long = long+nno(1)+nno(2)
 20 end do
!
! --- PAS DE NOEUDS TARDIFS
!
    call wkvect(ligrxf//'.NBNO', 'V V I', 1, jnbno)
    zi(jnbno-1+1) = 0
!
! --- VECTEUR DE TYPES DE MAILLE DU LIGREL
!
    call wkvect('&&XMLIGR.TYPNEMA', 'V V I', nbpc, jtynma)
!
! --- CREATION DE L'OBJET .NEMA
!
    call jecrec(ligrxf//'.NEMA', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbpc)
    call jeecra(ligrxf//'.NEMA', 'LONT', long)
    nbgrel = 0
    do 50 ipc = 1, nbpc
        nummae = nint(zr(jtabf+ztabf*(ipc-1)+1))
        nummam = nint(zr(jtabf+ztabf*(ipc-1)+2))
        call xmelel(ndim, jmail, jtymai, nummae, nummam,&
                    imod, iatt, imail, nno)
!
! ----- CREATION DE L'ELEMENT DE CONTACT DANS LE LIGREL
!
        call jecroc(jexnum(ligrxf//'.NEMA', ipc))
        nndel = nno(1)+nno(2)
        call jeecra(jexnum(ligrxf//'.NEMA', ipc), 'LONMAX', nndel+1)
        call jeveuo(jexnum(ligrxf//'.NEMA', ipc), 'E', jad)
        if (iatt(1) .ne. 3) then
            nomtm = mail2(ndim-1,imail(1))//mail2(ndim-1,imail(2))
        else
            nomtm = mail3(ndim-1,imail(1))
        endif
        call jenonu(jexnom('&CATA.TM.NOMTM', nomtm), numtyp)
        zi(jad-1+nndel+1) = numtyp
!
! ----- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE ESCLAVE
!
        do 30 ino = 1, nno(1)
            zi(jad-1+ino) = zi(iacnx1+zi(ilcnx1-1+nummae)-2+ino)
 30     continue
!
! ----- RECOPIE DES NUMEROS DE NOEUDS DE LA MAILLE MAITRE
!
        do 40 ino = 1, nno(2)
            zi(jad-1+nno(1)+ino) = zi(iacnx1+zi(ilcnx1-1+nummam)-2+ ino)
 40     continue
!
! --- TYPE D'ÉLÉMENT TARDIF
!
        nomte=mode(imod)//mail(ndim-1,imail(1))//attr(iatt(1))
        if (iatt(1) .ne. 3) then
            nomte=nomte(1:7)//mail(ndim-1,imail(2))//attr(iatt(2))//&
            '_XH'
        else
            nomte=nomte(1:7)//'_XH'
        endif
        do 60 k = 1, nbgrel
            if (nomte .eq. nomte2(k)) then
                compt(k) = compt(k)+1
                zi(jtynma-1+ipc)=k
                goto 50
            endif
 60     continue
        nbgrel = nbgrel+1
        nomte2(nbgrel) = nomte
        compt(nbgrel) = 1
        zi(jtynma-1+ipc)=nbgrel
 50 end do
    ASSERT(nbgrel.ne.0)
!
! --- CREATION DE L'OBJET .LIEL
!
    call jecrec(ligrxf//'.LIEL', 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbgrel)
!
    long = nbgrel + nbpc
    call jeecra(ligrxf//'.LIEL', 'LONT', long)
    ico = 0
    do 90 k = 1, nbgrel
        ico = ico + 1
        call jecroc(jexnum(ligrxf//'.LIEL', ico))
        call jeecra(jexnum(ligrxf//'.LIEL', ico), 'LONMAX', compt(k)+1)
        call jeveuo(jexnum(ligrxf//'.LIEL', ico), 'E', jad)
!
        call jenonu(jexnom('&CATA.TE.NOMTE', nomte2(k)), ityte)
        zi(jad-1+compt(k)+1) = ityte
!
        jco = 0
        do 80 ipc = 1, nbpc
            if (zi(jtynma-1+ipc) .eq. k) then
                jco = jco + 1
                zi(jad-1+jco) = -ipc
            endif
 80     continue
 90 end do
!
! --- IMPRESSIONS
!
    if (niv .ge. 2) then
        call mmimp2(ifm, noma, ligrxf, jtabf)
    endif
!
! --- INITIALISATION DU LIGREL
!
    call jeecra(ligrxf//'.LGRF', 'DOCU', cval='MECA')
    call initel(ligrxf)
!
! --- MENAGE
!
    call jedetr('&&XMLIGR.TYPNEMA')
!
    call jedema()
end subroutine
