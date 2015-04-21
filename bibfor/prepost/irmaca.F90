subroutine irmaca(ifc, ndim, nno, coordo, nbma,&
                  connex, point, noma, typma, lmod,&
                  nbgrn, nogn, nbgrm, nogm, nive)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/gicoor.h"
#include "asterfort/irmac2.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcaps.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma
    character(len=24) :: nogn(*), nogm(*)
    real(kind=8) :: coordo(*)
    integer :: connex(*), typma(*), point(*), ifc, nive
    aster_logical :: lmod
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT :   ECRITURE DU MAILLAGE AU FORMAT CASTEM2000
!     ENTREE:
!       IFC    : NUMERO D'UNITE LOGIQUE DU FICHIER CASTEM2000
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NNO    : NOMBRE DE NOEUDS DU MAILLAGE
!       COORDO : VECTEUR DES COORDONNEES DES NOEUDS
!       NBMA   : NOMBRE DE MAILLES DU MAILLAGE
!       CONNEX : CONNECTIVITES
!       POINT  : POINTEUR DANS LES CONNECTIVITES
!       NOMA   : NOM DU MAILLAGE
!       TYPMA  : TYPES DES MAILLES
!       LMOD   : LOGIQUE INDIQUANT SI IMPRESSION MODELE OU MAILLAGE
!                 .TRUE. MODELE
!       TOUT CE QUI SUIT CONCERNE LES GROUPES:
!          NBGRN: NOMBRE DE GROUPES DE NOEUDS
!          NOGN : NOM DES GROUPES DE NOEUDS
!          NBGRM: NOMBRE DE GROUPES DE MAILLES
!          NOGM : NOM DES GROUPES DE MAILLES
!       NIVE    : NIVEAU IMPRESSION CASTEM 3 OU 10
!     ------------------------------------------------------------------
    integer :: ibid, ierr, nbtyma
    real(kind=8) :: rbid
    character(len=8) :: ktype, kbid, gtype
    character(len=9) :: toto
    character(len=24) :: nolili, nomjv, nomob, nogrno
!
!-----------------------------------------------------------------------
    integer :: i, iacorr, iad, iagrma, iagrno, idep, ideu
    integer :: idi, iel, igm, ign, igr, igre, igrel, iexi
    integer :: ij, ijk, ijp, ilong, ima, imodl, imoin
    integer :: ino, inumm, iobj, iplu, ipog, ipoin, ipoin1
    integer :: ipoin2, iret, iso, itrdeu, itrtro, ity, ityca
    integer :: itype, iun, izero, j, jcoo, jcoul, jgn
    integer :: jgrele, jjl, jjm, jligr, jlongr, jmai, jmodl
    integer :: jmt, jnn, jno2, jnoe, jnom, jnum, jpla
    integer :: jpoi, jpos, jty, nbcoul, nbelgr, nbelt, nbgma
    integer :: nbgno, nbgr, nbgrel, nbgrm, nbgrn, nbm, nbma
    integer :: nbmodl, nbn, nbobj, nbobjn, nbpo, nbr, nbsmo
    integer :: nbsobj, nbtot, ndim, nmamax, nno, nnoe, nombre
!
!-----------------------------------------------------------------------
    nomjv = '&&OP0039.NOM_MODELE'
    call jeexin(nomjv, iret)
    if (iret .eq. 0) then
        nbmodl = 0
    else
        call jeveuo(nomjv, 'L', jmodl)
        call jelira(nomjv, 'LONUTI', nbmodl)
    endif
!
!     ECRITURE DES INFORMATIONS GENERALES DU MAILLAGE
    call jemarq()
    ibid = 4
    ierr = 0
    rbid = 0.d0
    write (ifc,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ibid
    if (nive .eq. 3) then
        ibid = 3
    else if (nive.eq.10) then
        ibid = 10
    endif
    write (ifc,'(3(A,I4))')  ' NIVEAU',ibid,' NIVEAU ERREUR',ierr,&
     &                    ' DIMENSION',ndim
    write (ifc,'(A,E12.5)')   ' DENSITE',rbid
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtyma)
    call wkvect('&&IRMACA.JM', 'V V I', nbtyma, jjm)
    call wkvect('&&IRMACA.LOGIQ', 'V V L', nbtyma, jjl)
!
!     ECRITURE DES INFORMATIONS GENERALES CASTEM 2000
    ibid = 7
    izero = 0
    imoin = -1
    iplu = 1
    ideu = 2
    write (ifc,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ibid
    if (nive .eq. 10) ibid = 8
    write (ifc,'(A,I4)')  ' NOMBRE INFO CASTEM2000',ibid
    if (ndim .eq. 2) then
        write (ifc,'(7(A,I4))') ' IFOUR',imoin,' NIFOUR',izero,&
        ' IFOMOD',imoin,' IECHO',iplu,' IIMPI',izero,' IOSPI',izero,&
        ' ISOTYP',iplu
        if (nive .eq. 10) write (ifc,'(A,I6)') ' NSDPGE',izero
    else if (ndim.eq.3) then
        write (ifc,'(7(A,I4))') ' IFOUR',ideu,' NIFOUR',izero,&
        ' IFOMOD',ideu,' IECHO',iplu,' IIMPI',izero,' IOSPI',izero,&
        ' ISOTYP',iplu
        if (nive .eq. 10) write (ifc,'(A,I6)') ' NSDPGE',izero
    endif
!
!     LECTURE DES NOEUDS
    nbobjn = 0
    if (nbgrn .ne. 0) then
        call wkvect('&&IRMACA.NOEUD', 'V V I', nbgrn, jgn)
        call wkvect('&&IRMACA.NNOEU', 'V V K24', nbgrn, jnn)
        do 50 ign = 1, nbgrn
            call jeveuo(jexnum(noma//'.GROUPENO', ign), 'L', iagrno)
            call jelira(jexnum(noma//'.GROUPENO', ign), 'LONUTI', nbn)
            if (nbn .eq. 1) then
                nbobjn = nbobjn + 1
                zi(jgn-1+nbobjn) = zi(iagrno-1+1)
                zk24(jnn-1+nbobjn) = nogn(ign)
            endif
 50     continue
    endif
!
! ECRITURE DES NOEUDS
!
    if (nive .eq. 3) then
        ibid = 2
        write (ifc,'(A,I4)') ' ENREGISTREMENT DE TYPE',ibid
        write (ifc,'(A,I4,A,I5,A,I5)')  ' PILE NUMERO',izero,&
     &     'NBRE OBJETS NOMMES',nbobjn,'NBRE OBJETS',nno
        if (nbobjn .ne. 0) then
            write(ifc,1002) (zk24(jnn-1+i)(1:8),i=1,nbobjn)
            write(ifc,1003) (zi(jgn-1+i),i=1,nbobjn)
        endif
        call wkvect('&&IRMACA.COOR', 'V V R', (ndim+1)*nno, jcoo)
        do 1 ino = 1, nno
            do 2 idi = 1, ndim
                zr(jcoo-1+(ino-1)*(ndim+1)+idi) = coordo(3*(ino-1)+ idi)
  2         continue
            zr(jcoo-1+(ino-1)*(ndim+1)+4) = rbid
  1     continue
        write(ifc,1001) (zr(jcoo-1+i),i=1,(ndim+1)*nno)
    endif
!
! ECRITURE DES MAILLES
!
    nbgno = nbgrn-nbobjn
    nbtot = nbgno
    ipog = 1
    call wkvect('&&IRMACA.NBSOB', 'V V I', nbgrm+1+nbmodl, jnum)
    call wkvect('&&IRMACA.NBMAI', 'V V I', (nbgrm+1)*nbtyma, jty)
    call wkvect('&&IRMACA.IPOSI', 'V V I', nbgrm+nbgno+2, jpos)
    call wkvect('&&IRMACA.NOMOB', 'V V K24', nbgrm+nbgno+2, jnom)
    call wkvect('&&IRMACA.NOMO2', 'V V K24', nbgrm+nbgno+2, jno2)
    call wkvect('&&IRMACA.NBTO', 'V V I ', nbgrm+1, jmt)
!
    if (nbgrn .ne. 0) then
        ijp = 0
        call jecreo('&&IRMACA.NOGNOK8', 'V N K8')
        call jeecra('&&IRMACA.NOGNOK8', 'NOMMAX', nbgrn)
        do 51 ign = 1, nbgrn
            call jenuno(jexnum(noma//'.GROUPENO', ign), nogrno)
            call jenonu(jexnom('&&IRMACA.NOGNOK8', nogrno(1:8)), iexi)
            if (iexi .ne. 0) call utmess('F', 'PREPOST5_47', sk=nogrno(1:8))
            call jecroc(jexnom('&&IRMACA.NOGNOK8', nogrno(1:8)))
            call jelira(jexnum(noma//'.GROUPENO', ign), 'LONUTI', nbn)
            if (nbn .ne. 1) then
                ijp =ijp+1
                zi(jpos-1+ijp) = ijp + 1
                ipog = ijp+1
                zk24(jnom-1+ijp) = nogn(ign)
                zk24(jno2-1+ijp) = nogn(ign)
                call lxcaps(zk24(jno2-1+ijp))
            endif
 51     continue
        call jedetr('&&IRMACA.NOGNOK8')
    endif
!
    nbgma = 0
    nmamax = nbma
    if (nbgrm .ne. 0) then
        do 21 igm = 1, nbgrm
            do 15 i = 1, nbtyma
                zl(jjl-1+i)= .false.
 15         continue
            call jeveuo(jexnum(noma//'.GROUPEMA', igm), 'L', iagrma)
            call jelira(jexnum(noma//'.GROUPEMA', igm), 'LONUTI', nbm)
            if (nbm .eq. 0) goto 21
            nmamax = max(nmamax,nbm)
            nbgma = nbgma + 1
            zi(jmt-1+igm) = nbm
            zk24(jnom-1+nbgno+nbgma) = nogm(igm)
            zk24(jno2-1+nbgno+nbgma) = nogm(igm)
            call lxcaps(zk24(jno2-1+nbgno+nbgma))
            nombre = 0
            do 22 jmai = 1, nbm
                ima = zi(iagrma-1+jmai)
                itype = typma(ima)
                if (.not.zl(jjl-1+itype)) then
                    call jenuno(jexnum('&CATA.TM.NOMTM', itype), kbid)
                    call jeexin('&&IRMA.G.'//nogm(igm)(1:8)//kbid(1:7), iret)
                    if (iret .gt. 0) call utmess('F', 'PREPOST5_47', sk=nogm(igm)(1:8))
                    call wkvect('&&IRMA.G.'//nogm(igm)(1:8)//kbid(1:7), 'V V I', nbm,&
                                zi(jjm-1+itype))
                    zl(jjl-1+itype) = .true.
                endif
                ibid = zi(jty-1+(nbgma-1)*nbtyma+itype) + 1
                zi(jty-1+(nbgma-1)*nbtyma+itype) = ibid
                zi(zi(jjm-1+itype)-1+ibid) = ima
 22         continue
            do 23 ity = 1, nbtyma
                if (zi(jty-1+(nbgma-1)*nbtyma+ity) .ne. 0) nombre = nombre+1
 23         continue
            zi(jnum-1+igm) = nombre
            if (nombre .eq. 1) then
                ipog = ipog + 1
                zi (jpos-1+nbgno+nbgma) = ipog
                nbtot = nbtot + nombre
            else
                ipog = ipog + nombre + 1
                zi (jpos-1+nbgno+igm) = ipog
                nbtot = nbtot + nombre + 1
            endif
 21     continue
    endif
!
! TOUT LE MAILLAGE
!
    do 16 i = 1, nbtyma
        zl(jjl-1+i) = .false.
 16 end do
    nombre= 0
    do 33 ima = 1, nbma
        itype = typma(ima)
        if (.not.zl(jjl-1+itype)) then
            call jenuno(jexnum('&CATA.TM.NOMTM', itype), kbid)
            call wkvect('&&IRMA.M.'//noma//kbid(1:7), 'V V I', nbma, zi( jjm-1+itype))
            zl(jjl-1+itype)= .true.
        endif
        ibid = zi(jty-1+nbgma*nbtyma+itype) + 1
        zi(jty-1+nbgma*nbtyma+itype) = ibid
        zi(zi(jjm-1+itype)-1+ibid) = ima
 33 end do
    do 34 ity = 1, nbtyma
        if (zi(jty-1+nbgma*nbtyma+ity) .ne. 0) nombre = nombre+1
 34 end do
    if (nombre .eq. 1) then
        zi(jpos-1+nbgma+nbgno+1) = ipog + nombre
        nbtot = nbtot + nombre
    else
        nbtot = nbtot + nombre + 1
        zi(jpos-1+nbgma+nbgno+1) = ipog+nombre+1
    endif
    zi(jnum-1+nbgrm+1) = nombre
    zk24(jnom-1+nbgma+nbgno+1) = noma
    zk24(jno2-1+nbgma+nbgno+1) = noma
    call lxcaps(zk24(jno2-1+nbgma+nbgno+1))
    zi(jmt-1+nbgma+1) = nbma
!
! IMPRESSION DU MODELE
!
    if (nbmodl .ne. 0) then
        call jeexin('&&OP0039.LIGREL', iret)
        if (iret .ne. 0) call jedetr('&&OP0039.LIGREL')
        call jecrec('&&OP0039.LIGREL', 'V V I', 'NU', 'DISPERSE', 'VARIABLE',&
                    nbmodl)
        do 200 i = 1, nbmodl
            nolili = zk24(jmodl+i-1)
            call jeveuo(nolili(1:19)//'.LIEL', 'L', jligr)
            call jelira(nolili(1:19)//'.LIEL', 'NUTIOC', nbgrel)
            call jeveuo(jexatr(nolili(1:19)//'.LIEL', 'LONCUM'), 'L', jlongr)
!
            ilong = nbgrel*(nbgrel+4)+1
            call jeecra(jexnum('&&OP0039.LIGREL', i), 'LONMAX', ilong)
            call jeveuo(jexnum('&&OP0039.LIGREL', i), 'E', jgrele)
!
            do 202 igre = 1, nbgrel
                ipoin1 = zi(jlongr-1+igre)
                ipoin2 = zi(jlongr-1+igre+1)
                nbelgr = ipoin2-ipoin1-1
                if ( nbelgr.eq.0 ) cycle
                ima = zi(jligr-1+ipoin1+1-1)
                if (ima .le. 0) goto 202
                ipoin = point(ima)
                nnoe = point(ima+1)-ipoin
                itype = typma(ima)
                if (igre .eq. 1) then
                    zi(jgrele-1+1) = 1
                    zi(jgrele-1+2) = itype
                    zi(jgrele-1+4) = 1
                    zi(jgrele-1+5) = nbelgr
                    zi(jgrele-1+6) = igre
                else
                    nbsmo = zi(jgrele-1+1)
                    do 210 iso = 1, nbsmo
                        if (itype .eq. zi(jgrele+(iso-1)*(4+nbgrel)+1)) then
                            nbr = zi(jgrele+(iso-1)*(4+nbgrel)+3)
                            zi(jgrele+(iso-1)*(4+nbgrel)+3) = nbr + 1
                            nbelt = zi(jgrele+(iso-1)*(4+nbgrel)+4)
                            zi(jgrele+(iso-1)*(4+nbgrel)+4) = nbelt + nbelgr
                            zi(jgrele+(iso-1)*(4+nbgrel)+5+nbr) =&
                            igre
                            goto 202
                        endif
210                 continue
                    nbsmo = nbsmo+1
                    zi(jgrele-1+1) = nbsmo
                    zi(jgrele+(nbsmo-1)*(4+nbgrel)+1) = itype
                    zi(jgrele+(nbsmo-1)*(4+nbgrel)+3) = 1
                    zi(jgrele+(nbsmo-1)*(4+nbgrel)+4) = nbelgr
                    zi(jgrele+(nbsmo-1)*(4+nbgrel)+5) = igre
                endif
202         continue
            nbobj = zi(jgrele-1+1)
            zi(jnum-1+nbgrm+1+i) = nbobj
            if (i .eq. 1) then
                zk24(jnom-1+nbgma+nbgno+2) = zk24(jmodl+i-1)(1:8)
                zk24(jno2-1+nbgma+nbgno+2) = zk24(jmodl+i-1)(1:8)
                call lxcaps(zk24(jno2-1+nbgma+nbgno+2))
            endif
            if (nbobj .gt. 1) then
                if (i .eq. 1) zi(jpos-1+nbgma+nbgno+2)=zi(jpos-1+nbgma+ nbgno+1)+nbobj+1
                nbtot = nbtot + nbobj + 1
            else
                if (i .eq. 1) zi(jpos-1+nbgma+nbgno+2) = zi( jpos-1+ nbgma+nbgno+1 )+1
                nbtot = nbtot + nbobj
            endif
200     continue
    else
        nbobj=0
    endif
!
!     ECRITURE DES MAILLES
!
    nbcoul = max(nno,nmamax)
    call wkvect('&&IRMACA.COUL', 'V V I', nbcoul, jcoul)
    do 12 i = 1, nbcoul
        zi(jcoul-1+i) = 0
 12 end do
    call wkvect('&&IRMACA.PLACE', 'V V I', nmamax, jpla)
    call wkvect('&&IRMACA.MPOI1', 'V V I', nno, jpoi)
    call wkvect('&&IRMACA.NOEU', 'V V I', nmamax*27, jnoe)
!
    ibid = 2
    write (ifc,'(A,I4)')   ' ENREGISTREMENT DE TYPE',ibid
    if (lmod) then
        if (nive .eq. 3) then
            write (ifc,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',iplu,&
            'NBRE OBJETS NOMMES',nbgma+nbgno+2,'NBRE OBJETS',nbtot+1
            write(ifc,1002) (zk24(jno2-1+i)(1:8),i=1,nbgma+nbgno+2)
            write(ifc,1003) (zi(jpos-1+i),i=1,nbgma+nbgno+2)
        else if (nive.eq.10) then
            write (ifc,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',iplu,&
            'NBRE OBJETS NOMMES',nbgma+nbgno+2,'NBRE OBJETS',nbtot+1
            write(ifc,1002) (zk24(jno2-1+i)(1:8),i=1,nbgma+nbgno+2)
            write(ifc,1005) (zi(jpos-1+i),i=1,nbgma+nbgno+2)
        endif
    else
        if (nive .eq. 3) then
            write (ifc,'(A,I4,A,I5,A,I5)') ' PILE NUMERO',iplu,&
            'NBRE OBJETS NOMMES',nbgma+nbgno+1,'NBRE OBJETS',nbtot+1
            write(ifc,1002) (zk24(jno2-1+i)(1:8),i=1,nbgma+nbgno+1)
            write(ifc,1003) (zi(jpos-1+i),i=1,nbgma+nbgno+1)
        else if (nive.eq.10) then
            write (ifc,'(A,I4,A,I8,A,I8)') ' PILE NUMERO',iplu,&
            'NBRE OBJETS NOMMES',nbgma+nbgno+1,'NBRE OBJETS',nbtot+1
            write(ifc,1002) (zk24(jno2-1+i)(1:8),i=1,nbgma+nbgno+1)
            write(ifc,1005) (zi(jpos-1+i),i=1,nbgma+nbgno+1)
        endif
    endif
! ECRITURE DES OBJETS NOMMES
!
! -- ECRITURE DE TOUS LES NOEUDS (MAILLES DE TYPE POINT) ---
!
    ityca = 1
    if (nive .eq. 3) then
        write(ifc,'(5(I5))') ityca,izero,izero,ityca,nno
        write(ifc,'(16(I5))') (zi(jcoul-1+i),i=1,nno)
        write(ifc,'(16(I5))') (i,i=1,nno)
    else if (nive.eq.10) then
        write(ifc,'(5(I8))') ityca,izero,izero,ityca,nno
        write(ifc,'(10(I8))') (zi(jcoul-1+i),i=1,nno)
        write(ifc,'(10(I8))') (i,i=1,nno)
    endif
!
! -- ECRITURE DE TOUS LES GROUPES DE NOEUDS (MAILLES DE TYPE POINT) ---
!
    do 155 ign = 1, nbgrn
        call jeveuo(jexnum(noma//'.GROUPENO', ign), 'L', iagrno)
        call jelira(jexnum(noma//'.GROUPENO', ign), 'LONUTI', nbn)
        if (nbn .ne. 1) then
            do 156 j = 1, nbn
                zi(jpoi-1+j) = zi(iagrno-1+j)
156         continue
            if (nive .eq. 3) then
                write(ifc,'(5(I5))') ityca,izero,izero,ityca,nbn
                write(ifc,'(16(I5))') (zi(jcoul-1+i),i=1,nbn)
                write(ifc,'(16(I5))') (zi(jpoi-1+i),i=1,nbn)
            else if (nive.eq.10) then
                write(ifc,'(5(I8))') ityca,izero,izero,ityca,nbn
                write(ifc,'(10(I8))') (zi(jcoul-1+i),i=1,nbn)
                write(ifc,'(10(I8))') (zi(jpoi-1+i),i=1,nbn)
            endif
        endif
155 end do
!
    call gicoor()
!
    nbpo = nbgno+1
    do 120 i = 1, nbcoul
        zi(jcoul-1+i) = 7
120 end do
    ijk =0
    do 81 igm = 1, nbgrm+1
        nbsobj = zi(jnum-1+igm)
        idep =1
        if (nbsobj .eq. 0) goto 81
        ijk = ijk + 1
        do 82 iobj = 1, nbsobj
            do 85 i = idep, nbtyma
                if (zi(jty-1+(ijk-1)*nbtyma+i) .ne. 0) then
                    itype = i
                    call jenuno(jexnum('&CATA.TM.NOMTM', i), ktype)
                    idep=itype+1
                    goto 87
                endif
 85         continue
 87         continue
            nomob =zk24(jnom-1+nbgno+ijk)
            if (igm .le. nbgrm) then
                toto = '&&IRMA.G.'
            else
                toto = '&&IRMA.M.'
            endif
            call jeveuo(toto//nomob(1:8)//ktype(1:7), 'L', iad)
            ima = zi(iad)
            ipoin=point(ima)
            nnoe=point(ima+1)-ipoin
            nbm =zi(jty-1+(ijk-1)*nbtyma+itype)
            call irmac2(ktype, ityca, gtype, nnoe)
            call jeveuo(jexnom('&&GILIRE.CORR_ASTER_GIBI', gtype), 'L', iacorr)
            if (nive .eq. 3) then
                write(ifc,'(5(I5))') ityca,izero,izero,nnoe,nbm
                write(ifc,'(16(I5))') (zi(jcoul-1+i),i=1,nbm)
            else if (nive.eq.10) then
                write(ifc,'(5(I8))') ityca,izero,izero,nnoe,nbm
                write(ifc,'(10(I8))') (zi(jcoul-1+i),i=1,nbm)
            endif
            do 55 i = 1, nbm
                ipoin = point(zi(iad-1+i))
                do 56 j = 1, nnoe
                    ij = zi(iacorr-1+j)
                    zi(jnoe-1+(i-1)*nnoe+j) = connex(ipoin-1+ij)
 56             continue
 55         continue
            if (nive .eq. 3) then
                write(ifc,'(16(I5))') (((zi(jnoe-1+(i-1)*nnoe+j))&
                ,j=1,nnoe),i=1,nbm)
            else if (nive.eq.10) then
                write(ifc,'(10(I8))') (((zi(jnoe-1+(i-1)*nnoe+j))&
                ,j=1,nnoe),i=1,nbm)
            endif
 82     end do
        if (nbsobj .gt. 1) then
            if (nive .eq. 3) then
                write(ifc,'(5(I5))') izero,nbsobj,izero,izero,izero
            else if (nive.eq.10) then
                write(ifc,'(5(I8))') izero,nbsobj,izero,izero,izero
            endif
            do 99 i = 1, nbsobj
                zi(jpla-1+i) = nbpo + i
 99         continue
            nbpo = nbpo+nbsobj+1
            if (nive .eq. 3) then
                write(ifc,'(16(I5))') ((zi(jpla-1+i)), i=1,nbsobj)
            else if (nive.eq.10) then
                write(ifc,'(10(I8))') ((zi(jpla-1+i)), i=1,nbsobj)
            endif
        else
            nbpo = nbpo + 1
        endif
 81 end do
!
! IMPRESSION DU MODELE
!
    if (nbmodl .ne. 0) then
        do 300 imodl = 1, nbmodl
            nolili = zk24(jmodl+imodl-1)
            call jeveuo(nolili(1:19)//'.LIEL', 'L', jligr)
            call jelira(nolili(1:19)//'.LIEL', 'NUTIOC', nbgrel)
            call jeveuo(jexatr(nolili(1:19)//'.LIEL', 'LONCUM'), 'L', jlongr)
            call jeveuo(jexnum('&&OP0039.LIGREL', imodl), 'E', jgrele)
            nbsmo = zi(jgrele-1+1)
            do 302 iso = 1, nbsmo
                nbgr = zi(jgrele+(iso-1)*(4+nbgrel)+3)
                nbelt = zi(jgrele+(iso-1)*(4+nbgrel)+4)
                iel = 0
                do 304 igr = 1, nbgr
                    igrel = zi(jgrele+(iso-1)*(4+nbgrel)+4+igr)
                    ipoin1 = zi(jlongr-1+igrel)
                    ipoin2 = zi(jlongr-1+igrel+1)
                    nbelgr = ipoin2-ipoin1-1
                    if ( nbelgr.eq.0 ) cycle
                    ima = zi(jligr-1+ipoin1+1-1)
                    ipoin = point(ima)
                    nnoe = point(ima+1)-ipoin
                    itype = typma(ima)
                    call jenuno(jexnum('&CATA.TM.NOMTM', itype), ktype)
                    call irmac2(ktype, ityca, gtype, nnoe)
                    call jeveuo(jexnom('&&GILIRE.CORR_ASTER_GIBI', gtype),&
                                'L', iacorr)
                    do 306 i = 1, nbelgr
                        iel = iel +1
                        ima = zi(jligr-1+ipoin1+i-1)
                        if (ima .le. 0) goto 306
                        ipoin = point(ima)
                        nnoe = point(ima+1) -ipoin
                        if (ktype .eq. 'QUAD9' .or. ktype .eq. 'TRIA7') nnoe=nnoe-1
                        if (ktype .eq. 'PENTA18') nnoe=nnoe-3
                        if (ktype .eq. 'SEG4') nnoe=nnoe-2
                        do 308 j = 1, nnoe
                            ij = zi(iacorr-1+j)
                            zi(jnoe-1+(iel-1)*nnoe+j) = connex(ipoin- 1+ij)
308                     continue
306                  continue
304             continue
                if (nive .eq. 3) then
                    write(ifc,'(5(I5))') ityca,izero,izero,nnoe,nbelt
                    write(ifc,'(16(I5))') (zi(jcoul-1+i),i=1,nbelt)
                    write(ifc,'(16(I5))') (((zi(jnoe-1+(i-1)*nnoe+j)),&
                    j=1,nnoe),i=1,nbelt)
                else if (nive.eq.10) then
                    write(ifc,'(5(I8))') ityca,izero,izero,nnoe,nbelt
                    write(ifc,'(10(I8))') (zi(jcoul-1+i),i=1,nbelt)
                    write(ifc,'(10(I8))') (((zi(jnoe-1+(i-1)*nnoe+j)),&
                    j=1,nnoe),i=1,nbelt)
                endif
302         continue
            if (nbsmo .gt. 1) then
                if (nive .eq. 3) then
                    write(ifc,'(5(I5))') izero,nbsmo,izero,izero,&
                    izero
                else if (nive.eq.10) then
                    write(ifc,'(5(I8))') izero,nbsmo,izero,izero,&
                    izero
                endif
                do 310 iso = 1, nbsmo
                    zi(jpla-1+iso) = nbpo + iso
                    zi(jgrele+(iso-1)*(4+nbgrel)+2) = zi(jpla-1+iso)
310             continue
                nbpo = nbpo+nbsmo+1
                if (nive .eq. 3) then
                    write(ifc,'(16(I5))') ((zi(jpla-1+i)), i=1,nbsmo)
                else if (nive.eq.10) then
                    write(ifc,'(10(I8))') ((zi(jpla-1+i)), i=1,nbsmo)
                endif
            else
                nbpo = nbpo + 1
                zi(jgrele-1+3) = nbpo
            endif
300     continue
    endif
!
! ECRITURE DES NOEUDS
!
    if (nive .eq. 10) then
        ibid = 2
        itrdeu = 32
        itrtro = 33
        write (ifc,'(A,I4)') ' ENREGISTREMENT DE TYPE',ibid
        write (ifc,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',itrdeu,&
     &     'NBRE OBJETS NOMMES',nbobjn,'NBRE OBJETS',nno
        if (nbobjn .ne. 0) then
            write(ifc,1002) (zk24(jnn-1+i)(1:8),i=1,nbobjn)
            write(ifc,1005) (zi(jgn-1+i),i=1,nbobjn)
        endif
        write (ifc,'(I8)') nno
        call wkvect('&&IRMACA.NUNN', 'V V I', nno, inumm)
        do 442 i = 1, nno
            zi(inumm-1+i) = i
442     continue
        write(ifc,'(10(I8))') (zi(inumm-1+i),i=1,nno)
!
        write (ifc,'(A,I4)') ' ENREGISTREMENT DE TYPE',ibid
        iun = 1
        write (ifc,'(A,I4,A,I8,A,I8)')  ' PILE NUMERO',itrtro,&
     &     'NBRE OBJETS NOMMES',izero,'NBRE OBJETS',iun
!
        call wkvect('&&IRMACA.COOR', 'V V R', (ndim+1)*nno, jcoo)
        write (ifc,'(I8)') nno*(ndim+1)
        do 111 ino = 1, nno
            do 222 idi = 1, ndim
                zr(jcoo-1+(ino-1)*(ndim+1)+idi) = coordo(3*(ino-1)+ idi)
222         continue
            zr(jcoo-1+(ino-1)*(ndim+1)+4) = rbid
111     continue
        write(ifc,1001) (zr(jcoo-1+i),i=1,(ndim+1)*nno)
    endif
!
    call jedetc('V', '&&IRMACA', 1)
    call jedetc('V', '&&IRMA.G', 1)
    call jedetc('V', '&&IRMA.M', 1)
    call jedetr('&&GILIRE.CORR_ASTER_GIBI')
!
    1001 format (3(1x,d21.14))
    1002 format (8(1x,a8))
    1003 format (16(i5))
    1005 format (10(i8))
!
    call jedema()
end subroutine
