subroutine creagm(nbmato, nbpart, sdb, ma, sdbord,&
                  masd)
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:
!       - CREATION DE NOUVEAUX GROUPES DE MAILLES
!              DANS LA STRUCTURE MAILLAGE
!
!    - IN :     NBMATO : NOMBRE DE MAILLES
!               NBPART : NOMBRE DE PARTITION
!               SDB    : VAUT 1 SI GROUP_MA POUR LES BORDS
!               RENUM  : RENUMEROTATION
!               NBMASD : NOMBRE DE MAILLES PAR SOUS DOMAINE
!               MA     : NOM DU MAILLAGE
!               NUMSDM : SOUS DOMAINES DE CHAQUES MAILLE
!
!    - OUT :    MASD   : DONNE LES MAILLES PAR SD
!               IDMASD : INDEX DE MASD
!               NOMSDM : NOM DES GROUP_MA
!               SDBORD : NOM DES GROUP_MA POUR LES BORDS
!
!----------------------------------------------------------------------
! person_in_charge: aimery.assire at edf.fr
!
! CORPS DU PROGRAMME
    implicit none
!
!
! DECLARATION VARIABLES D'APPEL
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterfort/cpclma.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcadr.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    integer :: nbmato, nbpart, renum, nbmasd, sdb, nomsdm, masd, idmasd, numsdm
    character(len=8) :: ma
!
! DECLARATION VARIABLES LOCALES
    integer :: id1, i, maxi, err, isd, nbma, ima, nbre, idma, jvg, jgg, ifm, niv
    integer :: nbgma
    real(kind=8) :: tmps(3)
    character(len=8) :: ktmp
    character(len=24) :: sdbord, nom, grpmav, grpma, gpptnm, grpema
    integer :: iarg
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo('&&FETSKP.RENUM', 'L', renum)
    call jeveuo('&&FETSKP.NBMASD', 'L', nbmasd)
    call jeveuo('&&FETSKP.NUMSDM', 'E', numsdm)
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREAGM', 'INIT', ' ')
        call uttcpu('CPU.CREAGM', 'DEBUT', ' ')
    endif
!
    call wkvect('&&FETSKP.ID1', 'V V I', nbpart, id1)
    call wkvect('&&FETSKP.IDMASD', 'V V I', nbpart+1, idmasd)
    call wkvect('&&FETSKP.NOMSDM', 'V V K24', nbpart, nomsdm)
    call wkvect('&&FETSKP.MASD', 'V V I', nbmato, masd)
!
! ------- On RECUPERE LE NOM DES SOUS DOMAINES
!
    call getvtx(' ', 'NOM_GROUP_MA', 0, iarg, 1,&
                nom, err)
    maxi=0
    do 50 i = 1, len(nom)
        if (nom(i:i) .ne. ' ') maxi=maxi+1
50  end do
!
    do 51 isd = 1, nbpart
        write(ktmp,'(I4)')isd-1
        call lxcadr(ktmp)
        zk24(nomsdm-1+isd) = nom(1:maxi)//ktmp
51  end do
!
    if (sdb .eq. 1) then
        maxi=0
        do 47 i = 1, len(sdbord)
            if (sdbord(i:i) .ne. ' ') maxi=maxi+1
47      continue
        do 66 isd = nbpart/2+1, nbpart
            write(ktmp,'(I4)')isd-nbpart/2-1
            call lxcadr(ktmp)
            zk24(nomsdm-1+isd) = sdbord(1:maxi)//ktmp
66      continue
    endif
!
! ------- CREATION DU TABLEAU DES PLACES DES GRPMA
!
    zi(idmasd)=1
    do 32 isd = 2, nbpart+1
        zi(idmasd-1+isd)=zi(idmasd-1+isd-1)+zi(nbmasd-1+isd-1)
32  end do
!
! ------- CREATION DU TABLEAU DONNANT LES MAILLES POUR CHAQUE GRMPA
!
    do 36 ima = 1, nbmato
        nbre=zi(numsdm-1+ima)+1
        zi(masd-1+zi(idmasd-1+nbre)+zi(id1-1+nbre)) = zi(renum-1+ima)
        zi(id1-1+nbre) = zi(id1-1+nbre)+1
36  end do
!
! ------- ON AGRANDIT LA COLLECTION GROUPE-MAILLE
!
    grpma = ma//'.GROUPEMA       '
    grpmav = '&&OP0029'//'.GROUPEMA       '
    gpptnm = ma//'.PTRNOMMAI      '
    call jeexin(grpma, err)
    if (err .eq. 0) then
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbpart)
        call jecrec(grpma, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbpart)
    else
        call jelira(ma//'.GROUPEMA', 'NMAXOC', nbre)
        call jelira(grpma, 'NOMUTI', nbgma)
        nbre = nbgma + nbpart
        call cpclma(ma, '&&OP0029', 'GROUPEMA', 'V')
        call jedetr(grpma)
        call jedetr(gpptnm)
        call jecreo(gpptnm, 'G N K24')
        call jeecra(gpptnm, 'NOMMAX', nbre)
        call jecrec(grpma, 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                    nbre)
        do 100 i = 1, nbgma
            call jenuno(jexnum(grpmav, i), nom)
            call jecroc(jexnom(grpma, nom))
            call jeveuo(jexnum(grpmav, i), 'L', jvg)
            call jelira(jexnum(grpmav, i), 'LONUTI', nbma)
            call jeecra(jexnom(grpma, nom), 'LONMAX', max(1,nbma))
            call jeecra(jexnom(grpma, nom), 'LONUTI', nbma)
            call jeveuo(jexnom(grpma, nom), 'E', jgg)
            do 102 ima = 0, nbma-1
                zi(jgg+ima) = zi(jvg+ima)
102          continue
100      continue
    endif
!
! ------- ON RAJOUTE LES SD DANS LES GROUPES DE MAILLES
!
    do 38 isd = 1, nbpart
        grpema=zk24(nomsdm-1+isd)
        nbma=zi(nbmasd-1+isd)
        if (nbma .ne. 0) then
            call jecroc(jexnom(ma//'.GROUPEMA', grpema))
            call jeecra(jexnom(ma//'.GROUPEMA', grpema), 'LONMAX', max(1,nbma))
            call jeecra(jexnom(ma//'.GROUPEMA', grpema), 'LONUTI', nbma)
            call jeveuo(jexnom(ma//'.GROUPEMA', grpema), 'E', idma)
            do 41 ima = 0, nbma - 1
                zi(idma+ima) = zi(masd+zi(idmasd-1+isd)-1+ima)
41          continue
        endif
38  end do
!
    call jedetr('&&FETSKP.ID1')
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREAGM', 'FIN', ' ')
        call uttcpr('CPU.CREAGM', 3, tmps)
        write(ifm,*)'--- CREATION DES GRPMA :',tmps(3)
        write(ifm,*)'  '
    endif
!
    call jedema()
end subroutine
