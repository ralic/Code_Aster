subroutine cfmmar(noma, defico, newgeo, sdappa, nzoco,&
                  ntpt, ndimg, ntma, ntno, ntmano)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/apmmvd.h"
#include "asterfort/assert.h"
#include "asterfort/cfcald.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisr.h"
#include "asterfort/cfnben.h"
#include "asterfort/infdbg.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfr.h"
    character(len=8) :: noma
    character(len=19) :: sdappa
    character(len=24) :: defico
    character(len=19) :: newgeo
    integer :: nzoco, ntpt, ndimg, ntma, ntmano, ntno
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - SD APPARIEMENT)
!
! REMPLISSAGE DE LA SD POUR L'APPARIEMENT
!
! ----------------------------------------------------------------------
!
! /!\ SAUF COORDONNNEES DES POINTS (VOIR MMPOIN ET CFPOIN)
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NEWGEO : GEOMETRIE ACTUALISEE
! IN  NZOCO  : NOMBRE DE ZONES
! IN  NTPT   : NOMBRE TOTAL DE POINT A APPARIER
! IN  NDIMG  : DIMENSION DE L'ESPACE
! IN  NTMA   : NOMBRE TOTAL DE MAILLES
! IN  NTNO   : NOMBRE TOTAL DE NOEUDS
! IN  NTMANO : NOMBRE TOTAL DE NOEUD AUX ELEMENTS (ELNO)
!
!
!
!
    integer :: ifm, niv
    character(len=24) :: nomsd
    integer :: jnomsd
    character(len=24) :: apinzi, apinzr
    integer :: jpinzi, jpinzr
    character(len=24) :: apinfi, apinfr
    integer :: jpinfi, jpinfr
    integer :: izone
    integer :: iappa, itypa, itypee, itypem
    integer :: nbpt, nbnom, nbnoe, nbmam, nbmae
    integer :: jdecnm, jdecmm, jdecne, jdecme
    character(len=24) :: aptgel
    integer :: longc, longt, nnosd, ibid, posma, ima
    integer :: zinzr, zinzi
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> ...... REMPLISSAGE SD APPARIEMENT'
    endif
!
! --- SD NOMS
!
    nomsd = sdappa(1:19)//'.NOSD'
    call jeveuo(nomsd, 'E', jnomsd)
!
    zk24(jnomsd+1 -1) = noma
    zk24(jnomsd+2 -1) = newgeo
    zk24(jnomsd+3 -1) = defico
!
! --- SD INFORMATIONS GLOBALES
!
    apinfi = sdappa(1:19)//'.INFI'
    apinfr = sdappa(1:19)//'.INFR'
    call jeveuo(apinfi, 'E', jpinfi)
    call jeveuo(apinfr, 'E', jpinfr)
    zinzr = apmmvd('ZINZR')
    zinzi = apmmvd('ZINZI')
!
    zi(jpinfi+1-1) = nzoco
    zi(jpinfi+2-1) = ntpt
    zi(jpinfi+3-1) = ntma
    zi(jpinfi+4-1) = cfdisi(defico,'PROJ_NEWT_ITER')
    zi(jpinfi+5-1) = ndimg
    zi(jpinfi+6-1) = ntno
!
    zr(jpinfr+1-1) = cfdisr(defico,'PROJ_NEWT_RESI')
!
! --- SD INFORMATIONS PAR ZONE
!
    apinzi = sdappa(1:19)//'.INZI'
    apinzr = sdappa(1:19)//'.INZR'
    call jeveuo(apinzi, 'E', jpinzi)
    call jeveuo(apinzr, 'E', jpinzr)
    do 30 izone = 1, nzoco
!
! ----- DIMENSIONS GENERALES
!
        nbpt = mminfi(defico,'NBPT' ,izone )
        nbnom = mminfi(defico,'NBNOM' ,izone )
        nbnoe = mminfi(defico,'NBNOE' ,izone )
        nbmam = mminfi(defico,'NBMAM' ,izone )
        nbmae = mminfi(defico,'NBMAE' ,izone )
        jdecnm = mminfi(defico,'JDECNM',izone )
        jdecmm = mminfi(defico,'JDECMM',izone )
        jdecne = mminfi(defico,'JDECNE',izone )
        jdecme = mminfi(defico,'JDECME',izone )
        zi(jpinzi+zinzi*(izone-1)+1 -1) = nbpt
        zi(jpinzi+zinzi*(izone-1)+2 -1) = nbnom
        zi(jpinzi+zinzi*(izone-1)+3 -1) = nbnoe
        zi(jpinzi+zinzi*(izone-1)+4 -1) = nbmam
        zi(jpinzi+zinzi*(izone-1)+5 -1) = nbmae
        zi(jpinzi+zinzi*(izone-1)+6 -1) = jdecnm
        zi(jpinzi+zinzi*(izone-1)+7 -1) = jdecmm
        zi(jpinzi+zinzi*(izone-1)+8 -1) = jdecne
        zi(jpinzi+zinzi*(izone-1)+9 -1) = jdecme
!
! ----- TOLERANCES ET TYPE APPARIEMENT
!
        itypa = mminfi(defico,'TYPE_APPA' ,izone )
        zi(jpinzi+zinzi*(izone-1)+10-1) = itypa
        iappa = mminfi(defico,'APPARIEMENT',izone)
        zi(jpinzi+zinzi*(izone-1)+11-1) = iappa
        zr(jpinzr+zinzr*(izone-1)+4 -1) = mminfr(defico,'TOLE_APPA' ,izone )
        zr(jpinzr+zinzr*(izone-1)+5 -1)= mminfr(defico,&
        'TOLE_PROJ_EXT' ,izone )
        if (itypa .eq. 1) then
            zr(jpinzr+zinzr*(izone-1)+1 -1) = mminfr( defico, 'TYPE_APPA_DIRX',izone)
            zr(jpinzr+zinzr*(izone-1)+2 -1) = mminfr( defico, 'TYPE_APPA_DIRY',izone)
            zr(jpinzr+zinzr*(izone-1)+3 -1)= mminfr(defico,&
            'TYPE_APPA_DIRZ',izone )
        endif
!
! ----- ORIENTATION BASE LOCALE (AUTO, FIXE, VECT_Y) - MAITRE
!
        itypem = mminfi(defico,'VECT_MAIT',izone )
        zi(jpinzi+zinzi*(izone-1)+12-1) = itypem
        if (itypem .ne. 0) then
            zr(jpinzr+zinzr*(izone-1)+6 -1) = mminfr( defico, 'VECT_MAIT_DIRX',izone)
            zr(jpinzr+zinzr*(izone-1)+7 -1) = mminfr( defico, 'VECT_MAIT_DIRY',izone)
            zr(jpinzr+zinzr*(izone-1)+8 -1)= mminfr(defico,&
            'VECT_MAIT_DIRZ',izone )
        endif
!
! ----- ORIENTATION BASE LOCALE (AUTO, FIXE, VECT_Y) - ESCLAVE
!
        itypee = mminfi(defico,'VECT_ESCL',izone )
        zi(jpinzi+zinzi*(izone-1)+13-1) = itypee
        if (itypee .ne. 0) then
            zr(jpinzr+zinzr*(izone-1)+9 -1) = mminfr( defico, 'VECT_ESCL_DIRX',izone)
            zr(jpinzr+zinzr*(izone-1)+10-1) = mminfr( defico, 'VECT_ESCL_DIRY',izone)
            zr(jpinzr+zinzr*(izone-1)+11-1)= mminfr(defico,&
            'VECT_ESCL_DIRZ',izone )
        endif
!
! ----- CALCUL AUTORISE MAITRE/ESCLAVE
!
        if (cfcald(defico,izone,'ESCL')) then
            zi(jpinzi+zinzi*(izone-1)+14-1) = 1
        endif
        if (cfcald(defico,izone,'MAIT')) then
            zi(jpinzi+zinzi*(izone-1)+15-1) = 1
        endif
!
30  end do
!
! --- TANGENTES AUX NOEUDS PAR ELEMENT
!
    aptgel = sdappa(1:19)//'.TGEL'
    longt = 0
    do 20 ima = 1, ntma
        posma = ima
        call cfnben(defico, posma, 'CONNEX', nnosd, ibid)
        longc = 6*nnosd
        call jeecra(jexnum(aptgel, ima), 'LONMAX', ival=longc)
        call jecroc(jexnum(aptgel, ima))
        longt = longt + longc
20  end do
    ASSERT(longt.eq.6*ntmano)
!
    call jedema()
!
end subroutine
