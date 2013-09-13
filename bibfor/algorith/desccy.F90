subroutine desccy(nomres)
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
!***********************************************************************
!    P. RICHARD     DATE 07/03/91
!-----------------------------------------------------------------------
!  BUT:  CREATION DE LA NUMEROTATION GENERALISEE POUR LE PROBLEME
    implicit none
!        CYCLIQUE
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM UTILISATEUR DU RESULTAT
! BASMOD   /I/: NOM UTILISATEUR DE L'EVENTUELLE BASE MODALE (OU BLANC)
! RESCYC   /I/: NOM UTILISATEUR EVENTUEL CONCEPT MODE CYCLIQUE(OU BLANC)
! NUMCYC   /O/: NOM K24 DE LA NUMEROTATION RESULTAT
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/bmnodi.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
!
    integer :: vali(3)
!
!
!
    character(len=8) :: intf, kbid, basmod, nomres
    character(len=24) :: noeint
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, ier, lddnin, ldnoea, ldnoed, ldnoeg, ldnumg
    integer :: lldesc, llref, nba, nbd, nbda, nbdd, nbdg
    integer :: nbg, nbmcal, nbmod, nbmod1, nbmod2, nbnot, nboc
    integer :: nbtemp, numa, numd, numg
!-----------------------------------------------------------------------
    call jemarq()
    kbid=' '
!-----------------------------------------------------------------------
!
!------------------RECUPERATION CONCEPT AMONT---------------------------
!
    call jeveuo(nomres//'.CYCL_REFE', 'L', llref)
    intf=zk24(llref+1)
    basmod=zk24(llref+2)
!
!-----RECUPERATION NUMEROS INTERFACES DROITE GAUCHE ET AXE--------------
!
    call jeveuo(nomres//'.CYCL_NUIN', 'L', lddnin)
    numd=zi(lddnin)
    numg=zi(lddnin+1)
    numa=zi(lddnin+2)
!
!----------RECUPERATION DU DESCRIPTEUR DES DEFORMEES STATIQUES----------
!
    call jeveuo(intf//'.IDC_DEFO', 'L', lldesc)
    call jelira(intf//'.IDC_DEFO', 'LONMAX', nbnot)
    nbnot=nbnot/3
!
!--------RECUPERATION DES DEFINITIONS DES INTERFACES DROITE ET GAUCHE---
!
    noeint=intf//'.IDC_LINO'
!
    call jeveuo(jexnum(noeint, numd), 'L', ldnoed)
    call jelira(jexnum(noeint, numd), 'LONMAX', nbd)
!
    call jeveuo(jexnum(noeint, numg), 'L', ldnoeg)
    call jelira(jexnum(noeint, numg), 'LONMAX', nbg)
!
    if (numa .gt. 0) then
        call jeveuo(jexnum(noeint, numa), 'L', ldnoea)
        call jelira(jexnum(noeint, numa), 'LONMAX', nba)
    endif
!
    if (nbg .ne. nbd) then
        vali (1) = nbd
        vali (2) = nbg
        call u2mesg('F', 'ALGORITH12_79', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
!
!------COMPTAGE DEFORMEES STATIQUES INTERFACE DROITE GAUCHE-------------
!
    call bmnodi(basmod, kbid, '        ', numd, 0,&
                ibid, nbdd)
    kbid=' '
    call bmnodi(basmod, kbid, '        ', numg, 0,&
                ibid, nbdg)
!
!--------------TEST SUR NOMBRE DE DDL AUX INTERFACES--------------------
!
    if (nbdd .ne. nbdg) then
        vali (1) = nbdd
        vali (2) = nbdg
        call u2mesg('F', 'ALGORITH12_80', 0, ' ', 2,&
                    vali, 0, 0.d0)
    endif
!
!-----COMPTAGE NOMBRE DEFORMEES STATIQUE SUR EVENTUELLE INTERFACE AXE---
!
    nbda=0
    if (numa .gt. 0) then
        kbid=' '
        call bmnodi(basmod, kbid, '        ', numa, 0,&
                    ibid, nbda)
    else
        nbda=0
    endif
!
!--------DETERMINATION DU NOMBRE DE MODES PROPRES DE LA BASE------------
!
!  NOMBRE DE MODES DEMANDES
!
    call getvis('   ', 'NB_MODE', iocc=1, scal=nbmod1, nbret=ibid)
!
!  NOMBRE DE MODES EXISTANTS
    call dismoi('F', 'NB_MODES_DYN', basmod, 'RESULTAT', nbmod2,&
                kbid, ier)
!
!  TEST
!
    if (nbmod2 .eq. 0) then
        call u2mesg('F', 'ALGORITH12_81', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
    nbmod=min(nbmod1,nbmod2)
!
!---------DETERMINATION DU NOMBRE DE MODES PROPRES A CALCULER-----------
!
    call getvis('CALCUL', 'NMAX_FREQ', iocc=1, nbval=0, nbret=nboc)
!
    if (nboc .eq. 0) then
        nbmcal=nbmod
    else
        call getvis('CALCUL', 'NMAX_FREQ', iocc=1, scal=nbmcal, nbret=ibid)
    endif
!
    if (nbmcal .gt. nbmod) then
        nbtemp=nbmcal-nbmod
        vali (1) = nbmcal
        vali (2) = nbmod
        vali (3) = nbtemp
        call u2mesg('A', 'ALGORITH12_82', 0, ' ', 3,&
                    vali, 0, 0.d0)
    endif
!
!----------------ALLOCATION DE L'OBJET .DESC----------------------------
!
    call wkvect(nomres//'.CYCL_DESC', 'G V I', 4, ldnumg)
!
!------------------CREATION DE LA NUMEROTATION--------------------------
!
    zi(ldnumg)=nbmod
    zi(ldnumg+1)=nbdd
    zi(ldnumg+2)=nbda
    zi(ldnumg+3)=nbmcal
!
    call jedema()
end subroutine
