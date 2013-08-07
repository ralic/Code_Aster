subroutine dismte(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(TYPE_ELEM)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismtm.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=32) :: repk
    character(len=16) :: nomob
! ----------------------------------------------------------------------
!    IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE TYPE_ELEM (K16)
!    OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    integer :: ibid
    character(len=8) :: nomtm
    character(len=16) :: nophen, nomodl
    integer :: ite, nbphen, nbtm, ico, iphen, nbmodl, imodl, iamodl, ii
    integer :: iaopte, nbopt, iopt, ioptte, nrig, irig
    parameter(nrig=5)
    character(len=16) :: optrig(nrig)
    data optrig/'RIGI_ACOU','RIGI_THER','RIGI_MECA','RIGI_MECA_TANG',&
     &     'FULL_MECA'/
!
!
!
    call jemarq()
    nomob=nomobz
    repk=' '
    ierd=0
!
    call jenonu(jexnom('&CATA.TE.NOMTE', nomob), ite)
    call jeveuo('&CATA.TE.TYPEMA', 'L', ibid)
    nomtm=zk8(ibid-1+ite)
!
!
    if (questi .eq. 'PHENOMENE') then
!     --------------------------------------
        call jelira('&CATA.PHENOMENE', 'NOMUTI', nbphen)
        call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm)
!
        ico=0
        do 20,iphen=1,nbphen
        call jenuno(jexnum('&CATA.PHENOMENE', iphen), nophen)
        call jelira('&CATA.'//nophen, 'NMAXOC', nbmodl)
        do 10,imodl=1,nbmodl
        call jeveuo(jexnum('&CATA.'//nophen, imodl), 'L', iamodl)
        ii=indiis(zi(iamodl),ite,1,nbtm)
        if (ii .gt. 0) then
            repk=nophen
            goto 30
!
        endif
10      continue
20      continue
        ierd=1
30      continue
!
!
    else if (questi.eq.'MODELISATION') then
!     --------------------------------------
        call jelira('&CATA.PHENOMENE', 'NOMUTI', nbphen)
        call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm)
!
        ico=0
        do 50,iphen=1,nbphen
        call jenuno(jexnum('&CATA.PHENOMENE', iphen), nophen)
        call jelira('&CATA.'//nophen, 'NMAXOC', nbmodl)
        do 40,imodl=1,nbmodl
        call jeveuo(jexnum('&CATA.'//nophen, imodl), 'L', iamodl)
        ii=indiis(zi(iamodl),ite,1,nbtm)
        if (ii .gt. 0) then
            call jenuno(jexnum('&CATA.'//nophen(1:13)// '.MODL', imodl), nomodl)
            repk=nomodl
            ico=ico+1
        endif
40      continue
        if (ico .gt. 0) then
            if (ico .eq. 1) then
                goto 60
!
            else if (ico.gt.1) then
                repk='#PLUSIEURS'
                goto 60
!
            endif
        endif
50      continue
        ierd=1
60      continue
!
!
    else if ((questi.eq.'PHEN_MODE')) then
!     --------------------------------------
        call jelira('&CATA.PHENOMENE', 'NOMUTI', nbphen)
        call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm)
!
        ico=0
        do 80,iphen=1,nbphen
        call jenuno(jexnum('&CATA.PHENOMENE', iphen), nophen)
        call jelira('&CATA.'//nophen, 'NMAXOC', nbmodl)
        do 70,imodl=1,nbmodl
        call jeveuo(jexnum('&CATA.'//nophen, imodl), 'L', iamodl)
        ii=indiis(zi(iamodl),ite,1,nbtm)
        if (ii .gt. 0) then
            call jenuno(jexnum('&CATA.'//nophen(1:13)// '.MODL', imodl), nomodl)
            repk=nophen//nomodl
            ico=ico+1
        endif
70      continue
80      continue
        if (ico .gt. 1) then
            repk='#PLUSIEURS'
        else if (ico.eq.0) then
            repk='#AUCUN'
        endif
!
!
    else if (questi.eq.'NOM_TYPMAIL') then
!     --------------------------------------
        call jeveuo('&CATA.TE.TYPEMA', 'L', ibid)
        repk=zk8(ibid-1+ite)
!
!
    else if (questi.eq.'TYPE_TYPMAIL') then
!     --------------------------------------
        call dismtm(questi, nomtm, repi, repk, ierd)
!
!
    else if (questi.eq.'NBNO_TYPMAIL') then
!     --------------------------------------
        call dismtm(questi, nomtm, repi, repk, ierd)
!
!
    else if (questi.eq.'DIM_TOPO') then
!     --------------------------------------
        call dismtm(questi, nomtm, repi, repk, ierd)
!
!
    else if (questi.eq.'DIM_GEOM') then
!     --------------------------------------
        call jeveuo('&CATA.TE.DIM_GEOM', 'L', ibid)
        repi=zi(ibid-1+ite)
!
!
    else if (questi.eq.'CALC_RIGI') then
!     --------------------------------------
        repk='NON'
        call jeveuo('&CATA.TE.OPTTE', 'L', iaopte)
        call jelira('&CATA.OP.NOMOPT', 'NOMMAX', nbopt)
        do 90,irig=1,nrig
        call jenonu(jexnom('&CATA.OP.NOMOPT', optrig(irig)), iopt)
        ASSERT(iopt.gt.0)
        ioptte=zi(iaopte-1+(ite-1)*nbopt+iopt)
        if (ioptte .eq. 0) goto 90
        repk='OUI'
        goto 100
!
90      continue
100      continue
!
!
    else
        ierd=1
    endif
!
    repkz=repk
    call jedema()
end subroutine
