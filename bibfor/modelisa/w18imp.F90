subroutine w18imp(ligrel, noma, nomo)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel
    character(len=8) :: noma, nomo
! person_in_charge: jacques.pellet at edf.fr
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
!     ------------------------------------------------------------------
!     AFFE_MODELE IMPRESSION SUR 'MESSAGE' DES ELEMENTS FINIS AFFECTES
!     ------------------------------------------------------------------
!
    integer :: ifm, numvec, nbgrel, i, nmgrel, numail, jdli, nutypm
    integer :: ntypoi,  nutype, jc, j, numnoe
    integer :: jdnw, k, niv, iexi, nbte, jnbele, jmodli, jtypma
    integer :: jtypel, nbele
    character(len=8) :: typema, nomail, tabmai(8)
    character(len=16) :: typele, typemo
    character(len=24) :: nommai, nomnoe
    character(len=32) :: phemod
    integer, pointer :: typmail(:) => null()
!
    nommai=noma//'.NOMMAI'
    nomnoe=noma//'.NOMNOE'
!
    call infniv(ifm, niv)
!
    write (ifm,9000)
!
    call jeexin(ligrel//'.LIEL', iexi)
    if (iexi .eq. 0) goto 50
!
!
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel)
    call jeveuo(ligrel//'.LIEL', 'L', jdli)
    call jeveuo(noma//'.TYPMAIL', 'L', vi=typmail)
    call jeexin(nomo//'.MODELE    .NEMA', iexi)
    jdnw=0
    if (iexi .gt. 0) call jeveuo(nomo//'.MODELE    .NEMA', 'L', jdnw)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ntypoi)
!
!
!     -- 1. ON COMPTE LES ELEMENTS DE CHAQUE TYPE :
!     ------------------------------------------------
    call jelira('&CATA.TE.NOMTE', 'NOMMAX', nbte)
    call wkvect('&&W18IMP.NBELE', 'V V I', nbte, jnbele)
    call wkvect('&&W18IMP.MODELI', 'V V K16', nbte, jmodli)
    call wkvect('&&W18IMP.TYPMA', 'V V K8', nbte, jtypma)
    call wkvect('&&W18IMP.TYPEL', 'V V K16', nbte, jtypel)
!
    numvec=1
    typemo=' '
    do i = 1, nbgrel
        call jelira(jexnum(ligrel//'.LIEL', i), 'LONMAX', nmgrel)
        if (nmgrel .eq. 1) goto 9
        nutype=zi(jdli+numvec+nmgrel-2)
        if (nutype .eq. 0) goto 10
!
        ASSERT(nutype.gt.0 .and. nutype.le.nbte)
        zi(jnbele-1+nutype)=zi(jnbele-1+nutype)+nmgrel-1
        if (zk16(jmodli-1+nutype) .eq. ' ') then
            call jenuno(jexnum('&CATA.TE.NOMTE', nutype), typele)
            numail=zi(jdli+numvec-1)
            if (numail .lt. 0) then
                nutypm=ntypoi
            else
                nutypm=typmail(numail)
            endif
            call jenuno(jexnum('&CATA.TM.NOMTM', nutypm), typema)
!
            if (typele .eq. 'MECA_HEXS8') then
                call utmess('A', 'ELEMENTS4_74')
            endif
            call dismoi('PHEN_MODE', typele, 'TYPE_ELEM', repk=phemod)
            typemo=phemod(17:32)
            if (phemod(1:10) .eq. '#PLUSIEURS') typemo=' '
            zk16(jtypel-1+nutype)=typele
            zk16(jmodli-1+nutype)=typemo
            zk8(jtypma-1+nutype)=typema
        endif
!
  9     continue
        numvec=numvec+nmgrel
 10     continue
    end do
!
!
!     -- 2. IMPRESSION DU NOMBRE D'ELEMENTS DE CHAQUE TYPE :
!     -------------------------------------------------------
    do i = 1, nbte
        nbele=zi(jnbele-1+i)
        if (nbele .eq. 0) goto 20
!
        typemo=zk16(jmodli-1+i)
        typema=zk8(jtypma-1+i)
        typele=zk16(jtypel-1+i)
        write (ifm,9010)typemo,typele,typema,nbele
 20     continue
    end do
    write(ifm,9050)
!
!
!     -- 3. IMPRESSIONS DE NIVEAU 2 (TOUS LES ELEMENTS) :
!     ---------------------------------------------------
    if (niv .eq. 2) then
        numvec=1
        typemo=' '
        do i = 1, nbgrel
            call jelira(jexnum(ligrel//'.LIEL', i), 'LONMAX', nmgrel)
            if (nmgrel .eq. 1) goto 39
            numail=zi(jdli+numvec-1)
            if (numail .lt. 0) then
                nutypm=ntypoi
            else
                nutypm=typmail(numail)
            endif
            nutype=zi(jdli+numvec+nmgrel-2)
            call jenuno(jexnum('&CATA.TM.NOMTM', nutypm), typema)
            call jenuno(jexnum('&CATA.TE.NOMTE', nutype), typele)
            if (typele .eq. 'MECA_HEXS8') then
                call utmess('A', 'ELEMENTS4_74')
            endif
!
            call dismoi('PHEN_MODE', typele, 'TYPE_ELEM', repk=phemod)
            typemo=phemod(17:32)
            if (phemod(1:10) .eq. '#PLUSIEURS') typemo=' '
            write (ifm,9010)typemo,typele,typema,nmgrel-1
            if (numail .lt. 0) then
                write (ifm,9020)
            else
                write (ifm,9030)
            endif
            jc=0
            do j = jdli+numvec-1, jdli+numvec+nmgrel-3
                jc=jc+1
                numail=zi(j)
                if (numail .lt. 0) then
                    ASSERT(jdnw.ne.0)
                    numnoe=zi(jdnw+(-numail*2-1)-1)
                    call jenuno(jexnum(nomnoe, numnoe), nomail)
                else
                    call jenuno(jexnum(nommai, numail), nomail)
                endif
                if (jc .eq. 8 .or. j .eq. jdli+numvec+nmgrel-3) then
                    tabmai(jc)=nomail
                    write (ifm,9040)(tabmai(k),k=1,jc)
                    jc=0
                else
                    tabmai(jc)=nomail
                endif
            end do
 39         continue
            numvec=numvec+nmgrel
        end do
        write(ifm,9050)
    endif
!
!
 50 continue
!
!
    9000 format (/,'    MODELISATION      ELEMENT FINI      TYPE MAILLE',&
     &       '          NOMBRE')
    9010 format (4x,a16,2x,a16,2x,a8,7x,i12)
    9020 format (6x,'LISTE DES NOEUDS AFFECTES:')
    9030 format (6x,'LISTE DES MAILLES AFFECTEES:')
    9040 format (4x,8(2x,a8))
    9050 format (1x)
end subroutine
