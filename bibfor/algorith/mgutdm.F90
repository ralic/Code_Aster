subroutine mgutdm(mdgenz, nmsstz, nusst, questi, repi,&
                  repkz)
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
!
!***********************************************************************
!    P. RICHARD     DATE 13/11/92
!-----------------------------------------------------------------------
!  BUT:      < MODELE GENERALISE UTILITAIRE DIS MOI >
!
!  UTILITAIRE PERMETTANT D'ACCEDER AUX CONCEPT RELATIFS AUX
!  SOUS-STRUCTURES D'UN MODELE GENERALISE
!
!  LISTE DES QUESTIONS POSSIBLES:
!    NOM_MACR_ELEM
!    NOM_BASE_MODALE
!    NOM_MAILLAGE
!    NOM_MODELE
!    NOM_NUME_DDL
!    NOM_LIST_INTERF
!    NB_CMP_MAX
!
!-----------------------------------------------------------------------
!
! MDGENZ   /I/: NOM UTILISATEUR DU MODELE GENERALISE
! NMSSTZ   /I/: NOM K8 DE LA SOUS-STRUCTURE
! NUSST    /I/: NUMERO DE LA SOUS-STRUCTURE
! QUESTI   /I/: QUESTION
! REPI     /O/: REPONSE ENTIERE
! REPKZ    /O/: REPONSE CARACTERE
!
!
!
!
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
!
!
    integer :: repi, iret, lldesc, llmcl, llref, nuss, nusst, ibid
    integer :: vali
    character(len=*) :: questi
    character(len=24) :: repk
    character(len=24) :: valk(2), nume
    character(len=8) :: modgen, nommcl, basmod, nomsst
    character(len=14) :: llref2
    character(len=*) :: mdgenz, nmsstz, repkz
!
!-----------------------------------------------------------------------
!
!------------RECUPERATION NUMRERO DE SOUS-STRUCTURE ET VERIFS-----------
!
    call jemarq()
    modgen = mdgenz
    nomsst = nmsstz
    repk = repkz
!
    if (nomsst(1:1) .ne. ' ') then
        call jenonu(jexnom(modgen//'      .MODG.SSNO', nomsst), nuss)
        if (nuss .eq. 0) then
            valk (1) = modgen
            valk (2) = nomsst
            call utmess('F', 'ALGORITH13_49', nk=2, valk=valk)
        endif
    else
        nuss=nusst
        call jeexin(jexnum(modgen//'      .MODG.SSME', nuss), iret)
        if (nuss .eq. 0) then
            valk (1) = modgen
            vali = nuss
            call utmess('F', 'ALGORITH13_50', sk=valk(1), si=vali)
        endif
        call jenuno(jexnum(modgen//'      .MODG.SSNO', nuss), nomsst)
    endif
!
!
    if (questi(1:13) .eq. 'NOM_MACR_ELEM') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        repk(1:8)=zk8(llmcl)
    else if (questi(1:15).eq.'NOM_BASE_MODALE') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        nommcl=zk8(llmcl)
        call jeveuo(nommcl//'.MAEL_REFE', 'L', llref)
        repk(1:8)=zk24(llref)
    else if (questi(1:12).eq.'NOM_MAILLAGE') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        nommcl=zk8(llmcl)
        call jeveuo(nommcl//'.MAEL_REFE', 'L', llref)
        repk(1:8)=zk24(llref+1)
    else if (questi(1:12).eq.'NOM_NUME_DDL') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        nommcl=zk8(llmcl)
        call jeveuo(nommcl//'.MAEL_REFE', 'L', llref)
        basmod(1:8)=zk24(llref)
        call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid,&
                    repk, iret)
    else if (questi(1:12).eq.'NOM_MODELE  ') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        nommcl=zk8(llmcl)
        call jeveuo(nommcl//'.MAEL_REFE', 'L', llref)
        basmod(1:8)=zk24(llref)
        call dismoi('F', 'NUME_DDL', basmod, 'RESU_DYNA', ibid,&
                    nume, iret)
        call jenuno(jexnum(nume(1:14)//'.NUME.LILI', 2), llref2)
        repk(1:8)=llref2(1:8)
    else if (questi(1:15).eq.'NOM_LIST_INTERF') then
        call jeveuo(jexnum(modgen//'      .MODG.SSME', nuss), 'L', llmcl)
        nommcl=zk8(llmcl)
        call jeveuo(nommcl//'.MAEL_REFE', 'L', llref)
        basmod(1:8)=zk24(llref)
!       call utimsd(6, 2, .false., .true.,basmod(1:8)//'           .REFD', 1, ' ')
        call dismoi('C', 'REF_INTD_PREM', basmod, 'RESU_DYNA', ibid,&
                    repk, iret)
    else if (questi(1:10).eq.'NB_CMP_MAX') then
        call jeveuo(modgen//'      .MODG.DESC', 'L', lldesc)
        repi=zi(lldesc+1)
    else
        repk = questi
        call utmess('F', 'UTILITAI_49', sk=repk)
        goto 9999
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
