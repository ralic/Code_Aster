subroutine dismcn(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(CHAM_NO)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismgd.h"
#include "asterfort/dismpn.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=24) :: questl
    character(len=32) :: repk
    character(len=19) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOB  : NOM D'UN OBJET DE TYPE NUM_DDL
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPK   : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: nogd
    integer :: iadesc, iarefe
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    questl = questi
!
    if (questl .eq. 'NB_EQUA') then
        call jelira(nomob//'.VALE', 'LONMAX', repi)
    else if (questl.eq.'NOM_MAILLA') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        repk = zk24(iarefe-1+1) (1:8)
    else if (questl.eq.'NB_DDLACT') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        call dismpn(questl, zk24(iarefe-1+2)(1:8)//'.NUME      ', repi, repk, ierd)
    else if (questl.eq.'TYPE_CHAMP') then
        repk = 'NOEU'
    else if (questl(1:7).eq.'NUM_GD ') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        repi = zi(iadesc)
    else if (questl(1:7).eq.'NOM_GD ') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(iadesc)), repk)
    else if (questl.eq.'TYPE_SUPERVIS' .or. questl.eq.'TYPE_SCA') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(iadesc)), nogd)
        if (questl.eq.'TYPE_SUPERVIS') then
            repk='CHAM_NO_'//nogd
        else
            call dismgd(questl, nogd, repi, repk, ierd)
        endif
    else if (questl.eq.'PROF_CHNO') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        repk = zk24(iarefe+1)
    else
        ierd=1
    endif
!
    repkz = repk
    call jedema()
end subroutine
