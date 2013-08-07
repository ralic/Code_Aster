subroutine dismcg(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(CHAM_NO)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/dismpn.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
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
    integer :: iadesc, iarefe, iret
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    questl = questi
!
    if (questi .eq. 'NB_EQUA') then
        call jelira(nomob//'.VALE', 'LONMAX', repi)
    else if (questi.eq.'NOM_MAILLA') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        repk = zk24(iarefe-1+1) (1:8)
    else if (questi.eq.'NB_DDLACT') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        call dismpn(questi, zk24(iarefe-1+2)(1:8)//'.NUME      ', repi, repk, ierd)
    else if (questi.eq.'TYPE_CHAMP') then
        repk = 'VGEN'
    else if (questl(1:7).eq.'NOM_GD ') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(iadesc)), repk)
    else if (questi.eq.'TYPE_SUPERVIS') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(iadesc)), nogd)
        repk='CHAM_NO_'//nogd
    else if (questi.eq.'PROF_CHNO') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        repk = zk24(iarefe+1)
    else if (questi.eq.'NOM_NUME_DDL') then
        call jeveuo(nomob//'.REFE', 'L', iarefe)
        repk = zk24(iarefe+1)
        call jeexin(repk(1:19)//'.NEQU', iret)
        if (iret .eq. 0) then
            call u2mess('F', 'UTILITAI_51')
            ierd=1
            goto 9999
        endif
    else
        ierd=1
    endif
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
