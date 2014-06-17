subroutine dismce(questi, nomobz, repi, repkz, ierd)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismgd.h"
#include "asterfort/dismlg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: questi, nomobz, repkz
! ----------------------------------------------------------------------
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
!
!     --     DISMOI(CHAM_ELEM)
!
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE LIGREL
!
! OUT : REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, -1 --> CHAMP INEXISTANT)
!
! ----------------------------------------------------------------------
!
    integer ::  iret, gd,  jcelk
    character(len=8) :: nogd, docu
    character(len=19) :: nomob
    character(len=24) :: questl, k24
    character(len=32) :: repk
    integer, pointer :: celd(:) => null()
! DEB-------------------------------------------------------------------
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
!
    nomob = nomobz
    questl = questi
!
    call jeexin(nomob//'.CELD', iret)
    if (iret .eq. 0) then
        ierd = 1
        goto 9999
    endif
!
    call jeveuo(nomob//'.CELD', 'L', vi=celd)
    call jelira(nomob//'.CELD', 'DOCU', cval=docu)
    ASSERT(docu.eq. 'CHML')
    gd = celd(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nogd)
!
    if (questi .eq. 'TYPE_CHAMP') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        repk = zk24(jcelk-1+3)(1:4)
!
    else if (questi .eq. 'TYPE_SUPERVIS') then
        repk = 'CHAM_ELEM_'//nogd
!
    else if (questi .eq. 'NOM_OPTION') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        repk = zk24(jcelk-1+2)(1:16)
!
    else if (questi .eq. 'NOM_PARAM') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        repk = zk24(jcelk-1+6)(1:8)
!
    else if (questi .eq. 'NOM_MAILLA') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        call dismlg(questi, zk24(jcelk), repi, repk, ierd)
!
    else if (questl(1:6) .eq. 'NUM_GD') then
        repi = gd
!
    else if (questl(1:6) .eq. 'NOM_GD') then
        repk = nogd
!
    else if (questi .eq. 'NOM_LIGREL') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        repk = zk24(jcelk)
!
    else if (questi .eq. 'MPI_COMPLET') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        k24 = zk24(jcelk-1+7)
        ASSERT(k24.eq.'MPI_COMPLET'.or.k24.eq.'MPI_INCOMPLET')
        if (k24 .eq. 'MPI_COMPLET') then
            repk='OUI'
        else
            repk='NON'
        endif
!
    else if (questi .eq. 'NOM_MODELE') then
        call jeveuo(nomob//'.CELK', 'L', jcelk)
        call dismlg(questi, zk24(jcelk), repi, repk, ierd)
!
    else if (questi .eq. 'MXNBSP') then
        repi=max(1,celd(3))
!
    else if (questi .eq. 'MXVARI') then
        repi=max(1,celd(4))
!
    else if (questi .eq. 'TYPE_SCA') then
        call dismgd(questi, nogd, repi, repk, ierd)
!
    else
        ierd = 1
    endif
!
9999  continue
    repkz = repk
!
    call jedema()
end subroutine
