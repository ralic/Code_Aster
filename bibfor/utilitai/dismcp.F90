subroutine dismcp(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(CHAMP)  CHAPEAU DE :
!       CHAM_NO, CHAM_NO_S, CARTE, CHAM_ELEM, CHAM_ELEM_S,
!       RESUELEM, CHAM_GENE
!
#include "jeveux.h"
#include "asterfort/dismca.h"
#include "asterfort/dismce.h"
#include "asterfort/dismcg.h"
#include "asterfort/dismcn.h"
#include "asterfort/dismes.h"
#include "asterfort/dismns.h"
#include "asterfort/dismre.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: repkz, nomobz
    character(len=32) :: repk
    character(len=19) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE CHAMP
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=4) :: tych
    integer ::  iexi
!
! DEB-------------------------------------------------------------------
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
!
    call jeexin(nomob(1:19)//'.DESC', iexi)
    if (iexi .gt. 0) then
        call jelira(nomob(1:19)//'.DESC', 'DOCU', cval=tych)
    else
        call jeexin(nomob(1:19)//'.CELD', iexi)
        if (iexi .gt. 0) then
            call jelira(nomob(1:19)//'.CELD', 'DOCU', cval=tych)
        else
            call jeexin(nomob(1:19)//'.CESD', iexi)
            if (iexi .gt. 0) then
                tych='CES'
            else
                call jeexin(nomob(1:19)//'.CNSD', iexi)
                if (iexi .gt. 0) then
                    tych='CNS'
                else
                    repk = '?'
                    ierd = 1
                    goto 10
                endif
            endif
        endif
    endif
!
!
    if (tych .eq. 'CHNO') then
        call dismcn(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'CART') then
        call dismca(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'CHML') then
        call dismce(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'RESL') then
        call dismre(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'VGEN') then
        call dismcg(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'CNS') then
        call dismns(questi, nomob, repi, repk, ierd)
    else if (tych.eq.'CES') then
        call dismes(questi, nomob, repi, repk, ierd)
    else
        ierd = 1
    endif
!
10  continue
    repkz = repk
end subroutine
