subroutine dismca(questi, nomobz, repi, repkz, ierd)
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
!     --     DISMOI(CARTE)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/fonbpa.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: repi, ierd
    character(len=*) :: nomobz, repkz
    character(len=*) :: questi
    character(len=24) :: questl
    character(len=32) :: repk
    character(len=19) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE CARTE
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=19) :: nomfon, nomcar
    character(len=8) ::  typfon, nompf(10), type, nogd
!
!-----------------------------------------------------------------------
    integer :: iadesc, iexi, iret, jdesc, jnoma, jprol
    integer :: jvale, k, l, long, ltyp, nbpf
!-----------------------------------------------------------------------
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob = nomobz
    questl = questi
!
    call jeexin(nomob//'.NOMA', iexi)
    if (iexi .eq. 0) then
        ierd=1
        goto 9999
    endif
!
!
    if (questi .eq. 'NOM_MAILLA') then
        call jeveuo(nomob//'.NOMA', 'L', jnoma)
        repk = zk8(jnoma-1+1)
!
    else if (questi.eq.'TYPE_CHAMP') then
        repk = 'CART'
!
    else if (questi.eq.'TYPE_SUPERVIS') then
        call jeveuo(nomob//'.DESC', 'L', iadesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(iadesc)), nogd)
        repk='CART_'//nogd
!
    else if (questl(1:7).eq.'NOM_GD ') then
        call jeveuo(nomob//'.DESC', 'L', jdesc)
        call jenuno(jexnum('&CATA.GD.NOMGD', zi(jdesc)), repk)
!
    else if (questi.eq.'PARA_INST') then
        repk = ' '
        nomcar = nomob
        call jeveuo(nomcar//'.VALE', 'L', jvale)
        call jelira(nomcar//'.VALE', 'TYPE', cval=type)
        if (type(1:1) .eq. 'K') then
            call jelira(nomcar//'.VALE', 'LONMAX', long)
            call jelira(nomcar//'.VALE', 'LTYP', ltyp)
            do 10 k = 1, long
                if (ltyp .eq. 8) then
                    nomfon = zk8(jvale+k-1)
                else if (ltyp.eq.24) then
                    nomfon = zk24(jvale+k-1)
                else
                    ASSERT(.false.)
                endif
!
!
                if (nomfon(1:8) .ne. '        ') then
                    call jeexin(nomfon//'.PROL', iret)
                    if (iret .gt. 0) then
                        call jeveuo(nomfon//'.PROL', 'L', jprol)
                        call fonbpa(nomfon, zk24(jprol), typfon, 10, nbpf,&
                                    nompf)
                        do 101 l = 1, nbpf
                            if (nompf(l)(1:4) .eq. 'INST') then
                                repk = 'OUI'
                                goto 11
                            endif
101                      continue
                    endif
                endif
10          continue
11          continue
        endif
!
!
    else
        ierd=1
    endif
!
!
9999  continue
    repkz = repk
    call jedema()
end subroutine
