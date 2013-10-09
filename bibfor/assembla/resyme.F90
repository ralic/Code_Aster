subroutine resyme(resu1z, basez, resu2z)
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
!.======================================================================
    implicit none
!
!      RESYME -- A PARTIR DU MATR_ELEM NON-SYMETRIQUE RESU1Z,
!                ON CREE SUR LA BASE BASEZ LE  MATR_ELEM SYMETRIQUE
!                RESU2Z.
!                LES MATRICES ELEMENTAIRES MAT2 DE RESU2Z
!                SONT OBTENUES A PARTIR DES MATRICES ELEMENTAIRES
!                DE RESU1Z DE LA MANIERE SUIVANTE :
!                 MAT2 = 1/2*(MAT1 + MAT1_T)
!
!
!   ARGUMENT        E/S  TYPE         ROLE
!    RESU1Z          IN    K*     NOM DU MATR_ELEM NON-SYMETRIQUE
!    BASEZ           IN    K*     NOM DE LA BASE SUR-LAQUELLE ON
!                                 ON VA CREER LE MATR_ELEM SYMETRIQUE
!                                 RESU2Z
!    RESU2Z          OUT   K*     NOM DU MATR_ELEM SYMETRIQUE
!                                 QUI VA ETRE CREE SUR LA
!                                 BASE BASEZ ET DONT LES MATRICES
!                                 ELEMENTAIRES MAT2 SONT OBTENUES A
!                                 PARTIR DES MATRICES ELEMENTAIRES MAT1
!                                 DE RESU1Z PAR LA RELATION DE
!                                 SYMETRISATION :
!                                  MAT2 = 1/2*(MAT1 + MAT1_T)
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: resu1z, basez, resu2z
! -----  VARIABLES LOCALES
    character(len=1) :: base
    character(len=8) :: symel
    character(len=8) :: modele
    character(len=16) :: phenom, option
    character(len=19) :: resl1, resl2, resul1, resul2
    character(len=19) :: ligrel
    integer ::  nbresu, idlre1, idlre2, iresu, iret, kresu
    integer :: jresl1, jresl2, igr, nbgr
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
    base = basez
    resul1 = resu1z
    resul2 = resu2z
    call dismoi('NOM_MODELE', resul1, 'MATR_ELEM', repk=modele)
!
!
! --- QUELLE OPTION APPELER ?
!     -------------------------------------------
    call dismoi('PHENOMENE', modele, 'MODELE', repk=phenom)
    if (phenom .eq. 'MECANIQUE') then
        option = 'SYME_MDNS_R'
    else if (phenom.eq.'THERMIQUE') then
        option = 'SYME_MTNS_R'
    else
        call utmess('F', 'ASSEMBLA_39', sk=phenom)
    endif
!
!
! --- CREATION DU MATR_ELEM SYMETRIQUE :
!     -------------------------------------------
    call jedetr(resul2//'.RELR')
    call jedetr(resul2//'.RERR')
    call jedupc(' ', resul1//'.RERR', 1, 'V', resul2//'.RERR',&
                .false.)
    call jelira(resul1//'.RELR', 'LONUTI', nbresu)
    call wkvect(resul2//'.RELR', base//' V K24', nbresu, idlre2)
    call jeveuo(resul1//'.RELR', 'L', idlre1)
!
!
!
! --- CALCUL DES MATRICES ELEMENTAIRES SYMETRIQUES MAT2 A PARTIR
! --- DES MATRICES ELEMENTAIRES NON-SYMETRIQUES MAT1 PAR
! --- SYMETRISATION DE CES DERNIERES
! --- (I.E. MAT2 = 1/2*(MAT1 + MAT1_T) :
!       --------------------------------
!
    kresu=0
    do 20 iresu = 1, nbresu
        resl1 = zk24(idlre1+iresu-1)(1:19)
        call jeexin(resl1//'.RESL', iret)
        if (iret .ne. 0) then
            kresu=kresu+1
            call gcncon('.', resl2)
            call dismoi('TYPE_MATRICE', resl1, 'RESUELEM', repk=symel)
            if (symel .eq. 'NON_SYM') then
                call dismoi('NOM_LIGREL', resl1, 'RESUELEM', repk=ligrel)
!
                call calcul('S', option, ligrel, 1, resl1,&
                            'PNOSYM', 1, resl2, 'PSYM', base,&
                            'OUI')
!           -- ON VERIFIE QUE LES DIFFERENTS TYPE_ELEMENT ON FAIT LEUR
!              TRAVAIL :
                call jeveuo(resl1//'.DESC', 'L', jresl1)
                call jeveuo(resl2//'.DESC', 'L', jresl2)
                nbgr=zi(jresl1-1+2)
                ASSERT(nbgr.eq.zi(jresl2-1+2))
                do 21 igr = 1, nbgr
                    if (zi(jresl1-1+2+igr) .gt. 0) then
                        ASSERT(zi(jresl2-1+2+igr).ne.0)
                    endif
 21             continue
            else
                call copisd('CHAMP_GD', base, resl1, resl2)
            endif
            zk24(idlre2+kresu-1) = resl2
        endif
 20 end do
    call jeecra(resul2//'.RELR', 'LONUTI', kresu)
!
!
    call jedema()
end subroutine
