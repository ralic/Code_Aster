subroutine lisccm(nomcmd, codarr, lischa)
!
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
!
    implicit     none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lisccc.h"
#include "asterfort/lislch.h"
#include "asterfort/lislco.h"
#include "asterfort/lisnnb.h"
#include "asterfort/u2mesk.h"
    character(len=16) :: nomcmd
    character(len=1) :: codarr
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! VERIFICATION COMPATIBILITE CHARGE/COMMANDE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMCMD : NOM DE LA COMMANDE
! IN  CODARR : TYPE DE MESSAGE INFO/ALARME/ERREUR SI PAS COMPATIBLE
! IN  LISCHA : SD LISTE DES CHARGES
!
! ----------------------------------------------------------------------
!
    integer :: ichar, nbchar
    integer :: nbauth, nbnaut, mclaut(2)
    integer :: motclc(2)
    character(len=24) :: valk(2)
    character(len=8) :: charge
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    if (nomcmd .eq. ' ') goto 999
!
! --- NOMBRE DE CHARGES
!
    call lisnnb(lischa, nbchar)
    if (nbchar .eq. 0) goto 999
!
    do 10 ichar = 1, nbchar
!
! ----- NOM DE LA CHARGE
!
        call lislch(lischa, ichar, charge)
!
! ----- CODE DES MOTS-CLEFS DE LA CHARGE
!
        call lislcm(lischa, ichar, motclc)
!
! ----- AUTORISATION ?
!
        call lisccc(nomcmd, motclc, nbauth, nbnaut, mclaut)
!
        if (nbnaut .ne. 0) then
            valk(1) = charge
            valk(2) = nomcmd
            call u2mesk(codarr, 'CHARGES5_3', 2, valk)
        endif
!
10  continue
!
999 continue
!
    call jedema()
end subroutine
