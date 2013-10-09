subroutine lrrefd(resu, prchnd)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/refdaj.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: resu
    character(len=19) :: prchnd
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT:
!       * REMPLIR LE .REFD A PARTIR DES DONNEES UTILISATEUR
!       * RETOURNER LE NOM D'UN PROF_CHNO QUI SERVIRA A NUMEROTER
!         LES CHAM_NO DE "DEPL" (DEPL/VITE/ACCE)
!       * PEUT RETOURNER PRCHND=' '
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   RESU     : NOM DE LA SD_RESULTAT
!
!      SORTIE :
!-------------
! OUT  PRCHND   : PROFIL DES CHAM_NO DEPL/VITE/ACCE
!
! ......................................................................
!
!
!
!
!
    integer :: iret, iret1, iret2, ibid
!
    character(len=8) :: matrig, matmas
    character(len=14) :: nuddlr, nuddlm
    character(len=19) :: pronur, pronum
    character(len=24) :: matric(3)
!
! ----------------------------------------------------------------------
!
!     SI L'UTILISATEUR NE DONNE PAS DE NUME_DDL ON LE DEDUIT DE LA
!     DE LA MATRICE DE RIGIDITE (MATRIG).
!
    call jemarq()
!
    prchnd = ' '
    matrig = ' '
    matmas = ' '
    nuddlr = ' '
    nuddlm = ' '
!
    ibid = 1
!
    call getvid(' ', 'MATR_RIGI', scal=matrig, nbret=iret1)
    call getvid(' ', 'MATR_MASS', scal=matmas, nbret=iret2)
!
    if (iret1 .eq. 1) then
        call utmess('I', 'PREPOST_14', sk=matrig)
        call dismoi('NOM_NUME_DDL', matrig, 'MATR_ASSE', repk=nuddlr)
        call dismoi('PROF_CHNO', nuddlr, 'NUME_DDL', repk=prchnd)
    endif
!
!     VERIFICATION : LES NUME_DDL DES MATRICES A ET B SONT IDENTIQUES
    if (iret1 .eq. 1 .and. iret2 .eq. 1) then
        call dismoi('NOM_NUME_DDL', matmas, 'MATR_ASSE', repk=nuddlm)
        if (nuddlm .ne. nuddlr) then
            pronur=(nuddlr//'.NUME')
            pronum=(nuddlm//'.NUME')
            if (.not.idensd('PROF_CHNO',pronur,pronum)) then
                call utmess('F', 'ALGELINE2_79')
            endif
        endif
    endif
!
    matric(1) = matrig
    matric(2) = matmas
    matric(3) = ' '
    call refdaj('F', resu, -1, nuddlr, 'DYNAMIQUE',&
                matric, iret)
!
    call jedema()
!
end subroutine
