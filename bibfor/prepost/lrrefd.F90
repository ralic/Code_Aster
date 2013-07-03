subroutine lrrefd(resu, prchnd)
    implicit  none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterfort/dismoi.h"
#include "asterfort/idensd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
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
    integer :: jrefd
!
    character(len=8) :: matrig, matmas
    character(len=14) :: nuddlr, nuddlm
    character(len=19) :: pronur, pronum
    integer :: iarg
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
    call getvid(' ', 'MATR_RIGI', ibid, iarg, 1,&
                matrig, iret1)
    call getvid(' ', 'MATR_MASS', ibid, iarg, 1,&
                matmas, iret2)
!
    if (iret1 .eq. 1) then
        call u2mesk('I', 'PREPOST_14', 1, matrig)
        call dismoi('F', 'NOM_NUME_DDL', matrig, 'MATR_ASSE', ibid,&
                    nuddlr, iret)
        call dismoi('F', 'PROF_CHNO', nuddlr, 'NUME_DDL', ibid,&
                    prchnd, iret)
    endif
!
!     VERIFICATION : LES NUME_DDL DES MATRICES A ET B SONT IDENTIQUES
    if (iret1 .eq. 1 .and. iret2 .eq. 1) then
        call dismoi('F', 'NOM_NUME_DDL', matmas, 'MATR_ASSE', ibid,&
                    nuddlm, iret)
        if (nuddlm .ne. nuddlr) then
            pronur=(nuddlr//'.NUME')
            pronum=(nuddlm//'.NUME')
            if (.not.idensd('PROF_CHNO',pronur,pronum)) then
                call u2mess('F', 'ALGELINE2_79')
            endif
        endif
    endif
!
    call wkvect(resu//'           .REFD', 'G V K24', 7, jrefd)
    zk24(jrefd-1+1) = matrig
    zk24(jrefd-1+2) = matmas
    zk24(jrefd-1+3) = ' '
    zk24(jrefd-1+4) = nuddlr
    zk24(jrefd-1+5) = ' '
    zk24(jrefd-1+6) = ' '
    zk24(jrefd-1+7) = ' '
!
    call jedema()
!
end subroutine
