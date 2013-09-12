subroutine pjeftc(ma1, ma2, resuou, base)
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
! person_in_charge: nicolas.greffet at edf.fr
! ======================================================================
!     COMMANDE:  PROJ_CHAMP  METHODE:'COUPLAGE' (COUPLAGE IFS VIA YACS)
! ----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/pjecou.h"
#include "asterfort/pjfuc2.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
    character(len=8) :: ma1, ma2
    character(len=16) :: resuou
    character(len=1) :: base
!
! 0.2. ==> COMMUNS
!
!
!
! 0.3. ==> VARIABLES LOCALES
    integer :: nbval, nbocc, iocc, nbgno2, vali(2)
    character(len=16) :: nomgma, nomgno
    character(len=16) :: corre1, corre2, corre3
    integer :: iarg
!
! DEB ------------------------------------------------------------------
    call jemarq()
    ASSERT(base.eq.'V')
    corre1 = '&&PJEFTC.CORRES1'
    corre2 = '&&PJEFTC.CORRES2'
    corre3 = '&&PJEFTC.CORRES3'
!
!
!     NOMBRE D'OCCURENCE (ou NOMBRE DE GROUP_MAILLE DEFINIS)
!     ------------------------------------------------------
    call getfac('VIS_A_VIS', nbocc)
!
!     COHERENCE ENTRE GROUP_MAILLE ET GOURP_NOEUDS
!     --------------------------------------------
    call jelira(ma2//'.GROUPENO', 'NMAXOC', nbgno2)
    if (nbgno2 .ne. nbocc) then
        vali(1) = nbgno2
        vali(2) = nbocc
        call u2mesi('F', 'COUPLAGEIFS_8', 2, vali)
    endif
!
!     PROJECTION ENTRE GROUP_MAILLE ET GROUP_NOEUDS
!     ---------------------------------------------
    if (nbocc .gt. 0) then
!
        do 10 iocc = 1, nbocc
!
!         -- NOMS DES GROUPES DE MAILLES ET DE NOEUDS COUPLES :
!         -----------------------------------------------------------
            call getvtx('VIS_A_VIS', 'GROUP_MA_1', iocc=iocc, scal=nomgma, nbret=nbval)
            call getvtx('VIS_A_VIS', 'GROUP_NO_2', iocc=iocc, scal=nomgno, nbret=nbval)
!
!         -- CALCUL DU CORRESP_2_MAILLA POUR IOCC :
!         ----------------------------------------------
            call pjecou(ma1, ma2, nomgma, nomgno, corre1)
!
!        -- SURCHARGE DU CORRESP_2_MAILLA :
!        ----------------------------------------------
            if (iocc .eq. 1) then
                call copisd('CORRESP_2_MAILLA', 'V', corre1, corre2)
            else
                call pjfuc2(corre2, corre1, 'V', corre3)
                call detrsd('CORRESP_2_MAILLA', corre2)
                call copisd('CORRESP_2_MAILLA', 'V', corre3, corre2)
                call detrsd('CORRESP_2_MAILLA', corre3)
            endif
            call detrsd('CORRESP_2_MAILLA', corre1)
10      continue
        call copisd('CORRESP_2_MAILLA', 'G', corre2, resuou)
        call detrsd('CORRESP_2_MAILLA', corre2)
    else
        call u2mess('F', 'COUPLAGEIFS_2')
    endif
!
    call jedema()
end subroutine
