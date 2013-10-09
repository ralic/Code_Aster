subroutine op0069()
    implicit none
# include "jeveux.h"
# include "asterc/getres.h"
# include "asterfort/assert.h"
# include "asterfort/copisd.h"
# include "asterfort/detrsd.h"
# include "asterfort/dismoi.h"
# include "asterfort/elg_calc_matk_red.h"
# include "asterfort/elg_gest_common.h"
# include "asterfort/gcncon.h"
# include "asterfort/getvid.h"
# include "asterfort/infmaj.h"
# include "asterfort/infniv.h"
# include "asterfort/jedema.h"
# include "asterfort/jemarq.h"
# include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
! person_in_charge: jacques.pellet at edf.fr
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
!     OPERATEUR ELIM_LAGR
! ======================================================================
!
!
!
    character(len=19) :: matass, matred, krigi, krigred, solv1, solv2
    character(len=16) :: concep, nomcmd
    character(len=14) :: nu1, nu2
    character(len=3) :: kellag
    integer ::  ifm, niv, jrefa, jslvk, jnslv, iautre
!   ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(matred, concep, nomcmd)
!
!   -- matrice de rigidite :
    call getvid(' ', 'MATR_RIGI', scal=krigi)
!
!   -- autre matrice a reduire (masse, amortissement, ...):
    call getvid(' ', 'MATR_ASSE', scal=matass, nbret=iautre)
!
!
!   -- si 2 matrices partagent leurs relations lineaires
!      elles doivent aussi partager leur nume_ddl :
    if (iautre .eq. 1) then
        call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=nu1)
        call dismoi('NOM_NUME_DDL', krigi, 'MATR_ASSE', repk=nu2)
        ASSERT(nu1.eq.nu2)
        call jeveuo(krigi//'.REFA', 'L', jrefa)
        krigred=zk24(jrefa-1+19)(1:19)
        if (krigred .eq. ' ') call utmess('F', 'ELIMLAGR_11')
    else
        matass=krigi
    endif
!
!
!   -- 1. Reduction de la matrice :
!   ----------------------------------------
!
!   -- On recupere le solveur de matass (solv1)
    call dismoi('SOLVEUR', matass, 'MATR_ASSE', repk=solv1)
    ASSERT(solv1.ne.' ')
!
!   -- On modifie (temporairement) la valeur de ELIM_LAGR :
    call jeveuo(solv1//'.SLVK', 'E', jslvk)
    kellag=zk24(jslvk-1+13)(1:3)
    zk24(jslvk-1+13)='OUI'
!
!   -- Calcul de la matrice reduite (matred) :
    call elg_gest_common('NOTE', matass, matred, krigi)
    call elg_calc_matk_red(matass, solv1, matred, 'G', .false.)
!
!   -- On retablit la valeur de ELIM_LAGR :
    zk24(jslvk-1+13)=kellag
!
!   -- On fabrique un solveur pour la matrice reduite :
    call gcncon('_', solv2)
    call copisd('SOLVEUR', 'G', solv1, solv2)
    call jeveuo(solv2//'.SLVK', 'L', jslvk)
    zk24(jslvk-1+13)='NON'
    call jeveuo(matred//'.REFA', 'E', jrefa)
    zk24(jrefa-1+7)=solv2
!
!
!
!   -- 2. Si 2 matrices reduites partagent leurs relations lineaires
!         elles doivent aussi partager leur nume_ddl :
!   -----------------------------------------------------------------
    if (iautre .eq. 1) then
        call jeveuo(krigi//'.REFA', 'L', jrefa)
        krigred=zk24(jrefa-1+19)(1:19)
        call dismoi('NOM_NUME_DDL', krigred, 'MATR_ASSE', repk=nu2)
!
        call jeveuo(matred//'.REFA', 'E', jrefa)
        call detrsd('NUME_DDL', zk24(jrefa-1+2))
        zk24(jrefa-1+2)=nu2
    endif
!
!
!   -- 2. Dans le nume_ddl de la matrice reduite, on stocke aussi le solveur
!   ------------------------------------------------------------------------
    call jeveuo(matred//'.REFA', 'L', jrefa)
    nu2= zk24(jrefa-1+2)
    call jeveuo(nu2//'.NSLV', 'E', jnslv)
    zk24(jnslv-1+1)=solv2
!
!
    call jedema()
end subroutine
