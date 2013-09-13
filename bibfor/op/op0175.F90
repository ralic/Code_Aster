subroutine op0175()
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     COMMANDE :  CALC_FERRAILLAGE
! ----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/imprsd.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsutnu.h"
#include "asterfort/w175af.h"
#include "asterfort/w175ca.h"
    integer :: ifm, niv, n0, nuord, jordr
    integer :: iret, jpara, ie, nbordr, i, nuordr
    character(len=8) :: resu, modele, cara, k8b
    character(len=16) :: crit
    character(len=19) :: chfer1, chfer2, chefge, resu19
    real(kind=8) :: prec
!     ------------------------------------------------------------------
!
    call jemarq()
!
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getvid(' ', 'RESULTAT', scal=resu, nbret=n0)
    resu19=resu
!
!     -- CHOIX DES INSTANTS DE CALCUL :
!     ---------------------------------
    call getvr8(' ', 'PRECISION', scal=prec, nbret=ie)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=ie)
    call rsutnu(resu19, ' ', 0, '&&OP0175.NUME_ORDRE', nbordr,&
                prec, crit, iret)
    ASSERT(iret.eq.0)
    ASSERT(nbordr.gt.0)
    call jeveuo('&&OP0175.NUME_ORDRE', 'L', jordr)
!
!
!     -- ON PREND LE MODELE POUR LE 1ER INSTANT :
!     --------------------------------------------
    nuord = zi(jordr-1+1)
!
    call rsadpa(resu, 'L', 1, 'MODELE', nuord,&
                0, jpara, k8b)
    modele=zk8(jpara)
    ASSERT(modele.ne.' ')
    call rsadpa(resu, 'L', 1, 'CARAELEM', nuord,&
                0, jpara, k8b)
    cara=zk8(jpara)
    ASSERT(cara.ne.' ')
!
!
!
!     -- 1. ON CREE LE CHAMP DE DONNEES (CHFER1) :
!     ---------------------------------------------
    chfer1='&&OP0175.CHFER1'
    call w175af(modele, chfer1)
    if (niv .gt. 1) call imprsd('CHAMP', chfer1, 6, 'CHFER1=')
!
!
!
!     -- 2. ON APPELLE L'OPTION FERRAILLAGE :
!     -------------------------------------------
    do 20,i = 1,nbordr
    nuordr = zi(jordr+i-1)
    call rsexch('F', resu19, 'EFGE_ELNO', nuordr, chefge,&
                iret)
    call rsexch(' ', resu19, 'FERRAILLAGE', nuordr, chfer2,&
                iret)
    call w175ca(modele, cara, chfer1, chefge, chfer2)
!
    if (niv .gt. 1) call imprsd('CHAMP', chfer2, 6, 'CHFER2=')
    call rsnoch(resu19, 'FERRAILLAGE', nuordr)
    20 end do
!
!
    call jedema()
end subroutine
