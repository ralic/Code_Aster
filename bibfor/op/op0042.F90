subroutine op0042()
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
! person_in_charge: josselin.delmas at edf.fr
!
!     COMMANDE :  CALC_ERREUR
!        CALCUL DES CONTRAINTES (DEFORM ...) ELEMENTAIRES EN MECANIQUE.
!        CALCUL DES FLUX ELEMENTAIRES EN THERMIQUE.
!        CALCUL DES INTENSITES        EN ACOUSTIQUE
!        CALCUL DES INDICATEURS D'ERREURS EN MECANIQUE ET EN THERMIQUE
!   -------------------------------------------------------------------
! CORPS DU PROGRAMME
! ----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterfort/ccvrpu.h"
#include "asterfort/cresol.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecalr.h"
#include "asterfort/medom1.h"
#include "asterfort/onerrf.h"
#include "asterfort/rsutnu.h"
#include "asterfort/thcalr.h"
#include "asterfort/utmess.h"
!
    character(len=6) :: nompro
    parameter  (nompro='OP0042')
!
    integer :: ifm, niv, n0, nuord, nchar, ibid, jordr, np, nc
    integer :: nbordr, iret
    real(kind=8) :: prec
    character(len=4) :: ctyp
    character(len=8) :: resuc1, resuco, modele, cara, crit
    character(len=16) :: nomcmd, tysd, pheno, concep, k16bid, compex
    character(len=19) :: knum, kcha, solveu
    character(len=24) :: mate
    logical :: newcal
!     ------------------------------------------------------------------
!
    call jemarq()
!
    kcha = '&&'//nompro//'.CHARGES   '
    knum = '&&'//nompro//'.NUME_ORDRE'
!
    call infmaj()
    call infniv(ifm, niv)
!
! --- ON STOCKE LE COMPORTEMENT EN CAS D'ERREUR AVANT MNL : COMPEX
! --- PUIS ON PASSE DANS LE MODE "VALIDATION DU CONCEPT EN CAS D'ERREUR"
    call onerrf(' ', compex, ibid)
    call onerrf('EXCEPTION+VALID', k16bid, ibid)
!
    call getres(resuc1, concep, nomcmd)
    call getvid(' ', 'RESULTAT', scal=resuco, nbret=n0)
!
    newcal = .false.
    call jeexin(resuc1//'           .DESC', iret)
    if (iret .eq. 0) newcal = .true.
    call gettco(resuco, tysd)
!
    call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
    call rsutnu(resuco, ' ', 0, knum, nbordr,&
                prec, crit, iret)
    if (iret .eq. 10) then
        call utmess('A', 'CALCULEL4_8', sk=resuco)
        goto 999
    endif
    if (iret .ne. 0) then
        call utmess('A', 'ALGORITH3_41')
        goto 999
    endif
!
!     -- ON VEUT INTERDIRE LA REENTRANCE DE LA COMMANDE SI
!        ON UTILISE L'UN DES MOTS CLES : MODELE, CARA_ELEM,
!        CHAM_MATER, EXCIT, GROUP_MA OU MAILLE
!     --------------------------------------------------------
    if (resuco .eq. resuc1) then
        call ccvrpu(resuco, knum, nbordr)
    endif
!
    call jeveuo(knum, 'L', jordr)
    nuord = zi(jordr)
!
!     -- CREATION DU SOLVEUR :
    solveu = '&&OP0042.SOLVEUR'
    call cresol(solveu)
!
    call medom1(modele, mate, cara, kcha, nchar,&
                ctyp, resuco, nuord)
    call dismoi('PHENOMENE', modele, 'MODELE', repk=pheno)
!
!     --- TRAITEMENT DU PHENOMENE MECANIQUE ---
    if (pheno(1:4) .eq. 'MECA') then
!
        call mecalr(newcal, tysd, knum, kcha, resuco,&
                    resuc1, nbordr, modele, mate, cara,&
                    nchar, ctyp)
!
!     --- TRAITEMENT DES PHENOMENES THERMIQUES ET ACOUSTIQUES ---
    else if (pheno(1:4).eq.'THER') then
!
        call thcalr(newcal, tysd, knum, kcha, resuco,&
                    resuc1, nbordr, modele, mate, cara,&
                    nchar, ctyp)
!
    endif
!
999 continue
!
!
! --- ON REMET LE MECANISME D'EXCEPTION A SA VALEUR INITIALE
    call onerrf(compex, k16bid, ibid)
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
!   call ajrefd(resuco, resuc1, 'COPIE')
!
    call jedema()
end subroutine
