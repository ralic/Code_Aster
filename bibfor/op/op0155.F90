subroutine op0155()
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
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
!     COMMANDE :  POST_CHAMP
! ----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/refdcp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutnu.h"
#include "asterfort/w155ce.h"
#include "asterfort/w155ex.h"
#include "asterfort/w155mx.h"
    integer :: ifm, niv, n0, iret, jordr, nbordr, ie, nuordr
    integer :: i, j, jnompa, iadin, iadou, nbac, nbpa, nbpara
    character(len=16) :: crit, typesd, k16b, nopara
    character(len=8) :: resu, nomres
    character(len=3) :: type
    character(len=19) :: resu19, nomr19
    real(kind=8) :: prec
    character(len=24) :: nompar
!     ------------------------------------------------------------------
!
    call jemarq()
!
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomres, typesd, k16b)
    call getvid(' ', 'RESULTAT', scal=resu, nbret=n0)
    resu19=resu
!
!     -- SELECTION DES NUMERO D'ORDRE :
!     ---------------------------------
    prec=-1.d0
    crit=' '
    call getvr8(' ', 'PRECISION', scal=prec, nbret=ie)
    call getvtx(' ', 'CRITERE', scal=crit, nbret=ie)
    call rsutnu(resu19, ' ', 0, '&&OP0155.NUME_ORDRE', nbordr,&
                prec, crit, iret)
    ASSERT(iret.eq.0)
    ASSERT(nbordr.gt.0)
    call jeveuo('&&OP0155.NUME_ORDRE', 'L', jordr)
!
!
!     -- 1. ON CREE LA SD_RESULTAT NOMRES :
!     ---------------------------------------------
    call rscrsd('G', nomres, typesd, nbordr)
!
!
!     -- 2. MOTS CLES EXTR_XXXX :
!     ----------------------------
    call w155ex(nomres, resu, nbordr, zi(jordr))
!
!
!     -- 3. MOT CLE MIN_MAX_SP :
!     ----------------------------
    call w155mx(nomres, resu, nbordr, zi(jordr))
!
!
!     -- 4. MOT CLE COQU_EXCENT :
!     ----------------------------
    call w155ce(nomres, resu, nbordr, zi(jordr))
!
!
!     -- 5. RECOPIE DES PARAMETRES DE RESU VERS NOMRES :
!     --------------------------------------------------
    nompar='&&OP0155'//'.NOMS_PARA'
    call rsnopa(resu, 2, nompar, nbac, nbpa)
    nbpara=nbac+nbpa
    call jeveuo(nompar, 'L', jnompa)
    nomr19 = nomres
    call jeveuo(nomr19//'.ORDR', 'L', jordr)
    call jelira(nomr19//'.ORDR', 'LONUTI', nbordr)
!
    do 20 i = 1, nbordr
        nuordr=zi(jordr-1+i)
        do 10 j = 1, nbpara
            nopara=zk16(jnompa-1+j)
            call rsadpa(resu, 'L', 1, nopara, nuordr,&
                        1, sjv=iadin, styp=type, istop=0)
            call rsadpa(nomres, 'E', 1, nopara, nuordr,&
                        1, sjv=iadou, styp=type)
            if (type(1:1) .eq. 'I') then
                zi(iadou)=zi(iadin)
            else if (type(1:1).eq.'R') then
                zr(iadou)=zr(iadin)
            else if (type(1:1).eq.'C') then
                zc(iadou)=zc(iadin)
            else if (type(1:3).eq.'K80') then
                zk80(iadou)=zk80(iadin)
            else if (type(1:3).eq.'K32') then
                zk32(iadou)=zk32(iadin)
            else if (type(1:3).eq.'K24') then
                zk24(iadou)=zk24(iadin)
                if (nopara(1:5) .eq. 'EXCIT' .and. zk24(iadin)(1:2) .ne. '  ') then
                    zk24(iadou)=nomres//zk24(iadin)(9:)
                    call copisd(' ', 'G', zk24(iadin)(1:19), zk24(iadou)( 1:19))
                endif
            else if (type(1:3).eq.'K16') then
                zk16(iadou)=zk16(iadin)
            else if (type(1:2).eq.'K8') then
                zk8(iadou)=zk8(iadin)
            endif
10      continue
20  continue
    call jedetr(nompar)
!
!
!     -- 6. RECOPIE DE L'OBJET .REFD (SI NECESSAIRE) :
!     --------------------------------------------------
    call refdcp(resu19, nomr19)
!
!
!
    call jedetr('&&OP0155.NUME_ORDRE')
    call jedema()
end subroutine
