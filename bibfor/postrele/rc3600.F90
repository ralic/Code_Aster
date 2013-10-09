subroutine rc3600()
    implicit none
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3600
!
!     LA PREMIERE ETAPE EST DE TRADUIRE LES DONNEES EN CHAM_ELEM_S
!     PUIS DE CALCULER LES SP, SN, ... EN CHAQUE NOEUD DE CHAQUE MAILLE
!     LE RESULTAT EST UN CHAM_ELEM_S QUE L'ON TRADUIRA DANS UNE TABLE
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cesimp.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rc36ac.h"
#include "asterfort/rc36ca.h"
#include "asterfort/rc36in.h"
#include "asterfort/rc36ma.h"
#include "asterfort/rc36rm.h"
#include "asterfort/rc36rs.h"
#include "asterfort/rc36si.h"
#include "asterfort/rc36zz.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    integer :: n1, nbtou, nbma, jma, ima, nbcmp, nbmat, ibid, ifm, niv
    character(len=8) :: k8b, nomres, noma, carael, modele, nommat, motcls(2)
    character(len=8) :: typmcs(2), nomgd
    character(len=16) :: nomcmd, concep, motclf, nocmp(5)
    character(len=24) :: mesmai, ncncin, chindi, chcara, chresu
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call infniv(ifm, niv)
!
    call getres(nomres, concep, nomcmd)
!
!     ------------------------------------------------------------------
!               LE MATERIAU , MODELE , CARA_ELEM
!     ------------------------------------------------------------------
    call getvid(' ', 'CHAM_MATER', scal=nommat, nbret=n1)
    call getvid(' ', 'MODELE', scal=modele, nbret=n1)
    call getvid(' ', 'CARA_ELEM', scal=carael, nbret=n1)
!
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmat)
!
!     ------------------------------------------------------------------
!                           ZONE D'ANALYSE
!     ------------------------------------------------------------------
!
    motclf = 'ZONE_ANALYSE'
!
    mesmai = '&&RC3600.MES_MAILLES'
    motcls(1) = 'GROUP_MA'
    motcls(2) = 'MAILLE'
    typmcs(1) = 'GROUP_MA'
    typmcs(2) = 'MAILLE'
!
    call getvtx(motclf, 'TOUT', iocc=1, scal=k8b, nbret=nbtou)
    if (nbtou .ne. 0) then
        nbma = nbmat
        call wkvect(mesmai, 'V V I', nbma, jma)
        do ima = 1, nbma
            zi(jma+ima-1) = ima
        end do
    else
        call reliem(' ', noma, 'NU_MAILLE', motclf, 1,&
                    2, motcls, typmcs, mesmai, nbma)
        call jeveuo(mesmai, 'L', jma)
    endif
!
    ncncin = '&&RC3600.CONNECINVERSE  '
    call jeexin(ncncin, ibid)
    if (ibid .eq. 0) call cncinv(noma, [ibid], 0, 'V', ncncin)
!
!     ------------------------------------------------------------------
!              RECUPERATION DES CARACTERISTIQUES MATERIAU
!     ------------------------------------------------------------------
!
    call rc36ma(nommat, noma)
!
!     ------------------------------------------------------------------
!            DEFINITION DES CARACTERISTIQUES ELEMENTAIRES
!     ------------------------------------------------------------------
!
    chcara = '&&RC3600.CARA_ELEM'
!
    call rc36ca(carael, noma, nbma, zi(jma), chcara)
!
    if (niv .ge. 2) then
        write(ifm,*)' LE CHAMP ', chcara
        call cesimp(chcara, ifm, 0, [ibid])
    endif
!
!
!     ------------------------------------------------------------------
!                    LES INDICES DE CONTRAINTES
!     ------------------------------------------------------------------
!
    chindi = '&&RC3600.INDI_SIGM'
!
    call rc36in(noma, nbma, zi(jma), chindi)
!
    if (niv .ge. 2) then
        write(ifm,*)' LE CHAMP ', chindi
        call cesimp(chindi, ifm, 0, [ibid])
    endif
!
!     ------------------------------------------------------------------
!                 LES RESULTATS DES CALCULS MECANIQUES
!     ------------------------------------------------------------------
!
    call rc36rm()
!
!     ------------------------------------------------------------------
!                           LES SITUATIONS
!     ------------------------------------------------------------------
!
    call rc36si(noma, nbma, zi(jma))
!
!     ------------------------------------------------------------------
!              CALCULS DES AMPLITUDES DE CONTRAINTES
!     ------------------------------------------------------------------
!
!     CALCUL DES AMPLITUDES DE CONTRAINTES QUI CORRESPONDENT AUX
!     COMBINAISONS DE TOUS LES ETATS STABILISES APPARTENANT AUX
!     SITUATIONS D'UN GROUPE DONNE
!
    nomgd = 'RCCM_R'
    nbcmp = 5
    nocmp(1) = 'SM'
    nocmp(2) = 'SN'
    nocmp(3) = 'SN_3SM'
    nocmp(4) = 'SALT'
    nocmp(5) = 'U_TOTAL'
!
    chresu = 'RC3600.RESULTAT'
    call rc36zz(noma, nomgd, nbcmp, nocmp, nbma,&
                zi(jma), chresu)
!
! --- CALCUL DES AMPLITUDES DE CONTRAINTES
!     CALCUL DU FACTEUR D'USAGE
!     -------------------------
!
    call rc36ac(noma, ncncin, chindi, chcara, nbma,&
                zi(jma), chresu)
!
    if (niv .ge. 2) then
        write(ifm,*)' LE CHAMP ', chresu
        call cesimp(chresu, ifm, 0, [ibid])
    endif
!
!
! --- PASSAGE DU CHAM_ELEM A UNE TABLE
!     --------------------------------
!
    call rc36rs(nomres, noma, nbma, zi(jma), chindi,&
                chresu)
!
    call detrsd('CHAM_ELEM_S', chindi)
    call detrsd('CHAM_ELEM_S', chcara)
    call detrsd('CHAM_ELEM_S', chresu)
    call jeexin(ncncin, ibid)
    if (ibid .ne. 0) call jedetr(ncncin)
    call jedetr(mesmai)
    call jedetc('V', '&&RC3600', 1)
!
    call jedema()
end subroutine
