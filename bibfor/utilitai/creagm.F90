subroutine creagm(nbmato, nbpart, ma, masd)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:
!       - CREATION DE NOUVEAUX GROUPES DE MAILLES
!              DANS LA STRUCTURE MAILLAGE
!
!    - IN :     NBMATO : NOMBRE DE MAILLES
!               NBPART : NOMBRE DE PARTITION
!               RENUM  : RENUMEROTATION
!               NBMASD : NOMBRE DE MAILLES PAR SOUS DOMAINE
!               MA     : NOM DU MAILLAGE
!               NUMSDM : SOUS DOMAINES DE CHAQUES MAILLE
!
!    - OUT :    MASD   : DONNE LES MAILLES PAR SD
!               IDMASD : INDEX DE MASD
!               NOMSDM : NOM DES GROUP_MA
!
!----------------------------------------------------------------------
! person_in_charge: aimery.assire at edf.fr
!
! CORPS DU PROGRAMME
    implicit none
!
!
! DECLARATION VARIABLES D'APPEL
#include "jeveux.h"
#include "asterfort/cpclma.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcadr.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
!
    integer :: nbmato, nbpart, renum, nbmasd, nomsdm, masd, idmasd, numsdm
    character(len=8) :: ma
!
! DECLARATION VARIABLES LOCALES
    integer :: id1, isd, nbma, ima, nbre, idma, ifm, niv
    integer ::  nbgrsd
    real(kind=8) :: tmps(7)
    character(len=8) :: ktmp
    character(len=24) ::  grpema
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo('&&FETSKP.RENUM', 'L', renum)
    call jeveuo('&&FETSKP.NBMASD', 'L', nbmasd)
    call jeveuo('&&FETSKP.NUMSDM', 'E', numsdm)
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREAGM', 'INIT', ' ')
        call uttcpu('CPU.CREAGM', 'DEBUT', ' ')
    endif
!
    call wkvect('&&FETSKP.ID1', 'V V I', nbpart, id1)
    call wkvect('&&FETSKP.IDMASD', 'V V I', nbpart+1, idmasd)
    call wkvect('&&FETSKP.NOMSDM', 'V V K24', nbpart, nomsdm)
    call wkvect('&&FETSKP.MASD', 'V V I', nbmato, masd)
!
! ------- On RECUPERE LE NOM DES SOUS DOMAINES
!
    do 51 isd = 1, nbpart
        write(ktmp,'(I4)')isd-1
        call lxcadr(ktmp)
        zk24(nomsdm-1+isd) = 'SD'//ktmp
51  end do
!
!
! ------- CREATION DU TABLEAU DES PLACES DES GRPMA
!
    zi(idmasd)=1
    do 32 isd = 2, nbpart+1
        zi(idmasd-1+isd)=zi(idmasd-1+isd-1)+zi(nbmasd-1+isd-1)
32  end do
!
! ------- CREATION DU TABLEAU DONNANT LES MAILLES POUR CHAQUE GRMPA
!
    do 36 ima = 1, nbmato
        nbre=zi(numsdm-1+ima)+1
        zi(masd-1+zi(idmasd-1+nbre)+zi(id1-1+nbre)) = zi(renum-1+ima)
        zi(id1-1+nbre) = zi(id1-1+nbre)+1
36  end do

!
!   -- on cree un "GROUPEMA" par sous-domaine :
    nbgrsd=0
    do isd = 1, nbpart
        nbma=zi(nbmasd-1+isd)
        if (nbma.gt.0) nbgrsd=nbgrsd+1
    enddo
    call jecrec('&&FETCRF.GROUPEMA', 'V V I', 'NO', 'DISPERSE', 'VARIABLE',nbgrsd)

    do 38 isd = 1, nbpart
        grpema=zk24(nomsdm-1+isd)
        nbma=zi(nbmasd-1+isd)
        if (nbma .gt. 0) then
            call jecroc(jexnom('&&FETCRF.GROUPEMA', grpema))
            call jeecra(jexnom('&&FETCRF.GROUPEMA', grpema), 'LONMAX', nbma)
            call jeecra(jexnom('&&FETCRF.GROUPEMA', grpema), 'LONUTI', nbma)
            call jeveuo(jexnom('&&FETCRF.GROUPEMA', grpema), 'E', idma)
            do 41 ima = 0, nbma - 1
                zi(idma+ima) = zi(masd+zi(idmasd-1+isd)-1+ima)
41          continue
        endif
38  end do

    call jedetr('&&FETSKP.ID1')
!
    if (niv .ge. 2) then
        call uttcpu('CPU.CREAGM', 'FIN', ' ')
        call uttcpr('CPU.CREAGM', 7, tmps)
        write(ifm,*)'--- CREATION DES GRPMA :',tmps(7)
        write(ifm,*)'  '
    endif
!
    call jedema()
end subroutine
