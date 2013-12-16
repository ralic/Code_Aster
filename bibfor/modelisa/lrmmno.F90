subroutine lrmmno(fid, nomam2, ndim, nbnoeu, nomu,&
                  nomnoe, coordo, coodsc, cooref, ifm,&
                  infmed)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE -  FORMAT MED - LES NOEUDS
!     -    -     -                  -         --
!-----------------------------------------------------------------------
!     LECTURE DU FICHIER MAILLAGE AU FORMAT MED
!     ENTREES :
!       FID    : IDENTIFIANT DU FICHIER MED
!       NOMAM2 : NOM MED DU MAILLAGE A LIRE
!       NDIM   : DIMENSION DU PROBLEME (2  OU 3)
!       NBNOEU : NOMBRE DE NOEUDS DU MAILLAGE
!     SORTIES:
!-----------------------------------------------------------------------
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/as_mmhcor.h"
#include "asterfort/as_mmhear.h"
#include "asterfort/codent.h"
#include "asterfort/codlet.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: fid
    integer :: ndim, nbnoeu
    integer :: ifm, infmed
!
    character(len=8) :: nomu
    character(len=24) :: coordo, coodsc, cooref
    character(len=24) :: nomnoe
    character(len=*) :: nomam2
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: typnoe
    parameter (typnoe=0)
    integer :: edfuin
    parameter (edfuin=0)
!
    integer :: codret
    integer :: iaux
    integer :: ntgeo
!
    character(len=4) :: dimesp
    character(len=15) :: saux15
    character(len=8) :: saux08
    character(len=64) :: nomamd

    real(kind=8), pointer :: tcoord(:) => null()
    real(kind=8), pointer :: tcoorl(:) => null()
    character(len=16), pointer :: nomno(:) => null()
    character(len=24), pointer :: tcoorf(:) => null()
    integer, pointer :: tcoods(:) => null()
!
!     ------------------------------------------------------------------
    call jemarq()
!
    nomamd=nomam2
!
!====
! 1. LES NOMS DES NOEUDS
!====
!
    if (infmed .ge. 3) then
        write (ifm,101) nbnoeu
    endif
 101 format('LECTURE DES',i10,' NOEUDS',/)
!
! 1.1. ==> LECTURE DU NOM DANS LE FICHIER
!          SI LE FICHIER NE CONTIENT PAS DE NOMMAGE DES NOEUDS, ON LEUR
!          DONNE UN NOM PAR DEFAUT FORME AVEC LE PREFIXE 'N' SUIVI DE
!          LEUR NUMERO
!
    call wkvect('&&LRMMNO.NOMNOE', 'V V K16', nbnoeu, vk16=nomno)
    call as_mmhear(fid, nomamd, nomno(1), ednoeu, typnoe,&
                   codret)
!
    if (codret .ne. 0) then
        if (nbnoeu .ge. 10000000) then
!        PLUS DE 10 MILLIONS DE NOEUDS, ON PASSE EN BASE 36
            do 11 , iaux = 1 , nbnoeu
            call codlet(iaux, 'G', saux15)
            nomno(iaux) = 'N'//saux15
11          continue
        else
!        MOINS DE 10 MILLIONS DE NOEUDS, ON RESTE EN BASE 10
            do 12 , iaux = 1 , nbnoeu
            call codent(iaux, 'G', saux15)
            nomno(iaux) = 'N'//saux15
12          continue
        endif
        codret = 0
    endif
!
! 1.2. ==> TRANSFERT DANS LA STRUCTURE GLOBALE
!
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbnoeu)
    do 13 , iaux = 1 , nbnoeu
    call jecroc(jexnom(nomnoe, nomno(iaux)(1:8)))
    13 end do
!
!====
! 2. LES COORDONNEES DES NOEUDS
!====
!
! 2.1. ==> CREATION DU TABLEAU DES COORDONNEES
!    LA DIMENSION DU PROBLEME PHYSIQUE EST VARIABLE (1,2,3), MAIS
!   Aster stocke toujours 3 coordonnees par noeud.
    call wkvect(coordo, 'G V R', nbnoeu*3, vr=tcoord)
    call codent(ndim, 'G', dimesp)
    call jeecra(coordo, 'DOCU', cval=dimesp)
!
! 2.1. ==> EN DIMENSION 3, ON LIT LE TABLEAU DES COORDONNEES
!
!    LE TABLEAU COORDO EST UTILISE AINSI : COORDO(NDIM,NBNOEU)
!    EN FORTRAN, CELA CORRESPOND AU STOCKAGE MEMOIRE SUIVANT :
!    COORDO(1,1), COORDO(2,1), COORDO(3,1), COORDO(1,2), COORDO(2,2),
!    COORDO(3,2), COORDO(1,3), ... , COORDO(1,NBNOEU), COORDO(2,NBNOEU),
!    COORDO(3,NBNOEU)
!    C'EST CE QUE MED APPELLE LE MODE ENTRELACE
!
    if (ndim .eq. 3) then
!
        call as_mmhcor(fid, nomamd, tcoord, edfuin, codret)
        if (codret .ne. 0) then
            saux08='mmhcor'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
! 2.2. ==> AUTRES DIMENSIONS : ON CREE UN TABLEAU COMPACT DANS LEQUEL
!          ON STOCKE LES COORDONNEES, NOEUD APRES NOEUD.
!          C'EST CE QUE MED APPELLE LE MODE ENTRELACE.
!
    else
!
        call wkvect('&&LRMMNO.COORL', 'V V R', nbnoeu*ndim, vr=tcoorl)
!
        call as_mmhcor(fid, nomamd, tcoorl, edfuin, codret)
        if (codret .ne. 0) then
            saux08='mmhcor'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
        if (ndim .eq. 2) then
            do 221 , iaux = 0,nbnoeu-1
            tcoord(3*iaux+1) = tcoorl(2*iaux+1)
            tcoord(3*iaux+2) = tcoorl(2*iaux+2)
            tcoord(3*iaux+3) = 0.d0
221          continue
        else
            do 222 , iaux = 0,nbnoeu-1
            tcoord(3*iaux+1) = tcoorl(iaux+1)
            tcoord(3*iaux+2) = 0.d0
            tcoord(3*iaux+3) = 0.d0
222          continue
        endif
!
    endif
!
! 2.3. ==> OBJET DESCRIPTEUR DU CHAMP DES COORDONNEES DES NOEUDS
! -   RECUPERATION DU NUMERO IDENTIFIANT LE TYPE DE CHAM_NO GEOMETRIE
    call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), ntgeo)
    call wkvect(coodsc, 'G V I', 3, vi=tcoods)
    call jeecra(coodsc, 'DOCU', 0, 'CHNO')
    tcoods(1) = ntgeo
    tcoods(2) = -3
    tcoods(3) = 14
!
! -   OBJET REFE COORDONNEES DES NOEUDS
    call wkvect(cooref, 'G V K24', 4, vk24=tcoorf)
    tcoorf(1) = nomu
!
!====
! 3. LA FIN
!====
!
!     MENAGE
    call jedetr('&&LRMMNO.NOMNOE')
    call jedetr('&&LRMMNO.COORL')
!
    call jedema()
!
end subroutine
