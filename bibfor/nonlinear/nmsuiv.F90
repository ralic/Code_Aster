subroutine nmsuiv(noma, sdieto, sdsuiv, sdimpr)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
#include "jeveux.h"
#include "asterfort/impfoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmext0.h"
#include "asterfort/nmext1.h"
#include "asterfort/nmextd.h"
#include "asterfort/nmextt.h"
#include "asterfort/nmsui3.h"
    character(len=24) :: sdimpr, sdsuiv
    character(len=8) :: noma
    character(len=24) :: sdieto
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - SUIVI_DDL)
!
! REALISER UN SUIVI_DDL
!
! ----------------------------------------------------------------------
!
!
! IN  SDIMPR : SD AFFICHAGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDSUIV : NOM DE LA SD POUR SUIVI_DDL
! IN  SDIETO : SD GESTION IN ET OUT
!
! ----------------------------------------------------------------------
!
    character(len=24) :: listno, listma, listpi, listsp, listcp
    integer :: jno, jma
    character(len=24) :: suiinf, suicha, suityp
    integer :: jsuiin, jsuich, jsuity
    integer :: nbcmp, nbno, nbma, nbcham
    integer :: nbpi, nbspi
    integer :: iocc, nbocc
    integer :: isuiv, icham
    character(len=2) :: chaine
    character(len=24) :: nomcha, nomchs
    character(len=4) :: typcha
    character(len=19) :: champ
    character(len=8) :: extrcp, extrch, extrga
    character(len=19) :: chgaus, chnoeu, chelga
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    isuiv = 1
!
! --- SD PRINCIPALE (INFO)
!
    suiinf = sdsuiv(1:14)//'     .INFO'
    call jeveuo(suiinf, 'L', jsuiin)
    nbocc  = zi(jsuiin-1+1)
    nbcham = zi(jsuiin-1+6)
    if (nbocc .eq. 0) goto 999
!
! --- SD LISTE DES CHAMPS
!
    suicha = sdsuiv(1:14)//'     .CHAM'
    call jeveuo(suicha, 'L', jsuich)
!
! --- SD TYPE D'EXTRACTIONS
!
    suityp = sdsuiv(1:14)//'     .EXTR'
    call jeveuo(suityp, 'L', jsuity)
!
    do 10 iocc = 1, nbocc
!
! ----- GENERATION NOM DES SD
!
        call impfoi(0, 2, iocc, chaine)
        listno = sdsuiv(1:14)//chaine(1:2)//'   .NOEU'
        listma = sdsuiv(1:14)//chaine(1:2)//'   .MAIL'
        listpi = sdsuiv(1:14)//chaine(1:2)//'   .POIN'
        listsp = sdsuiv(1:14)//chaine(1:2)//'   .SSPI'
        listcp = sdsuiv(1:14)//chaine(1:2)//'   .CMP '
!
! ----- NOM DU CHAMP
!
        icham = zi(jsuiin+7+7*(iocc-1)+7-1)
        nomcha = zk24(jsuich+4*(icham-1)+1-1)
        nomchs = zk24(jsuich+4*(icham-1)+2-1)
        if (nomcha .eq. 'NONE') goto 99
!
! ----- TYPE DE CHAMP
!
        typcha = zk24(jsuich-1+4*(icham-1)+3)
!
! ----- RECUPERATION DU CHAMP
!
        champ = zk24(jsuich-1+4*(icham-1)+4)
!
! ----- NOMBRE DE COMPOSANTES/NOEUDS/MAILLES
!
        nbcmp = zi(jsuiin+7+7*(iocc-1)-1+1)
        nbno = zi(jsuiin+7+7*(iocc-1)-1+2)
        nbma = zi(jsuiin+7+7*(iocc-1)-1+3)
        nbpi = zi(jsuiin+7+7*(iocc-1)-1+4)
        nbspi = zi(jsuiin+7+7*(iocc-1)-1+5)
!
! ----- ACCES LISTES
!
        if (typcha .eq. 'NOEU') call jeveuo(listno, 'L', jno)
        if (typcha .eq. 'ELGA') call jeveuo(listma, 'L', jma)
!
! ----- TYPES D'EXTRACTION
!
        extrch = zk8(jsuity+3*(iocc-1)+1-1)
        extrga = zk8(jsuity+3*(iocc-1)+2-1)
        extrcp = zk8(jsuity+3*(iocc-1)+3-1)
!
! ----- SD DONNEES TEMPORAIRES
!
        chelga = '&&NMSUIV.VALE.ELGA'
        chgaus = '&&NMSUIV.VALE.GAUS'
        chnoeu = '&&NMSUIV.VALE.NOEU'
        call nmext0(typcha, nbma, nbno, nbpi, nbspi,&
                    nbcmp, chnoeu, chgaus, chelga, extrga,&
                    extrch)
!
! ----- EXTRAIRE LES VALEURS
!
        call nmext1(noma, champ, typcha, nomcha, nomchs,&
                    nbma, nbno, nbpi, nbspi, nbcmp,&
                    extrga, extrch, extrcp, listno, listma,&
                    listpi, listsp, listcp, chnoeu, chgaus,&
                    chelga)
!
! ----- LES ECRIRE DANS LE TABLEAU
!
        call nmsui3(sdimpr, typcha, nbma, nbno, nbpi,&
                    nbspi, nbcmp, extrch, extrcp, extrga,&
                    listma, chnoeu, chelga, champ, isuiv)
!
        call jedetr(chgaus)
        call jedetr(chnoeu)
        call jedetr(chelga)
!
99      continue
!
10  end do
!
! --- DESTRUCTION DES CHAM_ELEM_S
!
    do 45 icham = 1, nbcham
        nomchs = zk24(jsuich+4*(icham-1)+2-1)
        call jedetr(nomchs)
45  end do
999  continue
!
    call jedema()
end subroutine
