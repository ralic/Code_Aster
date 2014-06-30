subroutine nmobse(noma, sdieto, sdobse, instan)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/impfoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmext0.h"
#include "asterfort/nmext1.h"
#include "asterfort/nmextd.h"
#include "asterfort/nmextt.h"
#include "asterfort/nmobs2.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma
    character(len=19) :: sdobse
    character(len=24) :: sdieto
    real(kind=8) :: instan
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (OBSERVATION)
!
! REALISER LES OBSERVATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  SDEXTR : NOM DE LA SD POUR EXTRACTION
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDOBSE : SD OBSERVATION
! IN  INSTAN : INSTANT COURANT
!
! ----------------------------------------------------------------------
!
    character(len=24) :: listno, listma, listpi, listsp, listcp
    integer :: jno, jma
    character(len=24) :: obsinf, obscha, obstyp, obsact
    integer :: jobsin, jobsch, jobsty, jobsac
    character(len=24) :: obstab
    integer :: jobst
    character(len=24) :: obsnom
    integer :: jobsno
    character(len=19) :: nomtab
    character(len=80) :: titobs
    integer :: nbcmp, nbno, nbma, nbcham
    integer :: nbpi, nbspi
    integer :: iocc, nbocc, nobsef, icham
    character(len=2) :: chaine
    character(len=24) :: nomcha, nomchs
    character(len=4) :: typcha
    character(len=19) :: champ
    character(len=8) :: extrcp, extrch, extrga
    character(len=19) :: chgaus, chnoeu, chelga
    logical(kind=1) :: lobsv
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- SD POUR LE NOM DE LA TABLE
!
    obstab = sdobse(1:14)//'     .TABL'
    call jeveuo(obstab, 'L', jobst)
    nomtab = zk24(jobst)(1:19)
!
! --- SD PRINCIPALE (INFO)
!
    obsinf = sdobse(1:14)//'     .INFO'
    call jeveuo(obsinf, 'L', jobsin)
    nbocc = zi(jobsin-1+1)
    ASSERT(nbocc.le.99)
!
! --- SD TITRES
!
    obsnom = sdobse(1:14)//'     .TITR'
    call jeveuo(obsnom, 'L', jobsno)
!
! --- SD LISTE DES CHAMPS
!
    obscha = sdobse(1:14)//'     .CHAM'
    call jeveuo(obscha, 'L', jobsch)
    call jelira(obscha, 'LONMAX', ival=nbcham)
    nbcham = nbcham / 2
!
! --- SD TYPE D'EXTRACTIONS
!
    obstyp = sdobse(1:14)//'     .EXTR'
    call jeveuo(obstyp, 'L', jobsty)
!
! --- SD ACTIVATION EXTRACTION
!
    obsact = sdobse(1:14)//'     .ACTI'
    call jeveuo(obsact, 'L', jobsac)
!
    nobsef = 0
!
    do 10 iocc = 1, nbocc
!
! ----- OBSERVATION
!
        lobsv = zl(jobsac+iocc-1)
        if (.not.lobsv) goto 99
!
! ----- GENERATION NOM DES SD
!
        call impfoi(0, 2, iocc, chaine)
        listno = sdobse(1:14)//chaine(1:2)//'   .NOEU'
        listma = sdobse(1:14)//chaine(1:2)//'   .MAIL'
        listpi = sdobse(1:14)//chaine(1:2)//'   .POIN'
        listsp = sdobse(1:14)//chaine(1:2)//'   .SSPI'
        listcp = sdobse(1:14)//chaine(1:2)//'   .CMP '
!
! ----- NOM DU CHAMP
!
        icham = zi(jobsin+4+7*(iocc-1)+7-1)
        nomcha = zk24(jobsch+2*(icham-1)+1-1)
        nomchs = zk24(jobsch+2*(icham-1)+2-1)
        if (nomcha .eq. 'NONE') goto 99
!
! ----- TYPE DE CHAMP
!
        call nmextt(sdieto, nomcha, typcha)
!
! ----- RECUPERATION DU CHAMP
!
        call nmextd(nomcha, sdieto, champ)
!
! ----- NOMBRE DE COMPOSANTES/NOEUDS/MAILLES
!
        nbcmp = zi(jobsin+4+7*(iocc-1)-1+1)
        nbno = zi(jobsin+4+7*(iocc-1)-1+2)
        nbma = zi(jobsin+4+7*(iocc-1)-1+3)
        nbpi = zi(jobsin+4+7*(iocc-1)-1+4)
        nbspi = zi(jobsin+4+7*(iocc-1)-1+5)
!
! ----- ACCES LISTES
!
        if (typcha .eq. 'NOEU') call jeveuo(listno, 'L', jno)
        if (typcha .eq. 'ELGA') call jeveuo(listma, 'L', jma)
!
! ----- TYPES D'EXTRACTION
!
        extrch = zk8(jobsty+3*(iocc-1)+1-1)
        extrga = zk8(jobsty+3*(iocc-1)+2-1)
        extrcp = zk8(jobsty+3*(iocc-1)+3-1)
!
! ----- CREATION SD DONNEES TEMPORAIRES
!
        chelga = '&&NMOBSE.VALE.ELGA'
        chgaus = '&&NMOBSE.VALE.GAUS'
        chnoeu = '&&NMOBSE.VALE.NOEU'
        call nmext0(typcha, nbma, nbno, nbpi, nbspi,&
                    nbcmp, chnoeu, chgaus, chelga, extrga,&
                    extrch)
!
! ----- EXTRAIRE LES VALEURS ET STOCKAGE DANS VECTEURS TEMPORAIRES
!
        call nmext1(noma, champ, typcha, nomcha, nomchs,&
                    nbma, nbno, nbpi, nbspi, nbcmp,&
                    extrga, extrch, extrcp, listno, listma,&
                    listpi, listsp, listcp, chnoeu, chgaus,&
                    chelga)
!
! ----- TITRE DE L'OBSERVATION
!
        titobs = zk80(jobsno+iocc-1)
!
! ----- SAUVER LES VALEURS DANS LA TABLE
!
        call nmobs2(noma, sdobse, nomtab, instan, titobs,&
                    typcha, nomcha, nomchs, nbma, nbno,&
                    nbpi, nbspi, nbcmp, extrga, extrch,&
                    extrcp, listno, listma, listpi, listsp,&
                    listcp, champ, chnoeu, chelga, nobsef)
!
        call jedetr(chgaus)
        call jedetr(chnoeu)
        call jedetr(chelga)
!
99      continue
!
10  end do
!
! --- AFFICHAGE
!
    if (nobsef .eq. 0) then
        call utmess('I', 'OBSERVATION_39')
    else if (nobsef.eq.1) then
        call utmess('I', 'OBSERVATION_38')
    else
        call utmess('I', 'OBSERVATION_37', si=nobsef)
    endif
!
! --- DESTRUCTION DES CHAM_ELEM_S
!
    do 45 icham = 1, nbcham
        nomchs = zk24(jobsch+2*(icham-1)+2-1)
        call jedetr(nomchs)
45  end do
!
    call jedema()
!
end subroutine
