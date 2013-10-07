subroutine nmobsz(sdobse, nomtab, titobs, nomcha, typcha,&
                  extrch, extrcp, extrga, nomcmp, nomnoe,&
                  nommai, num, snum, instan, valr)
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
    implicit     none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
    character(len=19) :: sdobse, nomtab
    character(len=4) :: typcha
    character(len=24) :: nomcha
    character(len=80) :: titobs
    character(len=8) :: extrch, extrcp, extrga
    character(len=8) :: nomnoe, nommai, nomcmp
    integer :: num, snum
    real(kind=8) :: instan, valr
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (OBSERVATION - UTILITAIRE)
!
! SAUVEGARDE DANS LA TABLE
!
! ----------------------------------------------------------------------
!
!
! IN  SDOBSE : SD OBSERVATION
! IN  NOMTAB : NOM DE LA TABLE
! IN  TITOBS : TITRE DE L'OBSERVATION
! IN  TYPCHA : TYPE DU CHAMP
! IN  NOMCHA : NOM DU CHAMP
! IN  NOMCMP : NOM DE LA COMPOSANTE
! IN  NOMNOE : NOM DU NOEUD
! IN  NOMMAI : NOM DE LA MAILLE
! IN  NUM    : NUMERO POINT DE GAUSS
! IN  SNUM   : NUMERO SOUS-POINT DE GAUSS
! IN  NUOBSV : NUMERO DE L'OBSERVATIONC
! IN  INSTAN : VALEUR DE L'INSTANT
! IN  VALR   : VALEUR A SAUVEGARDER DANS LA TABLE
!
! ----------------------------------------------------------------------
!
    integer :: nbpara
    parameter   (nbpara=16)
    character(len=16) :: nopara(nbpara)
    integer :: npar
    character(len=24) :: obsinf
    integer :: jobsin
    integer :: numreu, numobs
    complex(kind=8) :: c16bid
    character(len=16) :: typobj
    character(len=24) :: nomsd
    real(kind=8) :: tabr(nbpara)
    integer :: tabi(nbpara)
    character(len=24) :: tabk(nbpara), noparz(nbpara)
    integer :: ipar, kval, ival, rval
!
    data nopara/'NOM_OBSERVATION','TYPE_OBJET'  ,'NOM_SD' ,&
     &            'NUME_REUSE'     ,'NUME_OBSE'   ,'INST'   ,&
     &            'NOM_CHAM'       ,'EVAL_CHAM'   ,'NOM_CMP',&
     &            'EVAL_CMP'       ,'NOEUD'       ,'MAILLE' ,&
     &            'EVAL_ELGA'      ,'POINT'       ,'SOUS_POINT',&
     &            'VALE'           /
!
! ----------------------------------------------------------------------
!
    call jemarq()
    c16bid=(0.d0,0.d0)
!
! --- SD PRINCIPALE (INFO)
!
    obsinf = sdobse(1:14)//'     .INFO'
    call jeveuo(obsinf, 'E', jobsin)
!
! --- INITIALISATIONS
!
    ipar = 1
    kval = 1
    ival = 1
    rval = 1
    typobj = 'R'
    nomsd = ' '
    numobs = zi(jobsin-1+3)
    numreu = zi(jobsin-1+4)
!
! --- CE QUI EST COMMUN
!
    noparz(ipar) = nopara(1)
    ipar = ipar + 1
    tabk(kval) = titobs
    kval = kval + 1
!
    noparz(ipar) = nopara(2)
    ipar = ipar + 1
    tabk(kval) = typobj
    kval = kval + 1
!
    noparz(ipar) = nopara(3)
    ipar = ipar + 1
    tabk(kval) = nomsd
    kval = kval + 1
!
    noparz(ipar) = nopara(4)
    ipar = ipar + 1
    tabi(ival) = numreu
    ival = ival + 1
!
    noparz(ipar) = nopara(5)
    ipar = ipar + 1
    tabi(ival) = numobs
    ival = ival + 1
!
    noparz(ipar) = nopara(6)
    ipar = ipar + 1
    tabr(rval) = instan
    rval = rval + 1
!
    noparz(ipar) = nopara(7)
    ipar = ipar + 1
    tabk(kval) = nomcha
    kval = kval + 1
!
! --- EXTRACTION DU CHAMP: TYPE
!
    noparz(ipar) = nopara(8)
    ipar = ipar + 1
    tabk(kval) = extrch
    kval = kval + 1
!
! --- EXTRACTION DES COMPOSANTES: TYPE
!
    if (extrcp .eq. ' ') then
        noparz(ipar) = nopara(9)
        ipar = ipar + 1
        tabk(kval) = nomcmp
        kval = kval + 1
    else
        noparz(ipar) = nopara(10)
        ipar = ipar + 1
        tabk(kval) = extrcp
        kval = kval + 1
    endif
!
! --- NOEUD OU MAILLE
!
    if (typcha .eq. 'NOEU') then
        if (extrch .eq. 'VALE') then
            noparz(ipar) = nopara(11)
            ipar = ipar + 1
            tabk(kval) = nomnoe
            kval = kval + 1
        else
            noparz(ipar) = nopara(8)
            ipar = ipar + 1
            tabk(kval) = extrch
            kval = kval + 1
        endif
        noparz(ipar) = nopara(16)
        ipar = ipar + 1
        tabr(rval) = valr
        rval = rval + 1
    else if (typcha.eq.'ELGA') then
        if (extrch .eq. 'VALE') then
            noparz(ipar) = nopara(12)
            ipar = ipar + 1
            tabk(kval) = nommai
            kval = kval + 1
        else
            noparz(ipar) = nopara(8)
            ipar = ipar + 1
            tabk(kval) = extrch
            kval = kval + 1
        endif
        if (extrga .eq. 'VALE') then
            noparz(ipar) = nopara(14)
            ipar = ipar + 1
            tabi(ival) = num
            ival = ival + 1
            noparz(ipar) = nopara(15)
            ipar = ipar + 1
            tabi(ival) = snum
            ival = ival + 1
            noparz(ipar) = nopara(16)
            ipar = ipar + 1
            tabr(rval) = valr
            rval = rval + 1
        else
            noparz(ipar) = nopara(13)
            ipar = ipar + 1
            tabk(kval) = extrga
            kval = kval + 1
            noparz(ipar) = nopara(16)
            ipar = ipar + 1
            tabr(rval) = valr
            rval = rval + 1
        endif
    else
        ASSERT(.false.)
    endif
!
    npar = ipar -1
!
! --- AJOUT DANS LA TABLE
!
    call tbajli(nomtab, npar, noparz, tabi, tabr,&
                [c16bid], tabk, 0)
!
! --- OBSERVATION SUIVANTE
!
    zi(jobsin-1+3) = zi(jobsin-1+3) + 1
!
    call jedema()
!
end subroutine
