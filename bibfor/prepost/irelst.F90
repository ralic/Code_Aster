subroutine irelst(nofimd, chanom, nochmd, typech, nomaas,&
                  nomamd, nbimpr, caimpi, caimpk, sdcarm)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mmhcow.h"
#include "asterfort/as_mmhcyw.h"
#include "asterfort/as_msecre.h"
#include "asterfort/as_msense.h"
#include "asterfort/as_msesei.h"
#include "asterfort/as_msevac.h"
#include "asterfort/as_msmcre.h"
#include "asterfort/as_msmsmi.h"
#include "asterfort/assert.h"
#include "asterfort/elref2.h"
#include "asterfort/irmaes.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/lrmtyp.h"
#include "asterfort/uteref.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=8) :: nomaas, typech, sdcarm
    character(len=*) :: nofimd
    character(len=19) :: chanom
    character(len=64) :: nomamd, nochmd
    integer :: nbimpr, caimpi(10, nbimpr)
    character(len=80) :: caimpk(3, nbimpr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - IMPRESSION DES ELEMENTS DE STRUCTURE AU FORMAT MED
!  -    -                     --          --
! ----------------------------------------------------------------------
!
! IN  :
!   NOFIMD  K*   NOM DU FICHIER MED
!   CHANOM  K19  NOM DU CHAMP A IMPRIMER
!   TYPECH  K8   TYPE DU CHAMP
!   NOMAAS  K8   NOM DU MAILLAGE ASTER A COMPLETER DANS LE FICHIER MED
!   NOMAMD  K*   NOM DU MAILLAGE MED
!   NBIMPR  I    NOMBRE D'IMPRESSIONS
!   CAIMPI  I*   ENTIERS POUR CHAQUE IMPRESSION
!   CAIMPK  K80* CARACTERES POUR CHAQUE IMPRESSION
!   SDCARM  K*   SD_CARA_ELEM EN CHAM_ELEM_S
!
!
!
    integer :: inimpr, nbcouc, nbsect, nummai, lgmax, ntypef, codret
    integer :: nbnoso, nbnoto, nbrepg, ndim, nbfamx, nbelr
    integer :: edleaj, idfimd, edcart, edfuin, ntymax, nbtyp, nnomax
    integer :: edmail, ednoda, edtyre, medcel, nbmssu, nbattc, prespr
    parameter    (edleaj = 1)
    parameter    (nbfamx = 20)
    parameter    (lgmax  = 1000)
    parameter    (edcart = 0)
    parameter    (edfuin = 0)
    parameter    (ntymax = 69)
    parameter    (nnomax = 27)
    parameter    (edmail = 0)
    parameter    (ednoda = 0)
    parameter    (edtyre = 6)
    integer :: nnotyp(ntymax), typgeo(ntymax), renumd(ntymax)
    integer :: modnum(ntymax), nuanom(ntymax, nnomax), ino, inimp2
    integer :: numnoa(ntymax, nnomax), tymaas, tymamd, connex(9)
    integer :: imasup, jmasup, nbmasu, nbmsmx, nvtymd, edcar2, nbattv
    integer :: dimest, nbnosu, tygems
!
    character(len=8) :: lielrf(nbfamx), saux08, nomtyp(ntymax)
    character(len=16) :: nomtef, nomfpg, nocoor(3), uncoor(3)
    character(len=16) :: nocoo2(3), uncoo2(3)
    character(len=64) :: nomasu, atepai, atangv, atrmax, atrmin, nomaes
    character(len=200) :: desmed
    parameter    (atepai = 'EPAISSEUR')
    parameter    (atangv = 'ANGLE DE VRILLE')
    parameter    (atrmin = 'RAYON MIN')
    parameter    (atrmax = 'RAYON MAX')
!
    real(kind=8) :: refcoo(3*lgmax), gscoo(3*lgmax), wg(lgmax)
!
    aster_logical :: newest
    integer, pointer :: nv_type_med(:) => null()
!
    data nocoor  /'X               ',&
     &              'Y               ',&
     &              'Z               '/
    data uncoor  /'INCONNU         ',&
     &              'INCONNU         ',&
     &              'INCONNU         '/
!
    call as_mfiope(idfimd, nofimd, edleaj, codret)
    if (codret .ne. 0) then
        saux08='mfiope'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
!     -- RELECTURE DES ELEMENTS DE STRUCTURES DEJA PRESENTS
    nbmasu = 0
    call as_msense(idfimd, nbmasu, codret)
    if (codret .ne. 0) then
        saux08='msmnsm'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
    nbmsmx = nbmasu+10
    call wkvect('&&IRELST.MAIL_SUPP', 'V V K80', nbmsmx, jmasup)
    AS_ALLOCATE(vi=nv_type_med, size=nbmsmx)
    if (nbmasu .ne. 0) then
        do 40 imasup = 1, nbmasu
            call as_msmsmi(idfimd, imasup, nomasu, ndim, desmed,&
                           edcar2, nocoo2, uncoo2, codret)
            if (codret .ne. 0) then
                saux08='msmsmi'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
            zk80(jmasup+imasup-1) = nomasu
!
            call as_msesei(idfimd, imasup, nomaes, nvtymd, dimest,&
                           nomasu, medcel, nbnosu, nbmssu, tygems,&
                           nbattc, prespr, nbattv, codret)
            if (codret .ne. 0) then
                saux08='msesei'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
            nv_type_med(imasup) = nvtymd
 40     continue
    endif
!
    desmed = ' '
!
    call lrmtyp(nbtyp, nomtyp, nnotyp, typgeo, renumd,&
                modnum, nuanom, numnoa)
!
!     -- CREATION DES ELEMENTS DE STRUCTURES DANS LE FICHIER MED
!        UN ELEMENT DE STRUCTURE EST DEFINIT PAR UNE PAIRE :
!         TYPE ELEMENT (COQUE, TUYAU, ...) + TYPE MAILLE
    newest = .false.
    do 10 inimpr = 1, nbimpr
        ntypef = caimpi(1,inimpr)
        nbcouc = caimpi(4,inimpr)
        nbsect = caimpi(5,inimpr)
        nummai = caimpi(6,inimpr)
        tymaas = caimpi(8,inimpr)
        tymamd = caimpi(9,inimpr)
!
        call jenuno(jexnum('&CATA.TE.NOMTE', ntypef), nomtef)
!
        call elref2(nomtef, nbfamx, lielrf, nbelr)
        ASSERT(nbelr.gt.0)
!
        call uteref(chanom, typech, ntypef, nomtef, nomfpg,&
                    nbnoso, nbnoto, nbrepg, ndim, refcoo,&
                    gscoo, wg, nochmd, codret)
!
        nomasu = ' '
        if (nbcouc .ne. 0 .and. nbsect .eq. 0) then
!         -- CAS D'UNE COQUE
            nomasu(1:8) = 'COQUE   '
        else if (nbcouc.ne.0.and.nbsect.ne.0) then
!         -- CAS D'UN TUYAU
            nomasu(1:8) = 'TUYAU   '
        else if (nummai.ne.0) then
!         -- CAS D'UNE PMF
            nomasu(1:8) = 'PMF     '
        else if (nbcouc.eq.0.and.nbsect.eq.0.and.nummai.eq.0) then
            goto 50
        else
            ASSERT(.false.)
        endif
        nomasu(9:12) = nomfpg(1:3)
        do 70 inimp2 = 1, nbimpr
            if (caimpk(3,inimp2) .eq. nomasu) then
                caimpk(3,inimpr) = nomasu
                caimpi(9,inimpr) = caimpi(9,inimp2)
                goto 50
            endif
 70     continue
        do 60 imasup = 1, nbmasu
            if (zk80(jmasup+imasup-1) .eq. nomasu) then
                caimpk(3,inimpr) = zk80(jmasup+imasup-1)
                caimpi(9,inimpr) = nv_type_med(imasup)
                goto 50
            endif
 60     continue
!
!       -- DEFINITION DU MAILLAGE SUPPORT MED
        call as_msmcre(idfimd, nomasu, ndim, desmed, edcart,&
                       nocoor, uncoor, codret)
        if (codret .ne. 0) then
            saux08='msmcre'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
!       -- DEFINITION DES NOEUDS DU MAILLAGE SUPPORT MED
        call as_mmhcow(idfimd, nomasu, refcoo, edfuin, nbnoto,&
                       codret)
        if (codret .ne. 0) then
            saux08='mmhcow'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
!       -- CREATION DE LA CONNECTIVITE
        ASSERT(nbnoto.le.9)
        if (modnum(tymaas) .eq. 0) then
            do 20 ino = 1, nbnoto
                connex(ino) = ino
 20         continue
        else
            do 30 ino = 1, nbnoto
                connex(ino) = nuanom(tymaas,ino)
 30         continue
        endif
!
!       -- DEFINITION DE LA MAILLE DU MAILLAGE SUPPORT
        call as_mmhcyw(idfimd, nomasu, connex, nbnoto, edfuin,&
                       1, edmail, tymamd, ednoda, codret)
        if (codret .ne. 0) then
            saux08='mmhcyw'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
!       -- SAUVEGARDE DE L'ELEMENT DE STRUCTURE
        nbmasu = nbmasu+1
        if (nbmasu .gt. nbmsmx) then
            nbmsmx = nbmsmx+10
            call juveca('&&IRELST.MAIL_SUPP', nbmsmx)
            call jeveuo('&&IRELST.MAIL_SUPP', 'E', jmasup)
        endif
        zk80(jmasup+nbmasu-1) = nomasu
!
        nvtymd = -9999
        call as_msecre(idfimd, nomasu, ndim, nomasu, edmail,&
                       tymamd, nvtymd, codret)
        ASSERT(nvtymd.ne.-9999)
        if (codret .ne. 0) then
            saux08='msecre'
            call utmess('F', 'DVP_97', sk=saux08, si=codret)
        endif
!
        if (nomasu(1:5) .eq. 'COQUE') then
!         -- ATTRIBUT VARIABLE EPAISSEUR
            call as_msevac(idfimd, nomasu, atepai, edtyre, 1,&
                           codret)
            if (codret .ne. 0) then
                saux08='msevac'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
        else if (nomasu(1:5).eq.'TUYAU') then
!         -- ATTRIBUT VARIABLE RAYON MIN
            call as_msevac(idfimd, nomasu, atrmin, edtyre, 1,&
                           codret)
            if (codret .ne. 0) then
                saux08='msevac'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
!         -- ATTRIBUT VARIABLE RAYON MAX
            call as_msevac(idfimd, nomasu, atrmax, edtyre, 1,&
                           codret)
            if (codret .ne. 0) then
                saux08='msevac'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
!         -- ATTRIBUT VARIABLE ANGLE DE VRILLE
            call as_msevac(idfimd, nomasu, atangv, edtyre, 1,&
                           codret)
            if (codret .ne. 0) then
                saux08='msevac'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
        else if (nomasu(1:3).eq.'PMF') then
!         -- ATTRIBUT VARIABLE ANGLE DE VRILLE
            call as_msevac(idfimd, nomasu, atangv, edtyre, 1,&
                           codret)
            if (codret .ne. 0) then
                saux08='msevac'
                call utmess('F', 'DVP_97', sk=saux08, si=codret)
            endif
        else
            ASSERT(.false.)
        endif
!
!       -- MODIFICATION DU TYPE MED A IMPRIMER
        caimpi(9,inimpr) = nvtymd
        caimpk(3,inimpr) = nomasu
        newest = .true.
!
 50     continue
!
 10 end do
!
!     -- AJOUT DES MAILLES "STRUCTURES" AU MAILLAGE
    if (newest) then
        call irmaes(idfimd, nomaas, nomamd, nbimpr, caimpi,&
                    modnum, nuanom, nomtyp, nnotyp, sdcarm)
    endif
!
    call as_mficlo(idfimd, codret)
    if (codret .ne. 0) then
        saux08='mficlo'
        call utmess('F', 'DVP_97', sk=saux08, si=codret)
    endif
!
    call jedetr('&&IRELST.MAIL_SUPP')
    AS_DEALLOCATE(vi=nv_type_med)
!
end subroutine
