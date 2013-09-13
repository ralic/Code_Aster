subroutine rvcohe(xdicmp, xdncmp, vcheff, i, ier)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=24) :: xdicmp, xdncmp, vcheff
    integer :: i, ier
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     VERIFICATION DE COHERENCE DES ARGUMENTS D' APPEL DE LA COMMANDE
!     DE CALCUL DU POST TRAITEMENT (OP0051)
!       1. EXISTENCE DES CHAM_GD MIS EN JEU
!       2. LEGALITE DES COMPOSANTES MISES EN JEU POUR CES CHAM_GD
!       3. CONCORDANCE ENTRE LES MAILLAGES DES CHAMPS ET DES COURBES
!          OU DES NOEUDS
!     ------------------------------------------------------------------
! IN  XDNCMP : K : NOM DE OJB XD V K8 DES NOMS DE CMP MISES EN JEU
! IN  XDICMP : K : NOM DE OJB XD V I  DES NUMS DE CMP (0 <=> ILLEGALE)
! IN  VCHEFF : K : NOM DU OJB S  V K24 DES NOMS DE CHAMPS EFFECTIF
! IN  I      : I : NUMERO DE L' OCCURENCE A TRAITER
! OUT IER    : I : CODE RETOUR, 1 --> OK, 0 --> KO
!     ------------------------------------------------------------------
!
!
    character(len=24) :: ncheff, ndesc, valk(7), nomgrn
    character(len=19) :: nchp19
    character(len=16) :: nchsym, tresu
    character(len=15) :: nrepnd
    character(len=8) :: nresu, nomcmp, nmaich, nmaili, nomcrb, nomnd
    character(len=4) :: docu
    integer :: acheff, amaicb, agrpnd, alneud, anumcp, anomcp, nbcmp
    integer :: nbgrpn, nbneud, nbcrb, grel, nbgrel, jceld, amod, mod
    integer :: j, k, n1, ierd, ibid
    logical :: chelok
!
!=====================================================================
!
    call jemarq()
    ier = 1
    call jeveuo(vcheff, 'L', acheff)
    ncheff = zk24(acheff + i-1)
!
    if (ncheff(1:1) .eq. '&') then
!
!        CAS D'UN CHAMP SYMBOLIQUE ILLEGAL OU DE NON EXISTENCE
!                  D' UN CHAMP EFFECTIF ASSOCIE
!
        ier = 0
        call getvid('ACTION', 'RESULTAT', iocc=i, scal=nresu, nbret=n1)
        call getvtx('ACTION', 'NOM_CHAM', iocc=i, scal=nchsym, nbret=n1)
        call gettco(nresu, tresu)
!
        valk(1) = nchsym
        valk(2) = nresu
        valk(3) = tresu
        call utmess('F', 'POSTRELE_46', nk=3, valk=valk, si=i)
!
    else
!
!        LE CHAMP SYMBOLIQUE EXISTE ET UN CHAMP EFFECTIF LUI
!        CORRESPOND OU UN CHAMP EFFECTIF EST DIRECTEMENT ARGUMENT
!        VERIFICATION POUR LES CHAM_ELEM DU CARACTERE "AUX NOEUDS"
!
        nchp19 = ncheff(1:19)
        call jeexin(nchp19//'.DESC', ibid)
        if (ibid .gt. 0) then
            call jelira(nchp19//'.DESC', 'DOCU', cval=docu)
        else
            call jelira(nchp19//'.CELD', 'DOCU', cval=docu)
        endif
!
        if (docu .eq. 'CHML') then
            ndesc = nchp19//'.CELD'
            call jeveuo(ndesc, 'L', jceld)
            nbgrel = zi(jceld + 2-1)
            chelok = .true.
            grel = 0
10          continue
            if ((chelok) .and. (grel .lt. nbgrel)) then
                grel = grel + 1
                mod=zi(jceld-1+zi(jceld-1+4+grel) +2)
                if (mod .ne. 0) then
                    call jeveuo(jexnum('&CATA.TE.MODELOC', mod), 'L', amod)
                    chelok = (zi(amod-1+1).eq. 2)
                endif
                goto 10
            endif
            if (.not. chelok) then
                call utmess('F', 'POSTRELE_47', si=i)
            endif
        endif
!
!        --- VERIFICATION SUR LES CMPS ---
        call jelira(jexnum(xdicmp, i), 'LONMAX', nbcmp)
        call jeveuo(jexnum(xdicmp, i), 'L', anumcp)
        do 110, j = 1, nbcmp, 1
        if (zi(anumcp + j-1) .eq. 0) then
            call jeveuo(jexnum(xdncmp, i), 'L', anomcp)
            nomcmp = zk8(anomcp + j-1)
            call utmess('F', 'POSTRELE_48', sk=nomcmp, si=i)
        endif
110      continue
!
!        --- VERIFICATION DE CONCORDANCE DES MAILLAGES ---
        call dismoi('F', 'NOM_MAILLA', ncheff, 'CHAMP', n1,&
                    nmaich, ierd)
        call getvid('ACTION', 'CHEMIN', iocc=i, nbval=0, nbret=nbcrb)
        nbcrb = -nbcrb
        if (nbcrb .ne. 0) then
!           /* LE LIEU DU POST TRAITEMENT EST UNE COURBE */
            call getvid('ACTION', 'CHEMIN', iocc=i, scal=nomcrb, nbret=n1)
            call jeexin(nomcrb//'.NOMMAIL', n1)
            if (n1 .ne. 0) then
                call jeveuo(nomcrb//'.NOMMAIL', 'L', amaicb)
            else
                call jeveuo(nomcrb//'.NOMA', 'L', amaicb)
            endif
            nmaili = zk8(amaicb)
            if (nmaich .ne. nmaili) then
                valk(1) = nmaili
                valk(2) = nmaich
                call utmess('F', 'POSTRELE_49', nk=2, valk=valk, si=i)
            endif
        else
!           /* LE LIEU DU POST TRAITEMENT EST UN ENSMBLE DE NOEUDS */
!           VERIFICATION D' EXISTENCE DES NOEUDS DANS LE MAILLAGE DU CHP
            call getvtx('ACTION', 'GROUP_NO', iocc=i, nbval=0, nbret=nbgrpn)
            call getvtx('ACTION', 'NOEUD', iocc=i, nbval=0, nbret=nbneud)
            nbgrpn = -nbgrpn
            nbneud = -nbneud
            if (nbgrpn .ne. 0) then
                call jecreo('&&OP0051.NOM.GRPN', 'V V K24')
                call jeecra('&&OP0051.NOM.GRPN', 'LONMAX', nbgrpn)
                call jeveuo('&&OP0051.NOM.GRPN', 'E', agrpnd)
                call getvtx('ACTION', 'GROUP_NO', iocc=i, nbval=nbgrpn, vect=zk24(agrpnd),&
                            nbret=n1)
                do 120, k = 1, nbgrpn, 1
                nomgrn = zk24(agrpnd + k-1)
                call jenonu(jexnom(nmaich//'.GROUPENO', nomgrn), n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'POSTRELE_50', sk=nomgrn, si=i)
                endif
120              continue
                call jedetr('&&OP0051.NOM.GRPN')
            endif
            if (nbneud .ne. 0) then
                call wkvect('&&OP0051.NOM.NEUD', 'V V K8', nbneud, alneud)
                call getvtx('ACTION', 'NOEUD', iocc=i, nbval=nbneud, vect=zk8( alneud),&
                            nbret=n1)
                nrepnd = nmaich//'.NOMNOE'
                do 130, k = 1, nbneud, 1
                nomnd = zk8(alneud + k-1)
                call jenonu(jexnom(nrepnd, nomnd), n1)
                if (n1 .eq. 0) then
                    call utmess('F', 'POSTRELE_51', sk=nomnd, si=i)
                endif
130              continue
                call jedetr('&&OP0051.NOM.NEUD')
            endif
        endif
    endif
    call jedema()
end subroutine
