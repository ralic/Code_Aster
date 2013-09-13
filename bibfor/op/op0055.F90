subroutine op0055()
    implicit none
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!      OPERATEUR :     DEFI_FOND_FISS
!
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/fonbas.h"
#include "asterfort/fonfis.h"
#include "asterfort/fonimp.h"
#include "asterfort/foninf.h"
#include "asterfort/fonlev.h"
#include "asterfort/fonmai.h"
#include "asterfort/fonnoe.h"
#include "asterfort/fonnof.h"
#include "asterfort/fonvec.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: iadr1, ifm, niv
    integer :: nbocc, nbnoff
    integer :: ibas, ibid, iocc, idon, idonn, ifonoe, ndonn
    integer :: iret1, iret2, iret, irets
    integer :: n1, n2
    character(len=6) :: nompro
    character(len=8) :: resu, noma, typfon, confin
    character(len=9) :: entit(8)
    character(len=13) :: motcl(8)
    character(len=16) :: typres, oper
    character(len=19) :: basfon, basloc, cnxinv, fontyp, lnno, ltno
    character(len=24) :: valk(2), entnom, fondfi, fonoeu
! DEB-------------------------------------------------------------------
!
    call jemarq()
    nompro = 'OP0055'
!
    call infniv(ifm, niv)
!
! ---  RECUPERATION DES ARGUMENTS DE LA COMMANDE
!
    call getres(resu, typres, oper)
!
! ---  RECUPERATIONS RELATIVES AU MAILLAGE
!      -----------------------------------
!
    call getvid(' ', 'MAILLAGE', scal=noma, nbret=nbocc)
!
! ---  RECUPERATION DE LA CONNECTIVITE INVERSE
!
    cnxinv='&&'//nompro//'.CNXINV'
    call cncinv(noma, ibid, 0, 'V', cnxinv)
!
!
!     ---------------------------------------------------------------
!     RECUPERATION DU TYPE DE FOND
!     OUVERT OU FERME OU INF/SUP
!     ---------------------------------------------------------------
!
    call getfac('FOND_FISS', nbocc)
    do 1 iocc = 1, nbocc
!
        call getvtx('FOND_FISS', 'TYPE_FOND', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx('FOND_FISS', 'TYPE_FOND', iocc=iocc, scal=typfon, nbret=n1)
        else
            typfon = 'OUVERT'
        endif
!
!
!     ---------------------------------------------------------------
!     VERIFICATION DE L'EXISTANCE DES ENTITES DU MAILLAGE RENSEIGNEES
!     ET CONSTRUCTION DE VECTEURS DE TRAVAIL POUR CHACUNE D'ELLES
!     ---------------------------------------------------------------
!
        entit(1) = '.NOMNOE'
        entit(2) = '.NOMMAI'
        entit(3) = '.GROUPENO'
        entit(4) = '.GROUPEMA'
        entit(5) = '.NOMNOE'
        entit(6) = '.GROUPENO'
        motcl(1) = 'NOEUD'
        motcl(2) = 'MAILLE'
        motcl(3) = 'GROUP_NO'
        motcl(4) = 'GROUP_MA'
        motcl(5) = 'NOEUD_ORIG'
        motcl(6) = 'GROUP_NO_ORIG'
        if (typfon .eq. 'OUVERT') then
            entit(7) = '.NOMNOE'
            entit(8) = '.GROUPENO'
            motcl(7) = 'NOEUD_EXTR'
            motcl(8) = 'GROUP_NO_EXTR'
            ndonn = 8
        else if (typfon.eq.'FERME') then
            entit(7) = '.NOMMAI'
            entit(8) = '.GROUPEMA'
            motcl(7) = 'MAILLE_ORIG'
            motcl(8) = 'GROUP_MA_ORIG'
            ndonn = 8
        else
            ndonn = 6
        endif
        do 11 idonn = 1, ndonn
            call getvtx('FOND_FISS', motcl(idonn), iocc=iocc, nbval=0, nbret=n1)
            n1 = -n1
            if (n1 .gt. 0) then
                call wkvect('&&'//nompro//'.'//motcl(idonn), 'V V K24', n1, iadr1)
                call getvtx('FOND_FISS', motcl(idonn), iocc=iocc, nbval=n1, vect=zk24(iadr1),&
                            nbret=n2)
                do 111 idon = 1, n1
                    entnom = zk24(iadr1-1 + idon)
                    call jenonu(jexnom(noma//entit(idonn), entnom), ibid)
                    if (ibid .eq. 0) then
                        valk(1) = entnom
                        valk(2) = motcl(idonn)
                        call utmess('F', 'RUPTURE0_7', nk=2, valk=valk)
                    endif
111              continue
            endif
11      continue
!
!
!
!       ---------------------------------------------------------------
!       CONSTRUCTION DE FOND DE FISSURE
!       ---------------------------------------------------------------
!
!        SI LE MOT CLE FACTEUR EST NOEUD OU GROUP_NO
!        ----------------------------------------
!
        call jeexin('&&'//nompro//'.NOEUD', iret1)
        call jeexin('&&'//nompro//'.GROUP_NO', iret2)
        if ((iret1.ne.0) .or. (iret2.ne.0)) then
            call fonnoe(resu, noma, cnxinv, nompro, typfon,&
                        nbnoff)
        endif
!
!        SI LE MOT CLE FACTEUR EST MAILLE OU GROUP_MA
!        ----------------------------------------
!
        call jeexin('&&'//nompro//'.MAILLE', iret1)
        call jeexin('&&'//nompro//'.GROUP_MA', iret2)
        if ((iret1.ne.0) .or. (iret2.ne.0)) then
            call fonmai(resu, noma, typfon, iocc, nbnoff)
        endif
!C
!
!       DESTRUCTION DES VECTEURS DE TRAVAIL
!       ----------------------------------------
        do 20 idonn = 1, ndonn
            call jeexin('&&'//nompro//'.'//motcl(idonn), iret)
            if (iret .ne. 0) call jedetr('&&'//nompro//'.'//motcl(idonn))
20      continue
!
 1  end do
!
!
!     ---------------------------------------------------------------
!     VERIFICATION DES DONNEES SUR LES LEVRES ET LES VECTEURS
!     ---------------------------------------------------------------
!
!
!     TRAITEMENT DES LEVRES: LEVRE_SUP ET LEVRE_INF
!     ----------------------------------------
!
    call fonlev(resu, noma, nbnoff)
!
!
!     TRAITEMENT DE LA NORMALE ET DES
!     MOTS CLES FACTEUR : DTAN_EXTR, DTAN_ORIG
!                         VECT_GRNO_ORIG, VECT_GRNO_EXTR
!     ----------------------------------------
!
    call fonvec(resu, noma, cnxinv)
!
    call jedetr(cnxinv)
!
!     ---------------------------------------------------------------
!     CREATION DU VECTEUR .FONDFISS CONTENANT LES COORDONNEES ET LES
!     ABSCISSES CURVILIGNES DES NOEUDS DU FOND
!     ---------------------------------------------------------------
!
!     VECTEUR CONTENANT LES NOMS DES NOEUDS DU FOND DE FISSURE
!     ----------------------------------------
    call jeexin(resu//'.FOND.NOEU', iret)
    if (iret .ne. 0) then
        fonoeu = resu//'.FOND.NOEU'
        call jeveuo(fonoeu, 'L', ifonoe)
        if (typfon .eq. 'FERME') then
            ASSERT(zk8(ifonoe+1-1).eq.zk8(ifonoe+nbnoff-1))
        endif
    else
        fonoeu = resu//'.FOND_SUP.NOEU'
    endif
!
    fondfi = resu//'.FONDFISS'
    call fonfis(noma, nbnoff, fonoeu, fondfi)
!
!     ---------------------------------------------------------------
!     CREATION DE LA BASE LOCALE ET DES LEVEL SETS EN CHAQUE NOEUD
!     ---------------------------------------------------------------
!
!     LA BASE LOCALE ET DES LEVEL SETS SONT CALCULEES EN CHAQUE NOEUD
!     QUE SI L'OBJET .BASEFOND EXISTE DEJA
    call jeexin(resu//'.BASEFOND', ibas)
    if (ibas .ne. 0) then
        basfon = resu//'.BASEFOND'
        if (nbnoff .ne. 1) then
            fontyp = resu//'.FOND.TYPE'
        endif
        basloc = resu//'.BASLOC'
        lnno = resu//'.LNNO'
        ltno = resu//'.LTNO'
        call fonbas(noma, basfon, fontyp, fondfi, nbnoff,&
                    basloc, lnno, ltno)
    endif
!
!
!     ---------------------------------------------------------------
!     EXTRACTION DES NOEUDS DES LEVRES SUR DIRECTON NORMALE
!     ---------------------------------------------------------------
!
    call getvtx(' ', 'CONFIG_INIT', scal=confin, nbret=ibid)
    if (confin .eq. 'COLLEE') then
        call jeexin(resu//'.LEVRESUP.MAIL', irets)
        if (irets .ne. 0) then
            call fonnof(resu, noma, typfon, nbnoff)
        endif
    endif
!
!     ---------------------------------------------------------------
!     STOCKAGE D'INFOS UTILES DANS LA SD EN SORTIE
!     ---------------------------------------------------------------
!
    call foninf(resu, typfon)
!
!     ---------------------------------------------------------------
!     IMPRESSIONS SI INFO=2
!     ---------------------------------------------------------------
!
    if (niv .eq. 2) then
        call fonimp(resu)
    endif
!
    call jedema()
end subroutine
