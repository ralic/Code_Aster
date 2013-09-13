subroutine fonmai(resu, nomail, typfon, iocc, nbnoff)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/cgnoor.h"
#include "asterfort/getvtx.h"
#include "asterfort/i2extf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ornofd.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
    integer :: iocc, nbnoff
    character(len=8) :: resu, nomail, typfon
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!     VERIFICATION DES ENTITES LORSQUE LE FOND EST DECRIT PAR
!     DES MAILLES OU DE GROUPES DE MAILLES
!     RENSEIGNEES DANS DEFI_FOND_FISS
!     CONSTRUCTION DU FOND DE FISSURE A PARTIR CES DONNEES
!
!     ENTREES:
!        RESU   : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!        NOMAIL : NOM DU MAILLAGE
!        TYPFON : TYPE DE FOND
!                 IL PEUT VALOIR OUVERT/FERME/INF/SUP
!        IOCC   : OCCURENCE COURANTE DE MOTFAC
!     SORTIES:
!        NBNOFF : NOMBRE DE NOEUDS EN FOND DE FISSURE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: vecori(3)
!
    integer :: jcour2, jcour5, jtypm, iatyma, idnono, idlino, jtyp
    integer :: i, nbma, n1, im, nig
    integer :: nid, numno, iret, trouv, numma
    character(len=8) :: k8b, nomma, typm, ndorig, ndextr
    character(len=8) :: noeud, valk(2)
    character(len=16) :: k16bid, nomcmd, motfac
    character(len=16) :: motcle(2), typmcl(2)
    character(len=24) :: conec, typp, nommai, nomnoe, noeord
    character(len=24) :: mesnoe, mafour, nogrp
! DEB-------------------------------------------------------------------
    call jemarq()
!
    call getres(k8b, k16bid, nomcmd)
!     ------------------------------------------------------------------
!     INITIALISATION DE VARIABLES
!     ------------------------------------------------------------------
    motfac = 'FOND_FISS'
    typp = nomail//'.TYPMAIL        '
    nommai = nomail//'.NOMMAI         '
    nomnoe = nomail//'.NOMNOE         '
    conec = nomail//'.CONNEX         '
    call jeveuo(typp, 'L', iatyma)
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma)), typm)
!
!     ------------------------------------------------------------------
!     --- RECHERCHE DES NOEUDS SOMMET DES MAILLES RENSEIGNEES
!     ------------------------------------------------------------------
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    mafour='&&FONMAI.MALIGNE'
    call cgnoor(mafour, nomail, motfac, iocc, 2,&
                motcle, typmcl, typfon, nbma, ndorig,&
                ndextr, typm, vecori)
    call jeveuo(mafour, 'L', jcour2)
!
!
!     ------------------------------------------------------------------
!     --- SI FERME : RECUPERATION DE MAILLE_ORIG POUR AVOIR
!     --- LE SENS DE PARCOURS DE LA COURBE FERMEE
!     ------------------------------------------------------------------
!
    if (typfon .eq. 'FERME') then
!
        numma = 0
        call getvtx(motfac, 'MAILLE_ORIG', iocc=1, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            call getvtx(motfac, 'MAILLE_ORIG', iocc=1, scal=nomma, nbret=n1)
            call jenonu(jexnom(nommai, nomma), numma)
        else
            call getvtx(motfac, 'GROUP_MA_ORIG', iocc=1, nbval=0, nbret=n1)
            if (n1 .ne. 0) then
                call getvtx(motfac, 'GROUP_MA_ORIG', iocc=1, scal=nogrp, nbret=n1)
                call utnono(' ', nomail, 'MAILLE', nogrp, nomma,&
                            iret)
                if (iret .eq. 10) then
                    call utmess('F', 'RUPTURE0_41', sk=nogrp)
                else if (iret .eq. 1) then
                    call utmess('F', 'RUPTURE0_45', sk=ndorig)
                endif
                call jenonu(jexnom(nommai, nomma), numma)
            endif
        endif
!
        if (numma .eq. 0) then
            call utmess('F', 'RUPTURE0_42')
        else
            call jenonu(jexnom(nomnoe, ndorig), numno)
            call i2extf(numma, 1, conec(1:15), typp(1:16), nig,&
                        nid)
            if ((numno.ne.nig) .and. (numno.ne.nid)) then
                call utmess('F', 'RUPTURE0_43')
            endif
            trouv = 0
            do 545 im = 1, nbma
                if (numma .eq. zi(jcour2-1 + im)) trouv = im
545          continue
            if (trouv .eq. 0) then
                call utmess('F', 'RUPTURE0_44', sk=nomma)
            else
!
!     ON REMONTE LA MAILLE_ORIG EN TETE DE LISTE
!
                call wkvect('&&FONMAI.MAILLESTRIEES', 'V V I', 3*nbma, jcour5)
                do 546 im = trouv, nbma
                    zi(jcour5-1 + im+1-trouv) = zi(jcour2-1 + im)
546              continue
                do 547 im = 1, trouv-1
                    zi(jcour5-1 + im+1+nbma-trouv) = zi(jcour2-1 + im)
547              continue
                do 548 im = 1, nbma
                    zi(jcour2-1 + im)=zi(jcour5-1 + im)
548              continue
                call jedetr('&&FONMAI.MAILLESTRIEES')
            endif
        endif
    endif
!
!     ------------------------------------------------------------------
!     --- ORDONNANCEMENT DES NOEUDS EN FOND DE FISSURE
!     ------------------------------------------------------------------
    mesnoe = '&&FONMAI.NOEUD'
    call ornofd(mafour, nomail, nbma, mesnoe, ndorig,&
                ndextr, 'V', vecori)
    if (typfon .eq. 'INF') then
        noeord = resu//'.FOND_INF.NOEU'
    else if (typfon.eq.'SUP') then
        noeord = resu//'.FOND_SUP.NOEU'
    else
        noeord = resu//'.FOND.NOEU'
    endif
    call jelira(mesnoe, 'LONMAX', nbnoff)
    call jeveuo(mesnoe, 'L', idnono)
!
    call wkvect(noeord, 'G V K8', nbnoff, idlino)
    do 90 i = 1, nbnoff
        call jenuno(jexnum(nomail//'.NOMNOE', zi(idnono-1 + i)), noeud)
        zk8(idlino-1 + i) = noeud
90  end do
!
!
!
!     ------------------------------------------------------------------
!     --- ON STOCKE LE TYPE DE MAILLES DEFINISSANT LE FOND DE FISSURE
!     ------------------------------------------------------------------
!
    call jeexin(resu//'.FOND.TYPE', iret)
    if (iret .eq. 0) then
        call wkvect(resu//'.FOND.TYPE', 'G V K8', 1, jtypm)
        zk8(jtypm) = typm
    else
        call jeveuo(resu//'.FOND.TYPE', 'L', jtyp)
        if (typm .eq. zk8(jtyp)) then
            valk(1) = typm
            valk(2) = zk8(jtyp)
            call utmess('F', 'RUPTURE0_68', nk=2, valk=valk)
        endif
    endif
!
!     ------------------------------------------------------------------
    call jedetr(mesnoe)
    call jedetr(mafour)
!
    call jedema()
end subroutine
