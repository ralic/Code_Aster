subroutine fonlev(resu, noma, nbnoff)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: resu, noma
    integer :: nbnoff
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
!-----------------------------------------------------------------------
! FONCTION REALISEE:
!
!     VERIFICATION DES LEVRES ET DE LEUR CONNEXITE
!
!     ENTREES:
!        RESU       : NOM DU CONCEPT RESULTAT DE L'OPERATEUR
!        NOMA       : NOM DU MAILLAGE
!        NBNOFF     : NOMBRE DE NOEUDS EN FOND DE FISSURE
!
!     -----------------------------------------------------------------
!
    integer :: jmai1, jadr, jnoe1, jmai2, jmaii, jjj, iatyma
    integer :: jinf, jsup, iamase, ityp, jvale, jfinf, jfsup
    integer :: jufinf, jufsup, juinf2
    integer :: igr, ngr, ino, i, j, k, ibid, k2, j2
    integer :: nbmai, nent, indice
    integer :: nn, compta, nbmas1, nbmas2, nbmal
    integer :: iret, iret1, iret2, jjj2
    real(kind=8) :: d, prec, precr
    character(len=4) :: typma
    character(len=6) :: nompro
    character(len=8) ::  maille, type, noeug, typmcl(2), motcle(2)
    character(len=9) :: typlev(2), motfac, valk(2)
    character(len=24) :: nomobj, grouma, nommai, conec, trav, trav2
    parameter(prec=1.d-1)
!     -----------------------------------------------------------------
!
    call jemarq()
!
    nompro = 'FONLEV'
!
!     ------------------------------------------------------------------
!     INITIALISATION DE VARIABLES
!     ------------------------------------------------------------------
    grouma = noma//'.GROUPEMA'
    nommai = noma//'.NOMMAI'
    conec = noma//'.CONNEX'
    call jeveuo(noma//'.TYPMAIL', 'L', iatyma)
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES DEUX LEVRES
!     CELLES-CI SONT TRAITEES DE LA MEME MANIERE
!     ------------------------------------------------------------------
    typlev(1) = 'LEVRE_SUP'
    typlev(2) = 'LEVRE_INF'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    do 10 indice = 1, 2
        motfac=typlev(indice)
!
!       EVALUATION DU NOMBRE DE MAILLES ET CONSTRUCTION DU VECTEUR DE
!       MAILLES DE TRAVAIL
!
        trav = '&&'//nompro//'.'//motfac
        trav2 = '&&'//nompro//'.'//motfac//'2'
        call reliem(' ', noma, 'NO_MAILLE', motfac, 1,&
                    2, motcle, typmcl, trav, nbmal)
        if (nbmal .eq. 0) goto 9999
        call wkvect(trav2, 'V V K24', nbmal, jjj2)
        call jeveuo(trav, 'L', jjj)
!
!     ------------------------------------------------------------------
!     VERIFICATION DE L'EXISTENCE DES GROUPES DE MAILLES RENSEIGNEES
!     ET CALCUL DU NOMBRE TOTAL DE MAILLES
!     ------------------------------------------------------------------
!
        call getvtx(motfac, 'GROUP_MA', iocc=1, nbval=nbmal, vect=zk24(jjj2),&
                    nbret=ngr)
        call getvtx(motfac, 'MAILLE', iocc=1, nbval=0, nbret=nent)
!
!
! ---   ALLOCATION D'UN PREMIER OBJET DE TRAVAIL
!       DANS LEQUEL SERONT STOCKES LES MAILLES AVANT DE S'ASSURER QU'IL
!       N'Y A PAS DUPPLICATION
!
        call wkvect('&&'//nompro//'.MAIL', 'V V K8', nbmal, jmai1)
        jmaii = jmai1
!       ----------------------------------------------------------------
!      VERIFICATION POUR LES MAILLES DE LA LEVRE COURANTE
!      SI ON A UN SEUL NOEUD ALORS ELLES SONT DE TYPE SEG
!      SI ON A PLUSIEURS NOEUDS ALORS ELLES SONT DE TYPE QUAD OU TRIA
!      ET CALCUL DU NOMBRE TOTAL DE MAILLES DE LA LEVRE COURANTE
!      ----------------------------------------------------------------
!       SI GROUP_MA
        if (nent .eq. 0) then
            do 110 igr = 1, ngr
!
                call jelira(jexnom(grouma, zk24(jjj2-1 + igr)), 'LONMAX', nbmai)
                call jeveuo(jexnom(grouma, zk24(jjj2-1 + igr)), 'L', jadr)
!
!
                do 105 i = 1, nbmai
                    call jenuno(jexnum(nommai, zi(jadr-1 + i)), maille)
                    call jenonu(jexnom(nommai, maille), ibid)
                    ityp=iatyma-1+ibid
                    call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
                    typma = type(1:4)
                    if (((typma.ne.'QUAD').and.(typma.ne.'TRIA')) .and. (nbnoff.gt.1)) then
                        valk(1) = type
                        valk(2) = motfac
                        call utmess('F', 'RUPTURE0_65', nk=2, valk=valk)
                        elseif ((typma(1:3).ne.'SEG').and.(nbnoff.eq.1))&
                    then
                        valk(1) = type
                        valk(2) = motfac
                        call utmess('F', 'RUPTURE0_75', nk=2, valk=valk)
                    else
                        zk8(jmai1) = maille
                        jmai1 = jmai1 + 1
                    endif
!
105             continue
!
110         continue
        else
! SI MAILLE
            do 230 ino = 1, nbmal
                call jenonu(jexnom(nommai, zk8(jjj-1 + ino)), ibid)
                ityp=iatyma-1+ibid
                call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
                typma = type(1:4)
                if (((typma.ne.'QUAD').and.(typma.ne.'TRIA')) .and. ( nbnoff.gt.1)) then
                    valk(1) = type
                    valk(2) = motfac
                    call utmess('F', 'RUPTURE0_65', nk=2, valk=valk)
                else if ((typma(1:3).ne.'SEG').and.(nbnoff.eq.1)) then
                    valk(1) = type
                    valk(2) = motfac
                    call utmess('F', 'RUPTURE0_75', nk=2, valk=valk)
                else
                    zk8(jmai1) = zk8(jjj-1 + ino)
                    jmai1 = jmai1 + 1
                endif
230         continue
!
        endif
!
! --- VERIFICATION QU'IL N Y A PAS DUPLICATION DES ENTITES ET STOCKAGE
!     ON MET 'O' SI L'ENTITE EST DUPPLIQUEE
!
!       ALLOCATION DU VECTEUR .LEVRESUP.MAIL ET .LEVREINF.MAIL
        nomobj = resu//'.LEVRE'//motfac(7:9)//'.MAIL'
        call wkvect(nomobj, 'G V K8', nbmal, jmai2)
        k2 = 1
        do 600 i = 1, nbmal-1
            if (zk8(jmaii-1 + i) .ne. '0') then
                zk8(jmai2-1 + k2) = zk8(jmaii-1 + i)
                k2 = k2 + 1
                do 605 j = i+1, nbmal
                    if (zk8(jmaii-1 + i) .eq. zk8(jmaii-1 + j)) then
                        zk8(jmaii-1 + j) = '0'
                        j2 = i
                    endif
605             continue
            endif
600     continue
        if (zk8(jmaii-1 + nbmal) .ne. '0') then
            zk8(jmai2-1 + k2) = zk8(jmaii-1 + nbmal)
            k2 = k2 + 1
        endif
        k2 = k2 - 1
!
        if (k2 .ne. nbmal) then
            valk(1) = motfac
            valk(2) = zk8(jmaii-1 + j2)
            call utmess('E', 'RUPTURE0_70', nk=2, valk=valk)
        endif
!
! --- VERIFICATION COHERENCE LEVRE SUP / FOND
!
        call jeexin(resu//'.FOND.NOEU', iret)
        if (iret .ne. 0) then
            call jelira(resu//'.FOND.NOEU', 'LONUTI', nbnoff)
            call jeveuo(resu//'.FOND.NOEU', 'L', jnoe1)
        else
            if (motfac .eq. 'LEVRE_SUP') then
                call jelira(resu//'.FOND_SUP.NOEU', 'LONUTI', nbnoff)
                call jeveuo(resu//'.FOND_SUP.NOEU', 'L', jnoe1)
            else if (motfac .eq. 'LEVRE_INF') then
                call jelira(resu//'.FOND_INF.NOEU', 'LONUTI', nbnoff)
                call jeveuo(resu//'.FOND_INF.NOEU', 'L', jnoe1)
            endif
        endif
        if (nbnoff .gt. 1) then
            do 610 i = 1, nbnoff
                compta = 0
                do 620 j = 1, nbmal
                    call jenuno(jexnum(nommai, zi(jadr-1 + j)), maille)
                    call jenonu(jexnom(nommai, maille), ibid)
                    ityp=iatyma-1+ibid
                    call jenuno(jexnum('&CATA.TM.NOMTM', zi(ityp)), type)
                    call dismoi('NBNO_TYPMAIL', type, 'TYPE_MAILLE', repi=nn)
                    if ((type(1:5).ne.'QUAD8') .and. (type(1:5) .ne.'TRIA3') .and.&
                        (type(1:5).ne.'QUAD4') .and. ( type(1:5).ne.'TRIA6')) then
                        valk(1) = type(1:5)
                        valk(2) = motfac
                        call utmess('F', 'RUPTURE0_65', nk=2, valk=valk)
                    endif
                    call jeveuo(jexnum(conec, ibid), 'L', iamase)
                    call jenuno(jexnum(noma//'.NOMNOE', zi(iamase)), noeug)
                    do 630 k = 1, nn
                        call jenuno(jexnum(noma//'.NOMNOE', zi( iamase-1 + k)), noeug)
                        if (noeug .eq. zk8(jnoe1-1 + i)) then
                            compta = compta + 1
                            goto 610
                        endif
630                 continue
620             continue
                if (compta .eq. 0) then
                    valk(1) = zk8(jnoe1-1 + i)
                    valk(2) = motfac
                    call utmess('F', 'RUPTURE0_72', nk=2, valk=valk)
                endif
610         continue
        endif
!
!
! --- DESTRUCTION DES OBJETS DE TRAVAIL
!
        call jedetr(trav)
        call jedetr(trav2)
!
        call jedetr('&&'//nompro//'.MAIL')
!
 10 end do
! ----------------------------------------------------------
!    COMPARAISON LEVRE SUP / LEVRE INF AFIN DE S'ASSURER
!    QU'ELLES N'ONT PAS DE MAILLES EN COMMUN
! ----------------------------------------------------------
    call jeexin(resu//'.LEVRESUP.MAIL', iret1)
    call jeexin(resu//'.LEVREINF.MAIL', iret2)
    if ((iret1.ne.0) .and. (iret2.ne.0)) then
        call jeveuo(resu//'.LEVRESUP.MAIL', 'L', jsup)
        call jeveuo(resu//'.LEVREINF.MAIL', 'L', jinf)
        call jelira(resu//'.LEVRESUP.MAIL', 'LONMAX', nbmas1)
        call jelira(resu//'.LEVRESUP.MAIL', 'LONMAX', nbmas2)
        do 710 i = 1, nbmas1
            do 715 j = 1, nbmas2
                if (zk8(jsup-1 + i) .eq. zk8(jinf-1 + j)) then
                    call utmess('F', 'RUPTURE0_73', sk=zk8(jsup-1 + i))
                endif
715         continue
710     continue
    endif
9999 continue
!
!     LORSQUE LE FOND DE FISSURE EST DEFINI PAR FOND_INF ET FOND_SUP,
!     ON VERIFIE QUE LES NOEUDS SONT EN VIV A VIS
    call jeveuo(noma//'.COORDO    .VALE', 'L', jvale)
    call jeexin(resu//'.FOND_INF.NOEU', iret1)
    call jeexin(resu//'.FOND_SUP.NOEU', iret2)
    if (iret1 .ne. 0 .and. iret2 .ne. 0) then
        call jeveuo(resu//'.FOND_INF.NOEU', 'L', jfinf)
        call jeveuo(resu//'.FOND_SUP.NOEU', 'L', jfsup)
        call jenonu(jexnom(noma//'.NOMNOE', zk8(jfinf)), jufinf)
        call jenonu(jexnom(noma//'.NOMNOE', zk8(jfinf+1)), juinf2)
        d = abs(zr(jvale+3*(jufinf-1))- zr(jvale+3*(juinf2-1)))
        d = d+abs(zr(jvale+3*(jufinf-1)+1)- zr(jvale+3*(juinf2-1)+1))
        d = d+abs(zr(jvale+3*(jufinf-1)+2)- zr(jvale+3*(juinf2-1)+2))
        precr = prec*d
        do 555 ino = 1, nbnoff
            call jenonu(jexnom(noma//'.NOMNOE', zk8(jfinf-1+ino)), jufinf)
            call jenonu(jexnom(noma//'.NOMNOE', zk8(jfsup-1+ino)), jufsup)
            d = abs(zr(jvale+3*(jufinf-1))- zr(jvale+3*(jufsup-1)))
            d = d+abs(zr(jvale+3*(jufinf-1)+1)- zr(jvale+3*(jufsup-1)+ 1))
            d = d+abs(zr(jvale+3*(jufinf-1)+2)- zr(jvale+3*(jufsup-1)+ 2))
            if (sqrt(d) .gt. precr) then
                valk(1) = zk8(jfinf+ino-1)
                valk(2) = zk8(jfsup+ino-1)
                call utmess('F', 'RUPTURE0_69', nk=2, valk=valk)
            endif
555     continue
    endif
!
    call jedema()
!
end subroutine
