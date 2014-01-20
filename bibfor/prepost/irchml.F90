subroutine irchml(chamel, partie, ifi, form, titre,&
                  loc, nomsd, nomsym, numord, lcor,&
                  nbnot, numnoe, nbmat, nummai, nbcmp,&
                  nomcmp, lsup, borsup, linf, borinf,&
                  lmax, lmin, lresu, formr, ncmp,&
                  nucmp, nive)
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/celcel.h"
#include "asterfort/celces.h"
#include "asterfort/celver.h"
#include "asterfort/cesimp.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvis.h"
#include "asterfort/i2trgi.h"
#include "asterfort/imprsd.h"
#include "asterfort/iradhs.h"
#include "asterfort/irccmp.h"
#include "asterfort/irceca.h"
#include "asterfort/ircecl.h"
#include "asterfort/ircecs.h"
#include "asterfort/ircerl.h"
#include "asterfort/ircers.h"
#include "asterfort/irsspt.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxcaps.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: chamel, nomcmp(*), form, titre, loc, nomsd, nomsym
    character(len=*) :: formr, partie
    real(kind=8) :: borsup, borinf
    integer :: nbnot, numnoe(*), nbmat, nummai(*), nbcmp, ifi, numord, ncmp
    integer :: nucmp(*), nive
    logical :: lcor, lsup, linf, lmax, lmin, lresu
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
!        IMPRESSION D'UN CHAM_ELEM A COMPOSANTES REELLES OU COMPLEXES
!         AU FORMAT IDEAS, RESULTAT, CASTEM
!  ENTREES:
!     CHAMEL : NOM DU CHAM_ELEM A ECRIRE
!     PARTIE : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!     IFI    : NUMERO LOGIQUE DU FICHIER DE SORTIE
!     FORM   : FORMAT DES SORTIES: IDEAS, RESULTAT
!     TITRE  : TITRE POUR IDEAS
!     LOC    : LOCALISATION DES VALEURS ( ELNO OU ELGA)
!     NOMSD  : NOM DU RESULTAT
!     NOMSYM : NOM SYMBOLIQUE
!     NUMORD : NUMERO D'ORDRE DU CHAMP DANS LE RESULTAT_COMPOSE.
!     LCOR   : =.TRUE.  IMPRESSION DES COORDONNES DE NOEUDS DEMANDEE
!     NBNOT  : NOMBRE DE NOEUDS A IMPRIMER
!     NUMNOE : NUMEROS DES NOEUDS A IMPRIMER
!     NBMAT  : NOMBRE DE MAILLES A IMPRIMER
!     NUMMAI : NUMEROS DES MAILLES A IMPRIMER
!     NBCMP  : NOMBRE DE COMPOSANTES A IMPRIMER
!     NOMCMP : NOMS DES COMPOSANTES A IMPRIMER
!     LSUP   : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE SUPERIEURE
!     BORSUP : VALEUR DE LA BORNE SUPERIEURE
!     LINF   : =.TRUE.  INDIQUE PRESENCE D'UNE BORNE INFERIEURE
!     BORINF : VALEUR DE LA BORNE INFERIEURE
!     LMAX   : =.TRUE.  INDIQUE IMPRESSION VALEUR MAXIMALE
!     LMIN   : =.TRUE.  INDIQUE IMPRESSION VALEUR MINIMALE
!     LRESU  : =.TRUE.  INDIQUE IMPRESSION D'UN CONCEPT RESULTAT
!     FORMR  : FORMAT D'ECRITURE DES REELS SUR "RESULTAT"
!     NIVE   : NIVEAU IMPRESSION CASTEM 3 OU 10
! ----------------------------------------------------------------------
!
    character(len=1) :: type
    integer :: gd, nuti, jcelv, iprem
    integer :: vali(2), versio
    character(len=8) :: nomma, nomgd, nomel, nomno
    character(len=16) :: nomsy2
    character(len=19) :: chame, chames
    character(len=24) :: nolili, nconec, ncncin, valk(2)
    character(len=80) :: titmai
    logical :: lmasu
    integer :: i, iacelk, iad, iadr, ianoma, iel
    integer :: im, imod, in, ino, iret, itype
    integer :: jceld, jcncin, jcnx, jcoor, jdrvlc, jligr, jliste
    integer :: jlongr, jmode,  jncmp, jnmn,  jperm
    integer :: jpnt, jtitr, jtypm,  kk, libre, lon1
    integer :: maxnod, n, n2, nbcmpt, nbel, nbgrel, nbm
    integer :: nbmac, nbmodl, nbn, nbno, nbtitr, nbtma, ncmpmx
    integer :: ndim, ngr
    integer, pointer :: nbnoma(:) => null()
    character(len=8), pointer :: nommai(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    data iprem /0/
!
    call jemarq()
    jcoor=1
    iprem=iprem+1
    chame = chamel(1:19)
    nomsy2 = nomsym
    nbcmpt=0
    call jelira(chame//'.CELV', 'TYPE', cval=type)
    if (type(1:1) .eq. 'R') then
        itype = 1
    else if (type(1:1).eq.'C') then
        itype = 2
    else if (type(1:1).eq.'I') then
        itype = 3
    else if (type(1:1).eq.'K') then
        itype = 4
    else
        call utmess('A', 'PREPOST_97', sk=type(1:1))
        goto 999
    endif
!
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
!        SINON ON LE REMET SOUS SON ANCIENNE FORME :
!     ----------------------------------------------------------
    call celver(chame, 'NBVARI_CST', 'COOL', kk)
    if (kk .eq. 1) then
        if (iprem .eq. 1) then
            call utmess('I', 'PREPOST_2')
        endif
        call celcel('NBVARI_CST', chame, 'V', '&&IRCHML.CHAMEL1')
        chame= '&&IRCHML.CHAMEL1'
    endif
!
!     LES CHAMPS A SOUS-POINTS SONT TRAITES DIFFEREMMENT:
    call celver(chame, 'NBSPT_1', 'COOL', kk)
    if (kk .eq. 1) then
        if (form .eq. 'RESULTAT') then
            chames='&&IRCHML_CES'
            call celces(chame, 'V', chames)
!         SI VALE_MAX/VALE_MIN EST PRESENTE, ON IMPRIME LES MIN/MAX
            if (lmax .or. lmin) then
                call irsspt(chames, ifi, nbmat, nummai, nbcmp,&
                            nomcmp, lsup, linf, lmax, lmin,&
                            borinf, borsup)
            else
!           SINON ON IMPRIME LE CHAMP TEL QUEL
                call utmess('I', 'PREPOST_98', sk=nomsy2)
                call cesimp('&&IRCHML_CES', ifi, nbmat, nummai)
            endif
            call detrsd('CHAM_ELEM_S', chames)
            goto 999
        else
            call utmess('I', 'PREPOST_99', sk=nomsy2)
        endif
        call celcel('PAS_DE_SP', chame, 'V', '&&IRCHML.CHAMEL2')
        chame= '&&IRCHML.CHAMEL2'
    endif
!
    call jeveuo(chame//'.CELD', 'L', jceld)
    gd = zi(jceld-1+1)
    ngr = zi(jceld-1+2)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call wkvect('&&IRCHML.NUM_CMP', 'V V I', ncmpmx, jncmp)
    if (nbcmp .ne. 0 .and. nomgd .ne. 'VARI_R') then
        call irccmp(' ', nomgd, ncmpmx, zk8(iad), nbcmp,&
                    nomcmp, nbcmpt, jncmp)
    endif
    call jeveuo(chame//'.CELK', 'L', iacelk)
    nolili = zk24(iacelk)
    call jeveuo(nolili(1:19)//'.LGRF', 'L', ianoma)
    nomma = zk8(ianoma)
!     RECHERCHE DU NOMBRE D'ELEMENTS : NBEL
    call jelira(nomma//'.NOMMAI', 'NOMMAX', nbel)
    call dismoi('NB_NO_MAILLA', nomma, 'MAILLAGE', repi=nbno)
    AS_ALLOCATE(vk8=nommai, size=nbel)
    AS_ALLOCATE(vi=nbnoma, size=nbel)
    do iel = 1, nbel
        call jenuno(jexnum(nomma//'.NOMMAI', iel), nomel)
        nommai(iel) = nomel
        call jelira(jexnum(nomma//'.CONNEX', iel), 'LONMAX', nbn)
        nbnoma(iel) = nbn
    end do
    call jeveuo(chame//'.CELV', 'L', jcelv)
    call jeveuo(nolili(1:19)//'.LIEL', 'L', jligr)
    call jelira(nolili(1:19)//'.LIEL', 'NUTIOC', nbgrel)
    call jeveuo(jexatr(nolili(1:19)//'.LIEL', 'LONCUM'), 'L', jlongr)
    if (ngr .ne. nbgrel) then
        vali(1) = ngr
        vali(2) = nbgrel
        valk(1) = chame
        valk(2) = nolili
        call utmess('F', 'CALCULEL_19', nk=2, valk=valk, ni=2,&
                    vali=vali)
    endif
! ---------------------------------------------------------------------
!                    F O R M A T   R E S U L T A T
! ---------------------------------------------------------------------
    if (form .eq. 'RESULTAT') then
!
        if (nbmat .eq. 0 .and. nbnot .ne. 0) then
            nconec = nomma//'.CONNEX'
            call jelira(nconec, 'NMAXOC', nbtma)
            call wkvect('&&IRCHML.MAILLE', 'V V I', nbtma, jliste)
            ncncin = '&&IRCHML.CONNECINVERSE  '
            call jeexin(ncncin, n2)
            if (n2 .eq. 0) call cncinv(nomma, [0], 0, 'V', ncncin)
            libre = 1
            call jeveuo(jexatr(ncncin, 'LONCUM'), 'L', jdrvlc)
            call jeveuo(jexnum(ncncin, 1), 'L', jcncin)
            do in = 1, nbnot, 1
                n = numnoe(in)
                nbm = zi(jdrvlc + n+1-1) - zi(jdrvlc + n-1)
                iadr = zi(jdrvlc + n-1)
                call i2trgi(zi(jliste), zi(jcncin+iadr-1), nbm, libre)
            end do
            nbmac = libre - 1
        else
            nbmac = nbmat
            jliste = 1
            if (nbmat .ne. 0) then
                call wkvect('&&IRCHML.MAILLE', 'V V I', nbmac, jliste)
                do im = 1, nbmac
                    zi(jliste+im-1) = nummai(im)
                end do
            endif
        endif
!
        jcnx = 1
        if (loc .eq. 'ELNO') then
            call wkvect('&&IRCHML.NOMNOE', 'V V K8', nbno, jnmn)
            do ino = 1, nbno
                call jenuno(jexnum(nomma//'.NOMNOE', ino), nomno)
                zk8(jnmn-1+ino) = nomno
            end do
            call jeveuo(nomma//'.CONNEX', 'L', jcnx)
            call jeveuo(jexatr(nomma//'.CONNEX', 'LONCUM'), 'L', jpnt)
! --
! --  RECHERCHE DES COORDONNEES ET DE LA DIMENSION
!
            if (lcor) then
                call dismoi('DIM_GEOM_B', nomma, 'MAILLAGE', repi=ndim)
                call jeveuo(nomma//'.COORDO    .VALE', 'L', jcoor)
            endif
        else
            jnmn=1
            jpnt=1
        endif
!
        if (itype .eq. 1) then
            call ircerl(ifi, nbel, zi(jligr), nbgrel, zi(jlongr),&
                        ncmpmx, zr(jcelv), zk8(iad), nommai, loc,&
                        zi(jceld), zi(jcnx), zi( jpnt), zk8(jnmn), nbcmpt,&
                        zi(jncmp), nbnot, numnoe, nbmac, zi( jliste),&
                        lsup, borsup, linf, borinf, lmax,&
                        lmin, lcor, ndim, zr( jcoor), nolili(1:19),&
                        formr, ncmp, nucmp)
        else if (itype.eq.2) then
            call ircecl(ifi, nbel, zi(jligr), nbgrel, zi(jlongr),&
                        ncmpmx, zc(jcelv), zk8(iad), nommai, loc,&
                        zi(jceld), zi(jcnx), zi( jpnt), zk8(jnmn), nbcmpt,&
                        zi(jncmp), nbnot, numnoe, nbmac, zi( jliste),&
                        lsup, borsup, linf, borinf, lmax,&
                        lmin, lcor, ndim, zr( jcoor), nolili(1:19),&
                        formr, ncmp, nucmp)
        else if ((itype.eq.3).or.(itype.eq.4)) then
            call imprsd('CHAMP', chamel, ifi, nomsd)
        endif
!
        if (loc .eq. 'ELNO') call jedetr('&&IRCHML.NOMNOE')
        call jedetr('&&IRCHML.MAILLE')
! ---------------------------------------------------------------------
!                    F O R M A T   I D E A S
! ---------------------------------------------------------------------
    else if (form(1:5).eq.'IDEAS') then
        lmasu = .false.
        call jeexin(nomma//'           .TITR', iret)
        if (iret .ne. 0) then
            call jeveuo(nomma//'           .TITR', 'L', jtitr)
            call jelira(nomma//'           .TITR', 'LONMAX', nbtitr)
            if (nbtitr .ge. 1) then
                titmai=zk80(jtitr-1+1)
                if (titmai(10:31) .eq. 'AUTEUR=INTERFACE_IDEAS') lmasu= .true.
            endif
        endif
        call jeveuo(nomma//'.TYPMAIL', 'L', jtypm)
        call getvis(' ', 'VERSION', scal=versio, nbret=iret)
        call jeexin('&IRCHML.PERMUTA', iret)
        if (iret .eq. 0) call iradhs(versio)
        call jeveuo('&&IRADHS.PERMUTA', 'L', jperm)
        call jelira('&&IRADHS.PERMUTA', 'LONMAX', lon1)
        maxnod=zi(jperm-1+lon1)
        if (itype .eq. 1) then
            call ircers(ifi, zi(jligr), nbgrel, zi(jlongr), ncmpmx,&
                        zr(jcelv), nomgd, zk8(iad), titre, nommai,&
                        loc, zi(jceld), nbnoma, zi(jperm), maxnod,&
                        zi(jtypm), nomsd, nomsym, numord, nbmat,&
                        nummai, lmasu, ncmp, nucmp, nbcmp,&
                        zi(jncmp), nomcmp)
        else if (itype.eq.2) then
            call ircecs(ifi, zi(jligr), nbgrel, zi(jlongr), ncmpmx,&
                        zc(jcelv), zk8(iad), titre, nommai, loc,&
                        zi(jceld), nbnoma, zi(jperm), maxnod, zi(jtypm),&
                        nomsd, nomsym, numord, nbmat, nummai,&
                        lmasu, ncmp, nucmp)
        endif
        call jedetr('&&IRADHS.PERMUTA')
        call jedetr('&&IRADHS.CODEGRA')
        call jedetr('&&IRADHS.CODEPHY')
        call jedetr('&&IRADHS.CODEPHD')
! ---------------------------------------------------------------------
!                    F O R M A T   C A S T E M
! ---------------------------------------------------------------------
    else if (form.eq.'CASTEM') then
!
! ------ AU FORMAT CASTEM, PAS DE MINUSCULES
!        LE NOM DU CHAM_GD EST DANS LA VARIABLE NOMSYM
        if (.not. lresu) call lxcaps(nomsy2)
!
        call jeveuo('&&OP0039.NOM_MODELE', 'L', jmode)
        call jelira('&&OP0039.NOM_MODELE', 'LONUTI', nbmodl)
        do imod = 1, nbmodl
            if (nolili .eq. zk24(jmode-1+imod)) goto 202
        end do
        call utmess('A', 'PREPOST2_2', sk=chame)
        goto 204
202     continue
        if (itype .eq. 1) then
            call jeveuo(nomma//'.TYPMAIL', 'L', jtypm)
            if (loc .eq. 'ELNO') then
                call irceca(ifi, zi(jligr), nbgrel, zi(jlongr), ncmpmx,&
                            zr(jcelv), nomgd, zk8(iad), zi(jceld), nbnoma,&
                            zi( jtypm), nomsy2, nbmat, lresu, nbcmp,&
                            nomcmp, imod, ncmp, nucmp, nive)
            else if (loc.eq.'ELGA') then
                call utmess('A', 'PREPOST2_3')
            endif
        else if (itype.eq.2) then
            call jeveuo(nomma//'.TYPMAIL', 'L', jtypm)
            if (loc .eq. 'ELNO') then
                call jelira(chame//'.CELV', 'LONUTI', nuti)
                AS_ALLOCATE(vr=vale, size=nuti)
                if (partie .eq. 'REEL') then
                    do i = 1, nuti
                        vale(i)=dble(zc(jcelv-1+i))
                    end do
                else if (partie.eq.'IMAG') then
                    do i = 1, nuti
                        vale(i)=dimag(zc(jcelv-1+i))
                    end do
                else
                    call utmess('F', 'PREPOST2_4')
                endif
                call irceca(ifi, zi(jligr), nbgrel, zi(jlongr), ncmpmx,&
                            zr(jcelv), nomgd, zk8(iad), zi(jceld), nbnoma,&
                            zi( jtypm), nomsy2, nbmat, lresu, nbcmp,&
                            nomcmp, imod, ncmp, nucmp, nive)
            else if (loc.eq.'ELGA') then
                call utmess('A', 'PREPOST2_3')
            endif
        endif
    endif
204 continue
    call jedetr('&&IRCHML.NUM_CMP')
    AS_DEALLOCATE(vk8=nommai)
    AS_DEALLOCATE(vi=nbnoma)
    call jedetr('&&IRCHML.MAILLE')
    call jedetr('&&IRCHML.NOMNOE')
    AS_DEALLOCATE(vr=vale)
    call detrsd('CHAM_ELEM', '&&IRCHML.CHAMEL1')
    call detrsd('CHAM_ELEM', '&&IRCHML.CHAMEL2')
999 continue
    call jedema()
end subroutine
