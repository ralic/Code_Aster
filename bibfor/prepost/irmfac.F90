subroutine irmfac(ioccur, formaf, ifichi, niveau, versio,&
                  modele, nomail, nomare, resure, lgmsh)
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/irchor.h"
#include "asterfort/irecri.h"
#include "asterfort/iremed.h"
#include "asterfort/irmail.h"
#include "asterfort/irtitr.h"
#include "asterfort/irtopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: ioccur, ifichi, versio, niveau
    character(len=8) :: formaf, resure, modele, nomare, nomail
    logical :: lgmsh
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
! person_in_charge: nicolas.sellenet at edf.fr
! ----------------------------------------------------------------------
!  IMPR_RESU - TRAITEMENT DU MOT CLE FACTEUR IOCCUR
!  -    -                    -       ---
! ----------------------------------------------------------------------
!
! IN  :
!   IOCCUR  I    NUMERO D'OCCURENCE DU MOT CLE FACTEUR
!   FORMAF  K8   FORMAT DU FICHIER A IMPRIMER
!   IFICHI  I    UNITE LOGIQUE DU FICHIER A IMPRIMER
!   NIVEAU  I    NIVEAU D'IMPRESSION POUR GIBI
!   VERSIO  I    VERSION DU FICHIER IDEAS OU GMSH
!   MODELE  K8   NOM DU MODELE DONNE PAR L'UTILISATEUR
!   NOMAIL  K8   NOM DU MAILLAGE
!   NOMARE  K8   NOM DU MAILLAGE POUR LE MOT CLE RESTREINT
!   RESURE  K8   NOM DU CHAMP A IMPRIMER POUR LE MOT CLE RESTREINT
!
! IN/OUT :
!   LGMSH   L    LOGICAL SERVANT A ECRIRE L'ENTETE DU FICHIER GMSH
!
!
    integer :: nbnot, nvamin, nbpara, jpara, nbmat, nbcmp, nbnosy, nbordr
    integer :: nbcmdu, jnunot, jnosy, jordr, jcmp, jncmed, jnumat, nfor, nresu
    integer :: ncham, n01, nmail, ncoor, ninf, nsup, nvamax, npart, infmai
    integer :: ier, ibid, iret, ncarae, nvari
!
    real(kind=8) :: borsup, borinf
!
    character(len=1) :: cecr
    character(len=3) :: coor, tmax, tmin, saux03, variel
    character(len=4) :: partie
    character(len=8) :: tabl, resu, nomab, tycha, leresu, nomgd, carael
    character(len=16) :: formr, tyres
    character(len=19) :: resu19
    character(len=24) :: novcmp, nonuma, nonuno, nchsym, nnuord, nnopar, nlicmp
    character(len=80) :: titre
    parameter   (nonuma = '&&IRMFAC.NUMMAI')
    parameter   (nonuno = '&&IRMFAC.NUMNOT')
    parameter   (nchsym = '&&IRMFAC.NOM_SYMB')
    parameter   (nnuord = '&&IRMFAC.NUME_ORDRE')
    parameter   (nlicmp = '&&IRMFAC.NOM_CMP')
    parameter   (novcmp = '&&IRMFAC.NOM_CH_MED')
    parameter   (nnopar = '&&IRMFAC.NOM_PAR')
!
    logical :: lresu, lcor, lmax, lmin, linf, lsup, lvarie, lrestr, lmodel
!
    call jemarq()
!
    nbcmdu = 0
    borinf = 0.d0
    borsup = 0.d0
    lmax = .false.
    lmin = .false.
    linf = .false.
    lsup = .false.
    lcor = .false.
    cecr = 'L'
    lmodel = .false.
    if (modele .ne. ' ') lmodel = .true.
    lrestr = .false.
    if (nomare .ne. ' ') lrestr = .true.
!
!     --- FORMAF D'ECRITURE DES REELS ---
    formr=' '
    call getvtx('RESU', 'FORMAT_R', iocc=ioccur, scal=formr, nbret=nfor)
!
!     --- MODE D'ECRITURE DES PARAMETRES------
!         (RMQUE: UNIQUEMENT INTERESSANT POUR FORMAT 'RESULTAT')
    call getvtx('RESU', 'FORM_TABL', iocc=ioccur, scal=tabl, nbret=nfor)
    if (nfor .ne. 0) then
        if (tabl(1:3) .eq. 'OUI') then
            cecr = 'T'
        else if (tabl(1:5) .eq. 'EXCEL') then
            cecr = 'E'
        endif
    endif
!
!     --- RECUPERATION DU CARA_ELEM
    carael=' '
    call getvid('RESU', 'CARA_ELEM', iocc=ioccur, scal=carael, nbret=ncarae)
!
!     --- IMPRESSION DES COORDONNEES------
!         (ECRITURE VARIABLES DE TYPE RESULTAT AU FORMAT 'RESULTAT')
    coor = ' '
    call getvtx('RESU', 'IMPR_COOR', iocc=ioccur, scal=coor, nbret=ncoor)
    if (ncoor .ne. 0 .and. coor .eq. 'OUI') lcor = .true.
!
!     --- SEPARATION DES DIFFERENTES OCCURENCES (FORMAT 'RESULTAT')
    if (formaf .eq. 'RESULTAT') write(ifichi,'(/,1X,80(''-''))')
!
!     --- RECHERCHE TYPE DE DONNEES A TRAITER POUR L'OCCURENCE IOCCUR
!         VARIABLE DE TYPE RESULTAT (NRESU!=0)
!         OU CHAMP_GD (NCHAMP!=0)
    resu = ' '
    partie = ' '
    call getvid('RESU', 'RESULTAT', iocc=ioccur, scal=resu, nbret=nresu)
    if (lrestr) then
        nresu=1
        resu=resure
    endif
    call getvtx('RESU', 'PARTIE', iocc=ioccur, scal=partie, nbret=npart)
    if (nresu .ne. 0) then
        call gettco(resu, tyres)
        if (tyres(1:10) .eq. 'DYNA_HARMO' .or. tyres(1:10) .eq. 'ACOU_HARMO') then
            if (formaf(1:4) .eq. 'GMSH' .or. formaf(1:6) .eq. 'CASTEM' .or. formaf(1:3)&
                .eq. 'MED') then
                if (npart .eq. 0) then
                    call utmess('F', 'PREPOST3_69')
                endif
            endif
        endif
    endif
!
    call getvid('RESU', 'CHAM_GD', iocc=ioccur, scal=resu, nbret=ncham)
    if (ncham .ne. 0) then
        resu19=resu
        call dismoi('C', 'NOM_GD', resu19, 'CHAMP', ibid,&
                    nomgd, ier)
        if (nomgd(6:6) .eq. 'C') then
            if (formaf(1:4) .eq. 'GMSH' .or. formaf(1:6) .eq. 'CASTEM') then
                if (npart .eq. 0) then
                    call utmess('F', 'PREPOST3_69')
                endif
            endif
        endif
    endif
!
    lresu = nresu.ne.0
!     --- TEST PRESENCE DU MOT CLE INFO_MAILLAGE (FORMAT 'MED')
    infmai = 1
    call getvtx('RESU', 'INFO_MAILLAGE', iocc=ioccur, scal=saux03, nbret=n01)
    if (n01 .ne. 0) then
        if (saux03 .eq. 'OUI' .and. formaf .eq. 'MED') then
            infmai = 2
        else if (saux03.eq.'OUI'.and.formaf.ne.'MED') then
            call utmess('A', 'MED_63')
        endif
    endif
!
!     --- MAILLAGE AVEC OU SANS MODELE
!          SI LE MOT-CLE 'MODELE' EST PRESENT DANS LA COMMANDE
!          ET QUE L'ON DEMANDE L'IMPRESSION DU MAILLAGE, IL NE FAUDRA
!          IMPRIMER QUE LA PARTIE DU MAILLAGE AFFECTEE DANS LE MODELE
    nomail = ' '
    nomab = ' '
    call getvid('RESU', 'MAILLAGE', iocc=ioccur, scal=nomail, nbret=nmail)
    if ((formaf.eq.'ASTER') .and. (nomail.eq.' ')) then
        call utmess('A', 'PREPOST3_70')
    endif
    if (lrestr) then
        if (ioccur .eq. 1) then
            nmail=1
            nomail=nomare
        else
            nmail=0
        endif
    endif
!
!     --- TEST DE LA COHERENCE DU MAILLAGE ET DU MODELE ---
!
    if (lmodel .and. nmail .ne. 0) then
        call dismoi('C', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                    nomab, iret)
        if (nomail .ne. nomab) then
            call utmess('F', 'PREPOST3_66')
        endif
    endif
!
    nbcmp = 0
    nbmat = 0
    nbnot = 0
    leresu = resu
!
!     --- ECRITURE DU TITRE ---
!          SI NOMAIL = ' ' ON NE DEMANDE PAS L'IMPRESSION DU MAILLAGE
!          IRTITR SE RESUME ALORS A L'ECRITURE D'UN TITRE DANS UN K80
    call irtitr(resu, nomail, formaf, ifichi, titre)
!
!     ---  IMPRESSION DU MAILLAGE AU PREMIER PASSAGE -----
    if (nmail .ne. 0 .and. formaf .ne. 'CASTEM') then
        if (formaf(1:4) .ne. 'GMSH' .or. (nresu.eq.0.and.ncham.eq.0)) then
            call irmail(formaf, ifichi, versio, nomail, lmodel,&
                        modele, niveau, infmai, formr)
        endif
    endif
!
    if (ncham .ne. 0 .or. nresu .ne. 0) then
        call irchor(ioccur, leresu, lresu, nchsym, nnuord,&
                    nlicmp, novcmp, nnopar, nbnosy, nbordr,&
                    nbcmp, nbcmdu, nbpara, iret)
        if (iret .ne. 0) goto 99
        jnosy = 0
        if (nbnosy .gt. 0) call jeveuo(nchsym, 'L', jnosy)
        jordr = 0
        if (nbordr .gt. 0) call jeveuo(nnuord, 'L', jordr)
        jcmp = 0
        if (nbcmp .gt. 0) call jeveuo(nlicmp, 'L', jcmp)
        jncmed = 0
        if (nbcmdu .gt. 0) call jeveuo(novcmp, 'L', jncmed)
        jpara = 0
        if (nbpara .gt. 0) call jeveuo(nnopar, 'L', jpara)
    endif
!
!     ON RENTRE DANS CE QUI SUIT SAUF SI ON IMPRIME LE MAILLAGE
!       NCHAM!=0 SI CHAM_GD, NRESU!=0 SI RESULTAT COMPOSE
!       DE TYPE RESULTAT
    if (ncham .ne. 0 .or. nresu .ne. 0) then
        call irtopo(ioccur, formaf, ifichi, leresu, lresu,&
                    nbmat, nonuma, nbnot, nonuno, iret)
        if (iret .ne. 0) goto 99
        if (nbmat .ne. 0) then
            call jeveuo(nonuma, 'L', jnumat)
        else
            jnumat=0
        endif
        if (nbnot .ne. 0) then
            call jeveuo(nonuno, 'L', jnunot)
        else
            jnunot=0
        endif
    endif
!     --- IMPRESSION DANS UN INTERVALLE   BORINF,BORSUP
!      BORINF = 0.D
!      BORSUP = 0.D
    if ((ncham.ne.0.or.nresu.ne.0) .and. (formaf.eq.'RESULTAT')) then
        call getvr8('RESU', 'BORNE_INF', iocc=ioccur, scal=borinf, nbret=ninf)
        call getvr8('RESU', 'BORNE_SUP', iocc=ioccur, scal=borsup, nbret=nsup)
        if (ninf .ne. 0) linf=.true.
        if (nsup .ne. 0) lsup=.true.
    endif
!
!     ---- IMPRESSION VALEUR MAX, VALEUR MIN----
    if ((ncham.ne.0.or.nresu.ne.0) .and. (formaf.eq.'RESULTAT')) then
        tmax=' '
        tmin=' '
        call getvtx('RESU', 'VALE_MAX', iocc=ioccur, scal=tmax, nbret=nvamax)
        call getvtx('RESU', 'VALE_MIN', iocc=ioccur, scal=tmin, nbret=nvamin)
        if (nvamax .ne. 0 .and. tmax .eq. 'OUI') lmax=.true.
        if (nvamin .ne. 0 .and. tmin .eq. 'OUI') lmin=.true.
    endif
!
!     TYPE DE CHAMP A IMPRIMER POUR LE FORMAT GMSH (VERSION >= 1.2)
    tycha=' '
    if ((ncham.ne.0.or.nresu.ne.0) .and. formaf(1:4) .eq. 'GMSH' .and. versio .ge. 2) then
        call getvtx('RESU', 'TYPE_CHAM', iocc=ioccur, scal=tycha, nbret=ibid)
    endif
!
    variel=' '
    call getvtx('RESU', 'IMPR_NOM_VARI', iocc=ioccur, scal=variel, nbret=nvari)
    if (variel .eq. 'OUI') then
        lvarie=.true.
    else
        lvarie=.false.
    endif
!
!     --- APPEL A LA ROUTINE D'IMPRESSION ---
    if (ncham .ne. 0 .or. nresu .ne. 0) then
!
!       - ECRITURE DU CONCEPT LERESU SUR FICHIER FICH AU FORMAT FORM
        if (formaf(1:4) .eq. 'MED') then
            call iremed(leresu, ifichi, nchsym, novcmp, partie,&
                        nnuord, lresu, nbnot, zi(jnunot), nbmat,&
                        zi(jnumat), nlicmp, lvarie, carael)
        else
            call irecri(leresu, formaf, ifichi, titre, lgmsh,&
                        nbnosy, zk16(jnosy), partie, nbpara, zk16(jpara),&
                        nbordr, zi(jordr), lresu, 'RESU', ioccur,&
                        cecr, tycha, lcor, nbnot, zi(jnunot),&
                        nbmat, zi(jnumat), nbcmp, zk8(jcmp), lsup,&
                        borsup, linf, borinf, lmax, lmin,&
                        formr, niveau, versio)
        endif
    endif
!     **********************
!     --- FIN IMPRESSION ---
!     **********************
99  continue
!
!     --- DESTRUCTION TABLEAUX DE TRAVAIL
    call jedetr(nchsym)
    call jedetr(nnuord)
    call jedetr(nnopar)
    call jedetr(nlicmp)
    call jedetr(nnopar)
    call jedetr(novcmp)
    call jedetr(nonuma)
    call jedetr(nonuno)
!
    call jedema()
end subroutine
