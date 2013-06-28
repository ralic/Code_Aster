subroutine lrcame(nrofic, nochmd, nomamd, nomaas, ligrel,&
                  option, param, typech, typen, npgma,&
                  npgmm, nbcmpv, ncmpva, ncmpvm, iinst,&
                  numpt, numord, inst, crit, prec,&
                  nomgd, ncmprf, jnocmp, chames, codret)
!_____________________________________________________________________
!
! person_in_charge: nicolas.sellenet at edf.fr
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
!-----------------------------------------------------------------------
!     LECTURE D'UN CHAMP - FORMAT MED
!     -    -       - -            --
!-----------------------------------------------------------------------
!      ENTREES:
!        NROFIC : UNITE LOGIQUE DU FICHIER MED
!        NOCHMD : NOM MED DU CHAMP A LIRE
!        NOMAMD : NOM MED DU MAILLAGE LIE AU CHAMP A LIRE
!                  SI ' ' : ON SUPPOSE QUE C'EST LE PREMIER MAILLAGE
!                          DU FICHIER
!        NOMAAS : NOM ASTER DU MAILLAGE
!        NBVATO : NOMBRE DE VALEURS TOTAL
!        TYPECH : TYPE DE CHAMP AUX ELEMENTS : ELEM/ELGA/ELNO/NOEU
!        TYPEN  : TYPE D'ENTITE DU CHAMP
!                (MED_NOEUD=3,MED_MAILLE=0,MED_NOEUD_MAILLE=4)
!        NPGMA  : NOMBRE DE POINTS DE GAUSS PAR MAILLE (ASTER)
!        NPGMM  : NOMBRE DE POINTS DE GAUSS PAR MAILLE (MED)
!        NBCMPV : NOMBRE DE COMPOSANTES VOULUES
!                 SI NUL, ON LIT LES COMPOSANTES A NOM IDENTIQUE
!        NCMPVA : LISTE DES COMPOSANTES VOULUES POUR ASTER
!        NCMPVM : LISTE DES COMPOSANTES VOULUES DANS MED
!        IINST  : 1 SI LA DEMANDE EST FAITE SUR UN INSTANT, 0 SINON
!        NUMPT  : NUMERO DE PAS DE TEMPS EVENTUEL
!        NUMORD : NUMERO D'ORDRE EVENTUEL DU CHAMP
!        INST   : INSTANT EVENTUEL
!        CRIT   : CRITERE SUR LA RECHERCHE DU BON INSTANT
!        PREC   : PRECISION SUR LA RECHERCHE DU BON INSTANT
!        NOMGD  : NOM DE LA GRANDEUR ASSOCIEE AU CHAMP
!        NCMPRF : NOMBRE DE COMPOSANTES DE REFERENCE DU CHAMP SIMPLE
!        JNOCMP : ADRESSE DU NOM DES COMP. DE REF. DU CHAMP SIMPLE
!      SORTIES:
!         CHAMES : NOM DU CHAMP A CREER
!         CODRET : CODE DE RETOUR (0 : PAS DE PB, NON NUL SI PB)
!_____________________________________________________________________
!
! aslint: disable=W1501,W1504
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#   include "jeveux.h"
#   include "asterfort/assert.h"
#   include "asterfort/cescre.h"
#   include "asterfort/cnscre.h"
#   include "asterfort/codent.h"
#   include "asterfort/dismoi.h"
#   include "asterfort/infniv.h"
#   include "asterfort/jedema.h"
#   include "asterfort/jedetr.h"
#   include "asterfort/jeexin.h"
#   include "asterfort/jelira.h"
#   include "asterfort/jemarq.h"
#   include "asterfort/jeveuo.h"
#   include "asterfort/lrcmle.h"
#   include "asterfort/lrcmpr.h"
#   include "asterfort/lrcmva.h"
#   include "asterfort/lrcmve.h"
#   include "asterfort/lrmpga.h"
#   include "asterfort/lrmtyp.h"
#   include "asterfort/mdchin.h"
#   include "asterfort/mdexch.h"
#   include "asterfort/mdexma.h"
#   include "asterfort/mdexpm.h"
#   include "asterfort/as_mficlo.h"
#   include "asterfort/as_mficom.h"
#   include "asterfort/as_mmhnme.h"
#   include "asterfort/as_mfiope.h"
#   include "asterfort/as_mlbnuv.h"
#   include "asterfort/as_mfinvr.h"
#   include "asterfort/u2mesg.h"
#   include "asterfort/u2mesk.h"
#   include "asterfort/u2mess.h"
#   include "asterfort/ulisog.h"
#   include "asterfort/utlicm.h"
#   include "asterfort/wkvect.h"
    integer :: nrofic, typen
    integer :: ncmprf, jnocmp
    integer :: nbcmpv
    integer :: iinst, numpt, numord
    integer :: npgma(*), npgmm(*)
    integer :: codret, codre2
!
    character(len=*) :: typech
    character(len=8) :: nomgd, nomaas
    character(len=8) :: crit, param
    character(len=19) :: chames, ligrel
    character(len=24) :: option
    character(len=*) :: nochmd, nomamd
    character(len=*) :: ncmpva, ncmpvm
!
    real(kind=8) :: inst
    real(kind=8) :: prec
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRCAME' )
!
    integer :: edlect, typent
    integer :: vali(4)
    parameter (edlect=0)
    character(len=64) :: ednopf
    parameter ( ednopf=' ' )
!
    integer :: ntymax
    parameter (ntymax=69)
    integer :: nnomax
    parameter (nnomax=27)
    integer :: ednoeu
    parameter (ednoeu=3)
    integer :: edmail
    parameter (edmail=0)
    integer :: edconn
    parameter (edconn=1)
    integer :: ednoda
    parameter (ednoda=0)
    integer :: typnoe
    parameter (typnoe=0)
    integer :: ntypel, npgmax
    parameter(ntypel=26,npgmax=27)
    integer :: iaux, letype, vlib(3), vfic(3), iret
    integer :: idfimd, ifimed, indpg(ntypel, npgmax)
    integer :: nbvato, nbcmfi, nbval, nbma
    integer :: adsl, adsv, adsd, i, j
    integer :: ncmput
    integer :: existc
    integer :: ndim
    integer :: npas, adinst, adnume
    integer :: lgproa
    integer :: ifm, nivinf
    integer :: tygeom, nbtyp
    integer :: nnotyp(ntymax), modnum(ntymax), numnoa(ntymax, nnomax)
    integer :: typgeo(ntymax), lygeom(ntymax), lypent(ntymax), ltyp(ntymax)
    integer :: renumd(ntymax), nlyval(ntymax), nuanom(ntymax, nnomax)
    integer :: nbtylu, iaux2, k, nbty(ntymax)
    integer :: jtypma, nbnoma, nmatyp, jntpro, lgprof, cptyma
    integer :: jnumty, numma, ima, hdfok, medok, jlgrf, jmaill
!
    character(len=1) :: saux01
    character(len=8) :: saux08, k8b, modele
    character(len=8) :: nomtyp(ntymax)
    character(len=19) :: prefix
    character(len=24) :: numcmp, ntncmp, ntucmp, ntvale, nmcmfi(ntymax)
    character(len=24) :: valk(2)
    character(len=24) :: ntproa, nmcmfl
    character(len=64) :: nomprf
    character(len=200) :: nofimd
    character(len=255) :: kfic
    character(len=2) :: k2bid
!
    real(kind=8) :: valr
!
    logical :: existm, existt
    logical :: logaux
!
    call jemarq()
!
!====
! 1. PREALABLES
!====
!
! 1.1. ==> RECUPERATION DU NIVEAU D'IMPRESSION
!
    call infniv(ifm, nivinf)
    nomprf = ' '
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'DEBUT DE '//nompro
        write (ifm,*) '.. NOM DU CHAMP A LIRE : ',nochmd
    endif
    1001 format(/,10('='),a,10('='),/)
!
! 1.2. ==> NOMS DES TABLEAUX DE TRAVAIL
!               12   345678   9012345678901234
    numcmp = '&&'//nompro//'.NUMERO_CMP     '
    ntncmp = '&&'//nompro//'.NOMCMP         '
    ntucmp = '&&'//nompro//'.UNITECMP       '
    ntvale = '&&'//nompro//'.VALEUR         '
    ntproa = '&&'//nompro//'.PROFIL_ASTER   '
    prefix = '&&'//nompro//'.MED'
!
! 1.3. ==> NOM DU FICHIER MED
!
    call ulisog(nrofic, kfic, saux01)
    if (kfic(1:1) .eq. ' ') then
        call codent(nrofic, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,*) '<',nompro,'> NOM DU FICHIER MED : ',nofimd
    endif
!
! 1.4. ==> VERIFICATION DU FICHIER MED
!
! 1.4.1. ==> VERIFICATION DE LA VERSION HDF
!
    call as_mficom(nofimd, hdfok, medok, codret)
    if (hdfok .eq. 0) then
        saux08='mficom'
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
! 1.4.2. ==> VERIFICATION DE LA VERSION MED
!
    if (medok .eq. 0) then
        vali (1) = codret
        call u2mess('F+', 'MED_24')
        call as_mlbnuv(vlib(1), vlib(2), vlib(3), iret)
        if (iret .eq. 0) then
            vali (1) = vlib(1)
            vali (2) = vlib(2)
            vali (3) = vlib(3)
            call u2mesg('F+', 'MED_25', 0, ' ', 3,&
                        vali, 0, 0.d0)
        endif
        call as_mfiope(idfimd, nofimd, edlect, codret)
        call as_mfinvr(idfimd, vfic(1), vfic(2), vfic(3), iret)
        if (iret .eq. 0) then
            if (vfic(2) .eq. -1 .or. vfic(3) .eq. -1) then
                call u2mesg('F+', 'MED_26', 0, ' ', 0,&
                            0, 0, 0.d0)
            else
                vali (1) = vfic(1)
                vali (2) = vfic(2)
                vali (3) = vfic(3)
                call u2mesg('F+', 'MED_27', 0, ' ', 3,&
                            vali, 0, 0.d0)
            endif
            if (vfic(1) .lt. vlib(1) .or. ( vfic(1).eq.vlib(1) .and. vfic(2).lt.vlib(2) )&
                .or.&
                (&
                vfic(1) .eq. vlib(1) .and. vfic( 2) .eq. vlib(2) .and. vfic(3) .eq. vlib(3)&
                )) then
                call u2mesg('F+', 'MED_28', 0, ' ', 0,&
                            0, 0, 0.d0)
            endif
        endif
        call as_mficlo(idfimd, codret)
    endif
!
! 1.5. ==> VERIFICATION DE L'EXISTENCE DU MAILLAGE CONCERNE
!
! 1.5.1. ==> C'EST LE PREMIER MAILLAGE DU FICHIER
!            ON RECUPERE SON NOM ET SA DIMENSION.
!
    if (nomamd .eq. ' ') then
!
        ifimed = 0
        call mdexpm(nofimd, ifimed, nomamd, existm, ndim,&
                    codret)
        if (.not.existm) then
            call u2mesk('F', 'MED_50', 1, nofimd)
        endif
!
! 1.5.2. ==> C'EST UN MAILLAGE DESIGNE PAR UN NOM
!            ON RECUPERE SA DIMENSION.
!
    else
!
        iaux = 1
        ifimed = 0
        call mdexma(nofimd, ifimed, nomamd, iaux, existm,&
                    ndim, codret)
        if (.not.existm) then
            valk(1) = nomamd(1:24)
            valk(2) = nofimd(1:24)
            call u2mesk('F', 'MED_51', 2, valk)
        endif
!
    endif
!
    if (typech .eq. 'NOEU') then
        call dismoi('F', 'NB_NO_MAILLA', nomaas, 'MAILLAGE', nbnoma,&
                    k8b, iret)
        nbvato = nbnoma
    else
        call dismoi('F', 'NB_MA_MAILLA', nomaas, 'MAILLAGE', nbma,&
                    k8b, iret)
        nbvato = nbma
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,*) '.. NOM DU MAILLAGE MED ASSOCIE : ', nomamd
        write (ifm,*) '   DE DIMENSION ', ndim
    endif
!
! 2.2. ==> VERIFICATIONS DES COMPOSANTES ASTER DEMANDEES
!          EN SORTIE, ON A :
!       NCMPUT : NOMBRE DE COMPOSANTES VALIDES.
!       NUMCMP : TABLEAU DES NUMEROS DES COMPOSANTES VALIDES
!       NTNCMP : TABLEAU DES NOMS DES COMPOSANTES VALIDES (K8)
!       NTUCMP : TABLEAU DES UNITES DES COMPOSANTES VALIDES (K16)
!
    if (nbcmpv .ne. 0) then
        call jeveuo(ncmpva, 'L', iaux)
    else
        iaux = 1
    endif
!
    call utlicm(nbcmpv, zk8(iaux), nomgd, ncmprf, zk8(jnocmp),&
                ncmput, numcmp, ntncmp, ntucmp)
!
!====
! 2. OUVERTURE DU FICHIER EN LECTURE
!====
!
    call as_mfiope(idfimd, nofimd, edlect, codret)
    if (codret .ne. 0) then
        saux08='mfiope'
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
! 2.1. ==> . RECUPERATION DES NB/NOMS/NBNO/NBITEM DES TYPES DE MAILLES
!            DANS CATALOGUE
!          . RECUPERATION DES TYPES GEOMETRIE CORRESPONDANT POUR MED
!          . VERIF COHERENCE AVEC LE CATALOGUE
!
    call lrmtyp(nbtyp, nomtyp, nnotyp, typgeo, renumd,&
                modnum, nuanom, numnoa)
!
! 2.1.1 ==> LE CHAMP EXISTE-T-IL DANS LE FICHIER ?
!          AU BON NUMERO D'ORDRE ?
!          CONTIENT-IL A MINIMA LES COMPOSANTES VOULUES ?
!          LE NOMBRE DE VALEURS EST-IL CORRECT ?
!          SI OUI A TOUTES SES QUESTIONS, EXISTC VAUT 3.
!          ON RECUPERE ALORS :
!          . LE NOMBRE DE COMPOSANTES QU'IL Y A.
!          . LE NOM DE CES COMPOSANTES.
!
!     COMME DANS LRMMMA :
!    REMARQUE : GRACE A LA RENUMEROTATION, ON PARCOURT LES TYPES DE
!    MAILLES DANS L'ORDRE CROISSANT DE LEUR TYPE MED. CE N'EST PAS
!    OBLIGATOIRE SI ON DISPOSE DES TABLEAUX DE NUMEROTATION DES MAILLES.
!    MAIS QUAND CES TABLEAUX SONT ABSENTS, LES CONVENTIONS MED PRECISENT
!    QUE LA NUMEROTATION IMPLICITE SE FAIT DANS CET ORDRE. DONC ON
!    LE FAIT !
!
    nbtylu = 0
    nbcmfi = 0
    existt = .false.
!
    numma = 1
!     EN SORTIE DE MDEXMA/MDEXPM, CODRET=0
    codret = 0
    do 22 , letype = 0 , nbtyp
!
! 2.2.1. ==> LES BONS TYPES
!
    if (letype .eq. 0) then
        iaux = letype
    else
        iaux = renumd(letype)
    endif
!
    if (iaux .eq. 0) then
        typent = ednoeu
        tygeom = typnoe
    else
        typent = typen
        tygeom = typgeo(iaux)
    endif
!
!       RECUPERE LE NOMBRE DE MAILLES DE TYPE TYGEOM
    call as_mmhnme(idfimd, nomamd, edconn, edmail, tygeom,&
                ednoda, nmatyp, codre2)
!
    if (codre2 .eq. 0) then
!
! 2.2.2. ==> SI LE CHOIX S'EST FAIT AVEC UNE VALEUR D'INSTANT, ON REPERE
!            LE NUMERO D'ORDRE ASSOCIE
!
        if (iinst .ne. 0) then
!
            if (nivinf .gt. 1) then
                write (ifm,*) '.... INSTANT : ', inst
            endif
            call mdchin(nofimd, idfimd, nochmd, typent, tygeom,&
                        prefix, npas, codret)
!
            if (npas .ne. 0) then
                call jeveuo(prefix//'.INST', 'L', adinst)
                call jeveuo(prefix//'.NUME', 'L', adnume)
                logaux = .false.
                do 222 , iaux2 = 1 , npas
                if (crit(1:4) .eq. 'RELA') then
                    if (abs(zr(adinst-1+iaux2)-inst) .le. abs( prec*inst)) then
                        logaux = .true.
                    endif
                else if (crit(1:4).eq.'ABSO') then
                    if (abs(zr(adinst-1+iaux2)-inst) .le. abs( prec)) then
                        logaux = .true.
                    endif
                endif
                if (logaux) then
                    numpt = zi(adnume+2*iaux2-2)
                    numord = zi(adnume+2*iaux2-1)
                    goto 2221
                endif
222              continue
                valk (1) = nofimd(1:24)
                valk (2) = nochmd(1:24)
                valr = inst
                vali (1) = typent
                vali (2) = typgeo(1)
                call u2mesg('A', 'MED_97', 2, valk, 2,&
                            vali, 1, valr)
                call u2mess('A', 'MED_52')
                goto 22
2221              continue
!
                if (nivinf .gt. 1) then
                    valk (1) = nochmd(1:24)
                    vali (1) = typent
                    vali (2) = typgeo(1)
                    vali (3) = numord
                    vali (4) = numpt
                    valr = inst
                    call u2mesg('I', 'MED_86', 1, valk, 4,&
                                vali, 1, valr)
                endif
                call jedetr(prefix//'.INST')
                call jedetr(prefix//'.NUME')
            endif
!
!       ENDIF <<< IF ( IINST.NE.0 )
        endif
!
! 2.2.3. ==> RECHERCHE DES COMPOSANTES
!
        call codent(letype, 'G', k2bid)
        nmcmfl = '&&'//nompro//'.NOMCMP_FICHIE'//k2bid
!
        call mdexch(nofimd, idfimd, nochmd, numpt, numord,&
                    nbcmpv, ncmpvm, nbvato, typent, tygeom,&
                    existc, nbcmfi, nmcmfl, nbval, codret)
        if (existc .ge. 3) then
            existt = .true.
            nbtylu = nbtylu + 1
            nmcmfi(nbtylu) = nmcmfl
            if (typech(1:4) .ne. 'NOEU') then
                lypent(nbtylu) = typent
                lygeom(nbtylu) = tygeom
                nlyval(nbtylu) = nbval
                ltyp(nbtylu) = iaux
                nbty(nbtylu) = nmatyp
            endif
        endif
!
!       ENDIF <<< IF ( CODRET.EQ.0 )
    endif
!
!       INCREMENTE LE NUMERO INITIAL DES MAILLES DU TYPE SUIVANT
    if (nmatyp .gt. 0) then
        numma = numma + nmatyp
    endif
!
    22 end do
!
! 2.3. ==> IL MANQUE DES CHOSES !
!
    if (.not.existt) then
        valk (1) = nofimd(1:24)
        valk (2) = nochmd(1:24)
        call u2mesg('A+', 'MED_98', 2, valk, 0,&
                    0, 0, 0.d0)
        if (iinst .ne. 0) then
            valr = inst
            call u2mesg('A+', 'MED_68', 0, ' ', 0,&
                        0, 1, valr)
        else
            vali (1) = numord
            vali (2) = numpt
            call u2mesg('A+', 'MED_69', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
        if (existc .eq. 0) then
            call u2mess('A', 'MED_32')
        else if (existc.eq.1) then
            call u2mess('A', 'MED_33')
        else if (existc.eq.2) then
            if (iinst .ne. 0) then
                call u2mess('A', 'MED_34')
            else
                call u2mess('A', 'MED_35')
            endif
        else if (existc.eq.4) then
            call u2mess('A', 'MED_36')
        endif
        call u2mess('F', 'MED_37')
    endif
!
!====
! 0. TRAITEMENT PARTICULIER POUR LES CHAMPS ELGA
!====
!
! 0.1 ==> VERIFICATION DES LOCALISATIONS DES PG
!         CREATION DU TABLEAU DEFINISSANT LE NBRE DE PG PAR MAIL
!         CREATION D'UN TABLEAU DE CORRESPONDANCE ENTRE LES PG MED/ASTER
!
    do 5 i = 1, ntypel
        do 6 j = 1, npgmax
            indpg(i,j)=0
 6      continue
 5  end do
!
    if (typech(1:4) .eq. 'NOEU') then
        call cnscre(nomaas, nomgd, ncmprf, zk8(jnocmp), 'V',&
                    chames)
!
        call jeveuo(chames//'.CNSD', 'L', adsd)
        call jeveuo(chames//'.CNSV', 'E', adsv)
        call jeveuo(chames//'.CNSL', 'E', adsl)
    else
        if (typech(1:4) .eq. 'ELGA') then
            call lrmpga(nrofic, ligrel, nochmd, nbma, npgma,&
                        npgmm, ntypel, npgmax, indpg, numpt,&
                        numord, option, param)
            call cescre('V', chames, typech, nomaas, nomgd,&
                        ncmprf, zk8(jnocmp), npgma, -1, -ncmprf)
        else if (typech(1:4).eq.'CART') then
            call cescre('V', chames, 'ELEM', nomaas, nomgd,&
                        ncmprf, zk8(jnocmp), -1, -1, -ncmprf)
        else if (typech(1:4).eq.'ELNO'.or.typech(1:4).eq.'ELEM') then
            call cescre('V', chames, typech, nomaas, nomgd,&
                        ncmprf, zk8(jnocmp), -1, -1, -ncmprf)
        else
            call assert(.false.)
        endif
!
        call jeveuo(chames//'.CESD', 'L', adsd)
        call jeveuo(chames//'.CESV', 'E', adsv)
        call jeveuo(chames//'.CESL', 'E', adsl)
    endif
!
!=====================================================================
! 3. TRAITEMENT DES CHAMPS AUX NOEUDS                             ====
!=====================================================================
!
    if (typech(1:4) .eq. 'NOEU') then
!
!====
! 3.1 LECTURE DES VALEURS
!====
!
        typent = typen
        tygeom = typnoe
        call lrcmle(idfimd, nochmd, nbcmfi, nbvato, numpt,&
                    numord, typent, tygeom, ntvale, nomprf,&
                    codret)
!
!====
! 3.2   LECTURE DU PROFIL
!====
!
        if (nomprf .eq. ednopf) then
            lgproa = 0
        else
            call lrcmpr(idfimd, nomprf, ntproa, lgproa, codret)
        endif
!
!====
! 3.3   TRANFERT DES VALEURS
!====
!
        call lrcmva(ntvale, nbvato, ntproa, lgproa, ncmprf,&
                    zk8( jnocmp), nbcmfi, nmcmfi(1), nbcmpv, ncmpvm,&
                    numcmp, nochmd, adsl, adsv, codret)
!
    else
!
!=====================================================================
! 4. TRAITEMENT DES CHAMPS AUX ELEMENTS                           ====
!=====================================================================
!
!  ON BOUCLE (71) SUR LES TYPES DE MAILLE LUES DANS LE CHAMP MED.
!  LES VALEURS NUMERIQUES SONT SAUVEES DANS LE TABLEAU D ADRESSE ADSV
!  CE TABLEAU A ETE DIMENSIONNE PAR CESCRE A :
!  NB DE TYPE DE MAIL * NB DE VALEURS PAR MAILLE * NB DE COMPOSANTES
!  * NB DE MAILLES DU TYPE
!  LE NB DE VALEURS PAR MAILLE :
!       - POUR UN ELNO : NB DE NOEUDS (NBNOMA DONNE PAR CONNECTIVITE)
!       - POUR UN ELEM : 1
!       - POUR UN ELGA : VARIABLE (INFO PRESENTE DANS LE TABLEAU NPGMA)
!
        nbma=zi(adsd)
        call jeveuo(nomaas(1:8)//'.TYPMAIL', 'L', jtypma)
!
        do 71 , letype = 1 , nbtylu
!
        nbnoma=1
        if (typech(1:4) .eq. 'ELNO') then
            nbnoma = nnotyp(ltyp(letype))
        endif
        if (nivinf .gt. 1) then
            write (ifm,*) '.... NBNOMA : ', nbnoma
        endif
!
!
!
!====
! 4.0   LECTURE DES VALEURS
!====
!
        call jedetr(ntvale)
        call lrcmle(idfimd, nochmd, nbcmfi, nlyval(letype), numpt,&
                    numord, lypent(letype), lygeom(letype), ntvale, nomprf,&
                    codret)
!
!====
! 4.1   LECTURE DU PROFIL
!====
!
        if (nomprf .eq. ednopf) then
            lgproa = 0
        else
            call jedetr(ntproa)
            call lrcmpr(idfimd, nomprf, ntproa, lgproa, codret)
            call jeveuo(ntproa, 'L', jntpro)
            call jelira(ntproa, 'LONMAX', lgprof, k2bid)
        endif
!
!====
! 4.2   VECTEUR CONTENANT LES NUMEROS DES MAILLES POUR CE TYPE
!====
!         ON BOUCLE (72) SUR LES MAILLES DU MAILLAGE ASTER
!         ET ON RELEVE LES MAILLES CORRESPONDANT AU TYPE LU
!
!         ON SOUHAITE VERIFIER QUE LE MODELE ASTER ET LE PROFIL
!         MED ONT BIEN LE MEME NOMBRE DE MAILLE DE CHAQUE TYPE
        call jeexin(ligrel//'.LGRF', iret)
        if (iret .ne. 0) then
            call jeveuo(ligrel//'.LGRF', 'L', jlgrf)
            modele=zk8(jlgrf+1)
            call jeveuo(modele//'.MAILLE', 'L', jmaill)
        else
            jmaill=0
        endif
!
        call wkvect('&&'//nompro//'.NUM.'//nomtyp(ltyp(letype)), 'V V I', nbty(letype), jnumty)
        k=0
        if (lgproa .eq. 0) then
            do 72 ima = 1, nbma
                if (zi(jtypma+ima-1) .eq. ltyp(letype)) then
                    if (jmaill .eq. 0 .or. (jmaill.ne.0.and.zi(jmaill+ ima-1).ne.0)) then
                        k=k+1
                        zi(jnumty+k-1)=ima
                    endif
                endif
72          continue
            if (k .ne. nbty(letype)) then
                call u2mess('F', 'MED_58')
            endif
        else
            k=0
            cptyma=1
            do 73 ima = 1, nbma
                if (zi(jtypma+ima-1) .eq. ltyp(letype)) then
                    if (zi(jntpro+k) .eq. cptyma) then
                        if (jmaill .eq. 0 .or. (jmaill.ne.0.and.zi( jmaill+ima-1).ne.0)) then
                            k=k+1
                            zi(jnumty+k-1)=ima
                        endif
                    endif
                    cptyma=cptyma+1
                endif
73          continue
            if (k .ne. lgprof) then
                call u2mess('F', 'MED_58')
            endif
        endif
!
!====
! 4.3   TRANFERT DES VALEURS
!====
!
        call lrcmve(ntvale, nbty(letype), nbnoma, ntproa, lgproa,&
                    ncmprf, zk8(jnocmp), ntypel, npgmax, indpg,&
                    nbcmfi, nmcmfi(letype), nbcmpv, ncmpvm, numcmp,&
                    jnumty, nochmd, nbma, npgma, npgmm,&
                    typech, ltyp(letype), adsl, adsv, adsd,&
                    codret)
!
71      continue
    endif
!
!====
! 5. FIN
!====
!
! 5.1. ==> FERMETURE FICHIER
!
    call as_mficlo(idfimd, codret)
    if (codret .ne. 0) then
        saux08='mficlo'
        call u2mesg('F', 'DVP_97', 1, saux08, 1,&
                    codret, 0, 0.d0)
    endif
!
! 5.2. ==> MENAGE
!
    call jedetr(numcmp)
    call jedetr(ntncmp)
    call jedetr(ntucmp)
    call jedetr(ntvale)
    call jedetr(ntproa)
!
    do 8 , letype = 0 , nbtyp
    call codent(letype, 'G', k2bid)
    call jedetr('&&'//nompro//'.NOMCMP_FICHIE'//k2bid)
    8 end do
!
    if (typech(1:4) .ne. 'NOEU') then
        do 9 , letype = 1 , nbtylu
        call jedetr('&&'//nompro//'.NUM.'//nomtyp(ltyp(letype)))
 9      continue
    endif
!
    if (nivinf .gt. 1) then
        write (ifm,1001) 'FIN DE '//nompro
    endif
    call jedema()
!
end subroutine
