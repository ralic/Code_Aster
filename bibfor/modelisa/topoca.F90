subroutine topoca(tablca, mailla, icabl, nbf0, nbnoca,&
                  numaca)
! aslint: disable=W1501
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
!-----------------------------------------------------------------------
!  DESCRIPTION : CARACTERISATION DE LA TOPOLOGIE D'UN CABLE
!  -----------   APPELANT : OP0180 , OPERATEUR DEFI_CABLE_BP
!
!                EN SORTIE ON AJOUTE DES LIGNES DANS LA TABLE RESULTAT
!                LES CASES RENSEIGNEES CORRESPONDENT AUX PARAMETRES
!                <NUME_CABLE>, <NOEUD_CABLE>, <MAILLE_CABLE>,
!                <NOM_CABLE>, <NOM_ANCRAGE1> ET <NOM_ANCRAGE2>
!
!  IN     : TABLCA : CHARACTER*19
!                    NOM DE LA TABLE DECRIVANT LES CABLES
!  IN     : MAILLA : CHARACTER*8 , SCALAIRE
!                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
!  IN     : ICABL  : INTEGER , SCALAIRE
!                    NUMERO DU CABLE
!  OUT    : NBF0   : INTEGER , SCALAIRE
!                    NOMBRE D'ANCRAGES ACTIFS DU CABLE (0, 1 OU 2)
!  IN/OUT : NBNOCA : INTEGER , VECTEUR DE DIMENSION NBCABL
!                    CONTIENT LES NOMBRES DE NOEUDS DE CHAQUE CABLE
!  IN     : NUMACA : CHARACTER*19 , SCALAIRE
!                    NOM D'UN VECTEUR D'ENTIERS POUR STOCKAGE DES
!                    NUMEROS DES MAILLES APPARTENANT AUX CABLES
!                    CE VECTEUR EST COMPLETE A CHAQUE PASSAGE DANS LA
!                    ROUTINE TOPOCA : REAJUSTEMENT DE LA DIMENSION PUIS
!                    REMPLISSAGE DU DERNIER SOUS-BLOC ALLOUE
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
#include "jeveux.h"
!
#include "asterc/getvtx.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/tbajli.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
! ARGUMENTS
! ---------
    character(len=8) :: mailla
    integer :: icabl, nbf0, nbnoca(*)
    character(len=19) :: numaca, tablca
!
! VARIABLES LOCALES
! -----------------
    integer :: ibid, imail, ino, iret, isuiv, isuiv0(2), ivois, jcxma, jnomad
    integer :: jnono1, jnono2, jnonod, jnuma1, jnuma2, jnumac, jnumad, jtyma
    integer :: lonuti, nbchem, nbmail, nbno1, nbno2, nbsuiv, no1, no2, ntseg
    integer :: numail, n1
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    logical :: ok1, ok2
    character(len=1) :: k1b
    character(len=3) :: k3b
    character(len=8) :: k8b, noancr(2), nocour, noprec, nosui1, nosui2, nosuiv
    character(len=8) :: novois, tyancr(2)
    character(len=8) :: presen(2)
    character(len=24) :: conxma, grmama, nomama, nonoma, tymama
    character(len=24) :: valk(3), nogrno(2), nogrna(2), nogrma
    character(len=24) :: param(5), vk(4)
    integer :: iarg
    data          param /'NUME_CABLE              ',&
     &                     'NOEUD_CABLE             ',&
     &                     'NOM_CABLE               ',&
     &                     'NOM_ANCRAGE1            ',&
     &                     'NOM_ANCRAGE2            '/
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   SAISIE DES ENTITES TOPOLOGIQUES ASSOCIEES AU CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    conxma = mailla//'.CONNEX'
    grmama = mailla//'.GROUPEMA'
    nomama = mailla//'.NOMMAI'
    nonoma = mailla//'.NOMNOE'
    tymama = mailla//'.TYPMAIL'
    call jeveuo(tymama, 'L', jtyma)
!
! 1.1 SAISIE DES MAILLES ASSOCIEES
! ---
    call getvem(mailla, 'MAILLE', 'DEFI_CABLE', 'MAILLE', icabl,&
                iarg, 0, k8b, nbmail)
!
!.... SAISIE DIRECTE
!
    if (nbmail .ne. 0) then
!
        nbmail = abs(nbmail)
        call wkvect('&&TOPOCA.NOMAIL_DEF', 'V V K8', nbmail, jnomad)
        call wkvect('&&TOPOCA.NUMAIL_DEF', 'V V I', nbmail, jnumad)
        call getvem(mailla, 'MAILLE', 'DEFI_CABLE', 'MAILLE', icabl,&
                    iarg, nbmail, zk8(jnomad), ibid)
        do 10 imail = 1, nbmail
            call jenonu(jexnom(nomama, zk8(jnomad+imail-1)), zi(jnumad+ imail-1))
10      continue
!
!.... SAISIE INDIRECTE PAR UN GROUPE DE MAILLES
!
    else
!
        call getvem(mailla, 'GROUP_MA', 'DEFI_CABLE', 'GROUP_MA', icabl,&
                    iarg, 1, nogrma, ibid)
        call jelira(jexnom(grmama, nogrma), 'LONUTI', nbmail, k1b)
        call jeveuo(jexnom(grmama, nogrma), 'L', jnumad)
!
    endif
!
! 1.2 VERIFICATION DU TYPE DES MAILLES ET DETERMINATION SIMULTANEE
! --- DE LEURS NOEUDS EXTREMITES
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
!
    call wkvect('&&TOPOCA.NOMNOE_DEF', 'V V K8', 2*nbmail, jnonod)
!
    do 20 imail = 1, nbmail
        numail = zi(jnumad+imail-1)
        if (zi(jtyma+numail-1) .ne. ntseg) then
            write(k3b,'(I3)') icabl
            call u2mesk('F', 'MODELISA7_54', 1, k3b)
        endif
        call jeveuo(jexnum(conxma, numail), 'L', jcxma)
        no1 = zi(jcxma)
        no2 = zi(jcxma+1)
        call jenuno(jexnum(nonoma, no1), zk8(jnonod+2*(imail-1)))
        call jenuno(jexnum(nonoma, no2), zk8(jnonod+2*(imail-1)+1))
20  end do
!
! 1.3 SAISIE DU GROUP_NO D'ANCRAGE DU CABLE EVENTUELLEMENT
! ---
    nogrno(1) = ' '
    nogrno(2) = ' '
    call getvtx('DEFI_CABLE', 'GROUP_NO_FUT', icabl, iarg, 2,&
                nogrno, n1)
    if (n1 .eq. 1) then
        call getvtx('CONE', 'PRESENT', 1, iarg, 2,&
                    presen, n1)
        if (presen(2)(1:3) .eq. 'OUI') then
            nogrno(2) = nogrno(1)
            nogrno(1) = '        '
        endif
    endif
!
!
! 1.4 SAISIE DES NOEUDS D'ANCRAGE DU CABLE
! ---
    call getvem(mailla, 'NOEUD', 'DEFI_CABLE', 'NOEUD_ANCRAGE', icabl,&
                iarg, 0, k8b, ibid)
!
    if (ibid .eq. 0) then
!
        call getvem(mailla, 'GROUP_NO', 'DEFI_CABLE', 'GROUP_NO_ANCRAGE', icabl,&
                    iarg, 2, nogrna(1), ibid)
!
        call utnono(' ', mailla, 'NOEUD', nogrna(1), k8b,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogrna(1))
        else if (iret.eq.1) then
            valk(1) = nogrna(1)
            valk(2) = k8b
            call u2mesk('A', 'SOUSTRUC_87', 2, valk)
        endif
        noancr(1) = k8b
!
        call utnono(' ', mailla, 'NOEUD', nogrna(2), k8b,&
                    iret)
        if (iret .eq. 10) then
            call u2mesk('F', 'ELEMENTS_67', 1, nogrna(2))
        else if (iret.eq.1) then
            valk(1) = nogrna(2)
            valk(2) = k8b
            call u2mesk('A', 'SOUSTRUC_87', 2, valk)
        endif
        noancr(2) = k8b
!
    else
!
        call getvem(mailla, 'NOEUD', 'DEFI_CABLE', 'NOEUD_ANCRAGE', icabl,&
                    iarg, 2, noancr(1), ibid)
!
    endif
!
    call getvtx(' ', 'TYPE_ANCRAGE', icabl, iarg, 2,&
                tyancr(1), ibid)
    nbf0 = 0
    if (tyancr(1)(1:5) .eq. 'ACTIF') nbf0 = nbf0 + 1
    if (tyancr(2)(1:5) .eq. 'ACTIF') nbf0 = nbf0 + 1
    if ((nbf0.eq.1) .and. (tyancr(1)(1:6).eq.'PASSIF')) then
        k8b = noancr(1)
        noancr(1) = noancr(2)
        noancr(2) = k8b
        k8b = tyancr(1)
        tyancr(1) = tyancr(2)
        tyancr(2) = k8b
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   DETERMINATION D'UN CHEMIN CONTINU DEFINISSANT LE CABLE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 2.1 DETERMINATION DU NOMBRE DE CHEMINS POSSIBLES AU DEPART DU PREMIER
! --- NOEUD D'ANCRAGE
!
    nbchem = 0
    do 30 ino = 1, 2*nbmail
        if (zk8(jnonod+ino-1) .eq. noancr(1)) then
            if (mod(ino,2) .eq. 0) then
                isuiv = ino - 1
            else
                isuiv = ino + 1
            endif
            nosuiv = zk8(jnonod+isuiv-1)
            if (nosuiv .ne. noancr(1)) then
                nbchem = nbchem + 1
                if (nbchem .gt. 2) then
                    write(k3b,'(I3)') icabl
                    valk(1) = k3b
                    valk(2) = noancr(1)
                    call u2mesk('F', 'MODELISA7_55', 2, valk)
                endif
                isuiv0(nbchem) = isuiv
            endif
        endif
30  end do
!
    if (nbchem .eq. 0) then
        write(k3b,'(I3)') icabl
        valk(1) = k3b
        valk(2) = noancr(1)
        call u2mesk('F', 'MODELISA7_56', 2, valk)
    endif
!
    nosui1 = zk8(jnonod+isuiv0(1)-1)
    if (nbchem .eq. 2) then
        nosui2 = zk8(jnonod+isuiv0(2)-1)
        if (nosui1 .eq. nosui2) nbchem = 1
    endif
!
! 2.2 TENTATIVE DE PARCOURS DU PREMIER CHEMIN POSSIBLE
! ---
    call wkvect('&&TOPOCA.NUMAIL_CH1', 'V V I', nbmail, jnuma1)
    call wkvect('&&TOPOCA.NOMNOE_CH1', 'V V K8', nbmail+1, jnono1)
!
    ok1 = .false.
!
    nbno1 = 1
    zk8(jnono1) = noancr(1)
    if (mod(isuiv0(1),2) .eq. 0) then
        imail = isuiv0(1)/2
    else
        imail = (isuiv0(1)+1)/2
    endif
    zi(jnuma1) = zi(jnumad+imail-1)
    noprec = noancr(1)
    nocour = nosui1
!
!.... REPETER (DEBUT)
40  continue
    if (nocour .eq. noancr(2)) then
        nbno1 = nbno1 + 1
        zk8(jnono1+nbno1-1) = noancr(2)
        ok1 = .true.
        goto 60
    endif
    nbsuiv = 0
    do 50 ino = 1, 2*nbmail
        if (zk8(jnonod+ino-1) .eq. nocour) then
            if (mod(ino,2) .eq. 0) then
                ivois = ino - 1
            else
                ivois = ino + 1
            endif
            novois = zk8(jnonod+ivois-1)
            if ((novois.ne.nocour) .and. (novois.ne.noprec)) then
                nbsuiv = nbsuiv + 1
                if (nbsuiv .gt. 1) goto 60
                nosuiv = novois
                isuiv = ivois
            endif
        endif
50  continue
    if (nbsuiv .eq. 0) goto 60
    nbno1 = nbno1 + 1
    zk8(jnono1+nbno1-1) = nocour
    if (mod(isuiv,2) .eq. 0) then
        imail = isuiv/2
    else
        imail = (isuiv+1)/2
    endif
    zi(jnuma1+nbno1-1) = zi(jnumad+imail-1)
    noprec = nocour
    nocour = nosuiv
    if (nbno1 .lt. nbmail+1) goto 40
!
!.... REPETER (FIN)
60  continue
!
! 2.3 TENTATIVE DE PARCOURS DU SECOND CHEMIN POSSIBLE LE CAS ECHEANT
! ---
    ok2 = .false.
!
    if (nbchem .eq. 2) then
!
        call wkvect('&&TOPOCA.NUMAIL_CH2', 'V V I', nbmail, jnuma2)
        call wkvect('&&TOPOCA.NOMNOE_CH2', 'V V K8', nbmail+1, jnono2)
!
        nbno2 = 1
        zk8(jnono2) = noancr(1)
        if (mod(isuiv0(2),2) .eq. 0) then
            imail = isuiv0(2)/2
        else
            imail = (isuiv0(2)+1)/2
        endif
        zi(jnuma2) = zi(jnumad+imail-1)
        noprec = noancr(1)
        nocour = nosui2
!
!....... REPETER (DEBUT)
70      continue
        if (nocour .eq. noancr(2)) then
            nbno2 = nbno2 + 1
            zk8(jnono2+nbno2-1) = noancr(2)
            ok2 = .true.
            goto 90
        endif
        nbsuiv = 0
        do 80 ino = 1, 2*nbmail
            if (zk8(jnonod+ino-1) .eq. nocour) then
                if (mod(ino,2) .eq. 0) then
                    ivois = ino - 1
                else
                    ivois = ino + 1
                endif
                novois = zk8(jnonod+ivois-1)
                if ((novois.ne.nocour) .and. (novois.ne.noprec)) then
                    nbsuiv = nbsuiv + 1
                    if (nbsuiv .gt. 1) goto 90
                    nosuiv = novois
                    isuiv = ivois
                endif
            endif
80      continue
        if (nbsuiv .eq. 0) goto 90
        nbno2 = nbno2 + 1
        zk8(jnono2+nbno2-1) = nocour
        if (mod(isuiv,2) .eq. 0) then
            imail = isuiv/2
        else
            imail = (isuiv+1)/2
        endif
        zi(jnuma2+nbno2-1) = zi(jnumad+imail-1)
        noprec = nocour
        nocour = nosuiv
        if (nbno2 .lt. nbmail+1) goto 70
!
!....... REPETER (FIN)
90      continue
!
    endif
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   MISE A JOUR DES OBJETS DE SORTIE
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 3.1 AMBIGUITE SI DEUX CHEMINS CONTINUS POSSIBLES
! ---
    if (ok1 .and. ok2) then
        write(k3b,'(I3)') icabl
        valk(1) = k3b
        valk(2) = noancr(1)
        valk(3) = noancr(2)
        call u2mesk('F', 'MODELISA7_57', 3, valk)
!
! 3.2 MISE A JOUR DES OBJETS DE SORTIE
! ---
    else
!
! 3.2.1  CAS OU LE PREMIER CHEMIN POSSIBLE EST VALIDE
! .....
        if (ok1) then
!
            nbnoca(icabl) = nbno1
!
            if (icabl .eq. 1) then
                call jeecra(numaca, 'LONUTI', nbno1-1, ' ')
                call jeveuo(numaca, 'E', jnumac)
                do 100 imail = 1, nbno1-1
                    zi(jnumac+imail-1) = zi(jnuma1+imail-1)
100              continue
            else
                call jelira(numaca, 'LONUTI', lonuti, k1b)
                call jeecra(numaca, 'LONUTI', lonuti+nbno1-1, ' ')
                call jeveuo(numaca, 'E', jnumac)
                do 110 imail = 1, nbno1-1
                    zi(jnumac+lonuti+imail-1) = zi(jnuma1+imail-1)
110              continue
            endif
!
            do 120 ino = 1, nbno1
!                CALL TBAJLI(TABLCA,2,PARAM,
!      +                     ICABL,RBID,CBID,ZK8(JNONO1+INO-1),0)
!
                vk(1) = zk8(jnono1+ino-1)
                vk(2) = nogrma
                vk(3) = nogrno(1)
                vk(4) = nogrno(2)
                call tbajli(tablca, 5, param, icabl, rbid,&
                            cbid, vk, 0)
!
120          continue
!
! 3.2.2  CAS OU LE SECOND CHEMIN POSSIBLE EST VALIDE
! .....
        else if (ok2) then
!
            nbnoca(icabl) = nbno2
!
            if (icabl .eq. 1) then
                call jeecra(numaca, 'LONUTI', nbno2-1, ' ')
                call jeveuo(numaca, 'E', jnumac)
                do 130 imail = 1, nbno2-1
                    zi(jnumac+imail-1) = zi(jnuma2+imail-1)
130              continue
            else
                call jelira(numaca, 'LONUTI', lonuti, k1b)
                call jeecra(numaca, 'LONUTI', lonuti+nbno2-1, ' ')
                call jeveuo(numaca, 'E', jnumac)
                do 140 imail = 1, nbno2-1
                    zi(jnumac+lonuti+imail-1) = zi(jnuma2+imail-1)
140              continue
            endif
!
            do 150 ino = 1, nbno2
!                CALL TBAJLI(TABLCA,2,PARAM,
!      +                     ICABL,RBID,CBID,ZK8(JNONO2+INO-1),0)
!
                vk(1) = zk8(jnono2+ino-1)
                vk(2) = nogrma
                vk(3) = nogrno(1)
                vk(4) = nogrno(2)
                call tbajli(tablca, 5, param, icabl, rbid,&
                            cbid, vk, 0)
!
150          continue
!
! 3.2.3  AUCUN CHEMIN CONTINU VALIDE
! .....
        else
!
            write(k3b,'(I3)') icabl
            call u2mesk('F', 'MODELISA7_58', 1, k3b)
        endif
!
    endif
!
! --- MENAGE
    call jedetr('&&TOPOCA.NOMAIL_DEF')
    call jedetr('&&TOPOCA.NUMAIL_DEF')
    call jedetr('&&TOPOCA.NOMNOE_DEF')
    call jedetr('&&TOPOCA.NUMAIL_CH1')
    call jedetr('&&TOPOCA.NOMNOE_CH1')
    call jedetr('&&TOPOCA.NUMAIL_CH2')
    call jedetr('&&TOPOCA.NOMNOE_CH2')
!
    call jedema()
!
! --- FIN DE TOPOCA.
end subroutine
