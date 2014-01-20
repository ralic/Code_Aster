subroutine topoca(tablca, mailla, icabl, nbf0, nbnoca,&
                  numaca, quad, sens)
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
!  OUT    : QUAD   : VRAI SI MAILLAGE QUADRATIQUE (SEG3)
!           SENS   : ORIENTATION DES MAILLES
!-------------------   DECLARATION DES VARIABLES   ---------------------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
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
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
! ARGUMENTS
! ---------
    character(len=8) :: mailla
    integer :: icabl, nbf0, nbnoca(*), sens
    character(len=19) :: numaca, tablca
    logical :: quad
!
! VARIABLES LOCALES
! -----------------
    integer :: ibid, imail, ino, iret, isuiv, isuiv0(2), ivois, jcxma
    integer ::      jnumac, jnumad, jtyma
    integer :: lonuti, nbchem, nbmail, nbno1, nbno2, nbsuiv, no1, no2, ntseg
    integer :: numail, n1, nbse2, nbse3, no3, ntseg2
    real(kind=8) :: rbid
    complex(kind=8) :: cbid
    logical :: ok1, ok2
    character(len=3) :: k3b
    character(len=8) :: k8b, noancr(2), nocour, noprec, nosui1, nosui2, nosuiv
    character(len=8) :: novois, tyancr(2)
    character(len=8) :: presen(2)
    character(len=24) :: conxma, grmama, nomama, nonoma, tymama
    character(len=24) :: valk(3), nogrno(2), nogrna(2), nogrma
    character(len=24) :: param(6), vk(5)
    integer :: iarg
    character(len=8), pointer :: nomail_def(:) => null()
    character(len=8), pointer :: nomnoe_ch1(:) => null()
    character(len=8), pointer :: nomnoe_ch2(:) => null()
    character(len=8), pointer :: nomnoe_def(:) => null()
    integer, pointer :: numail_ch1(:) => null()
    integer, pointer :: numail_ch2(:) => null()
    data          param /'NUME_CABLE              ',&
     &                     'NOEUD_CABLE             ',&
     &                     'NOM_CABLE               ',&
     &                     'NOM_ANCRAGE1            ',&
     &                     'NOM_ANCRAGE2            ',&
     &                     'NOEUD_MILIEU'/
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    cbid=(0.d0,0.d0)
    rbid=0.d0
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
        AS_ALLOCATE(vk8=nomail_def, size=nbmail)
        call wkvect('&&TOPOCA.NUMAIL_DEF', 'V V I', nbmail, jnumad)
        call getvem(mailla, 'MAILLE', 'DEFI_CABLE', 'MAILLE', icabl,&
                    iarg, nbmail, nomail_def, ibid)
        do 10 imail = 1, nbmail
            call jenonu(jexnom(nomama, nomail_def(imail)), zi(jnumad+ imail-1))
10      continue
!
!.... SAISIE INDIRECTE PAR UN GROUPE DE MAILLES
!
    else
!
        call getvem(mailla, 'GROUP_MA', 'DEFI_CABLE', 'GROUP_MA', icabl,&
                    iarg, 1, nogrma, ibid)
        call jelira(jexnom(grmama, nogrma), 'LONUTI', nbmail)
        call jeveuo(jexnom(grmama, nogrma), 'L', jnumad)
!
    endif
!
! 1.2 VERIFICATION DU TYPE DES MAILLES ET DETERMINATION SIMULTANEE
! --- DE LEURS NOEUDS EXTREMITES
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ntseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ntseg2)
!
    AS_ALLOCATE(vk8=nomnoe_def, size=2*nbmail)
!
    nbse2=0
    nbse3=0
    do imail = 1, nbmail
        numail = zi(jnumad+imail-1)
        if ((zi(jtyma+numail-1).ne.ntseg) .and. (zi(jtyma+numail-1) .ne.ntseg2)) then
            write(k3b,'(I3)') icabl
            call utmess('F', 'MODELISA7_54', sk=k3b)
        endif
        if (zi(jtyma+numail-1) .eq. ntseg) nbse2=nbse2+1
        if (zi(jtyma+numail-1) .eq. ntseg2) nbse3=nbse3+1
        call jeveuo(jexnum(conxma, numail), 'L', jcxma)
        no1 = zi(jcxma)
        no2 = zi(jcxma+1)
        call jenuno(jexnum(nonoma, no1), nomnoe_def(1+2*(imail-1)))
        call jenuno(jexnum(nonoma, no2), nomnoe_def(1+2*(imail-1)+1))
    end do
    ASSERT((nbse2.eq.0).or.(nbse3.eq.0))
    quad=.false.
    if (nbse3 .gt. 0) quad=.true.
!
! 1.3 SAISIE DU GROUP_NO D'ANCRAGE DU CABLE EVENTUELLEMENT
! ---
    nogrno(1) = '        '
    nogrno(2) = '        '
    call getvtx('DEFI_CABLE', 'GROUP_NO_FUT', iocc=icabl, nbval=2, vect=nogrno,&
                nbret=n1)
    if (n1 .eq. 1) then
        call getvtx('CONE', 'PRESENT', iocc=1, nbval=2, vect=presen,&
                    nbret=n1)
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
            call utmess('F', 'ELEMENTS_67', sk=nogrna(1))
        else if (iret.eq.1) then
            valk(1) = nogrna(1)
            valk(2) = k8b
            call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
        endif
        noancr(1) = k8b
!
        call utnono(' ', mailla, 'NOEUD', nogrna(2), k8b,&
                    iret)
        if (iret .eq. 10) then
            call utmess('F', 'ELEMENTS_67', sk=nogrna(2))
        else if (iret.eq.1) then
            valk(1) = nogrna(2)
            valk(2) = k8b
            call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
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
    call getvtx(' ', 'TYPE_ANCRAGE', nbval=2, vect=tyancr(1), nbret=ibid)
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
    do ino = 1, 2*nbmail
        if (nomnoe_def(ino) .eq. noancr(1)) then
            if (mod(ino,2) .eq. 0) then
                isuiv = ino - 1
            else
                isuiv = ino + 1
            endif
            nosuiv = nomnoe_def(isuiv)
            if (nosuiv .ne. noancr(1)) then
                nbchem = nbchem + 1
                if (nbchem .gt. 2) then
                    write(k3b,'(I3)') icabl
                    valk(1) = k3b
                    valk(2) = noancr(1)
                    call utmess('F', 'MODELISA7_55', nk=2, valk=valk)
                endif
                isuiv0(nbchem) = isuiv
            endif
        endif
    end do
!
    if (nbchem .eq. 0) then
        write(k3b,'(I3)') icabl
        valk(1) = k3b
        valk(2) = noancr(1)
        call utmess('F', 'MODELISA7_56', nk=2, valk=valk)
    endif
!
    nosui1 = nomnoe_def(1+isuiv0(1)-1)
    if (nbchem .eq. 2) then
        nosui2 = nomnoe_def(1+isuiv0(2)-1)
        if (nosui1 .eq. nosui2) nbchem = 1
    endif
!
! 2.2 TENTATIVE DE PARCOURS DU PREMIER CHEMIN POSSIBLE
! ---
    AS_ALLOCATE(vi=numail_ch1, size=nbmail)
    AS_ALLOCATE(vk8=nomnoe_ch1, size=nbmail+1)
!
    ok1 = .false.
!
    nbno1 = 1
    nomnoe_ch1(1) = noancr(1)
    if (mod(isuiv0(1),2) .eq. 0) then
        imail = isuiv0(1)/2
    else
        imail = (isuiv0(1)+1)/2
    endif
    numail_ch1(1) = zi(jnumad+imail-1)
    noprec = noancr(1)
    nocour = nosui1
!
!.... REPETER (DEBUT)
40  continue
    if (nocour .eq. noancr(2)) then
        nbno1 = nbno1 + 1
        nomnoe_ch1(nbno1) = noancr(2)
        ok1 = .true.
        goto 60
    endif
    nbsuiv = 0
    do 50 ino = 1, 2*nbmail
        if (nomnoe_def(ino) .eq. nocour) then
            if (mod(ino,2) .eq. 0) then
                ivois = ino - 1
            else
                ivois = ino + 1
            endif
            novois = nomnoe_def(ivois)
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
    nomnoe_ch1(nbno1) = nocour
    if (mod(isuiv,2) .eq. 0) then
        imail = isuiv/2
    else
        imail = (isuiv+1)/2
    endif
    numail_ch1(nbno1) = zi(jnumad+imail-1)
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
        AS_ALLOCATE(vi=numail_ch2, size=nbmail)
        AS_ALLOCATE(vk8=nomnoe_ch2, size=nbmail+1)
!
        nbno2 = 1
        nomnoe_ch2(1) = noancr(1)
        if (mod(isuiv0(2),2) .eq. 0) then
            imail = isuiv0(2)/2
        else
            imail = (isuiv0(2)+1)/2
        endif
        numail_ch2(1) = zi(jnumad+imail-1)
        noprec = noancr(1)
        nocour = nosui2
!
!....... REPETER (DEBUT)
70      continue
        if (nocour .eq. noancr(2)) then
            nbno2 = nbno2 + 1
            nomnoe_ch2(nbno2) = noancr(2)
            ok2 = .true.
            goto 90
        endif
        nbsuiv = 0
        do 80 ino = 1, 2*nbmail
            if (nomnoe_def(ino) .eq. nocour) then
                if (mod(ino,2) .eq. 0) then
                    ivois = ino - 1
                else
                    ivois = ino + 1
                endif
                novois = nomnoe_def(ivois)
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
        nomnoe_ch2(nbno2) = nocour
        if (mod(isuiv,2) .eq. 0) then
            imail = isuiv/2
        else
            imail = (isuiv+1)/2
        endif
        numail_ch2(nbno2) = zi(jnumad+imail-1)
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
        call utmess('F', 'MODELISA7_57', nk=3, valk=valk)
!
! 3.2 MISE A JOUR DES OBJETS DE SORTIE
! ---
    else
!
! 3.2.1  CAS OU LE PREMIER CHEMIN POSSIBLE EST VALIDE
! .....
        if (ok1) then
!
            if (quad) then
                nbnoca(icabl) = 2*nbno1-1
            else
                nbnoca(icabl) = nbno1
            endif
!
            if (icabl .eq. 1) then
                call jeecra(numaca, 'LONUTI', nbno1-1)
                call jeveuo(numaca, 'E', jnumac)
                lonuti=0
            else
                call jelira(numaca, 'LONUTI', lonuti)
                call jeecra(numaca, 'LONUTI', lonuti+nbno1-1)
                call jeveuo(numaca, 'E', jnumac)
            endif
!
            sens=0
!
            do 100 imail = 1, nbno1-1
                zi(jnumac+lonuti+imail-1) = numail_ch1(imail)
                ino=imail
                vk(1) = nomnoe_ch1(ino)
                vk(2) = nogrma
                vk(3) = nogrno(1)
                vk(4) = nogrno(2)
                vk(5) = 'NON'
                call tbajli(tablca, 6, param, [icabl], [rbid],&
                            [cbid], vk, 0)
                if (quad) then
                    numail=numail_ch1(imail)
                    call jeveuo(jexnum(conxma, numail), 'L', jcxma)
                    no3=zi(jcxma+2)
                    no1=zi(jcxma)
                    call jenuno(jexnum(nonoma, no1), vk(1))
                    if (sens .eq. 0) then
                        if (nomnoe_ch1(ino) .eq. vk(1)) then
                            sens=1
                        else
                            sens=-1
                        endif
                    else
!                   TOUTES LES MAILLES DOIVENT ETRE DANS LE MEME SENS
                        if (nomnoe_ch1(ino) .eq. vk(1)) then
                            if (sens .ne.1) call utmess('F', 'MODELISA7_14', nk=1, valk=nogrma)
!                             ASSERT(sens.eq.1)
                        else
                            ASSERT(sens.eq.-1)
                        endif
                    endif
                    call jenuno(jexnum(nonoma, no3), vk(1))
                    vk(2) = nogrma
                    vk(3) = nogrno(1)
                    vk(4) = nogrno(2)
                    vk(5) = 'OUI'
                    call tbajli(tablca, 6, param, [icabl], [rbid],&
                                [cbid], vk, 0)
                endif
100          continue
            vk(1) = nomnoe_ch1(nbno1)
            vk(2) = nogrma
            vk(3) = nogrno(1)
            vk(4) = nogrno(2)
            vk(5) = 'NON'
            call tbajli(tablca, 6, param, [icabl], [rbid],&
                        [cbid], vk, 0)
!
!
! 3.2.2  CAS OU LE SECOND CHEMIN POSSIBLE EST VALIDE
! .....
        else if (ok2) then
!
            if (quad) then
                nbnoca(icabl) = 2*nbno2-1
            else
                nbnoca(icabl) = nbno2
            endif
!
            if (icabl .eq. 1) then
                call jeecra(numaca, 'LONUTI', nbno2-1)
                call jeveuo(numaca, 'E', jnumac)
                lonuti=0
            else
                call jelira(numaca, 'LONUTI', lonuti)
                call jeecra(numaca, 'LONUTI', lonuti+nbno2-1)
                call jeveuo(numaca, 'E', jnumac)
            endif
            sens=0
            do 150 imail = 1, nbno2-1
                zi(jnumac+lonuti+imail-1) = numail_ch2(imail)
                ino=imail
                vk(1) = nomnoe_ch2(ino)
                vk(2) = nogrma
                vk(3) = nogrno(1)
                vk(4) = nogrno(2)
                vk(5) = 'NON'
                call tbajli(tablca, 6, param, [icabl], [rbid],&
                            [cbid], vk, 0)
                if (quad) then
                    numail=numail_ch2(imail)
                    call jeveuo(jexnum(conxma, numail), 'L', jcxma)
                    no3=zi(jcxma+2)
                    no1=zi(jcxma)
                    call jenuno(jexnum(nonoma, no1), vk(1))
                    if (sens .eq. 0) then
                        if (nomnoe_ch1(ino) .eq. vk(1)) then
                            sens=1
                        else
                            sens=-1
                        endif
                    else
!                   TOUTES LES MAILLES DOIVENT ETRE DANS LE MEME SENS
                        if (nomnoe_ch1(ino) .eq. vk(1)) then
                            ASSERT(sens.eq.1)
                        else
                            ASSERT(sens.eq.-1)
                        endif
                    endif
                    call jenuno(jexnum(nonoma, no3), vk(1))
                    vk(2) = nogrma
                    vk(3) = nogrno(1)
                    vk(4) = nogrno(2)
                    vk(5) = 'OUI'
                    call tbajli(tablca, 6, param, [icabl], [rbid],&
                                [cbid], vk, 0)
                endif
150          continue
            vk(1) = nomnoe_ch2(nbno2)
            vk(2) = nogrma
            vk(3) = nogrno(1)
            vk(4) = nogrno(2)
            vk(5) = 'NON'
            call tbajli(tablca, 6, param, [icabl], [rbid],&
                        [cbid], vk, 0)
!
! 3.2.3  AUCUN CHEMIN CONTINU VALIDE
! .....
        else
!
            write(k3b,'(I3)') icabl
            call utmess('F', 'MODELISA7_58', sk=k3b)
        endif
!
    endif
!
! --- MENAGE
    AS_DEALLOCATE(vk8=nomail_def)
    call jedetr('&&TOPOCA.NUMAIL_DEF')
    AS_DEALLOCATE(vk8=nomnoe_def)
    AS_DEALLOCATE(vi=numail_ch1)
    AS_DEALLOCATE(vk8=nomnoe_ch1)
    AS_DEALLOCATE(vi=numail_ch2)
    AS_DEALLOCATE(vk8=nomnoe_ch2)
!
    call jedema()
!
! --- FIN DE TOPOCA.
end subroutine
