subroutine op0180()
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
!  DESCRIPTION :
!  -----------       O P E R A T E U R    D E F I _ C A B L E _ B P
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
! ARGUMENTS
! ---------
!
! VARIABLES LOCALES
! -----------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/alcart.h"
#include "asterfort/caelca.h"
#include "asterfort/cncinv.h"
#include "asterfort/crelrl.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gromab.h"
#include "asterfort/immeca.h"
#include "asterfort/infmaj.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ltcrsd.h"
#include "asterfort/ltnotb.h"
#include "asterfort/nocart.h"
#include "asterfort/projca.h"
#include "asterfort/reliem.h"
#include "asterfort/sigmca.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/tensca.h"
#include "asterfort/titre.h"
#include "asterfort/tomabe.h"
#include "asterfort/topoca.h"
#include "asterfort/trajca.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/voisca.h"
#include "asterfort/wkvect.h"
!
    integer :: ibid, icabl, icmp, irana1, iret, jcaba, jnbno, jncmp, jsief
    integer :: jvalv, n1, n2, nbancr, nbcabl, nbf0, nbmama, nbnobe, nbnoma
    integer :: ncaba, nsief, nbmabe, jlimab, nbnoca
    real(kind=8) :: delta, ea, f0, frco, frli, mu0, rh1000, sa, fprg, xflu, xret
    real(kind=8) :: trelax, valr(2)
    logical :: mail2d, relax, quad
    character(len=3) :: k3b
    character(len=8) :: caelem, chmat, k8b, mailla, modele, noancr(2), nomu, adher
    character(len=8) :: typanc(2)
    character(len=16) :: cmd, concep
    character(len=19) :: carsig, carte, ligrmo, lirela, numaca, nunobe, xnoca
    character(len=19) :: ynoca, znoca, nomt19, nunobi, nomg19
    character(len=24) :: cadesc, ncncin, nmabet, comima, gromai
    character(len=8) :: aire, effnor(3), valk(7)
    complex(kind=8) :: cbid
    integer :: nbpar, nbnobi, sens, nbpar2
    parameter    (nbpar=14)
    character(len=3) :: typpar(nbpar)
    character(len=24) :: nompar(nbpar), typrel
    character(len=4) :: regl
    parameter    (nbpar2=9)
    character(len=3) :: typpa2(nbpar2)
    character(len=24) :: nompa2(nbpar2)
!
    data          aire  /'A1      '/
    data          effnor/'N       ','CONT_X  ','CONT_Y  '/
    data          typpar /'I ','K8','R ','R ','R ',&
     &                    'K8','K8', 'I ','I ','R ','K24','K24','K24','K8'/
    data          nompar /'NUME_CABLE              ',&
     &                      'NOEUD_CABLE             ',&
     &                      'ABSC_CURV               ',&
     &                      'ALPHA                   ',&
     &                      'TENSION                 ',&
     &                      'MAILLE_BETON_VOISINE    ',&
     &                      'NOEUD_BETON_VOISIN      ',&
     &                      'INDICE_IMMERSION        ',&
     &                      'INDICE_PROJECTION       ',&
     &                      'EXCENTRICITE            ',&
     &                      'NOM_CABLE               ',&
     &                      'NOM_ANCRAGE1            ',&
     &                      'NOM_ANCRAGE2            ',&
     &                      'NOEUD_MILIEU'/
!
    data          typpa2 /'K8','K8','K24','K8','K8','K24','R','R','K8'/
    data          nompa2 /'TYPE_ANCRAGE1           ',&
     &                      'TYPE_NOEUD1             ',&
     &                      'NOM_ANCRAGE1            ',&
     &                      'TYPE_ANCRAGE2           ',&
     &                      'TYPE_NOEUD2             ',&
     &                      'NOM_ANCRAGE2            ',&
     &                      'TENSION                 ',&
     &                      'RECUL_ANCRAGE           ',&
     &                      'ADHERENT'                 /
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
    call jemarq()
    call infmaj()
!
    call getres(nomu, concep, cmd)
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 1   SAISIE DES ARGUMENTS POUR VERIFICATION AVANT EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call getfac('DEFI_CABLE', nbcabl)
    nbcabl = abs(nbcabl)
    call getvr8(' ', 'TENSION_INIT', scal=f0, nbret=ibid)
    call getvr8(' ', 'RECUL_ANCRAGE', scal=delta, nbret=ibid)
    call getvtx(' ', 'ADHERENT', scal=adher, nbret=ibid)
    valr(1)=f0
    valr(2)=delta
    valk(7)= adher
!
    if (adher .eq. 'NON') call u2mess('I', 'MODELISA3_39')
!
    call getvtx(' ', 'TYPE_RELAX', scal=typrel, nbret=ibid)
    if (typrel .eq. 'BPEL') then
        relax = .true.
        call getvr8(' ', 'R_J', scal=trelax, nbret=ibid)
    else if (typrel(1:4).eq.'ETCC') then
        relax = .true.
        call getvr8(' ', 'NBH_RELAX', scal=trelax, nbret=ibid)
    else
        relax = .false.
    endif
!
!   CREATION DE LA TABLE CONTENANT LES MOTS-CLES UTILES DANS CALC_PRECONT
    call jeexin(nomu//'           .LTNT', iret)
    if (iret .eq. 0) call ltcrsd(nomu, 'G')
    nomg19 = ' '
    call ltnotb(nomu, 'CABLE_GL', nomg19)
    call jeexin(nomg19//'.TBBA', iret)
    if (iret .eq. 0) call tbcrsd(nomg19, 'G')
    call tbajpa(nomg19, nbpar2, nompa2, typpa2)
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 2   VERIFICATION DES ARGUMENTS AVANT EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    do icabl = 1, nbcabl
!
        call getvtx('DEFI_CABLE', 'NOEUD_ANCRAGE', iocc=icabl, nbval=0, nbret=n1)
        call getvtx('DEFI_CABLE', 'GROUP_NO_ANCRAGE', iocc=icabl, nbval=0, nbret=n2)
        nbancr = n1 + n2
        if (abs(nbancr) .ne. 2) then
            write(k3b,'(I3)') icabl
            if (n1 .ne. 0) then
                call u2mesk('F', 'MODELISA5_83', 1, k3b)
            else
                call u2mesk('F', 'MODELISA5_84', 1, k3b)
            endif
        else
            if (n1 .ne. 0) then
                call getvtx('DEFI_CABLE', 'NOEUD_ANCRAGE', iocc=icabl, nbval=2, vect=noancr(1),&
                            nbret=ibid)
                if (noancr(1) .eq. noancr(2)) then
                    write(k3b,'(I3)') icabl
                    call u2mesk('F', 'MODELISA5_85', 1, k3b)
                endif
                valk(2) = 'NOEUD'
                valk(5) = 'NOEUD'
                valk(3) = noancr(1)
                valk(6) = noancr(2)
            else
                call getvtx('DEFI_CABLE', 'GROUP_NO_ANCRAGE', iocc=icabl, nbval=2,&
                            vect=noancr(1), nbret=ibid)
                if (noancr(1) .eq. noancr(2)) then
                    write(k3b,'(I3)') icabl
                    call u2mesk('F', 'MODELISA5_86', 1, k3b)
                endif
                valk(2) = 'GROUP_NO'
                valk(5) = 'GROUP_NO'
                valk(3) = noancr(1)
                valk(6) = noancr(2)
            endif
        endif
!
! TEST DU TYPE D'ANCRAGE
!    LE CATALOGUE ASSURE QU'IL Y A DEUX OCCURENCES DE CE MOT-CLE
        call getvtx(' ', 'TYPE_ANCRAGE', nbval=2, vect=typanc(1), nbret=ibid)
        if (typanc(1)(1:5) .eq. 'ACTIF') then
            valk(1) = 'ACTIF'
        else
            valk(1) = 'PASSIF'
        endif
        if (typanc(2)(1:5) .eq. 'ACTIF') then
            valk(4) = 'ACTIF'
        else
            valk(4) = 'PASSIF'
        endif
!
!    SI TYPES D'ANCRAGE SONT TOUS LES DEUX PASSIFS
        if ((typanc(1).eq.'PASSIF') .and. (typanc(2).eq.'PASSIF')) then
            write(k3b,'(I3)') icabl
!    SI LA TENSION EST NULLE : SIMPLE ALARME
            if (f0 .eq. 0.d0) then
                call u2mesk('A', 'MODELISA5_87', 1, k3b)
!    SI LA TENSION EST NON-NULLE : ARRET FATAL
            else
                call u2mesk('F', 'MODELISA5_88', 1, k3b)
!
            endif
        endif
!
!       REMPLISSAGE DE LA TABLE
        call tbajli(nomg19, nbpar2, nompa2, ibid, valr,&
                    cbid, valk, 0)
!
    end do
!
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   SAISIE DES ARGUMENTS A L'EXECUTION
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    call getvid(' ', 'MODELE', scal=modele, nbret=ibid)
    call getvid(' ', 'CHAM_MATER', scal=chmat, nbret=ibid)
    call getvid(' ', 'CARA_ELEM', scal=caelem, nbret=ibid)
    call titre()
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 4   EXECUTION DES OPERATIONS
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! 4.1 RECUPERATION DU NOM DU CONCEPT MAILLAGE
! ---
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                mailla, iret)
    call dismoi('F', 'NB_MA_MAILLA', mailla, 'MAILLAGE', nbmama,&
                k8b, iret)
    call dismoi('F', 'NB_NO_MAILLA', mailla, 'MAILLAGE', nbnoma,&
                k8b, iret)
!
! 4.2 CREATION DES OBJETS DE TRAVAIL
! ---
    call wkvect('&&OP0180.NBNO_CABLE', 'V V I', nbcabl, jnbno)
!
    nunobe = '&&OP0180.NUMNOE_BET'
    call jecreo(nunobe, 'V V I')
    call jeecra(nunobe, 'LONMAX', nbnoma)
!
    numaca = '&&OP0180.NUMAIL_CAB'
    call jecreo(numaca, 'V V I')
    call jeecra(numaca, 'LONMAX', nbmama)
!
    xnoca = '&&OP0180.X_NOEU_CAB'
    call jecreo(xnoca, 'V V R')
    call jeecra(xnoca, 'LONMAX', nbnoma)
    ynoca = '&&OP0180.Y_NOEU_CAB'
    call jecreo(ynoca, 'V V R')
    call jeecra(ynoca, 'LONMAX', nbnoma)
    znoca = '&&OP0180.Z_NOEU_CAB'
    call jecreo(znoca, 'V V R')
    call jeecra(znoca, 'LONMAX', nbnoma)
!
! 4.3 EXTENSION DES CARTES ELEMENTAIRES : CREATION DE VECTEURS
! --- D'ADRESSES DES CARACTERISTIQUES POINTES PAR LE NUMERO DE
!     MAILLE
!
    ligrmo = modele//'.MODELE    '
!
    carte = chmat//'.CHAMP_MAT '
    cadesc = carte//'.DESC'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) call u2mess('F', 'MODELISA5_89')
    call etenca(carte, ligrmo, iret)
    if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, carte)
!
    carte = caelem//'.CARGENBA  '
    cadesc = carte//'.DESC'
    call jeexin(cadesc, iret)
    if (iret .eq. 0) call u2mess('F', 'MODELISA5_90')
    call etenca(carte, ligrmo, iret)
    if (iret .ne. 0) call u2mesk('F', 'MODELISA3_37', 1, carte)
!
!.... DETERMINATION DU RANG DE LA COMPOSANTE <A1>
!.... DE LA GRANDEUR <CAGNBA>
!
    call jelira(jexnom('&CATA.GD.NOMCMP', 'CAGNBA'), 'LONMAX', ncaba)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'CAGNBA'), 'L', jcaba)
    irana1 = 0
    do icmp = 1, ncaba
        if (zk8(jcaba+icmp-1) .eq. aire) then
            irana1 = icmp
            goto 21
        endif
    end do
21  continue
    if (irana1 .eq. 0) call u2mess('F', 'MODELISA5_91')
!
! 4.4 CREATION DE LA SD TABLE RESULTAT
! ---
    call jeexin(nomu//'           .LTNT', iret)
    if (iret .eq. 0) call ltcrsd(nomu, 'G')
    nomt19 = ' '
    call ltnotb(nomu, 'CABLE_BP', nomt19)
    call jeexin(nomt19//'.TBBA', iret)
    if (iret .eq. 0) call tbcrsd(nomt19, 'G')
    call tbajpa(nomt19, nbpar, nompar, typpar)
!
!
!
! 4.5 CREATION ET INITIALISATION DE LA CARTE ELEMENTAIRE (= 0)
! --- DES CONTRAINTES INITIALES
!
    carsig = nomu//'.CHME.SIGIN'
    call alcart('G', carsig, mailla, 'SIEF_R')
!
    call jelira(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'LONMAX', nsief)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', 'SIEF_R'), 'L', jsief)
    call jeveuo(carsig//'.NCMP', 'E', jncmp)
    call jeveuo(carsig//'.VALV', 'E', jvalv)
    do icmp = 1, nsief
        zk8(jncmp+icmp-1) = zk8(jsief+icmp-1)
        zr(jvalv+icmp-1) = 0.0d0
    end do
    call nocart(carsig, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, nsief)
    zk8(jncmp) = effnor(1)
    zk8(jncmp+1) = effnor(2)
    zk8(jncmp+2) = effnor(3)
!
! 4.6 CREATION DE LA SD DE TYPE LISTE_DE_RELATIONS
! ---
    lirela = nomu//'.LIRELA    '
    call crelrl('REEL', 'REEL', 'G', lirela)
!
! 4.7 CARACTERISATION DE LA TOPOLOGIE DE LA STRUCTURE BETON
! --- ET RECUPERATION DES CARACTERISTIQUES DU MATERIAU CONSTITUTIF
!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! 3   CREATION DE LA CONNECTIVITE INVERSE LIMITEE AU GROUP_MA BETON
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
    ncncin = '&&OP0180.CONNECINVBETON '
    nmabet = '&&OP0180.MABETON '
    comima = '&&OP0180.COOR_MIN_MAX'
    nunobi = '&&OP0180.NU_BET_ICA'
    gromai = '&&OP0180.DIM_MAX_MABET'
!
    call wkvect(comima, 'V V R', 6, ibid)
    call wkvect(gromai, 'V V R', 3, ibid)
!
    call reliem(' ', mailla, 'NU_MAILLE', ' ', 0,&
                1, 'GROUP_MA_BETON', 'GROUP_MA', nmabet, nbmabe)
!
    call jeexin(ncncin, n2)
    call jeveuo(nmabet, 'L', jlimab)
!
    if (n2 .eq. 0) call cncinv(mailla, zi(jlimab), nbmabe, 'V', ncncin)
!
    call tomabe(chmat, nmabet, nbmabe, mailla, nbnoma,&
                mail2d, nbnobe, nunobe, xflu, xret,&
                regl)
!
    call gromab(mailla, nmabet, nbmabe, mail2d, caelem,&
                gromai)
!
    call wkvect(nunobi, 'V V I', nbnobe, ibid)
!
! 4.8 BOUCLE SUR LE NOMBRE DE CABLES
! ---
    do icabl = 1, nbcabl
!
! 4.8.1  CARACTERISATION DE LA TOPOLOGIE DU CABLE
! .....
        call topoca(nomt19, mailla, icabl, nbf0, zi(jnbno),&
                    numaca, quad, sens)
!
! 4.8.2  RECUPERATION DES CARACTERISTIQUES ELEMENTAIRES DU CABLE
! .....
        call caelca(modele, chmat, caelem, irana1, icabl,&
                    zi(jnbno), numaca, quad, regl, relax,&
                    ea, rh1000, mu0, fprg, frco,&
                    frli, sa)
!
! 4.8.3  INTERPOLATION DE LA TRAJECTOIRE DU CABLE
! .....
        call trajca(nomt19, mailla, icabl, zi(jnbno), xnoca,&
                    ynoca, znoca, comima, gromai)
!
! 4.8.X   SELECTION D'UNE PARTIE DES NOEUDS DE BETON
! .....
        call voisca(mailla, nbnobe, nunobe, comima, nbnobi,&
                    nunobi)
!
! 4.8.4  CALCUL DE LA TENSION LE LONG DU CABLE
! .....
        nbnoca= zi(jnbno+icabl-1)
        call tensca(nomt19, icabl, nbnoca, nbf0, f0,&
                    delta, typrel, trelax, xflu, xret,&
                    ea, rh1000, mu0, fprg, frco,&
                    frli, sa, regl)
!
! 4.8.5  MISE A JOUR DE LA CARTE ELEMENTAIRE DES CONTRAINTES INITIALES
! .....
        call sigmca(nomt19, carsig, icabl, zi(jnbno), numaca,&
                    quad)
!
! 4.8.6  DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES DDL DES
! .....  NOEUDS DU CABLE ET CEUX DES NOEUDS DE LA STRUCTURE BETON
!
!......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 2D :
!......  PROJECTION DU CABLE SUR LE MAILLAGE BETON
!
        if (mail2d) then
            call projca(nomt19, lirela, nmabet, nbmabe, mailla,&
                        nbnobi, nunobi, icabl, zi(jnbno), xnoca,&
                        ynoca, znoca)
!
!......  REPRESENTATION DE LA STRUCTURE BETON PAR DES MAILLES 3D :
!......  IMMERSION DU CABLE DANS LE MAILLAGE BETON
!
        else
            call immeca(nomt19, lirela, mailla, nbnobi, nunobi,&
                        icabl, zi( jnbno), xnoca, ynoca, znoca,&
                        ncncin, nmabet)
        endif
!
    end do
!
    call jedema()
!
! --- FIN DE OP0180.
end subroutine
