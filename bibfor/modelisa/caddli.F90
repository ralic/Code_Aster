subroutine caddli(nomcmd, motfac, fonree, char)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getmjm.h"
#include "asterc/getvc8.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/indik8.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=4) :: fonree
    character(len=8) :: char
    character(len=16) :: nomcmd, motfac
! ---------------------------------------------------------------------
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
!
!     BUT: CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
!          ET REMPLIR LIGRCH, EN SE SERVANT DE L'OBJET .PRNM
!          POUR AFFECTER LE BON NOMBRE DE DEGRES DE LIBERTE A CHAQUE NOE
!
! ARGUMENTS D'ENTREE:
!
!      NOMCMD  : NOM DE LA COMMANDE
!      MOTFAC  : DDL_IMPO OU TEMP_IMPO OU PRES_IMPO
!      FONREE  : TYPE DE LA VALEUR IMPOSEE :
!                REEL OU FONC OU COMP
!      CHAR    : NOM UTILISATEUR DU RESULTAT DE CHARGE
!
!-----------------------------------------------------------------------
!---------------- DECLARATION DES VARIABLES LOCALES  -------------------
!
    integer :: nmocl
    parameter (nmocl=300)
    integer :: vali(2)
    integer :: ddlimp(nmocl), nddli, n, nmcl, i, j, nddla, ibid
    integer :: ier, nbec, jnoma, nbnoeu, jprnm, jval
    integer :: jdirec, jdimen, nbno, ialino, k, ino
    integer :: jnoxfl, jnoxfv
    logical :: lxfem
    real(kind=8) :: valimr(nmocl)
    complex(kind=8) :: valimc(nmocl)
!
    character(len=1) :: k1bid
    character(len=3) :: tymocl(nmocl)
    character(len=8) :: mod, noma, k8bid
    character(len=8) :: nomn, valimf(nmocl)
    character(len=16) :: motcle(nmocl), motcl2(5), tymoc2(5)
    character(len=19) :: cnxinv
    character(len=19) :: ligrmo, lisrel
    character(len=19) :: noxfem, ch1, ch2, ch3
    character(len=24) :: nomnoe
!
!--- Variables pour le mot-clef "LIAISON = ENCASTRE"
    integer :: ndlia, liaimp, inom, nbcmp, jcompt
    integer :: icmp1, icmp2, icmp3, icmp4, icmp5, icmp6
    character(len=8) :: nomg
    character(len=72) :: vallia
    integer :: iarg
    data         vallia  /'XXXXXXXX'/
!-------------------------------------------------------------
!
    call jemarq()
!
    motcl2(1) = 'NOEUD'
    tymoc2(1) = 'NOEUD'
    motcl2(2) = 'GROUP_NO'
    tymoc2(2) = 'GROUP_NO'
    motcl2(3) = 'MAILLE'
    tymoc2(3) = 'MAILLE'
    motcl2(4) = 'GROUP_MA'
    tymoc2(4) = 'GROUP_MA'
    motcl2(5) = 'TOUT'
    tymoc2(5) = 'TOUT'
!
    lisrel = '&&CADDLI.RLLISTE'
    call getfac(motfac, nddli)
    if (nddli .eq. 0) goto 999
!
! --- MODELE ASSOCIE A LA CHARGE ---
    call dismoi('F', 'NOM_MODELE', char(1:8), 'CHARGE', ibid,&
                mod, ier)
    ligrmo = mod(1:8)//'.MODELE'
!
    if (nomcmd(11:14) .eq. 'MECA') then
        nomg='DEPL_R'
    else if (nomcmd(11:14).eq.'THER') then
        nomg='TEMP_R'
    else if (nomcmd(11:14).eq.'ACOU') then
        nomg='PRES_C'
    else
        call assert(.false.)
    endif
    call dismoi('F', 'NB_EC', nomg, 'GRANDEUR', nbec,&
                k8bid, ier)
    call assert(nbec.le.10)
!
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
!
! --- MAILLAGE ASSOCIE AU MODELE ---
!
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
    nomnoe = noma//'.NOMNOE'
    call jelira(nomnoe, 'NOMMAX', nbnoeu, k1bid)
!
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
!    --------------------------------------------------------
!    MODELE X-FEM
!    --------------------------------------------------------
    call jeexin(mod//'.XFEM_CONT', ier)
    if (ier .eq. 0) then
        lxfem = .false.
        noxfem = ' '
        ch1 = ' '
        ch2 = ' '
        ch3 = ' '
    else
        lxfem = .true.
!       CONNECTIVITE INVERSE
        cnxinv='&&CADDLI.CNXINV'
        call cncinv(noma, ibid, 0, 'V', cnxinv)
!
        noxfem = '&&CADDLI.NOXFEM'
        call cnocns(mod//'.NOXFEM', 'V', noxfem)
        call jeveuo(noxfem//'.CNSL', 'L', jnoxfl)
        call jeveuo(noxfem//'.CNSV', 'L', jnoxfv)
!       STATUT DU NOEUD ET LEVEL SETS
        ch1 = '&&CADDLI.CHS1'
        ch2 = '&&CADDLI.CHS2'
        ch3 = '&&CADDLI.CHS3'
        call celces(mod//'.STNO', 'V', ch1)
        call celces(mod//'.LNNO', 'V', ch2)
        call celces(mod//'.LTNO', 'V', ch3)
    endif
!
! --------------------------------------------------------------
! 3   BOUCLE SUR LES OCCURENCES DU MOT-CLE FACTEUR DDL IMPOSE
! --------------------------------------------------------------
!
    do 100 i = 1, nddli
!
! ---------------------------------------------------
! 1.  RECUPERATION DES MOTS-CLES DDL SOUS XXX_IMPO
!     MOTCLE(J) : K8 CONTENANT LE J-EME MOT-CLE DDL
!     NDDLA     : NOMBRE DE MOTS CLES DU TYPE DDL
! ---------------------------------------------------
        call getmjm(motfac, i, 0, motcle, tymocl,&
                    n)
        nmcl = abs(n)
        if (nmcl .gt. nmocl) then
            vali (1) = nmocl
            vali (2) = nmcl
            call u2mesg('F', 'MODELISA8_31', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
        call getmjm(motfac, i, nmcl, motcle, tymocl,&
                    n)
!
! --- RECUPERATION DU MOT-CLEF "LIAISON"
!
!
        nddla = 1
        do 20 j = 1, nmcl
            if (motcle(j) .ne. 'TOUT' .and. motcle(j) .ne. 'GROUP_NO' .and. motcle(j) .ne.&
                'NOEUD' .and. motcle(j) .ne. 'GROUP_MA' .and. motcle(j) .ne. 'MAILLE' .and.&
                motcle(j) .ne. 'EVOL_THER' .and. motcle(j) .ne. 'DDL' .and. motcle(j) .ne.&
                'LIAISON') then
                motcle(nddla) = motcle(j)
                nddla = nddla + 1
            endif
!
20      continue
        nddla = nddla - 1
!
        ndlia = 0
        do 30 j = 1, nmcl
            if (motcle(j) .eq. 'LIAISON') then
                ndlia = 1
                nddla = 6
                motcle(1) = 'DX'
                motcle(2) = 'DY'
                motcle(3) = 'DZ'
                motcle(4) = 'DRX'
                motcle(5) = 'DRY'
                motcle(6) = 'DRZ'
!
                icmp1 = indik8(zk8(inom),motcle(1) (1:8),1,nbcmp)
                icmp2 = indik8(zk8(inom),motcle(2) (1:8),1,nbcmp)
                icmp3 = indik8(zk8(inom),motcle(3) (1:8),1,nbcmp)
                icmp4 = indik8(zk8(inom),motcle(4) (1:8),1,nbcmp)
                icmp5 = indik8(zk8(inom),motcle(5) (1:8),1,nbcmp)
                icmp6 = indik8(zk8(inom),motcle(6) (1:8),1,nbcmp)
            endif
30      continue
!
!
! ---------------------------------------------------
! 2   ALLOCATION DE TABLEAUX DE TRAVAIL
! ---------------------------------------------------
!   OBJETS INTERMEDIAIRES PERMETTANT D'APPLIQUER LA REGLE DE SURCHARGE
!
!        -  VECTEUR (K8) CONTENANT LES NOMS DES NOEUDS
!        -  TABLEAU DES VALEURS DES DDLS DES NOEUDS BLOQUES
!                         DIM NBNOEU * NBCOMP
!        -  VECTEUR (IS) CONTENANT LE DESCRIPTEUR GRANDEUR ASSOCIE AUX
!                         DDLS IMPOSES PAR NOEUD
!
        if (nddla .ne. 0) then
            if (fonree .eq. 'REEL') then
                call wkvect('&&CADDLI.VALDDL', 'V V R', nddla*nbnoeu, jval)
            else if (fonree.eq.'COMP') then
                call wkvect('&&CADDLI.VALDDL', 'V V C', nddla*nbnoeu, jval)
            else if (fonree.eq.'FONC') then
                call wkvect('&&CADDLI.VALDDL', 'V V K8', nddla*nbnoeu, jval)
            else
                call assert(.false.)
            endif
            call wkvect('&&CADDLI.DIRECT', 'V V R', 3*nbnoeu, jdirec)
            call wkvect('&&CADDLI.DIMENSION', 'V V I', nbnoeu, jdimen)
        endif
!
!        3.1- RECUPERATION DE LA LISTE DES NOEUDS :
!        ----------------------------------------------
        call reliem(' ', noma, 'NU_NOEUD', motfac, i,&
                    5, motcl2, tymoc2, '&&CADDLI.NUNOTMP', nbno)
        if (nbno .eq. 0) goto 98
        call jeveuo('&&CADDLI.NUNOTMP', 'L', ialino)
!
!
!
!        3.2- RECUPERATION DE LA VALEUR IMPOSEE  (MOCLE(J)):
!        ---------------------------------------------------
        if (fonree .eq. 'REEL') then
            do 40 j = 1, nddla
                call getvr8(motfac, motcle(j), i, iarg, 1,&
                            valimr(j), ddlimp(j))
40          continue
!
        else if (fonree.eq.'COMP') then
            do 50 j = 1, nddla
                call getvc8(motfac, motcle(j), i, iarg, 1,&
                            valimc(j), ddlimp(j))
50          continue
!
!
        else if (fonree.eq.'FONC') then
            do 60 j = 1, nddla
                call getvid(motfac, motcle(j), i, iarg, 1,&
                            valimf(j), ddlimp(j))
60          continue
        endif
!
!
!
!        3.3- AFFECTATION DES RELATIONS LINEAIRES :
!        ----------------------------------------------
        call wkvect('&&CADDLI.ICOMPT', 'V V I', max(nddla, 1), jcompt)
        do 90 k = 1, nbno
            ino = zi(ialino-1+k)
            call jenuno(jexnum(nomnoe, ino), nomn)
!---  GESTION DU MOT-CLEF "LIAISON"
            do 80 j = 1, ndlia
                vallia='XXXXXXXX'
                call getvtx(motfac, 'LIAISON', i, iarg, 1,&
                            vallia, liaimp)
                if (vallia .eq. 'ENCASTRE') then
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp1)) then
                        valimr(1) = 0.d0
                        valimc(1) = (0.d0,0.d0)
                        valimf(1) = '&FOZERO'
                        ddlimp(1) = 1
                    endif
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp2)) then
                        valimr(2) = 0.d0
                        valimc(2) = (0.d0,0.d0)
                        valimf(2) = '&FOZERO'
                        ddlimp(2) = 1
                    endif
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp3)) then
                        valimr(3) = 0.d0
                        valimc(3) = (0.d0,0.d0)
                        valimf(3) = '&FOZERO'
                        ddlimp(3) = 1
                    endif
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp4)) then
                        valimr(4) = 0.d0
                        valimc(4) = (0.d0,0.d0)
                        valimf(4) = '&FOZERO'
                        ddlimp(4) = 1
                    endif
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp5)) then
                        valimr(5) = 0.d0
                        valimc(5) = (0.d0,0.d0)
                        valimf(5) = '&FOZERO'
                        ddlimp(5) = 1
                    endif
                    if (exisdg(zi(jprnm-1+ (ino-1)*nbec+1),icmp6)) then
                        valimr(6) = 0.d0
                        valimc(6) = (0.d0,0.d0)
                        valimf(6) = '&FOZERO'
                        ddlimp(6) = 1
                    endif
                endif
80          continue
!
            call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+ (ino- 1)*nbec+1), nddla,&
                        fonree, nomn, ino, ddlimp, valimr,&
                        valimf, valimc, motcle, zr(jdirec+3*(ino-1)), 0,&
                        mod, lisrel, zk8( inom), nbcmp, zi(jcompt),&
                        lxfem, jnoxfl, jnoxfv, ch1, ch2,&
                        ch3, cnxinv)
90      continue
!
!       -- IL NE FAUT PAS GRONDER L'UTILISATEUR SI 'ENCASTRE' :
        if (vallia .ne. 'ENCASTRE') then
            do 91,k=1,nddla
            if (zi(jcompt-1+k) .eq. 0) call u2mesk('F', 'MODELISA2_45', 1, motcle(k))
91          continue
        endif
!
98      continue
        call jedetr('&&CADDLI.ICOMPT')
!
        call jedetr('&&CADDLI.VALDDL')
        call jedetr('&&CADDLI.DIRECT')
        call jedetr('&&CADDLI.DIMENSION')
        call jedetr('&&CADDLI.NUNOTMP')
!
100  end do
!
    call aflrch(lisrel, char)
!
    if (lxfem) then
        call jedetr(cnxinv)
        call detrsd('CHAM_NO_S', noxfem)
        call detrsd('CHAM_ELEM_S', ch1)
        call detrsd('CHAM_ELEM_S', ch2)
        call detrsd('CHAM_ELEM_S', ch3)
    endif
!
999  continue
    call jedema()
end subroutine
