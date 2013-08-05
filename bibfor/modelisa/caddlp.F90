subroutine caddlp(fonree, char)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvr8.h"
#include "asterfort/afddli.h"
#include "asterfort/aflrch.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/matloc.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utpvlg.h"
#include "asterfort/wkvect.h"
    character(len=4) :: fonree
    character(len=8) :: char
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     CREER LES CARTES CHAR.CHME.CMULT ET CHAR.CHME.CIMPO
!          ET REMPLIR LIGRCH, POUR LE MOT-CLE 'DDL_POUTRE'
!
! IN  : FONREE : 'REEL' OU 'FONC'
! IN  : CHARGE : NOM UTILISATEUR DU RESULTAT DE CHARGE
! ---------------------------------------------------------------------
!
    integer :: nddla
    parameter    ( nddla = 6)
    integer :: ddlimp(nddla), nddli, n1, n2, ioc, i, j, k, ibid
    integer :: ier, nbec, jnoma, nbnoeu, jprnm, jval, ifm, niv
    integer :: jdirec, jdimen, nbno, ialino, ino, nbma, ialima
    integer :: inom, nbcmp, jcompt, nn1(3)
    real(kind=8) :: valimr(nddla), pgl(3, 3), dloc(3), dglo(3), zero
    real(kind=8) :: rln1(3), rgn1(3)
    complex(kind=8) :: valimc(nddla)
!
    character(len=1) :: k1bid
    character(len=8) :: mod, noma, k8bid, nomg
    character(len=8) :: nomn, valimf(nddla), ddl(nddla)
    character(len=16) :: motfac, motcle(nddla), motcl1(2), tymoc1(2), motcl2(2)
    character(len=16) :: tymoc2(2), nomcmd
    character(len=19) :: ligrmo, lisrel, k19bid
    character(len=24) :: nomnoe, ncncin
    integer :: iarg
! ----------------------------------------------------------------------
    data motcle / 'DX' , 'DY' , 'DZ' , 'DRX' , 'DRY' , 'DRZ' /
! ----------------------------------------------------------------------
    call jemarq()
!
    motfac = 'DDL_POUTRE      '
    call getfac(motfac, nddli)
    if (nddli .eq. 0) goto 9999
    call getres(k8bid, k8bid, nomcmd)
!
    call infniv(ifm, niv)
!
    if (nomcmd(11:14) .eq. 'MECA') then
        nomg='DEPL_R'
    else if (nomcmd(11:14).eq.'THER') then
        nomg='TEMP_R'
    else if (nomcmd(11:14).eq.'ACOU') then
        nomg='PRES_C'
    else
        ASSERT(.false.)
    endif
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg), 'L', inom)
    call jelira(jexnom('&CATA.GD.NOMCMP', nomg), 'LONMAX', nbcmp, k1bid)
!
    lisrel = '&&CADDLP.RLLISTE'
    motcl1(1) = 'NOEUD'
    tymoc1(1) = 'NOEUD'
    motcl1(2) = 'GROUP_NO'
    tymoc1(2) = 'GROUP_NO'
!
    motcl2(1) = 'MAILLE'
    tymoc2(1) = 'MAILLE'
    motcl2(2) = 'GROUP_MA'
    tymoc2(2) = 'GROUP_MA'
!
    zero = 0.d0
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE ---
!
    call dismoi('F', 'NOM_MODELE', char(1:8), 'CHARGE', ibid,&
                mod, ier)
    ligrmo = mod(1:8)//'.MODELE'
!
    call dismoi('F', 'NB_NO_MAILLA', ligrmo, 'LIGREL', n1,&
                k8bid, ier)
    call jelira(ligrmo//'.PRNM', 'LONMAX', n2, k1bid)
    nbec = n2/n1
    if (nbec .gt. 10) then
        call u2mess('F', 'MODELISA2_46')
    endif
!
! --- MAILLAGE ASSOCIE AU MODELE ---
!
    call jeveuo(ligrmo//'.LGRF', 'L', jnoma)
    noma = zk8(jnoma)
!
    nomnoe = noma//'.NOMNOE'
    call jelira(nomnoe, 'NOMMAX', nbnoeu, k1bid)
!
    call jeveuo(ligrmo//'.PRNM', 'L', jprnm)
!
    ncncin = '&&CADDLP.CONNECINVERSE  '
    call jeexin(ncncin, n1)
    if (n1 .eq. 0) call cncinv(noma, ibid, 0, 'V', ncncin)
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
    if (fonree .eq. 'REEL') then
        call wkvect('&&CADDLP.VALDDL', 'V V R', nddla*nbnoeu, jval)
    else
        call u2mesk('F', 'MODELISA2_47', 1, fonree)
    endif
    call wkvect('&&CADDLP.DIRECT', 'V V R', 3*nbnoeu, jdirec)
    call wkvect('&&CADDLP.DIMENSION', 'V V I', nbnoeu, jdimen)
! --------------------------------------------------------------
! 3   BOUCLE SUR LES OCCURENCES DU MOT-CLE FACTEUR DDL IMPOSE
! --------------------------------------------------------------
!
    if (niv .ge. 2) write(ifm,1020)
!
    do 100 ioc = 1, nddli
!
! ------ RECUPERATION DE LA LISTE DES NOEUDS :
        call reliem(' ', noma, 'NU_NOEUD', motfac, ioc,&
                    2, motcl1, tymoc1, '&&CADDLP.NOEUD', nbno)
        call jeveuo('&&CADDLP.NOEUD', 'L', ialino)
        call reliem(mod, noma, 'NU_MAILLE', motfac, ioc,&
                    2, motcl2, tymoc2, '&&CADDLP.MAILLE', nbma)
        if (nbma .ne. 0) then
            call jeveuo('&&CADDLP.MAILLE', 'L', ialima)
        else
            ialima = 1
        endif
!
        call wkvect('&&CADDLP.ICOMPT', 'V V I', nddla, jcompt)
        do 110 k = 1, nbno
            ino = zi(ialino-1+k)
            call jenuno(jexnum(nomnoe, ino), nomn)
!
! --------- MATRICE DE PASSAGE AU REPERE GLOBAL ---
!
            call matloc(noma, ncncin, motfac, ioc, ino,&
                        nbma, zi(ialima), pgl)
!
! --------- RECUPERATION DE LA VALEUR IMPOSEE  (MOCLE(J)):
!           ----------------------------------------------
            if (fonree .eq. 'REEL') then
                do 120 j = 1, 3
                    ddlimp(j) = 0
                    ddl(j) = ' '
                    dloc(j) = zero
                    rln1(j) = zero
                    call getvr8(motfac, motcle(j), ioc, iarg, 1,&
                                dloc(j), nn1(j))
                    if (nn1(j) .ge. 1) rln1(j) = 1.0d0
120              continue
                call utpvlg(1, 3, pgl, dloc, dglo)
                call utpvlg(1, 3, pgl, rln1, rgn1)
                do 122 j = 1, 3
                    if (rgn1(j) .ne. zero) then
                        ddl(j) = motcle(j)
                        valimr(j) = dglo(j)
                        ddlimp(j) = 1
                    endif
122              continue
!
                do 130 j = 1, 3
                    ddlimp(j+3) = 0
                    ddl(j+3) = ' '
                    dloc(j) = zero
                    rln1(j) = zero
                    call getvr8(motfac, motcle(j+3), ioc, iarg, 1,&
                                dloc(j), nn1(j))
                    if (nn1(j) .ge. 1) rln1(j) = 1.0d0
130              continue
                call utpvlg(1, 3, pgl, dloc, dglo)
                call utpvlg(1, 3, pgl, rln1, rgn1)
                do 132 j = 1, 3
                    if (rgn1(j) .ne. zero) then
                        ddl(j+3) = motcle(j+3)
                        valimr(j+3) = dglo(j)
                        ddlimp(j+3) = 1
                    endif
132              continue
            endif
            if (niv .ge. 2) then
                i = 0
                do 77 j = 1, nddla
                    if (ddlimp(j) .eq. 0) goto 77
                    if (i .eq. 0) then
                        write(ifm,1000) nomn, ddl(j), valimr(j)
                    else
                        write(ifm,1010) ddl(j), valimr(j)
                    endif
                    i = i + 1
77              continue
            endif
!
            call afddli(zr(jval), zk8(jval), zc(jval), zi(jprnm-1+( ino-1)*nbec+1), nddla,&
                        fonree, nomn, ino, ddlimp, valimr,&
                        valimf, valimc, motcle, zr(jdirec+3*(ino-1)), zi(jdimen+ ino-1),&
                        mod, lisrel, zk8(inom), nbcmp, zi(jcompt),&
                        .false., ibid, ibid, k19bid, k19bid,&
                        k19bid, k19bid)
110      continue
        do 111,k=1,nddla
        if (zi(jcompt-1+k) .eq. 0) call u2mesk('F', 'MODELISA2_45', 1, motcle(k))
111      continue
        call jedetr('&&CADDLP.ICOMPT')
100  end do
!
    call aflrch(lisrel, char)
    call jedetr('&&CADDLP.VALDDL')
    call jedetr('&&CADDLP.DIRECT')
    call jedetr('&&CADDLP.DIMENSION')
    call jedetr('&&CADDLP.NUNOTMP')
    call jeexin(ncncin, n1)
    if (n1 .ne. 0) call jedetr(ncncin)
!
9999  continue
!
    1020 format( '"DDL_POUTRE" DANS LE REPERE GLOBAL : ' )
    1000 format( /,'NOEUD = ',a8,', ',a8,' = ',1p,e12.5 )
    1010 format(                  18x,a8,' = ',1p,e12.5 )
!
    call jedema()
!
end subroutine
