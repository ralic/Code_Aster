subroutine op0046()
!
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
!
    implicit none
!
! ----------------------------------------------------------------------
!
! COMMANDE:  MECA_STATIQUE
!
! ----------------------------------------------------------------------
!
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterc/r8vide.h"
#include "asterfort/allir8.h"
#include "asterfort/assert.h"
#include "asterfort/cochre.h"
#include "asterfort/copisd.h"
#include "asterfort/detmat.h"
#include "asterfort/dismoi.h"
#include "asterfort/fointe.h"
#include "asterfort/gnomsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecalc.h"
#include "asterfort/mecham.h"
#include "asterfort/mechti.h"
#include "asterfort/mestat.h"
#include "asterfort/nmlect.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rssepa.h"
#include "asterfort/titre.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrcins.h"
#include "asterfort/vrcref.h"
    character(len=6) :: nompro
    parameter ( nompro = 'OP0046' )
!
    integer :: ibid, nh, nbchre, n1, n4, n5, n7
    integer :: ierd, iordr, nbmax, nchar, jchar
    integer :: iocc, nfon, iainst, iret, i, jordr, nbuti
    integer :: ifm, niv, ier
!
    real(kind=8) :: temps, time, alpha
    real(kind=8) :: rundf
!
    character(len=1) :: base, typcoe
    character(len=2) :: codret
    character(len=8) :: k8bla, result, listps, nomode, noma
    character(len=8) :: nomfon, charep, kstr
    character(len=16) :: nosy
    character(len=19) :: solveu, lischa, ligrel, lisch2
    character(len=19) :: matass
    character(len=24) :: modele, carele, charge, fomult
    character(len=24) :: chtime, chamgd
    character(len=24) :: chamel, chstrx
    character(len=24) :: chgeom, chcara(18), chharm
    character(len=24) :: chvarc, chvref
    character(len=24) :: mate
    character(len=24) :: k24bla, noobj
    character(len=24) :: compor
!
    logical :: exipou
!
    complex(kind=8) :: calpha
    integer :: iarg
! DEB ------------------------------------------------------------------
!
    call jemarq()
    rundf=r8vide()
!
! -- TITRE
!
    call titre()
    call infmaj()
    call infdbg('MECA_STATIQUE', ifm, niv)
!
! -- INITIALISATIONS
!
    base ='G'
    solveu = '&&'//nompro//'.SOLVEUR   '
    lischa = '&&'//nompro//'.LISCHA    '
    matass = '&&'//nompro//'_MATR_ASSEM'
    chtime = ' '
    charge = ' '
    nh = 0
    typcoe = ' '
    charep = ' '
    k24bla = ' '
    k8bla = ' '
    alpha = 0.d0
    calpha = (0.d0 , 0.d0)
    nfon = 0
    chvarc='&&OP0046.VARC'
    chvref='&&OP0046.VREF'
!
! --- LECTURE DES OPERANDES DE LA COMMANDE
!
    call nmlect(result, modele, mate, carele, compor,&
                lischa, solveu)
!
! -- ACCES A LA LISTE DES CHARGGES
!
    charge = lischa//'.LCHA'
    fomult = lischa//'.FCHA'
!
! -- ACCES A LA LISTE D'INSTANTS
!
    call getvid(' ', 'LIST_INST', 0, iarg, 1,&
                listps, n4)
    if (n4 .eq. 0) then
        call getvr8(' ', 'INST', 0, iarg, 1,&
                    temps, n5)
        if (n5 .eq. 0) then
            temps = 0.d0
        endif
        listps = result
        call allir8('V', listps, 1, [temps])
    endif
!
! ---- CALCUL MECANIQUE
!
    call mestat(modele, fomult, lischa, mate, carele,&
                listps, solveu, compor, matass)
!
! ---- CALCUL DE L'OPTION SIEF_ELGA OU RIEN
!
    nomode = modele(1:8)
    ligrel = nomode//'.MODELE'
!
    call dismoi('F', 'NOM_MAILLA', nomode, 'MODELE', ibid,&
                noma, ierd)
    call dismoi('F', 'NB_CHAMP_MAX', result, 'RESULTAT', nbmax,&
                k8bla, ierd)
    call getvtx(' ', 'OPTION', 0, iarg, 1,&
                nosy, n7)
    ASSERT(nosy.eq.'SIEF_ELGA'.or.nosy.eq.'SANS')
!
!   A-t-on des POU_D_EM qui utilisent le champ STRX_ELGA en lineaire
    call dismoi('F', 'EXI_STR2', nomode, 'MODELE', ibid,&
                kstr, ierd)
    if ((nosy.eq.'SANS') .and. (kstr(1:3).eq.'NON')) goto 9999
!
!   A-t-on des VARC
    call dismoi('F', 'EXI_VARC', mate, 'CHAM_MATER', ibid,&
                k8bla, iret)
!   On interdit provisoirement les POU_D_EM avec les VARC
    if ( (k8bla(1:3).eq.'OUI').and.(kstr(1:3).eq.'OUI')) then
        call u2mess('F', 'MECASTATIQUE_1')
    endif
!
    exipou = .false.
!
    call dismoi('F', 'EXI_POUX', modele, 'MODELE', ibid,&
                k8bla, ierd)
    if (k8bla(1:3) .eq. 'OUI') exipou = .true.
    call jelira(charge, 'LONMAX', nchar, k8bla)
!
    if (exipou) then
!
        call jeveuo(charge, 'L', jchar)
        call cochre(zk24(jchar), nchar, nbchre, iocc)
        if (nbchre .gt. 1) then
            call u2mess('F', 'MECASTATIQUE_25')
        endif
!
        typcoe = 'R'
        alpha = 1.d0
        if (iocc .gt. 0) then
            call getvid('EXCIT', 'CHARGE', iocc, iarg, 1,&
                        charep, n1)
            call getvid('EXCIT', 'FONC_MULT', iocc, iarg, 1,&
                        nomfon, nfon)
        endif
    endif
!
    call jeveuo(listps//'           .VALE', 'L', iainst)
    do 13 iordr = 1, nbmax
        call rsexch(' ', result, 'DEPL', iordr, chamgd,&
                    iret)
        if (iret .gt. 0) goto 13
!
        call mecham(nosy, nomode, carele, nh, chgeom,&
                    chcara, chharm, iret)
        if (iret .ne. 0) goto 13
        time = zr(iainst-1+iordr)
        call mechti(chgeom(1:8), time, rundf, rundf, chtime)
        call vrcins(modele, mate, carele, time, chvarc(1:19),&
                    codret)
        call vrcref(modele(1:8), mate(1:8), carele(1:8), chvref(1:19))
!
        if (exipou .and. nfon .ne. 0) then
            call fointe('F ', nomfon, 1, 'INST', time,&
                        alpha, ier)
        endif
!
        if (nosy .eq. 'SIEF_ELGA') then
            call rsexch(' ', result, nosy, iordr, chamel,&
                        iret)
!           -- SI LE CHAMP A DEJE ETE CALCULE :
            if (iret .eq. 0) goto 62
            ibid = 0
            call mecalc(nosy, nomode, chamgd, chgeom, mate,&
                        chcara, k24bla, k24bla, chtime, k24bla,&
                        chharm, k24bla, k24bla, k24bla, k24bla,&
                        k24bla, charep, typcoe, alpha, calpha,&
                        k24bla, k24bla, chamel, k24bla, ligrel,&
                        base, chvarc, chvref, k24bla, compor,&
                        k24bla, k24bla, k8bla, ibid, k24bla,&
                        iret)
            call rsnoch(result, nosy, iordr)
        endif
62      continue
!
        if (kstr(1:3) .eq. 'OUI') then
            ibid = 0
            call rsexch(' ', result, 'STRX_ELGA', iordr, chstrx,&
                        iret)
!         -- SI LE CHAMP A DEJE ETE CALCULE :
            if (iret .eq. 0) goto 13
            call mecalc('STRX_ELGA', nomode, chamgd, chgeom, mate,&
                        chcara, k24bla, k24bla, chtime, k24bla,&
                        chharm, k24bla, k24bla, k24bla, k24bla,&
                        k24bla, charep, typcoe, alpha, calpha,&
                        k24bla, k24bla, chstrx, k24bla, ligrel,&
                        base, chvarc, chvref, k24bla, compor,&
                        k24bla, k24bla, k8bla, ibid, k24bla,&
                        iret)
!
            call rsnoch(result, 'STRX_ELGA', iordr)
        endif
!
!
13  continue
!
!
9999  continue
!
!     ----------------------------------------------------------------
! --- STOCKAGE POUR CHAQUE NUMERO D'ORDRE DU MODELE, DU CHAMP MATERIAU
!     DES CARACTERISTIQUES ELEMENTAIRES ET DES CHARGES DANS LA SD RESU
!     ----------------------------------------------------------------
!             12345678    90123    45678901234
    noobj ='12345678'//'.1234'//'.EXCIT.INFC'
    call gnomsd(' ', noobj, 10, 13)
    lisch2 = noobj(1:19)
    call dismoi('F', 'NB_CHAMP_UTI', result, 'RESULTAT', nbuti,&
                k8bla, ierd)
    call jeveuo(result//'           .ORDR', 'L', jordr)
    do 14 i = 1, nbuti
        iordr=zi(jordr+i-1)
        call rssepa(result, iordr, modele(1:8), mate(1:8), carele(1:8),&
                    lisch2(1:19))
14  continue
!
!     -----------------------------------------------
! --- COPIE DE LA SD INFO_CHARGE DANS LA BASE GLOBALE
!     -----------------------------------------------
    call copisd(' ', 'G', lischa, lisch2(1:19))
!
!     -----------------------------------------------
! --- MENAGE FINAL
!     -----------------------------------------------
!
! --- DESTRUCTION DE TOUTES LES MATRICES CREEES
!
    call detmat()
!
    call jedema()
!
end subroutine
