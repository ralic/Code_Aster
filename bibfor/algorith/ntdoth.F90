subroutine ntdoth(modele, mate, carele, fomult, matcst,&
                  coecst, infcha, result, nuord)
!
!     THERMIQUE - DONNEES EN THERMIQUE
!     *           **         **
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
! ----------------------------------------------------------------------
!     SAISIE ET VERIFICATION DE LA COHERENCE DES DONNEES THERMIQUES DU
!     PROBLEME.
!
! VAR MODELE  : NOM DU MODELE
! OUT MATE    : NOM DE LA CARTE CODEE DU CHAMP MATERIAU
! OUT CARELE  : CARACTERISTIQUES DES POUTRES ET COQUES
! OUT FOMULT  : LISTE DES FONCTIONS MULTIPLICATIVES
! OUT MATCST  : LOGIQUE INDIQUANT SI LE MATERIAU EST CONSTANT / TEMPS
! OUT COECST  : LOGIQUE INDIQUANT SI LES C.L. SONT CONSTANTES / TEMPS
!               POUR LE RECALCUL OU NON DE LA RIGIDITE DANS OP0025
! VAR INFCHA  : CONTIENT LA LISTE DES CHARGES ET DES INFOS SUR
!               SUR LES CHARGES
! IN  : RESULT : NOM DE LA SD RESULTAT
! IN  : NUORD  : NUMERO D'ORDRE
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsutnu.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nuord, np, nc, iexcit, jordr
    integer :: jlcha, jinfc, jfcha
    integer :: nbordr
    character(len=8) :: result, crit
    character(len=16) :: k16bid, nomcmd
    character(len=19) :: infcha, knume, excit
    character(len=24) :: modele, carele, fomult, mate
    real(kind=8) :: prec
    logical :: matcst, coecst
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'NTDOTH' )
!
    integer :: n1, nchar, ialich, ibid, ierd, ialifc, ich, iret, jinf, jpro
    integer :: jval, k, nchci
    character(len=8) :: lchci, k8bid, cara, mode, typch, parcha, repk, materi
    character(len=8) :: blan8
    character(len=16) :: nommod, nomexc, nomcar
    character(len=24) :: ligrch, lchin, nomfct, nomcha
    logical :: fmult
!
! --- NOMBRE MAXIMUM DE TYPE DE CHARGE : NBTYCH
!
    integer :: nbtych
    parameter   (nbtych = 11)
    character(len=6) :: nomlig(nbtych)
!
    data nomlig/&
     &     '.CIMPO'  ,'.SOURE'  ,'.FLURE'  ,'.FLUR2'  ,&
     &     '.T_EXT'  ,'.COEFH'  ,'.HECHP'  ,'.GRAIN'  ,'.FLUNL'  ,&
     &     '.SOUNL'  , '.RAYO'   /
!
!====
! 1. PREALABLES
!====
    call jemarq()
    blan8 ='        '
    nommod = 'MODELE'
    nomcar = 'CARA_ELEM'
    nomexc = 'EXCIT'
    coecst = .true.
    iexcit = 1
!
    call getres(k8bid, k16bid, nomcmd)
    if (nomcmd .eq. 'LIRE_RESU') goto 500
!====
! 2. RECUPERATIONS
!====
    if ((nomcmd.eq.'CALC_CHAMP') .or. (nomcmd.eq.'POST_ELEM')) then
!
!     RECUPERATION DU PREMIER NUMERO D'ORDRE
!     --------------------------------------
        knume = '&&'//nompro//'.NUME_ORDRE'
        call jeexin(knume, iret)
        if (iret .ne. 0) call jedetr(knume)
        call getvr8(' ', 'PRECISION', scal=prec, nbret=np)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=nc)
        call rsutnu(result, ' ', 0, knume, nbordr,&
                    prec, crit, iret)
        call jeveuo(knume, 'L', jordr)
        nuord = zi(jordr)
!
!     RECUPERATION DU MODELE, MATERIAU, CARA_ELEM et EXCIT
!     POUR LE NUMERO D'ORDRE NUORDR
!
        call rslesd(result, nuord, modele(1:8), materi, carele(1:8),&
                    excit, iexcit)
!
        if (materi .ne. blan8) then
            call rcmfmc(materi, mate)
        else
            mate = ' '
        endif
        cara = carele
    else
!
! 2.1. ==> LE MODELE
        call getvid(' ', nommod, scal=mode, nbret=n1)
        modele = mode
!
! 2.2. ==> LE MATERIAU
        materi = ' '
        call getvid(' ', 'CHAM_MATER', scal=materi, nbret=n1)
        call dismoi('F', 'THER_F_INST', materi, 'CHAM_MATER', ibid,&
                    repk, ierd)
        matcst = .false.
        if (repk .eq. 'NON') matcst = .true.
        call rcmfmc(materi, mate)
!
! 2.3. ==> LES CARACTERISTIQUES ELEMENTAIRES
        call getvid(' ', nomcar, scal=cara, nbret=n1)
        if (n1 .le. 0) cara = '        '
        carele = cara
    endif
!
500  continue
!====
! 3. LES CHARGES
!====
    if (iexcit .eq. 1) then
        call getfac(nomexc, nchar)
    else
        call jeveuo(excit//'.INFC', 'L', jinfc)
        nchar = zi(jinfc)
    endif
    if (nchar .ne. 0) then
! 3.1. ==> LISTE DES CHARGES
!
        call jedetr(infcha//'.LCHA')
        call wkvect(infcha//'.LCHA', 'V V K24', nchar, ialich)
!
        call jedetr(infcha//'.INFC')
        call wkvect(infcha//'.INFC', 'V V IS', 2*nchar+1, jinf)
!
        zi(jinf) = nchar
        fomult = infcha//'.FCHA'
        call jedetr(fomult)
        call wkvect(fomult, 'V V K24', nchar, ialifc)
        nchci = 0
!
! 3.2. ==> DETAIL DE CHAQUE CHARGE
!
        do 32 , ich = 1 , nchar
!
        if (iexcit .eq. 1) then
            call getvid(nomexc, 'CHARGE', iocc=ich, scal=nomcha, nbret=n1)
            zk24(ialich+ich-1) = nomcha
        else
            call jeveuo(excit//'.LCHA', 'L', jlcha)
            zk24(ialich+ich-1) = zk24(jlcha+ich-1)
            nomcha = zk24(jlcha+ich-1)
        endif
!
! 3.2.2. ==> TYPES DE CHARGES UTILISEES
!
        call dismoi('F', 'TYPE_CHARGE', nomcha, 'CHARGE', ibid,&
                    typch, ierd)
        if ((typch(1:5).ne.'THER_') .and. (typch(1:5).ne.'CITH_')) then
            call utmess('E', 'ALGORITH9_5', sk=nomcha(1:8))
        endif
        ligrch = nomcha(1:8)//'.CHTH.LIGRE'
!
! 3.2.3. ==> ON REGARDE LES CHARGES DU TYPE DIRICHLET PAR AFFE_CHAR_CINE
!
        if (typch(1:5) .eq. 'CITH_') then
            call jeexin(nomcha(1:19)//'.AFCK', iret)
            ASSERT(iret.ne.0)
            if (typch(5:7) .eq. '_FT') then
                zi(jinf+ich) = -3
            else if (typch(5:7).eq.'_FO') then
                zi(jinf+ich) = -2
            else
                zi(jinf+ich) = -1
            endif
        endif
!
! 3.2.4. ==> ON REGARDE LES CHARGES DU TYPE DIRICHLET
!
        lchin = ligrch(1:13)//'.CIMPO.DESC'
        call jeexin(lchin, iret)
        if (iret .ne. 0) then
            if (typch(5:7) .eq. '_FO') then
                zi(jinf+ich) = 2
                call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                            parcha, ierd)
                if (parcha(1:3) .eq. 'OUI') then
                    zi(jinf+ich) = 3
                endif
            else
                zi(jinf+ich) = 1
            endif
        endif
!
! 3.2.5. ==> FONCTIONS MULTIPLICATIVES DES CHARGES
!
        fmult = .false.
        if (iexcit .eq. 1) then
            call getvid(nomexc, 'FONC_MULT', iocc=ich, scal=zk24( ialifc+ich-1), nbret=n1)
        else
            call jeveuo(excit//'.FCHA', 'L', jfcha)
            n1=0
            if (zk24(jfcha-1+ich)(1:2) .ne. '&&') then
                n1 = 1
                zk24(ialifc+ich-1)=zk24(jfcha+ich-1)
            endif
        endif
        if (n1 .eq. 0) then
            nomfct = '&&'//nompro
            call jeexin(nomfct(1:19)//'.PROL', iret)
            if (iret .eq. 0) then
                ASSERT(lxlgut(nomfct).le.24)
                call wkvect(nomfct(1:19)//'.PROL', 'V V K24', 6, jpro)
                zk24(jpro) = 'CONSTANT'
                zk24(jpro+1) = 'CONSTANT'
                zk24(jpro+2) = 'TOUTPARA'
                zk24(jpro+3) = 'TOUTRESU'
                zk24(jpro+4) = 'CC      '
                zk24(jpro+5) = nomfct
!
                call wkvect(nomfct(1:19)//'.VALE', 'V V R', 2, jval)
                zr(jval) = 1.0d0
                zr(jval+1)= 1.0d0
            endif
            zk24(ialifc+ich-1) = '&&'//nompro
        else
            fmult = .true.
        endif
!
! 3.2.6. ==> ON REGARDE LES AUTRES CHARGES
!
        do 326 , k = 2 ,nbtych
        lchin = ligrch(1:13)//nomlig(k)//'.DESC'
        call exisd('CHAMP_GD', lchin, iret)
        if (iret .ne. 0) then
            if ((k.ge.7) .and. fmult) then
                call utmess('F', 'ALGORITH9_7', sk=nomcha(1:8))
            endif
            if (typch(5:7) .eq. '_FO') then
                zi(jinf+nchar+ich) = max(2,zi(jinf+nchar+ich))
                call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                            parcha, ierd)
                if (parcha(1:3) .eq. 'OUI') then
!
!               IL EST INUTILE DE REASSEMBLER LA MATRICE DE RIGIDITE
!               SI T_EXT VARIE SEULE (ET COEFH CONSTANT)
!               TRAVAIL A COMPLETER POUR SOURCE, FLUX,...
!
                    if (nomlig(k) .ne. '.T_EXT') then
                        coecst = .false.
                    endif
                    zi(jinf+nchar+ich) = max(3,zi(jinf+nchar+ ich))
                endif
            else
                zi(jinf+nchar+ich) = max(1,zi(jinf+nchar+ich))
            endif
        endif
326      continue
!
        32     end do
!
        if (nchci .gt. 0) call jeecra(lchci, 'LONUTI', nchci)
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
!
end subroutine
