subroutine chpass(tychr, ma, celmod, nomgd, prol0,&
                  chou)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescar.h"
#include "asterfort/cescel.h"
#include "asterfort/cesred.h"
#include "asterfort/chsfus.h"
#include "asterfort/chsut1.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnsred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: ma, chou
    character(len=*) :: celmod
    character(len=4) :: tychr, tych2
!     -----------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!     BUT : TRAITER :
!          - OPTION 'ASSE' DE LA COMMANDE CREA_CHAMP
!     -----------------------------------------------------------------
!
!
    integer :: n1, ib, nbocc, iocc, nbtrou, jnutro, nbmocl, lnom, ibid
    logical :: chgcmp, cumul, lcumul(2)
    integer :: ncmp, jlicmp, gd, jcmpgd, jlicm2, iret, nncp, nchg
    real(kind=8) :: coefr, lcoefr(2)
    complex(kind=8) :: coefc, lcoefc(2)
    character(len=8) :: kbid, modele
    character(len=8) :: champ, nomgd, nomgd2, ma2
    character(len=3) :: prol0, tsca
    character(len=16) :: limocl(5), tymocl(5), typem
    character(len=19) :: chs1, chs2, nutrou, lichs(2), cesmod, option
    character(len=19) :: chs3, ligrel
    character(len=24) :: cnom, valk(3)
!
    logical :: lcoc
!     -----------------------------------------------------------------
!
    call jemarq()
!
    chs1 = '&&CHPASS.CHS1'
    chs2 = '&&CHPASS.CHS2'
    chs3 = '&&CHPASS.CHS3'
!
!
! 1- CALCUL DE:
!      NOMGD : NOM DE LA GRANDEUR
!      GD    : NUMERO DE LA GRANDEUR
!      JCMPGD: ADRESSE DES CMPS DE LA GRANDEUR
!      PROL0 :/'OUI' POUR PROLONGER PAR ZERO LES CHAM_ELEM
!                    NON DEFINIS PARTOUT
!             /'NON' POUR ARRETER <F> DANS LE CAS PRECEDENT
!      LIGREL: NOM DU LIGREL ASSOCIE A CELMOD
!      OPTION: OPTION ASSOCIEE A CELMOD (SI CHAM_ELEM)
!      CESMOD: CHAM_ELEM_S EQUIVALENT A CELMOD (SI CHAM_ELEM)
!
!     ------------------------------------------------------------------
!
    if (ma .eq. ' ') then
        call utmess('F', 'UTILITAI_27')
    endif
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) then
        call utmess('F', 'CALCULEL_67', sk=nomgd)
    endif
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', jcmpgd)
!
    if (tychr(1:2) .eq. 'EL') then
        call dismoi('NOM_LIGREL', celmod, 'CHAM_ELEM', repk=ligrel)
        call dismoi('NOM_OPTION', celmod, 'CHAM_ELEM', repk=option)
        cesmod = '&&CHPASS.CESMOD'
        call celces(celmod, 'V', cesmod)
!
        modele = ligrel(1:8)
        call exisd('MODELE', modele, iret)
        if (iret .ne. 1) modele = ' '
!
    else
        ligrel = ' '
        option = ' '
        cesmod = ' '
        modele = ' '
    endif
!
!
!     2- BOUCLE DE VERIF SUR LES OCCURENCES DU MOT CLE "ASSE" :
!     ---------------------------------------------------------
    call getfac('ASSE', nbocc)
    cnom = '&&CHPASS.CHAM_GD_LISTE'
    call wkvect(cnom, 'V V K24', nbocc, lnom)
!
    do 10 iocc = 1, nbocc
!
!       2.1 VERIFICATION DES CARACTERISTIQUES DU CHAMP :
!       ------------------------------------------------
        call getvid('ASSE', 'CHAM_GD', iocc=iocc, scal=champ, nbret=ib)
        zk24(lnom+iocc-1) = champ
        call dismoi('TYPE_CHAMP', champ, 'CHAMP', repk=tych2)
        call dismoi('NOM_MAILLA', champ, 'CHAMP', repk=ma2)
        if (ma .ne. ma2) then
            valk(1)=champ
            valk(2)=ma2
            valk(3)=ma
            call utmess('F', 'CALCULEL2_17', nk=3, valk=valk)
        endif
!
!
        valk(1)=champ
        valk(2)=tychr
        if (tychr .eq. 'NOEU') then
            if (tych2 .ne. 'NOEU') then
                call utmess('F', 'UTILITAI_28', nk=2, valk=valk)
            endif
!
        else if (tychr.eq.'ELGA') then
            if ((tych2.ne.'CART') .and. (tych2.ne.'ELEM') .and. (tych2.ne.'ELGA')) then
                call utmess('F', 'UTILITAI_28', nk=2, valk=valk)
            endif
!
        else if (tychr.eq.'ELNO') then
            if ((tych2.ne.'CART') .and. (tych2.ne. 'ELNO')) then
                call utmess('F', 'UTILITAI_28', nk=2, valk=valk)
            endif
!
        else if (tychr.eq.'ELEM') then
            if ((tych2.ne.'CART') .and. (tych2.ne. 'ELEM')) then
                call utmess('F', 'UTILITAI_28', nk=2, valk=valk)
            endif
!
        else if (tychr.eq.'CART') then
            if ((tych2.ne.'CART') .and. (tych2.ne. 'ELEM')) then
                call utmess('F', 'UTILITAI_28', nk=2, valk=valk)
            endif
!
        else
            ASSERT(.false.)
        endif
!
 10 end do
!
!
!
!     3- ARGUMENTS POUR APPELER RELIEM :
!     ---------------------------------
    limocl(1) = 'TOUT'
    tymocl(1) = 'TOUT'
    limocl(2) = 'MAILLE'
    tymocl(2) = 'MAILLE'
    limocl(3) = 'GROUP_MA'
    tymocl(3) = 'GROUP_MA'
    limocl(4) = 'NOEUD'
    tymocl(4) = 'NOEUD'
    limocl(5) = 'GROUP_NO'
    tymocl(5) = 'GROUP_NO'
    nutrou = '&&CHPASS.NU_TROUVES'
    if (tychr .eq. 'NOEU') then
        typem = 'NU_NOEUD'
        nbmocl = 5
!
    else
        typem = 'NU_MAILLE'
        nbmocl = 3
    endif
!
!
!
!     4- BOUCLE SUR LES OCCURENCES DU MOT CLE "ASSE" :
!     -----------------------------------------------------
    nchg = 0
    do 20 iocc = 1, nbocc
        call getvid('ASSE', 'CHAM_GD', iocc=iocc, scal=champ, nbret=ib)
        call dismoi('TYPE_CHAMP', champ, 'CHAMP', repk=tych2)
!
        cumul = .false.
        call getvtx('ASSE', 'CUMUL', iocc=iocc, scal=kbid, nbret=ib)
        if (kbid .eq. 'OUI') cumul = .true.
!
!       4.0 CALCUL DE LA LISTE DES CMPS ET DU BOOLEEN CHGCMP
!        QUI INDIQUE QUE L'ON DOIT MODIFIER LES CMPS ET/OU LA GRANDEUR.
!       ---------------------------------------------------------------
        call getvtx('ASSE', 'NOM_CMP', iocc=iocc, nbval=0, nbret=n1)
        chgcmp = .false.
        if (n1 .lt. 0) then
            ncmp = -n1
            call wkvect('&&CHPASS.LICMP', 'V V K8', ncmp, jlicmp)
            call getvtx('ASSE', 'NOM_CMP', iocc=iocc, nbval=ncmp, vect=zk8(jlicmp),&
                        nbret=ib)
            call getvtx('ASSE', 'NOM_CMP_RESU', iocc=iocc, nbval=0, nbret=n1)
            if (n1 .lt. 0) then
                chgcmp = .true.
                nchg = nchg + 1
                if (n1 .ne. -ncmp) then
                    call utmess('F', 'UTILITAI_31')
                endif
                call wkvect('&&CHPASS.LICMP2', 'V V K8', ncmp, jlicm2)
                call getvtx('ASSE', 'NOM_CMP_RESU', iocc=iocc, nbval=ncmp, vect=zk8(jlicm2),&
                            nbret=ib)
            endif
!
        else
            ncmp = 0
            jlicmp = 0
        endif
!
!       4.1 VERIFICATION DE LA GRANDEUR ASSOCIEE AU CHAMP
!       ------------------------------------------------------
        call dismoi('NOM_GD', champ, 'CHAMP', repk=nomgd2)
        if ((.not.chgcmp) .and. (nomgd2.ne.nomgd)) then
            valk(1)=champ
            valk(2)=nomgd
            valk(3)=nomgd2
            call utmess('F', 'UTILITAI_32', nk=3, valk=valk)
        endif
!
!
        call dismoi('TYPE_SCA', nomgd2, 'GRANDEUR', repk=tsca)
        call getvc8('ASSE', 'COEF_C', iocc=iocc, scal=coefc, nbret=iret)
        if (iret .ne. 0) then
            if (tsca .ne. 'C') then
                call utmess('F', 'UTILITAI_33')
            endif
            lcoc = .true.
!
        else
            lcoc = .false.
            call getvr8('ASSE', 'COEF_R', iocc=iocc, scal=coefr, nbret=ib)
        endif
!
!       4.2 RECUPERATION DE LA LISTE DES NOEUDS OU MAILLES :
!       ----------------------------------------------------
        call reliem(modele, ma, typem, 'ASSE', iocc,&
                    nbmocl, limocl, tymocl, nutrou, nbtrou)
        call jeveuo(nutrou, 'L', jnutro)
!
!
!       4.3 TRANSFORMATION ET REDUCTION DU CHAMP :
!       ------------------------------------------
        if (tych2 .eq. 'NOEU') then
            call cnocns(champ, 'V', chs1)
            call cnsred(chs1, nbtrou, zi(jnutro), ncmp, zk8(jlicmp),&
                        'V', chs2)
!
        else if (tych2(1:2).eq.'EL') then
            call celces(champ, 'V', chs1)
            call cesred(chs1, nbtrou, zi(jnutro), ncmp, zk8(jlicmp),&
                        'V', chs2)
!
        else if (tych2.eq.'CART') then
            if (tychr .eq. 'CART') then
                call carces(champ, 'ELEM', cesmod, 'V', chs1,&
                            'A', ib)
            else
                call carces(champ, tychr, cesmod, 'V', chs1,&
                            'A', ib)
            endif
            call cesred(chs1, nbtrou, zi(jnutro), ncmp, zk8(jlicmp),&
                        'V', chs2)
!
        else
            ASSERT(.false.)
        endif
!
!
!       4.4 SI ON DOIT CHANGER LES CMPS ET/OU LA GRANDEUR :
!       ----------------------------------------------------
        if (chgcmp) then
            call chsut1(chs2, nomgd, ncmp, zk8(jlicmp), zk8(jlicm2),&
                        'V', chs2)
        endif
!
!
!       4.4 FUSION DU CHAMP REDUIT AVEC LE CHAMP RESULTAT :
!       ----------------------------------------------------
        if (iocc .eq. 1) then
            call chsfus(1, chs2, [.false.], [coefr], [coefc],&
                        lcoc, 'V', chs3)
        else
            lichs(1) = chs3
            lichs(2) = chs2
            lcumul(1) = .false.
            lcumul(2) = cumul
            lcoefr(1) = 1.d0
            lcoefr(2) = coefr
            lcoefc(1) = 1.d0
            lcoefc(2) = coefc
            call chsfus(2, lichs, lcumul, lcoefr, lcoefc,&
                        lcoc, 'V', chs3)
        endif
!
        call jedetr('&&CHPASS.LICMP')
        call jedetr('&&CHPASS.LICMP2')
 20 end do
!
!     5 TRANSFORMATION DU CHAMP_S EN CHAMP :
!     ----------------------------------------------------
    if (tychr .eq. 'NOEU') then
        call cnscno(chs3, ' ', 'NON', 'G', chou,&
                    'F', ibid)
    else if (tychr(1:2).eq.'EL') then
        call cescel(chs3, ligrel, option, ' ', prol0,&
                    nncp, 'G', chou, 'F', ibid)
    else if (tychr.eq.'CART') then
        call cescar(chs3, chou, 'G')
    else
        ASSERT(.false.)
    endif
!
!
!     6- MENAGE :
!     -----------------------------------------------------
    call detrsd('CHAM_NO_S', chs1)
    call detrsd('CHAM_NO_S', chs2)
    call detrsd('CHAM_NO_S', chs3)
    call detrsd('CHAM_ELEM_S', chs1)
    call detrsd('CHAM_ELEM_S', chs2)
    call detrsd('CHAM_ELEM_S', chs3)
!
    call detrsd('CHAM_ELEM_S', cesmod)
    call jedetr(nutrou)
!
    call jedema()
end subroutine
