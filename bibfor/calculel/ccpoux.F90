subroutine ccpoux(resuin, typesd, nordre, nbchre, ioccur,&
                  kcharg, modele, nbpain, lipain, lichin,&
                  suropt, iret)
    implicit none
!     --- ARGUMENTS ---
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/focste.h"
#include "asterfort/fointe.h"
#include "asterfort/fozero.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
    integer :: nbpain, nordre, nbchre, ioccur, iret
    character(len=8) :: resuin, lipain(*)
    character(len=16) :: typesd
    character(len=19) :: kcharg
    character(len=24) :: lichin(*), suropt
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!  CALC_CHAMP - POUTRES POUX
!  -    -               ----
! ----------------------------------------------------------------------
!
!  CALC_CHAMP ET POUTRE POUX
!
! IN  :
!   RESUIN  K8   NOM DE LA STRUCUTRE DE DONNEES RESULTAT IN
!   TYPESD  K16  TYPE DE LA STRUCTURE DE DONNEES RESULTAT
!   NORDRE  I    NUMERO D'ORDRE COURANT
!   NBCHRE  I    NOMBRE DE CHARGES REPARTIES (POUTRES)
!   IOCCUR  I    NUMERO D'OCCURENCE OU SE TROUVE LE CHARGE REPARTIE
!   KCHARG  K19  NOM DE L'OBJET JEVEUX CONTENANT LES CHARGES
!   MODELE  K8   NOM DU MODELE
!   NBPAIN  I    NOMBRE DE PARAMETRES IN
!   LIPAIN  K8*  LISTE DES PARAMETRES IN
!   LICHIN  K8*  LISTE DES CHAMPS IN
!   SUROPT  K24
!
! OUT :
!   IRET    I    CODE RETOUR (0 SI OK, 1 SINON)
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    aster_logical :: exif1d
!
    integer :: ltymo, lfreq, neq, lvale, lacce, ii, i
    integer :: l1, l3, n1, ipara, ier, linst
!
    real(kind=8) :: zero, un, coeff, valres
    real(kind=8) :: alpha, tps(11), freq, inst
    parameter    (zero=0.d0,un=1.d0)
!
    complex(kind=8) :: czero, calpha, tpc(11)
    parameter    (czero= (0.d0,0.d0))
!
    character(len=1) :: typcoe
    character(len=5) :: ch5
    character(len=8) :: k8b, curpar, ncmppe(4), tpf(11), charge, typcha
    character(len=8) :: ncmpfo(11), modele, fmult
    character(len=16) :: typemo
    character(len=19) :: chdynr, chacce
    character(len=24) :: chamgd, nochin, nochi1, chdepl, ligrmo
    character(len=8), pointer :: fcha(:) => null()
    real(kind=8), pointer :: nldepl(:) => null()
    character(len=8), pointer :: lcha(:) => null()
!
    data         ncmppe/ 'G' , 'AG' , 'BG' , 'CG' /
    data         ncmpfo/ 'FX' , 'FY' , 'FZ' , 'MX' , 'MY' , 'MZ' ,&
     &                     'BX' , 'REP' , 'ALPHA' , 'BETA' , 'GAMMA' /
!
    call jemarq()
!
    iret = 0
!
    typemo = ' '
    if (typesd .eq. 'MODE_MECA') then
        call rsadpa(resuin, 'L', 1, 'TYPE_MODE', 1,&
                    0, sjv=ltymo, styp=k8b)
        typemo=zk16(ltymo)
    endif
    call rsexch('F', resuin, 'DEPL', nordre, chdepl,&
                ier)
!
    ligrmo = modele//'.MODELE'
!
    typcoe = ' '
    alpha = zero
    calpha = czero
    chdynr = '&&MECALM.M.GAMMA'
    if ((typesd.eq.'MODE_MECA'.and.typemo(1:8).eq.'MODE_DYN' ) .or. (typesd.eq.'MODE_ACOU')) then
        call jeveuo(chdynr//'.VALE', 'E', lvale)
        call jelira(chdepl(1:19)//'.VALE', 'LONMAX', neq)
        call rsexch('F', resuin, 'DEPL', nordre, chamgd,&
                    ier)
        call jeveuo(chamgd(1:19)//'.VALE', 'L', vr=nldepl)
        call rsadpa(resuin, 'L', 1, 'OMEGA2', nordre,&
                    0, sjv=lfreq, styp=k8b)
        do ii = 0, neq - 1
            zr(lvale+ii) = -zr(lfreq)*nldepl(ii+1)
        end do
        call jelibe(chamgd(1:19)//'.VALE')
    else if (typesd.eq.'DYNA_TRANS') then
        call jeveuo(chdynr//'.VALE', 'E', lvale)
        call jelira(chdepl(1:19)//'.VALE', 'LONMAX', neq)
        call rsexch(' ', resuin, 'ACCE', nordre, chacce,&
                    ier)
        if (ier .eq. 0) then
            call jeveuo(chacce//'.VALE', 'L', lacce)
            do ii = 0, neq - 1
                zr(lvale+ii) = zr(lacce+ii)
            end do
            call jelibe(chacce//'.VALE')
        else
            call utmess('A', 'CALCULEL3_1')
            do ii = 0, neq - 1
                zr(lvale+ii) = zero
            end do
        endif
    else if (typesd.eq.'DYNA_HARMO') then
        call jeveuo(chdynr//'.VALE', 'E', lvale)
        call jelira(chdepl(1:19)//'.VALE', 'LONMAX', neq)
        call rsexch(' ', resuin, 'ACCE', nordre, chacce,&
                    ier)
        if (ier .eq. 0) then
            call jeveuo(chacce//'.VALE', 'L', lacce)
            do ii = 0, neq - 1
                zc(lvale+ii) = zc(lacce+ii)
            end do
            call jelibe(chacce//'.VALE')
        else
            call utmess('A', 'CALCULEL3_1')
            do ii = 0, neq - 1
                zc(lvale+ii) = czero
            end do
        endif
    endif
!
! --- CALCUL DU COEFFICIENT MULTIPLICATIF DE LA CHARGE
!     CE CALCUL N'EST EFFECTIF QUE POUR LES CONDITIONS SUIVANTES
!        * MODELISATION POUTRE
!        * PRESENCE D'UNE (ET D'UNE SEULE) CHARGE REPARTIE
!
!     IOCCUR C'EST SOIT :
!        * L'OCCURENCE DANS LE MOT CLEF EXIT
!        * L'INDEX DE LA CHARGE DANS KCHARG
!     LES VERIFICATIONS D'EXISTANCE DE LA CHARGE ET DE SA FMULT SONT
!     FAITES DANS RSLESD
    charge = ' '
    fmult = ' '
    coeff = 0.0d0
    if (nbchre .ne. 0) then
!        LA CHARGE REPARTIE EST :
!           SOUS EXIT DE LA COMMANDE
!           DANS KCHARG
        call getvid('EXCIT', 'CHARGE', iocc=ioccur, scal=charge, nbret=n1)
        if (n1 .eq. 0) then
            call jeveuo(kcharg//'.LCHA', 'L', vk8=lcha)
            call jeveuo(kcharg//'.FCHA', 'L', vk8=fcha)
            charge = lcha(ioccur)
            fmult = fcha(ioccur)
!           LA FONCTION PEUT AVOIR ETE CREEE, SUR LA BASE V
!           SI C'EST LE CAS C'EST LA FONCTION UNITE
            if (fmult(1:2) .eq. '&&') then
!              NORMALEMENT SEUL NMDOME DOIT CREER CETTE FONCTION
                ASSERT(fmult.eq.'&&NMDOME')
                coeff = 1.d0
                call focste(fmult, 'TOUTRESU', coeff, 'V')
            endif
            l1 = 1
            l3 = 0
        else
            call getvid('EXCIT', 'FONC_MULT', iocc=ioccur, scal=fmult, nbret=l1)
            call getvr8('EXCIT', 'COEF_MULT', iocc=ioccur, scal=coeff, nbret=l3)
            if (l1+l3 .ne. 0) then
                if ((typesd.ne.'DYNA_HARMO') .and. ( typesd.ne.'DYNA_TRANS') .and.&
                    (typesd.ne.'EVOL_ELAS')) then
                    call utmess('A', 'CALCULEL3_4')
                    iret = 1
                    goto 999
                endif
            endif
        endif
!
        if (l1 .ne. 0 .or. l3 .ne. 0) then
            if (typesd .eq. 'DYNA_HARMO') then
                typcoe = 'C'
                call rsadpa(resuin, 'L', 1, 'FREQ', nordre,&
                            0, sjv=lfreq, styp=k8b)
                freq = zr(lfreq)
                if (l1 .ne. 0) then
                    call fointe('F ', fmult, 1, ['FREQ'], [freq],&
                                valres, ier)
                    calpha = dcmplx(valres,zero)
                else if (l3.ne.0) then
                    calpha = dcmplx(coeff,un)
                endif
            else if (typesd.eq.'DYNA_TRANS') then
                typcoe = 'R'
                call rsadpa(resuin, 'L', 1, 'INST', nordre,&
                            0, sjv=linst, styp=k8b)
                inst = zr(linst)
                if (l1 .ne. 0) then
                    call fointe('F ', fmult, 1, ['INST'], [inst],&
                                alpha, ier)
                else if (l3.ne.0) then
                    alpha = coeff
                else
                    call utmess('A', 'CALCULEL3_2')
                    iret = 1
                    goto 999
                endif
            else if (typesd.eq.'EVOL_ELAS') then
                typcoe = 'R'
                call rsadpa(resuin, 'L', 1, 'INST', nordre,&
                            0, sjv=linst, styp=k8b)
                inst = zr(linst)
                if (l1 .ne. 0) then
                    call fointe('F ', fmult, 1, ['INST'], [inst],&
                                alpha, ier)
                else
                    call utmess('A', 'CALCULEL3_3')
                    iret = 1
                    goto 999
                endif
            endif
        endif
    endif
!
    ch5 = '.    '
    do i = 1, 11
        tps(i) = zero
        tpf(i) = '&FOZERO'
        tpc(i) = czero
    end do
!
    nochi1 = charge//'.CHME.F1D1D.DESC'
    exif1d = .false.
    call jeexin(nochi1, ier)
    if (ier .eq. 0) then
        exif1d = .true.
    else
        call dismoi('TYPE_CHARGE', charge, 'CHARGE', repk=typcha)
    endif
!
    do ipara = 1, nbpain
        curpar = lipain(ipara)
        ch5 = '.    '
        if ((curpar.eq.'PCOEFFR') .and. (typcoe.eq.'R')) then
            nochin = '&&MECHPO'//ch5//'.COEFF'
            call mecact('V', nochin, 'MODELE', ligrmo, 'IMPE_R',&
                        ncmp=1, nomcmp='IMPE', sr=alpha)
            lichin(ipara) = nochin
        endif
        if ((curpar.eq.'PCOEFFC') .and. (typcoe.eq.'C')) then
            nochin = '&&MECHPO'//ch5//'.COEFF'
            call mecact('V', nochin, 'MODELE', ligrmo, 'IMPE_C',&
                        ncmp=1, nomcmp='IMPE', sc=calpha)
            lichin(ipara) = nochin
        endif
!
        if (curpar .eq. 'PPESANR') then
            nochin = charge//'.CHME.PESAN.DESC'
            call jeexin(nochin, ier)
            if (ier .eq. 0) then
                call codent(ipara, 'D0', ch5(2:5))
                nochin = '&&MECHPO'//ch5//'.PESAN.DESC'
                lichin(ipara) = nochin
                call mecact('V', nochin, 'MODELE', ligrmo, 'PESA_R  ',&
                            ncmp=4, lnomcmp=ncmppe, vr=tps)
            else
                lichin(ipara) = nochin
            endif
        else if (curpar.eq.'PFF1D1D') then
            if (exif1d) then
                call codent(ipara, 'D0', ch5(2:5))
                nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                lichin(ipara) = nochin
                call fozero(tpf(1))
                call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_F  ',&
                            ncmp=11, lnomcmp=ncmpfo, vk=tpf)
            else
                if (typcha(5:7) .eq. '_FO') then
                    lichin(ipara) = nochi1
                else
                    call codent(ipara, 'D0', ch5(2:5))
                    nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                    lichin(ipara) = nochin
                    call fozero(tpf(1))
                    call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_F  ',&
                                ncmp=11, lnomcmp=ncmpfo, vk=tpf)
                endif
            endif
        else if (curpar.eq.'PFR1D1D') then
            if (exif1d) then
                call codent(ipara, 'D0', ch5(2:5))
                nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                lichin(ipara) = nochin
                call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_R  ',&
                            ncmp=11, lnomcmp=ncmpfo, vr=tps)
            else
                if ((typcha(5:7).eq.'_FO') .or. (typcha(5:7).eq.'_RI')) then
                    call codent(ipara, 'D0', ch5(2:5))
                    nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                    lichin(ipara) = nochin
                    call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_R  ',&
                                ncmp=11, lnomcmp=ncmpfo, vr=tps)
                else
                    lichin(ipara) = nochi1
                endif
            endif
        else if (curpar.eq.'PFC1D1D') then
            if (exif1d) then
                call codent(ipara, 'D0', ch5(2:5))
                nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                lichin(ipara) = nochin
                call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_C  ',&
                            ncmp=11, lnomcmp=ncmpfo, vc=tpc)
            else
                if (typcha(5:7) .eq. '_RI') then
                    lichin(ipara) = nochi1
                else
                    call codent(ipara, 'D0', ch5(2:5))
                    nochin = '&&MECHPO'//ch5//'.P1D1D.DESC'
                    lichin(ipara) = nochin
                    call mecact('V', nochin, 'MODELE', ligrmo, 'FORC_C  ',&
                                ncmp=11, lnomcmp=ncmpfo, vc=tpc)
                endif
            endif
        else if (curpar.eq.'PCHDYNR') then
            nochin = chdynr//'.VALE'
            call jeexin(nochin, ier)
            if (ier .eq. 0) then
                call codent(ipara, 'D0', ch5(2:5))
                nochin = '&&MECHPO'//ch5//'.PCHDY'
                call copisd('CHAMP_GD', 'V', chdepl, nochin)
            endif
            lichin(ipara) = nochin
        else if (curpar.eq.'PSUROPT') then
            call codent(ipara, 'D0', ch5(2:5))
            nochin = '&&MECHPO'//ch5//'.SUR_OPTION'
            lichin(ipara) = nochin
            call mecact('V', nochin, 'MODELE', ligrmo, 'NEUT_K24',&
                        ncmp=1, nomcmp='Z1', sk=suropt)
        endif
    end do
!
999 continue
!
    call jedema()
end subroutine
