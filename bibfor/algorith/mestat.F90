subroutine mestat(modelz, fomulz, lischz, mate, caraz,&
                  ltpsz, solvez, compor, matasz)
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
! ---------------------------------------------------------------------
!     BUT:  FAIRE UN CALCUL DE MECANIQUE STATIQUE : K(T)*U = F(T)
!           POUR LES DIFFERENTS INSTANTS "T" DE LTPS.
!     IN: MODELZ : NOM D'1 MODELE
!         FOMULZ : LISTE DES FONCTIONS MULTIPLICATRICES
!         LISCHZ : INFORMATION SUR LES CHARGEMENTS
!         MATE   : NOM DU MATERIAU
!         CARAZ  : NOM D'1 CARAC_ELEM
!         LTPSZ  : LISTE DES INSTANTS DE CALCUL
!         SOLVEZ : METHODE DE RESOLUTION 'LDLT' OU 'GCPC'
!         COMPOR : COMPOR POUR LES MULTIFIBRE (POU_D_EM)
!         MATASZ : MATRICE ASSEMBLEE DU SYSTEME
!
!    OUT: L'EVOL_ELAS  EST REMPLI (POUR SA PARTIE 'DEPL')
! ---------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterc/etausr.h"
#include "asterc/getres.h"
#include "asterc/r8maem.h"
#include "asterfort/alfeti.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerecu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mereso.h"
#include "asterfort/nmvcd2.h"
#include "asterfort/numero.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnume.h"
#include "asterfort/sigusr.h"
#include "asterfort/u2mess.h"
#include "asterfort/utcrre.h"
#include "asterfort/utexcm.h"
#include "asterfort/uttcpg.h"
#include "asterfort/uttcpr.h"
#include "asterfort/uttcpu.h"
#include "asterfort/vtcreb.h"
    character(len=*) :: modelz, fomulz, lischz, mate, caraz, ltpsz, solvez
    character(len=*) :: matasz
    character(len=8) :: ltps
    character(len=19) :: lischa, solveu
    character(len=24) :: compor
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter    (nompro = 'MESTAT')
    integer :: nbval, ibid, ierd, jval, itps, itps0, iret, ninstc, islvk, neq
    integer :: vali
    real(kind=8) :: time, instf, tps1(4), tps2(4), tps3(4), tcpu, partps(3)
    real(kind=8) :: rbid, valr(3)
    character(len=1) :: base
    character(len=8) :: repk, result
    character(len=14) :: nuposs
    character(len=16) :: k16bid
    character(len=19) :: maprec, vecass, chdepl, k19b, matass
    character(len=24) :: numedd, method, criter, opt, k24b, modele, carele
    character(len=24) :: fomult, noojb
    logical :: matcst, assmat, lfeti
    logical :: lbid, ltemp, lhydr, lsech, lptot
!
! DEB------------------------------------------------------------------
!====
! 1. PREALABLES
!====
!
    call jemarq()
!
! 1.1. ==> LES ARGUMENTS
!
    solveu = solvez
    modele = modelz
    carele = caraz
    matass = matasz
    fomult = fomulz
    lischa = lischz
    ltps = ltpsz
!
! 1.2. ==> LES CONSTANTES
!
!               12   345678   90123456789
    vecass = '&&'//nompro//'.2NDMBR_ASS'
    maprec = '&&'//nompro//'_MAT_PRECON'
!
    partps(2) = 0.d0
    partps(3) = 0.d0
    base = 'V'
    criter = '&&RESGRA_GCPC'
    call getres(result, k16bid, k16bid)
!
!
! 1.3. ==> ALLOCATION DES RESULTATS
    call jelira(ltps//'           .VALE', 'LONMAX', nbval)
    call utcrre(result, nbval)
!
! 1.4. ==> ON REGARDE SI LE MATERIAU EST UNE FONCTION DU TEMPS
!     (DEPENDANCE AVEC LA TEMPERATURE, HYDRATATION, SECHAGE)
!
    call nmvcd2('HYDR', mate, lhydr, lbid)
    call nmvcd2('SECH', mate, lsech, lbid)
    call nmvcd2('PTOT', mate, lptot, lbid)
    call nmvcd2('TEMP', mate, ltemp, lbid)
!
!
!     -- LE MATERIAU (ELAS) PEUT-IL CHANGER AU COURS DU TEMPS ?
    call dismoi('F', 'ELAS_FO', mate, 'CHAM_MATER', ibid,&
                repk, ierd)
    matcst=(.not.(repk.eq.'OUI'))
!
! 2.2. ==> NUMEROTATION ET CREATION DU PROFIL DE LA MATRICE
! FETI OR NOT FETI ?
    call jeveuo(solveu(1:19)//'.SLVK', 'L', islvk)
    method=zk24(islvk)
! SI FETI, INITIALISATION DES OBJETS TEMPORAIRES DE MONITORING
    if (method(1:4) .eq. 'FETI') then
        lfeti=.true.
    else
        lfeti=.false.
    endif
!
    numedd=  '12345678.NUMED'
    noojb='12345678.00000.NUME.PRNO'
    call gnomsd(' ', noojb, 10, 14)
    numedd=noojb(1:14)
!
!
    call rsnume(result, 'DEPL', nuposs)
!
    call numero(nuposs, modele, lischa, solveu, 'VG',&
                numedd)
!
    call vtcreb(vecass, numedd, 'V', 'R', neq)
!
!     ??? IL SERAIT PEUT ETRE BON DE VERIFIER QUE QUELQUE CHOSE BOUGE
!     AVEC LE TEMPS. POUR L'INSTANT ON RECALCULE LE 2EME MEMBRE A CHA
!     QUE FOIS.
!
    call uttcpu('CPU.OP0046.1', 'INIT', ' ')
    call uttcpu('CPU.OP0046.2', 'INIT', ' ')
    call uttcpu('CPU.OP0046.3', 'INIT', ' ')
!
    call jeveuo(ltps//'           .VALE', 'L', jval)
    instf=r8maem()
    call getvr8(' ', 'INST_FIN', scal=instf, nbret=ibid)
!
!
!====
! 2. BOUCLE 2 SUR LES PAS DE TEMPS
!====
!
    ninstc=0
    do itps = 1, nbval
        call jerecu('V')
!
!       SI LE PAS DE TEMPS A DEJA ETE CALCULE, ON SAUTE L'ITERATION
        call rsexch(' ', result, 'DEPL', itps, chdepl,&
                    iret)
        if (iret .eq. 0) goto 2
!
! 2.1. ==> L'INSTANT
        itps0 = itps
        time = zr(jval-1+itps)
!
!       -- SI ON A DEPASSE INSTF, ON SORT :
        if (time .gt. instf) goto 999
!
        ninstc=ninstc+1
        partps(1) = time
!
!
! 2.2.1. ==> Y-A-T'IL ASSEMBLAGE DES MATRICES ?
!
        if (.not.matcst .or. ninstc .eq. 1) then
            assmat = .true.
        else
            assmat = .false.
        endif
! 2.2.2. ==> RESOLUTION
!
        call mereso(result, modele, mate, carele, fomult,&
                    lischa, itps0, partps, numedd, vecass,&
                    assmat, solveu, matass, maprec, base,&
                    compor)
!
!       -- IMPRESSION EVENTUELLE DES MESURES DE TEMPS:
        call uttcpg('IMPR', 'INCR')
!
!       --- VERIFICATION SI INTERRUPTION DEMANDEE PAR SIGNAL USR1
!
        if (etausr() .eq. 1) call sigusr()
!
!
! 2.3. ==> CONTROLE DU TEMPS CPU
!
        call uttcpr('CPU.OP0046.1', 4, tps1)
        call uttcpr('CPU.OP0046.2', 4, tps2)
        call uttcpr('CPU.OP0046.3', 4, tps3)
        if (.not.matcst .or. ninstc .eq. 1) then
            tcpu = tps1(4) + tps2(4) + tps3(4)
        else
            tcpu = tps3(4)
        endif
        if (nbval .gt. 1 .and. itps .lt. nbval .and. tcpu .gt. .95d0* tps3(1)) then
            vali = itps
            valr (1) = time
            valr (2) = tcpu
            valr (3) = tcpu
            call utexcm(28, 'ALGORITH16_88', 0, ' ', 1,&
                        vali, 3, valr)
            call u2mess('F', 'ALGORITH11_83')
            goto 999
        endif
!
 2      continue
    end do
!
999  continue
    call detrsd('CHAMP_GD', vecass)
!
    if (lfeti) then
! NETTOYAGE DES SD FETI SI NECESSAIRE (SUCCESSION DE CALCULS DECOUPLES)
! ET INITIALISATION NUMERO D'INCREMENT
        opt='NETTOYAGE_SDT'
        call alfeti(opt, k19b, k19b, k19b, k19b,&
                    ibid, rbid, k24b, rbid, ibid,&
                    k24b, k24b, k24b, k24b, ibid,&
                    k24b, k24b, ibid)
    endif
!
    call jeexin(criter(1:19)//'.CRTI', iret)
    if (iret .ne. 0) then
        call jedetr(criter(1:19)//'.CRTI')
        call jedetr(criter(1:19)//'.CRTR')
        call jedetr(criter(1:19)//'.CRDE')
    endif
    call jedema()
end subroutine
