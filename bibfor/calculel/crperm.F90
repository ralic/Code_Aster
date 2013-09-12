subroutine crperm()
    implicit none
! ----------------------------------------------------------------------
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
!     COMMANDE:  CREA_RESU
!     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAM"
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cetran.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cntran.h"
#include "asterfort/crpcvg.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/u2mesg.h"
#include "asterfort/wkvect.h"
!
!
!
    integer :: n1, nbcham, iord1, iord2, nbperm, jordr, nbtrou, ip, ibid, ic
    integer :: iret, jlim1, jlim2, nbma, jlino, nbno2, nncp
    real(kind=8) :: inst1, tran(3), prec
    real(kind=8) :: valr
    complex(kind=8) :: cbid
    character(len=8) :: k8b, crit, resu1, resu2, resu3, ma1, ma2
    character(len=16) :: typres, nomcmd, cham(4), option
    character(len=24) :: valk(2)
    character(len=19) :: prchno
    character(len=24) :: ch1, ch2, chs1, chs2, linoeu, gma1, gma2, lima1, lima2
    character(len=24) :: ligrel, chsi1(4), chsi2(4)
    integer :: iarg
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call getres(resu3, typres, nomcmd)
!
! --- RECUPERATION DES DONNEES UTILISATEUR :
!     ------------------------------------
!
    call getvid(' ', 'RESU_INIT', scal=resu1, nbret=n1)
    call getvr8(' ', 'INST_INIT', scal=inst1, nbret=n1)
    if (n1 .eq. 0) then
        call jelira(resu1//'           .ORDR', 'LONUTI', ibid)
        call jeveuo(resu1//'           .ORDR', 'L', jordr)
        iord1 = zi(jordr+ibid-1)
    else
        call getvr8(' ', 'PRECISION', scal=prec, nbret=n1)
        call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
        call rsorac(resu1, 'INST', ibid, inst1, k8b,&
                    cbid, prec, crit, iord1, 1,&
                    nbtrou)
        if (nbtrou .eq. 0) then
            valr = inst1
            valk (1) = resu1
            call u2mesg('F', 'CALCULEL5_70', 1, valk, 0,&
                        0, 1, valr)
        else if (nbtrou .ne. 1) then
            valr = inst1
            call u2mesg('F', 'CALCULEL5_71', 0, ' ', 0,&
                        0, 1, valr)
        endif
    endif
    call getvid(' ', 'MAILLAGE_INIT', scal=ma1, nbret=n1)
    call getvid(' ', 'RESU_FINAL', scal=resu2, nbret=n1)
    call getvid(' ', 'MAILLAGE_FINAL', scal=ma2, nbret=n1)
    call getvtx(' ', 'NOM_CHAM', nbval=0, nbret=n1)
    if (n1 .eq. 0) then
        nbcham = 3
        cham(1) = 'DEPL'
        cham(2) = 'SIEF_ELGA'
        cham(3) = 'VARI_ELGA'
    else
        nbcham = -n1
        call getvtx(' ', 'NOM_CHAM', nbval=nbcham, vect=cham, nbret=n1)
!
    endif
!
    call dismoi('F', 'NB_NO_MAILLA', ma2, 'MAILLAGE', nbno2,&
                k8b, ibid)
    iord2 = 1
!
! --- VERIFICATIONS SUPPLEMENTAIRES :
!     -----------------------------
!
    if (resu2 .ne. resu3) then
        valk (1) = resu3
        valk (2) = resu2
        call u2mesg('F', 'CALCULEL5_72', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    call jelira(resu2//'           .ORDR', 'LONUTI', ibid)
    if (ibid .ne. 1) then
        valk (1) = resu2
        valk (2) = k8b
        call u2mesg('F', 'CALCULEL5_73', 2, valk, 0,&
                    0, 0, 0.d0)
    endif
!
    do 100 ic = 1, nbcham
        call rsexch('F', resu1, cham(ic), iord1, ch1,&
                    iret)
        call rsexch('F', resu2, cham(ic), iord2, ch2,&
                    iret)
!
        if (cham(ic) .eq. 'DEPL') then
            chs1 = '&&CRPERM.DEPL_1'
            call cnocns(ch1, 'V', chs1)
            chsi1(ic) = chs1
            chs2 = '&&CRPERM.DEPL_2'
            call cnocns(ch2, 'V', chs2)
            chsi2(ic) = chs2
        else if (cham(ic) .eq. 'SIEF_ELGA') then
            chs1 = '&&CRPERM.SIEF_1'
            call celces(ch1, 'V', chs1)
            chsi1(ic) = chs1
            chs2 = '&&CRPERM.SIEF_2'
            call celces(ch2, 'V', chs2)
            chsi2(ic) = chs2
        else if (cham(ic) .eq. 'VARI_ELGA') then
            chs1 = '&&CRPERM.VARI_1'
            call celces(ch1, 'V', chs1)
            chsi1(ic) = chs1
            chs2 = '&&CRPERM.VARI_2'
            call celces(ch2, 'V', chs2)
            chsi2(ic) = chs2
        else if (cham(ic) .eq. 'STRX_ELGA') then
            chs1 = '&&CRPERM.STRX_1'
            call celces(ch1, 'V', chs1)
            chsi1(ic) = chs1
            chs2 = '&&CRPERM.STRX_2'
            call celces(ch2, 'V', chs2)
            chsi2(ic) = chs2
        endif
!
100  end do
!
!
    linoeu = '&&CRPERM.LISTE_NOEU'
    lima1 = '&&CRPERM.LISTE_MA_1'
    lima2 = '&&CRPERM.LISTE_MA_2'
!
    call getfac('PERM_CHAM', nbperm)
!
! --- BOUCLE SUR LES TRANSLATIONS A EFFECTUER :
!     ---------------------------------------
!
    do 10 ip = 1, nbperm
!
        call getvem(ma1, 'GROUP_MA', 'PERM_CHAM', 'GROUP_MA_INIT', ip,&
                    iarg, 1, gma1, n1)
        call getvem(ma2, 'GROUP_MA', 'PERM_CHAM', 'GROUP_MA_FINAL', ip,&
                    iarg, 1, gma2, n1)
!
        call getvr8('PERM_CHAM', 'TRAN', iocc=ip, nbval=3, vect=tran,&
                    nbret=n1)
        call getvr8('PERM_CHAM', 'PRECISION', iocc=ip, scal=prec, nbret=n1)
!
! ------ VERIFICATION DES GROUPES DE MAILLES FOURNIES :
!        --------------------------------------------
!
        call wkvect(linoeu, 'V V I', nbno2, jlino)
!
        call crpcvg(ma1, ma2, gma1, gma2, tran,&
                    prec, lima1, lima2, zi(jlino))
!
        call jelira(lima1, 'LONMAX', nbma)
        call jeveuo(lima1, 'L', jlim1)
        call jeveuo(lima2, 'L', jlim2)
!
        do 20 ic = 1, nbcham
!
            chs1 = chsi1(ic)
            chs2 = chsi2(ic)
!
! --------- ON TRANSFERE LES VALEURS DE 1 VERS 2 :
!           ------------------------------------
!
            if (cham(ic) .eq. 'DEPL') then
                call cntran(zi(jlino), nbno2, chs1, chs2)
            else
                call cetran(zi(jlim1), zi(jlim2), nbma, chs1, chs2)
!
            endif
!
20      continue
!
        call jedetr(lima1)
        call jedetr(lima2)
        call jedetr(linoeu)
!
10  end do
!
    do 110 ic = 1, nbcham
        call rsexch('F', resu2, cham(ic), iord2, ch2,&
                    iret)
        chs1 = chsi1(ic)
        chs2 = chsi2(ic)
        if (cham(ic) .eq. 'DEPL') then
            call dismoi('F', 'PROF_CHNO', ch2, 'CHAM_NO', ibid,&
                        prchno, ibid)
            call cnscno(chs2, prchno, 'NON', 'G', ch2,&
                        'F', ibid)
            call detrsd('CHAM_NO_S', chs1)
            call detrsd('CHAM_NO_S', chs2)
        else
            call dismoi('F', 'NOM_LIGREL', ch2, 'CHAM_ELEM', ibid,&
                        ligrel, ibid)
            call dismoi('F', 'NOM_OPTION', ch2, 'CHAM_ELEM', ibid,&
                        option, ibid)
            call cescel(chs2, ligrel, option, ' ', 'OUI',&
                        nncp, 'G', ch2, 'F', ibid)
            call detrsd('CHAM_ELEM_S', chs1)
            call detrsd('CHAM_ELEM_S', chs2)
        endif
110  end do
!
    call jedema()
end subroutine
