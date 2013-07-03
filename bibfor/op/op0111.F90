subroutine op0111()
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
! person_in_charge: nicolas.greffet at edf.fr
! =====================================================================
!
!     COMMANDE:  ENV_CINE_YACS
!  ENVOI DES CHAMPS CINEMATIQUES A SATURNE VIA YACS
!
! =====================================================================
! aslint: disable=W1304
    implicit   none
!     ------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterc/cpedb.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
! 0.3. ==> VARIABLES LOCALES
!
!
!     ------------------------------------------------------------------
    integer :: ibid, icode, icmp, ino1, ino2, ii, idecal
    integer :: iresu, ietin, idepl, ivite, iacce, icmpg, iocc
    integer :: jdepsk, jdepsd, jdepsc, jdepsv, jdepsl, jdepl
    integer :: jvitsk, jvitsd, jvitsc, jvitsv, jvitsl, jvite
    integer :: jaccsk, jaccsd, jaccsc, jaccsv, jaccsl, jacce
    integer :: jacono, jaconb, jaconu, jacocf, ialin2
    integer :: jj, ilengt
    integer :: nordre, nbno1, nbcmp, nbno2, nbocc
    integer :: ddldep(3), ddlvit(3), ddlacc(3)
    integer :: ifm, niv
    character(len=1) :: kbid
    character(len=8) :: ma, ma1, ma2
    character(len=16) :: corres, valk(2)
    character(len=19) :: resu
    character(len=19) :: chdepl, chdeps, chvite, chvits, chacce, chaccs
    character(len=24) :: nomgno, grpno
!     COUPLAGE =>
    integer(kind=4) :: lenvar, cpiter, numpa4, ibid4, idim
    parameter (lenvar = 144)
    character(len=lenvar) :: nomvar
    parameter (cpiter= 41)
    integer :: icompo, numpas, iadr
    real(kind=8) :: tf, dt
    character(len=24) :: ayacs
    integer :: iarg
!     COUPLAGE <=
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call infniv(ifm, niv)
!
!     ASSIGNATION DES NOMS POUR LES ADRESSES DANS LES COMMON ASTER
!     ------------------------------------------------------------
    ayacs='&ADR_YACS'
!
!     RECUPERATION DE L'ADRESSE YACS
!     ------------------------------
    call jeveuo(ayacs, 'L', iadr)
    icompo=zi(iadr)
!     ------------------------------------------------------------------
!
!     ! ========================== !
!     ! RECUPERATION DES MOTS-CLES !
!     ! ========================== !
    call getvid(' ', 'MATR_PROJECTION', 1, iarg, 1,&
                corres, ibid)
    call getvr8(' ', 'INST', 0, iarg, 1,&
                tf, ibid)
    call getvr8(' ', 'PAS', 0, iarg, 1,&
                dt, ibid)
    call getvis(' ', 'NUME_ORDRE_YACS', 0, iarg, 1,&
                numpas, ibid)
    numpa4 = numpas
    call getfac('VIS_A_VIS', nbocc)
    call getfac('RESULTAT', iresu)
    call getfac('ETAT_INIT', ietin)
    if (iresu+ietin .gt. 1) call u2mess('F', 'COUPLAGEIFS_1')
    if (nbocc .lt. 1) call u2mess('F', 'COUPLAGEIFS_2')
    idepl = 0
    ivite = 0
    iacce = 0
    if (iresu .eq. 1) then
        call getvid('RESULTAT', 'RESU', 1, iarg, 1,&
                    resu, ibid)
        call getvis('RESULTAT', 'NUME_ORDRE', 1, iarg, 1,&
                    nordre, ibid)
        if (niv .eq. 2) then
            valk(1) = 'OP0111'
            valk(2) = 'NUME_ORDRE'
            call u2mesg('I+', 'COUPLAGEIFS_8', 2, valk, 1,&
                        nordre, 0, 0.d0)
        endif
        call rsexch(' ', resu, 'DEPL', nordre, chdepl,&
                    icode)
        call rsexch(' ', resu, 'VITE', nordre, chvite,&
                    icode)
        call rsexch(' ', resu, 'ACCE', nordre, chacce,&
                    icode)
        idepl = 1
        ivite = 1
        iacce = 1
    else if (ietin.eq.1) then
        call getvid('ETAT_INIT', 'DEPL', 1, iarg, 1,&
                    chdepl, idepl)
        call getvid('ETAT_INIT', 'VITE', 1, iarg, 1,&
                    chvite, ivite)
        call getvid('ETAT_INIT', 'ACCE', 1, iarg, 1,&
                    chacce, iacce)
        if (idepl .gt. 1 .or. ivite .gt. 1 .or. iacce .gt. 1) then
            call u2mess('F', 'COUPLAGEIFS_3')
        endif
    endif
!     ! ================================================ !
!     ! TRANSFORMATION DES CHAMPS EN CHAMPS NOEUD SIMPLE !
!     ! ================================================ !
    nbno1 = 0
    nbcmp = 0
    if (idepl .eq. 1) then
        chdeps = '&&IRGMCN.DEPL'
        call cnocns(chdepl, 'V', chdeps)
        call jeveuo(chdeps//'.CNSK', 'L', jdepsk)
        call jeveuo(chdeps//'.CNSD', 'L', jdepsd)
        call jeveuo(chdeps//'.CNSC', 'L', jdepsc)
        call jeveuo(chdeps//'.CNSV', 'L', jdepsv)
        call jeveuo(chdeps//'.CNSL', 'L', jdepsl)
        ma = zk8(jdepsk)
        nbno1 = zi(jdepsd-1+1)
        nbcmp = zi(jdepsd-1+2)
    endif
    if (ivite .eq. 1) then
        chvits = '&&IRGMCN.VITE'
        call cnocns(chvite, 'V', chvits)
        call jeveuo(chvits//'.CNSK', 'L', jvitsk)
        call jeveuo(chvits//'.CNSD', 'L', jvitsd)
        call jeveuo(chvits//'.CNSC', 'L', jvitsc)
        call jeveuo(chvits//'.CNSV', 'L', jvitsv)
        call jeveuo(chvits//'.CNSL', 'L', jvitsl)
        ma = zk8(jvitsk)
        nbno1 = zi(jvitsd-1+1)
        nbcmp = zi(jvitsd-1+2)
    endif
    if (iacce .eq. 1) then
        chaccs = '&&IRGMCN.ACCE'
        call cnocns(chacce, 'V', chaccs)
        call jeveuo(chaccs//'.CNSK', 'L', jaccsk)
        call jeveuo(chaccs//'.CNSD', 'L', jaccsd)
        call jeveuo(chaccs//'.CNSC', 'L', jaccsc)
        call jeveuo(chaccs//'.CNSV', 'L', jaccsv)
        call jeveuo(chaccs//'.CNSL', 'L', jaccsl)
        ma = zk8(jaccsk)
        nbno1 = zi(jaccsd-1+1)
        nbcmp = zi(jaccsd-1+2)
    endif
!     ! ========================================== !
!     ! APPEL DES POINTEURS DE CORRESP_2_MAILLAGES !
!     ! ========================================== !
!      CALL JEVEUO(CORRES//'.PJEF_NO', 'L', JACONO)
    call jeveuo(corres//'.PJXX_K1', 'L', jacono)
    call jeveuo(corres//'.PJEF_NB', 'L', jaconb)
    call jeveuo(corres//'.PJEF_NU', 'L', jaconu)
    call jeveuo(corres//'.PJEF_CF', 'L', jacocf)
!     ! ===================================================== !
!     ! VERIFICATIONS ELEMENTAIRES SUR LES NOMS DES MAILLAGES !
!     ! ===================================================== !
    ma1 = zk24(jacono-1+1)(1:8)
    ma2 = zk24(jacono-1+2)(1:8)
    if (nbno1 .gt. 0) then
        if (ma .ne. ma1) call u2mess('F', 'COUPLAGEIFS_4')
    endif
!     ! ======================================= !
!     ! RECUPERATIONS DES DONNEES DU MAILLAGE 1 !
!     ! ======================================= !
    do 10 icmp = 1, 3
        ddldep(icmp) = 0
        ddlvit(icmp) = 0
        ddlacc(icmp) = 0
10  end do
    do 20 icmp = 1, nbcmp
        if (idepl .eq. 1) then
            if (zk8(jdepsc-1+icmp) .eq. 'DX') ddldep(1) = icmp
            if (zk8(jdepsc-1+icmp) .eq. 'DY') ddldep(2) = icmp
            if (zk8(jdepsc-1+icmp) .eq. 'DZ') ddldep(3) = icmp
        endif
        if (ivite .eq. 1) then
            if (zk8(jvitsc-1+icmp) .eq. 'DX') ddlvit(1) = icmp
            if (zk8(jvitsc-1+icmp) .eq. 'DY') ddlvit(2) = icmp
            if (zk8(jvitsc-1+icmp) .eq. 'DZ') ddlvit(3) = icmp
        endif
        if (iacce .eq. 1) then
            if (zk8(jaccsc-1+icmp) .eq. 'DX') ddlacc(1) = icmp
            if (zk8(jaccsc-1+icmp) .eq. 'DY') ddlacc(2) = icmp
            if (zk8(jaccsc-1+icmp) .eq. 'DZ') ddlacc(3) = icmp
        endif
20  end do
!     ! ======================================= !
!     ! RECUPERATIONS DES DONNEES DU MAILLAGE 2 !
!     ! ======================================= !
    call dismoi('F', 'NB_NO_MAILLA', ma2, 'MAILLAGE', nbno2,&
                kbid, ibid)
!     ! ===================================================== !
!     ! PROJECTIONS DES DEPLACEMENTS ENTRE LES DEUX MAILLAGES !
!     ! ===================================================== !
    call wkvect('&&OP0111.DEPL', 'V V R', 3*nbno2, jdepl)
    call wkvect('&&OP0111.VITE', 'V V R', 3*nbno2, jvite)
    call wkvect('&&OP0111.ACCE', 'V V R', 3*nbno2, jacce)
    do 30 ino2 = 1, nbno2
        do 40 icmp = 1, 3
            zr(jdepl-1+3*(ino2-1)+icmp) = 0.d0
            zr(jvite-1+3*(ino2-1)+icmp) = 0.d0
            zr(jacce-1+3*(ino2-1)+icmp) = 0.d0
40      continue
30  end do
!     Condition if pour le cas ETAT_INIT == None
    if (nbno1 .gt. 0) then
        idecal = 0
        ilengt = 0
        grpno = ma2//'.GROUPENO'
        do 50 iocc = 1, nbocc
            call getvtx('VIS_A_VIS', 'GROUP_NO_2', iocc, iarg, 1,&
                        nomgno, ibid)
            call jelira(jexnom(grpno, nomgno), 'LONMAX', nbno2, kbid)
            call jeveuo(jexnom(grpno, nomgno), 'L', ialin2)
            do 60 jj = 1, nbno2
                ino2 = zi(ialin2-1+jj)
                do 70 ii = 1, zi(jaconb-1+ilengt+jj)
                    ino1 = zi(jaconu-1+idecal+ii)
                    do 80 icmp = 1, 3
                        if (ddldep(icmp) .ne. 0) then
                            icmpg = ddldep(icmp)+nbcmp*(ino1-1)
                            if (zl(jdepsl-1+icmpg)) then
                                zr(jdepl-1+3*(ino2-1)+icmp) = + zr(&
                                                              jdepl-1+3*(ino2-1)+icmp) + zr(jdeps&
                                                              &v- 1+icmpg)*zr(jacocf-1+idecal+ii&
                                                              )
                            endif
                        endif
                        if (ddlvit(icmp) .ne. 0) then
                            icmpg = ddlvit(icmp)+nbcmp*(ino1-1)
                            if (zl(jvitsl-1+icmpg)) then
                                zr(jvite-1+3*(ino2-1)+icmp) = + zr(&
                                                              jvite-1+3*(ino2-1)+icmp) + zr(jvits&
                                                              &v- 1+icmpg)*zr(jacocf-1+idecal+ii&
                                                              )
                            endif
                        endif
                        if (ddlacc(icmp) .ne. 0) then
                            icmpg = ddlacc(icmp)+nbcmp*(ino1-1)
                            if (zl(jaccsl-1+icmpg)) then
                                zr(jacce-1+3*(ino2-1)+icmp) = + zr(&
                                                              jacce-1+3*(ino2-1)+icmp) + zr(jaccs&
                                                              &v- 1+icmpg)*zr(jacocf-1+idecal+ii&
                                                              )
                            endif
                        endif
80                  continue
70              continue
                idecal = idecal + zi(jaconb-1+ilengt+jj)
60          continue
            ilengt = ilengt + nbno2
50      continue
    endif
!     ! ================================ !
!     ! ENVOI DES GRANDEURS CINEMATIQUES !
!     ! ================================ !
    idim = 3 * nbno2
    nomvar = 'DEPAST'
    call cpedb(icompo, cpiter, tf, numpa4, nomvar,&
               idim, zr(jdepl), ibid4)
    nomvar = 'VITAST'
    call cpedb(icompo, cpiter, tf, numpa4, nomvar,&
               idim, zr(jvite), ibid4)
!  DEBUG
!      IF (TF .GT. 0.29) THEN
!        WRITE (6,*) 'OP0111 : DEPAST=',
!     &     ZR(JDEPL),ZR(JDEPL+IDIM-25),ZR(JDEPL+IDIM-4)
!        WRITE (6,*) 'OP0111 : VITAST=',
!     &     ZR(JVITE),ZR(JVITE+IDIM-25),ZR(JVITE+IDIM-4)
!      ENDIF
!  FIN DEBUG
!
!     ! ======================== !
!     ! LIBERATION DE LA MEMOIRE !
!     ! ======================== !
    call jedetr('&&OP0111.DEPL')
    call jedetr('&&OP0111.VITE')
    call jedetr('&&OP0111.ACCE')
    call jedema()
!     ------------------------------------------------------------------
end subroutine
