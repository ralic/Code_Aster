subroutine op0111()
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
! person_in_charge: nicolas.greffet at edf.fr
! =====================================================================
!
!     COMMANDE:  ENV_CINE_YACS
!  ENVOI DES CHAMPS CINEMATIQUES A SATURNE VIA YACS
!
! =====================================================================
! aslint: disable=W1304
    implicit none
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/cpedb.h"
#include "asterc/getfac.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
!     ------------------------------------------------------------------
    integer :: ibid, icode, icmp, ino1, ino2, ii, idecal
    integer :: iresu, ietin, idepl, ivite, iacce, icmpg, iocc
    integer ::    jdepsv, jdepsl, jdepl
    integer ::    jvitsv, jvitsl, jvite
    integer ::    jaccsv, jaccsl, jacce
    integer :: jacono,    ialin2
    integer :: jj, ilengt
    integer :: nordre, nbno1, nbcmp, nbno2, nbocc
    integer :: ddldep(3), ddlvit(3), ddlacc(3)
    integer :: ifm, niv
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
    character(len=8), pointer :: accsk(:) => null()
    character(len=8), pointer :: depsk(:) => null()
    character(len=8), pointer :: vitsk(:) => null()
    integer, pointer :: pjef_nu(:) => null()
    integer, pointer :: pjef_nb(:) => null()
    character(len=8), pointer :: accsc(:) => null()
    character(len=8), pointer :: depsc(:) => null()
    character(len=8), pointer :: vitsc(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    integer, pointer :: accsd(:) => null()
    integer, pointer :: depsd(:) => null()
    integer, pointer :: vitsd(:) => null()
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
    call getvid(' ', 'MATR_PROJECTION', scal=corres, nbret=ibid)
    call getvr8(' ', 'INST', scal=tf, nbret=ibid)
    call getvr8(' ', 'PAS', scal=dt, nbret=ibid)
    call getvis(' ', 'NUME_ORDRE_YACS', scal=numpas, nbret=ibid)
    numpa4 = numpas
    call getfac('VIS_A_VIS', nbocc)
    call getfac('RESULTAT', iresu)
    call getfac('ETAT_INIT', ietin)
    if (iresu+ietin .gt. 1) then
        call utmess('F', 'COUPLAGEIFS_1')
    endif
    if (nbocc .lt. 1) then
        call utmess('F', 'COUPLAGEIFS_2')
    endif
    idepl = 0
    ivite = 0
    iacce = 0
    if (iresu .eq. 1) then
        call getvid('RESULTAT', 'RESU', iocc=1, scal=resu, nbret=ibid)
        call getvis('RESULTAT', 'NUME_ORDRE', iocc=1, scal=nordre, nbret=ibid)
        if (niv .eq. 2) then
            valk(1) = 'OP0111'
            valk(2) = 'NUME_ORDRE'
            call utmess('I+', 'COUPLAGEIFS_8', nk=2, valk=valk, si=nordre)
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
        call getvid('ETAT_INIT', 'DEPL', iocc=1, scal=chdepl, nbret=idepl)
        call getvid('ETAT_INIT', 'VITE', iocc=1, scal=chvite, nbret=ivite)
        call getvid('ETAT_INIT', 'ACCE', iocc=1, scal=chacce, nbret=iacce)
        if (idepl .gt. 1 .or. ivite .gt. 1 .or. iacce .gt. 1) then
            call utmess('F', 'COUPLAGEIFS_3')
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
        call jeveuo(chdeps//'.CNSK', 'L', vk8=depsk)
        call jeveuo(chdeps//'.CNSD', 'L', vi=depsd)
        call jeveuo(chdeps//'.CNSC', 'L', vk8=depsc)
        call jeveuo(chdeps//'.CNSV', 'L', jdepsv)
        call jeveuo(chdeps//'.CNSL', 'L', jdepsl)
        ma = depsk(1)
        nbno1 = depsd(1)
        nbcmp = depsd(2)
    endif
    if (ivite .eq. 1) then
        chvits = '&&IRGMCN.VITE'
        call cnocns(chvite, 'V', chvits)
        call jeveuo(chvits//'.CNSK', 'L', vk8=vitsk)
        call jeveuo(chvits//'.CNSD', 'L', vi=vitsd)
        call jeveuo(chvits//'.CNSC', 'L', vk8=vitsc)
        call jeveuo(chvits//'.CNSV', 'L', jvitsv)
        call jeveuo(chvits//'.CNSL', 'L', jvitsl)
        ma = vitsk(1)
        nbno1 = vitsd(1)
        nbcmp = vitsd(2)
    endif
    if (iacce .eq. 1) then
        chaccs = '&&IRGMCN.ACCE'
        call cnocns(chacce, 'V', chaccs)
        call jeveuo(chaccs//'.CNSK', 'L', vk8=accsk)
        call jeveuo(chaccs//'.CNSD', 'L', vi=accsd)
        call jeveuo(chaccs//'.CNSC', 'L', vk8=accsc)
        call jeveuo(chaccs//'.CNSV', 'L', jaccsv)
        call jeveuo(chaccs//'.CNSL', 'L', jaccsl)
        ma = accsk(1)
        nbno1 = accsd(1)
        nbcmp = accsd(2)
    endif
!     ! ========================================== !
!     ! APPEL DES POINTEURS DE CORRESP_2_MAILLAGES !
!     ! ========================================== !
!      CALL JEVEUO(CORRES//'.PJEF_NO', 'L', JACONO)
    call jeveuo(corres//'.PJXX_K1', 'L', jacono)
    call jeveuo(corres//'.PJEF_NB', 'L', vi=pjef_nb)
    call jeveuo(corres//'.PJEF_NU', 'L', vi=pjef_nu)
    call jeveuo(corres//'.PJEF_CF', 'L', vr=pjef_cf)
!     ! ===================================================== !
!     ! VERIFICATIONS ELEMENTAIRES SUR LES NOMS DES MAILLAGES !
!     ! ===================================================== !
    ma1 = zk24(jacono-1+1)(1:8)
    ma2 = zk24(jacono-1+2)(1:8)
    if (nbno1 .gt. 0) then
        if (ma .ne. ma1) then
            call utmess('F', 'COUPLAGEIFS_4')
        endif
    endif
!     ! ======================================= !
!     ! RECUPERATIONS DES DONNEES DU MAILLAGE 1 !
!     ! ======================================= !
    do icmp = 1, 3
        ddldep(icmp) = 0
        ddlvit(icmp) = 0
        ddlacc(icmp) = 0
    end do
    do icmp = 1, nbcmp
        if (idepl .eq. 1) then
            if (depsc(icmp) .eq. 'DX') ddldep(1) = icmp
            if (depsc(icmp) .eq. 'DY') ddldep(2) = icmp
            if (depsc(icmp) .eq. 'DZ') ddldep(3) = icmp
        endif
        if (ivite .eq. 1) then
            if (vitsc(icmp) .eq. 'DX') ddlvit(1) = icmp
            if (vitsc(icmp) .eq. 'DY') ddlvit(2) = icmp
            if (vitsc(icmp) .eq. 'DZ') ddlvit(3) = icmp
        endif
        if (iacce .eq. 1) then
            if (accsc(icmp) .eq. 'DX') ddlacc(1) = icmp
            if (accsc(icmp) .eq. 'DY') ddlacc(2) = icmp
            if (accsc(icmp) .eq. 'DZ') ddlacc(3) = icmp
        endif
    end do
!     ! ======================================= !
!     ! RECUPERATIONS DES DONNEES DU MAILLAGE 2 !
!     ! ======================================= !
    call dismoi('NB_NO_MAILLA', ma2, 'MAILLAGE', repi=nbno2)
!     ! ===================================================== !
!     ! PROJECTIONS DES DEPLACEMENTS ENTRE LES DEUX MAILLAGES !
!     ! ===================================================== !
    call wkvect('&&OP0111.DEPL', 'V V R', 3*nbno2, jdepl)
    call wkvect('&&OP0111.VITE', 'V V R', 3*nbno2, jvite)
    call wkvect('&&OP0111.ACCE', 'V V R', 3*nbno2, jacce)
    do ino2 = 1, nbno2
        do icmp = 1, 3
            zr(jdepl-1+3*(ino2-1)+icmp) = 0.d0
            zr(jvite-1+3*(ino2-1)+icmp) = 0.d0
            zr(jacce-1+3*(ino2-1)+icmp) = 0.d0
        end do
    end do
!     Condition if pour le cas ETAT_INIT == None
    if (nbno1 .gt. 0) then
        idecal = 0
        ilengt = 0
        grpno = ma2//'.GROUPENO'
        do iocc = 1, nbocc
            call getvtx('VIS_A_VIS', 'GROUP_NO_2', iocc=iocc, scal=nomgno, nbret=ibid)
            call jelira(jexnom(grpno, nomgno), 'LONMAX', nbno2)
            call jeveuo(jexnom(grpno, nomgno), 'L', ialin2)
            do jj = 1, nbno2
                ino2 = zi(ialin2-1+jj)
                do ii = 1, pjef_nb(ilengt+jj)
                    ino1 = pjef_nu(idecal+ii)
                    do icmp = 1, 3
                        if (ddldep(icmp) .ne. 0) then
                            icmpg = ddldep(icmp)+nbcmp*(ino1-1)
                            if (zl(jdepsl-1+icmpg)) then
                                zr(jdepl-1+3*(ino2-1)+icmp) = + zr(&
                                                              jdepl-1+3*(ino2-1)+icmp) + zr(jdeps&
                                                              &v- 1+icmpg)*pjef_cf(idecal+ii&
                                                              )
                            endif
                        endif
                        if (ddlvit(icmp) .ne. 0) then
                            icmpg = ddlvit(icmp)+nbcmp*(ino1-1)
                            if (zl(jvitsl-1+icmpg)) then
                                zr(jvite-1+3*(ino2-1)+icmp) = + zr(&
                                                              jvite-1+3*(ino2-1)+icmp) + zr(jvits&
                                                              &v- 1+icmpg)*pjef_cf(idecal+ii&
                                                              )
                            endif
                        endif
                        if (ddlacc(icmp) .ne. 0) then
                            icmpg = ddlacc(icmp)+nbcmp*(ino1-1)
                            if (zl(jaccsl-1+icmpg)) then
                                zr(jacce-1+3*(ino2-1)+icmp) = + zr(&
                                                              jacce-1+3*(ino2-1)+icmp) + zr(jaccs&
                                                              &v- 1+icmpg)*pjef_cf(idecal+ii&
                                                              )
                            endif
                        endif
                    end do
                end do
                idecal = idecal + pjef_nb(ilengt+jj)
            end do
            ilengt = ilengt + nbno2
        end do
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
