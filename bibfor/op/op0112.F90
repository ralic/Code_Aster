subroutine op0112()
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
! ======================================================================
!
!     COMMANDE:  MODI_CHAR_YACS
!  RECUPERATION DES EFFORTS DE SATURNE VIA YACS POUR COUPLAGE IFS
!
! ======================================================================
! aslint: disable=W1304
    implicit none
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/cpldb.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
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
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!
    integer :: nbval, icmp, ibid, idecal, ino2
    integer :: ino1, ii, jj, icmpg, iocc, ima, nbnog1, nbmag1
    integer ::  jchnsk, jchnsd, jchnsc,  jchnsl
    integer :: jacono,     ialin2
    integer ::   jligr, jalim1, jcxma1
    integer ::  ilengt
    integer :: nbcmpg, nbno2, nbno1, nbocc
    integer :: ddlfor(3)
    character(len=8) :: charg, modele, ma, ma1, ma2
    character(len=16) :: corres, ncmpgd(10), nomcmp(3)
    character(len=19) :: chnos, carte
    character(len=24) :: liel, grpma, grpno, nomgma, nomgno
! ======================================================================
! ======================================================================
!     COUPLAGE =>
    integer(kind=4) :: lenvar, cpiter, numpa4, nbno4, taille, ibid4
    parameter (lenvar = 144)
    character(len=lenvar) :: nomvar
    parameter (cpiter= 41)
    integer :: icompo, numpas, iadr, ifm, niv
    real(kind=8) :: ti, tf, dt
    character(len=24) :: ayacs
    integer, pointer :: flagn1(:) => null()
    real(kind=8), pointer :: force2(:) => null()
    real(kind=8), pointer :: valv(:) => null()
    integer, pointer :: pjef_nb(:) => null()
    character(len=8), pointer :: ncmp(:) => null()
    real(kind=8), pointer :: pjef_cf(:) => null()
    integer, pointer :: pjef_nu(:) => null()
    character(len=8), pointer :: nomo(:) => null()
    real(kind=8), pointer :: cnsv(:) => null()
!     COUPLAGE <=
!
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
!
!     ! ========================== !
!     ! RECUPERATION DES MOTS-CLES !
!     ! ========================== !
    call getvr8(' ', 'INST', scal=tf, nbret=ibid)
    call getvr8(' ', 'PAS', scal=dt, nbret=ibid)
    call getvis(' ', 'NUME_ORDRE_YACS', scal=numpas, nbret=ibid)
    numpa4 = int(numpas, 4)
    call getvid(' ', 'CHAR_MECA', scal=charg, nbret=ibid)
    call getvid(' ', 'MATR_PROJECTION', scal=corres, nbret=ibid)
    call getvtx(' ', 'NOM_CMP_IFS', nbval=3, vect=nomcmp, nbret=ibid)
    call getfac('VIS_A_VIS', nbocc)
    if (nbocc .lt. 1) then
        call utmess('F', 'COUPLAGEIFS_5')
    endif
    ncmpgd(1) = 'FX'
    ncmpgd(2) = 'FY'
    ncmpgd(3) = 'FZ'
    ncmpgd(4) = 'MX'
    ncmpgd(5) = 'MY'
    ncmpgd(6) = 'MZ'
    ncmpgd(7) = 'REP'
    ncmpgd(8) = 'ALPHA'
    ncmpgd(9) = 'BETA'
    ncmpgd(10) = 'GAMMA'
    nbcmpg = 10
!
!     ! =============================== !
!     ! RECUPERATION DU NOM DU MAILLAGE !
!     ! =============================== !
    call jeveuo(charg//'.CHME.MODEL.NOMO', 'L', vk8=nomo)
    modele = nomo(1)
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=ma)
!
!      ! ========================= !
!      ! CREATION D'UN CHAMPS_NO_S !
!      ! ========================= !
    chnos='&&OP0112.CHNOS'
    call cnscre(ma, 'FORC_R', nbcmpg, ncmpgd, 'V',&
                chnos)
    call jeveuo(chnos//'.CNSK', 'L', jchnsk)
    call jeveuo(chnos//'.CNSD', 'L', jchnsd)
    call jeveuo(chnos//'.CNSC', 'L', jchnsc)
    call jeveuo(chnos//'.CNSV', 'E', vr=cnsv)
    call jeveuo(chnos//'.CNSL', 'E', jchnsl)
!
!     ! ======================================== !
!     ! RECUPERATION DES POINTEURS DE PROJECTION !
!     ! ======================================== !
!      CALL JEVEUO(CORRES//'.PJEF_NO','L',JACONO)
    call jeveuo(corres//'.PJXX_K1', 'L', jacono)
    call jeveuo(corres//'.PJEF_NB', 'L', vi=pjef_nb)
    call jeveuo(corres//'.PJEF_NU', 'L', vi=pjef_nu)
    call jeveuo(corres//'.PJEF_CF', 'L', vr=pjef_cf)
!     ! ================= !
!     ! NOM DES MAILLAGES !
!     ! ================= !
    ma1 = zk24(jacono-1+1)(1:8)
    ma2 = zk24(jacono-1+2)(1:8)
!
!     ! ======================== !
!     ! VERIFICATION ELEMENTAIRE !
!     ! ======================== !
    if (ma .ne. ma1) then
        call utmess('F', 'COUPLAGEIFS_6')
    endif
!
!     ! ======================================= !
!     ! RECUPERATIONS DES DONNEES DU MAILLAGE 1 !
!     ! ======================================= !
    do icmp = 1, 3
        ddlfor(icmp) = 0
    end do
    do icmp = 1, 3
        if (nomcmp(icmp) .eq. 'FX') ddlfor(1) = 1
        if (nomcmp(icmp) .eq. 'FY') ddlfor(2) = 1
        if (nomcmp(icmp) .eq. 'FZ') ddlfor(3) = 1
    end do
!
!     ! ================================================= !
!     ! RECUPERATIONS DES NOMBRES DE NOEUDS DES MAILLAGES !
!     ! ================================================= !
    call dismoi('NB_NO_MAILLA', ma1, 'MAILLAGE', repi=nbno1)
    call dismoi('NB_NO_MAILLA', ma2, 'MAILLAGE', repi=nbno2)
    nbno4 = int(nbno2, 4)
!
!     ! ===================================== !
!     ! RECUPERATION DES FORCES DU MAILLAGE 2 !
!     ! ===================================== !
    AS_ALLOCATE(vr=force2, size=3*nbno2)
    nomvar = 'FORAST'
    ti = tf
    call cpldb(icompo, cpiter, ti, tf, numpa4,&
               nomvar, int(3*nbno4, 4), taille, force2, ibid4)
!
!     ! ====================================== !
!     ! LISTE DES NOEUDS DU MAILLAGE 1 COUPLES !
!     ! ====================================== !
    AS_ALLOCATE(vi=flagn1, size=nbno1)
    do ino1 = 1, nbno1
        flagn1(ino1) = 0
        do icmp = 1, nbcmpg
            cnsv(nbcmpg*(ino1-1)+icmp) = 0.d0
        end do
    end do
    grpma = ma1//'.GROUPEMA'
    do iocc = 1, nbocc
!        CALL GETVID('VIS_A_VIS','GROUP_MA_1',IOCC,IARG,1,NOMGMA,IBID)
        call getvtx('VIS_A_VIS', 'GROUP_MA_1', iocc=iocc, scal=nomgma, nbret=ibid)
        call jelira(jexnom(grpma, nomgma), 'LONMAX', nbmag1)
        call jeveuo(jexnom(grpma, nomgma), 'L', jalim1)
        do ii = 1, nbmag1
            ima = zi(jalim1-1+ii)
            call jelira(jexnum(ma1//'.CONNEX', ima), 'LONMAX', nbnog1)
            call jeveuo(jexnum(ma1//'.CONNEX', ima), 'L', jcxma1)
            do jj = 1, nbnog1
                ino1 = zi(jcxma1-1+jj)
                flagn1(ino1) = 1
            end do
        end do
    end do
!
!     ! =============================================== !
!     ! PROJECTIONS DES FORCES ENTRE LES DEUX MAILLAGES !
!     ! =============================================== !
    idecal = 0
    ilengt = 0
    grpno = ma2//'.GROUPENO'
    do iocc = 1, nbocc
!        CALL GETVID('VIS_A_VIS','GROUP_NO_2',IOCC,IARG,1,NOMGNO,IBID)
        call getvtx('VIS_A_VIS', 'GROUP_NO_2', iocc=iocc, scal=nomgno, nbret=ibid)
        call jelira(jexnom(grpno, nomgno), 'LONMAX', nbno2)
        call jeveuo(jexnom(grpno, nomgno), 'L', ialin2)
        do jj = 1, nbno2
            ino2 = zi(ialin2-1+jj)
            do ii = 1, pjef_nb(ilengt+jj)
                ino1 = pjef_nu(idecal+ii)
                do icmp = 1, 3
                    if (ddlfor(icmp) .eq. 1) then
                        icmpg = nbcmpg*(ino1-1)+icmp
                        cnsv(icmpg) = cnsv(icmpg) + force2(3*(ino2-1)+icmp) * &
                                             &pjef_cf(idecal+ii)
                    endif
                end do
            end do
            idecal = idecal + pjef_nb(ilengt+jj)
        end do
        ilengt = ilengt + nbno2
    end do
!
!
!     ! ===================================== !
!     ! TRANSFORMATION DU CHAMP_NO_S EN CARTE !
!     ! ===================================== !
    carte = charg//'.CHME.FORNO'
    liel = charg//'.CHME.LIGRE.LIEL'
    call jelira(liel, 'NUTIOC', nbval)
    if (nbval .ne. 1) then
        call utmess('F', 'COUPLAGEIFS_7')
    endif
    call detrsd('CARTE', carte)
    call alcart('G', carte, ma, 'FORC_R')
    call jeveuo(carte//'.NCMP', 'E', vk8=ncmp)
    call jeveuo(carte//'.VALV', 'E', vr=valv)
    call jeveuo(jexnum(liel, 1), 'L', jligr)
    do icmp = 1, nbcmpg
        ncmp(icmp) = ncmpgd(icmp)(1:8)
    end do
    idecal = 0
    do ino1 = 1, nbno1
        if (flagn1(ino1) .eq. 1) then
            idecal = idecal + 1
            do icmp = 1, 3
                valv(icmp) = cnsv(nbcmpg*(ino1-1)+icmp)
            end do
            do icmp = 4, nbcmpg
                valv(icmp) = 0.d0
            end do
            ii = zi(jligr-1+idecal)
            call nocart(carte, -3, nbcmpg, ligrel=liel, nma=1,&
                        limanu=[ii])
        endif
    end do
!
!     ! ======================== !
!     ! LIBERATION DE LA MEMOIRE !
!     ! ======================== !
    call jedetr(chnos)
    call jedetr('&&OP0112.NOGRMA')
    AS_DEALLOCATE(vr=force2)
    AS_DEALLOCATE(vi=flagn1)
    call jedema()
!
!=======================================================================
end subroutine
