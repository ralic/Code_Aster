subroutine dfllad(sdlist)
!
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
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/dfllvd.h"
#include "asterfort/dinogd.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
#include "asterfort/utcmp2.h"
#include "asterfort/wkvect.h"
    character(len=8) :: sdlist
!
! ----------------------------------------------------------------------
!
! OPERATEUR DEFI_LIST_INST
!
! LECTURE DES ADAPTATIONS
!
! MOT-CLEF ADAPTATION
!
! ----------------------------------------------------------------------
!
! CONSTRUCTION DE SDLIST//'.ADAP.EVENR'
!
!     ZR(JAEVR-1 + LAEVR*(IADAPT-1) + 1) <===> 'EVENEMENT'
!                                             = 0 SI 'AUCUN'
!                                             = 1 SI 'TOUT_INST'
!                                             = 2 SI  SEUIL SANS FORMULE
!                                             = 3 SI  SEUIL AVEC FORMULE
!     ZR(JAEVR-1 + LAEVR*(IADAPT-1) + 2) <===> 'NB_INCR_SEUIL'
!     ZR(JAEVR-1 + LAEVR*(IADAPT-1) + 3) <===> 'NOM_PARA'
!                                               = 1 SI 'NB_ITER_NEWTON'
!     ZR(JAEVR-1 + LAEVR*(IADAPT-1) + 4) <===> 'CRIT_COMP'
!                                               = 1 SI 'LT'
!                                               = 2 SI 'GT'
!                                               = 3 SI 'LE'
!                                               = 4 SI 'GE'
!     ZR(JAEVR-1 + LAEVR*(IADAPT-1) + 5) <===> 'VALE'
!
! CONSTRUCTION DE SDLIST//'.ADAP.EVENK'
!
!     ZR(JAEVK-1 + LAEVK*(IADAPT-1) + 1) <===> 'FORMULE_SEUIL'
!
! CONSTRUCTION DE SDLIST//'.ADAP.TPLUR'
!
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 1) <===> 'MODE_CALCUL_TPLUS'
!                                               = 1 SI 'FIXE'
!                                               = 2 SI 'DELTA_GRANDEUR'
!                                               = 3 SI 'ITER_NEWTON'
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 2) <===> 'PCENT_AUGM'
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 3) <===> 'VALE_REF'
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 4) <===> NUMERO DE LA COMPOSANTE
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 5) <===> 'NB_ITER_NEWTON_REF'
!     ZR(JATPR-1 + LATPR*(IADAPT-1) + 6) <===> NUMERO DE LA FORMULE
!                                               = 1 SI 'OLIVER'
!
! CONSTRUCTION DE SDLIST//'.ADAP.TPLUK'
!
!     ZR(JATPK-1 + LATPK*(IADAPT-1) + 1) <===> 'NOM_PARA'
!     ZR(JATPK-1 + LATPK*(IADAPT-1) + 2) <===> 'NOM_CHAM'
!     ZR(JATPK-1 + LATPK*(IADAPT-1) + 3) <===> 'NOM_CMP'
!     ZR(JATPK-1 + LATPK*(IADAPT-1) + 4) <===> 'FORMULE_TPLUS'
!
! IN  SDLIST : NOM DE LA SD RESULTAT
!
!
!
!
    character(len=16) :: mcfact
    integer :: nadapt
    integer :: ibid, nit, nbinse
    integer :: iadapt
    character(len=16) :: even, nopara, cricom, modetp, nocham
    character(len=8) :: nomgd, nocmp
    integer :: jlinr
    real(kind=8) :: pcent, valere, vale
    integer :: valei, nucmp(1)
    character(len=24) :: lisavr, lisavk, listpr, listpk
    integer :: laevr, laevk, latpr, latpk
    integer :: jaevr, jaevk, jatpr, jatpk
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    mcfact = 'ADAPTATION'
    nadapt = 0
    call jeveuo(sdlist//'.LIST.INFOR', 'E', jlinr)
!
! --- TAILLE DES VECTEURS
!
    laevr = dfllvd('LAEVR')
    laevk = dfllvd('LAEVK')
    latpr = dfllvd('LATPR')
    latpk = dfllvd('LATPK')
!
! --- NADAPT: NOMBRE D'OCCURENCE DU MOT-CLE FACTEUR 'ADAPTATION'
!
    call getfac(mcfact, nadapt)
!
! --- CREATION DES OBJETS
!
    lisavr = sdlist(1:8)//'.ADAP.EVENR'
    lisavk = sdlist(1:8)//'.ADAP.EVENK'
    listpr = sdlist(1:8)//'.ADAP.TPLUR'
    listpk = sdlist(1:8)//'.ADAP.TPLUK'
    call wkvect(lisavr, 'G V R', nadapt*laevr, jaevr)
    call wkvect(lisavk, 'G V K8', nadapt*laevk, jaevk)
    call wkvect(listpr, 'G V R', nadapt*latpr, jatpr)
    call wkvect(listpk, 'G V K16', nadapt*latpk, jatpk)
    zr(jlinr-1+10) = nadapt
!
! --- LECTURE INFOS
!
    do 200 iadapt = 1, nadapt
!
! ----- EVENEMENT POUR L'ADAPTATION
!
        call getvtx(mcfact, 'EVENEMENT', iocc=iadapt, scal=even, nbret=ibid)
        if (even .eq. 'AUCUN') then
            zr(jaevr-1+laevr*(iadapt-1)+1) = 0.d0
            call u2mess('A', 'DISCRETISATION_5')
        else if (even.eq.'TOUT_INST') then
            zr(jaevr-1+laevr*(iadapt-1)+1) = 1.d0
        else if (even.eq.'SEUIL') then
            zr(jaevr-1+laevr*(iadapt-1)+1) = 2.d0
        else
            ASSERT(.false.)
        endif
!
! ----- OPTIONS POUR L'ADAPTATION 'SEUIL'
!
        if (even .eq. 'SEUIL') then
            call getvis(mcfact, 'NB_INCR_SEUIL', iocc=iadapt, scal=nbinse, nbret=ibid)
            zr(jaevr-1+laevr*(iadapt-1)+2) = nbinse
            call getvtx(mcfact, 'NOM_PARA', iocc=iadapt, scal=nopara, nbret=ibid)
            if (nopara .eq. 'NB_ITER_NEWTON') then
                zr(jaevr-1+laevr*(iadapt-1)+3) = 1.d0
                valei = 0
                call getvis(mcfact, 'VALE_I', iocc=iadapt, scal=valei, nbret=ibid)
                vale = valei
            else
                ASSERT(.false.)
            endif
            zr(jaevr-1+laevr*(iadapt-1)+5) = vale
            call getvtx(mcfact, 'CRIT_COMP', iocc=iadapt, scal=cricom, nbret=ibid)
            if (cricom .eq. 'LT') then
                zr(jaevr-1+laevr*(iadapt-1)+4) = 1.d0
            else if (cricom.eq.'GT') then
                zr(jaevr-1+laevr*(iadapt-1)+4) = 2.d0
            else if (cricom.eq.'LE') then
                zr(jaevr-1+laevr*(iadapt-1)+4) = 3.d0
            else if (cricom.eq.'GE') then
                zr(jaevr-1+laevr*(iadapt-1)+4) = 4.d0
            else
                ASSERT(.false.)
            endif
        endif
!
! ----- DONNEES CONCERNANT LE MODE DE CALCUL DE T+
!
        call getvtx(mcfact, 'MODE_CALCUL_TPLUS', iocc=iadapt, scal=modetp, nbret=ibid)
        if (modetp .eq. 'FIXE') then
            zr(jatpr-1+latpr*(iadapt-1)+1) = 1.d0
        else if (modetp.eq.'DELTA_GRANDEUR') then
            zr(jatpr-1+latpr*(iadapt-1)+1) = 2.d0
        else if (modetp.eq.'ITER_NEWTON') then
            zr(jatpr-1+latpr*(iadapt-1)+1) = 3.d0
        else if (modetp.eq.'IMPLEX') then
            zr(jatpr-1+latpr*(iadapt-1)+1) = 5.d0
            if (even .ne. 'TOUT_INST') then
                call u2mess('F', 'DISCRETISATION_14')
            endif
        else
            ASSERT(.false.)
        endif
!
! ----- OPTIONS MODE DE CALCUL DE T+, FIXE
!
        if (modetp .eq. 'FIXE') then
            call getvr8(mcfact, 'PCENT_AUGM', iocc=iadapt, scal=pcent, nbret=ibid)
            zr(jatpr-1+latpr*(iadapt-1)+2) = pcent
        endif
!
! ----- OPTIONS MODE DE CALCUL DE T+, DELTA_GRANDEUR
!
        if (modetp .eq. 'DELTA_GRANDEUR') then
            call getvr8(mcfact, 'VALE_REF', iocc=iadapt, scal=valere, nbret=ibid)
            zr(jatpr-1+latpr*(iadapt-1)+3) = valere
            call getvtx(mcfact, 'NOM_PARA', iocc=iadapt, scal=nopara, nbret=ibid)
            call getvtx(mcfact, 'NOM_CHAM', iocc=iadapt, scal=nocham, nbret=ibid)
            call getvtx(mcfact, 'NOM_CMP', iocc=iadapt, scal=nocmp, nbret=ibid)
            ASSERT(ibid.eq.1)
            nomgd = dinogd(nocham)
            call utcmp2(nomgd, mcfact, iadapt, 1, nocmp,&
                        nucmp, ibid)
            zk16(jatpk-1+latpk*(iadapt-1)+1) = nopara
            zk16(jatpk-1+latpk*(iadapt-1)+2) = nocham
            zk16(jatpk-1+latpk*(iadapt-1)+3) = nocmp
            zr(jatpr-1+latpr*(iadapt-1)+4) = nucmp(1)
        endif
!
! ----- OPTIONS MODE DE CALCUL DE T+, ITER_NEWTON
!
        if (modetp .eq. 'ITER_NEWTON') then
            call getvis(mcfact, 'NB_ITER_NEWTON_REF', iocc=iadapt, scal=nit, nbret=ibid)
            zr(jatpr-1+latpr*(iadapt-1)+5) = nit
        endif
!
200  end do
!
    call jedema()
end subroutine
