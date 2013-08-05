subroutine cmtref(chmat, nomail)
    implicit   none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/cmtrf2.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/imprsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/tecart.h"
#include "asterfort/u2mesg.h"
#include "asterfort/utimsd.h"
#include "asterfort/wkvect.h"
    character(len=8) :: chmat, nomail
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!  IN/VAR : CHMAT   : CHAM_MATER
!  IN     : NOMAIL  : MAILLAGE
! ----------------------------------------------------------------------
    logical :: dbg
! ----------------------------------------------------------------------
!
    integer :: iret, jdcm1, jvcm1, jlcm1, jdtrf, jvtrf, jltrf
    integer :: nbcm1, nbtrf, kcm1, ktrf, codcm1, codtrf, igd
    integer :: nccm1, nctrf, jncmp, jvalv, nucm1, nutrf, kk
    integer :: ico, ibid, nm, nbma, ninter, codint, jlint, ncm1, ntrf
    real(kind=8) :: tref, valr(2)
    character(len=8) :: k8b, mater, nocp
    character(len=8) :: ktref, nomgd
    character(len=19) :: carcm1, carcm2, cartrf
! ----------------------------------------------------------------------
!
    call jemarq()
!
    carcm1 = chmat//'.CHAMP_MAT'
    carcm2 = chmat//'.CHAMP_MA2'
    cartrf = chmat//'.TEMP    .1'
!
!     1) IL N'Y A RIEN LIEU DE FAIRE S'IL N'Y A PAS DE AFFE_VARC/'TEMP':
!     ------------------------------------------------------------------
    call jeexin(cartrf//'.DESC', iret)
    if (iret .eq. 0) goto 50
    call dismoi('F', 'NB_MA_MAILLA', nomail, 'MAILLAGE', nbma,&
                k8b, iret)
    if (nbma .eq. 0) goto 50
    call wkvect('&&CMTREF.LISMAIL', 'V V I', nbma, jlint)
!
!     2) MISE EN MEMOIRE DES OBJETS DE CARCM1 ET CARTRF :
!     ---------------------------------------------------------------
    call jeveuo(carcm1//'.DESC', 'L', jdcm1)
    call jeveuo(carcm1//'.VALE', 'L', jvcm1)
    nbcm1 = zi(jdcm1-1+3)
    igd = zi(jdcm1-1+1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    ASSERT(nomgd.eq.'NOMMATER')
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nccm1, k8b)
    ASSERT(nccm1.ge.30)
!
    call jeveuo(cartrf//'.DESC', 'L', jdtrf)
    call jeveuo(cartrf//'.VALE', 'L', jvtrf)
    nbtrf = zi(jdtrf-1+3)
    igd = zi(jdtrf-1+1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    ASSERT(nomgd.eq.'NEUT_R')
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nctrf, k8b)
!
!     3) ALLOCATION DE CARCM2 :
!     ---------------------------------------------------------------
    call alcart('V', carcm2, nomail, 'NOMMATER')
    call jeveuo(carcm2//'.NCMP', 'E', jncmp)
    call jeveuo(carcm2//'.VALV', 'E', jvalv)
!
!     4) REMPLISSAGE DE CARCM2 :
!     ---------------------------------------------------------------
    do 40,kcm1 = 1,nbcm1
    codcm1 = zi(jdcm1-1+3+2* (kcm1-1)+1)
    ASSERT(codcm1.eq.1 .or. codcm1.eq.3)
    nucm1 = zi(jdcm1-1+3+2* (kcm1-1)+2)
!        -- ON STOCKE LES NOMS DES MATERIAUX AFFECTES (28 MAX) :
    ico = 0
    nocp='MATXX'
    do 20,kk = 1,28
    mater = zk8(jvcm1-1+nccm1* (kcm1-1)+kk)
    if (mater .eq. ' ') goto 20
    ico = ico + 1
    zk8(jvalv-1+ico) = mater
    call codent(ico, 'G', nocp(4:5))
    zk8(jncmp+ico-1) = nocp
20  continue
    nm = ico
    ASSERT(nm.gt.0 .and. nm.le.28)
    zk8(jncmp-1+nm+1) = 'LREF'
    zk8(jvalv-1+nm+1) = 'TREF=>'
    zk8(jncmp-1+nm+2) = 'VREF'
!
!        -- LISTE DES MAILLES CONCERNEES PAR KCM1 :
    if (codcm1 .eq. 3) then
        call jeveuo(jexnum(carcm1//'.LIMA', nucm1), 'L', jlcm1)
        call jelira(jexnum(carcm1//'.LIMA', nucm1), 'LONMAX', ncm1, k8b)
    else
        jlcm1 = 1
        ncm1 = 0
    endif
!       -- POUR NE PAS PERDRE LES MAILLES QUI NE SONT
!          CONCERNEES PAR AUCUN KTRF :
    zk8(jvalv-1+nm+2) = 'NAN'
    if (codcm1 .eq. 1) then
        call nocart(carcm2, 1, k8b, k8b, 0,&
                    k8b, ibid, ' ', nm+2)
!
    else
        call nocart(carcm2, 3, k8b, 'NUM', ncm1,&
                    k8b, zi(jlcm1), ' ', nm+2)
    endif
!
    do 30,ktrf = 1,nbtrf
    codtrf = zi(jdtrf-1+3+2* (ktrf-1)+1)
    ASSERT(codtrf.eq.1 .or. codtrf.eq.3)
    nutrf = zi(jdtrf-1+3+2* (ktrf-1)+2)
    tref = zr(jvtrf-1+nctrf* (ktrf-1)+1)
    if (tref .eq. r8vide()) then
        ktref='NAN'
    else
        valr(1) = -275.d0
        valr(2) = 4000.d0
        if (tref .le. valr(1) .and. tref .ge. valr(2)) call u2mesg('F', 'MODELISA_22', 0, ' ', 0,&
                                                                   0, 2, valr)
        write (ktref,'(F8.2)') tref
    endif
    zk8(jvalv-1+nm+2) = ktref
!           -- LISTE DES MAILLES CONCERNEES PAR KTRF :
    if (codtrf .eq. 3) then
        call jeveuo(jexnum(cartrf//'.LIMA', nutrf), 'L', jltrf)
        call jelira(jexnum(cartrf//'.LIMA', nutrf), 'LONMAX', ntrf, k8b)
    else
        jltrf = 1
        ntrf = 0
    endif
!           -- CALCUL DE LA LISTE DES MAILLES CONCERNEES PAR KCM1/KTRF:
    call cmtrf2(codcm1, codtrf, ncm1, zi(jlcm1), ntrf,&
                zi(jltrf), nbma, codint, zi(jlint), ninter)
    ASSERT(codint.eq.1 .or. codint.eq.3)
    if (ninter .eq. 0) goto 30
!
    if (codint .eq. 1) then
        call nocart(carcm2, 1, k8b, k8b, 0,&
                    k8b, ibid, ' ', nm+2)
!
    else
        call nocart(carcm2, 3, k8b, 'NUM', ninter,&
                    k8b, zi(jlint), ' ', nm+2)
    endif
!
30  continue
    40 end do
!
!     5) RECOPIE DE CARCM2 DANS CARCM1 :
!     ---------------------------------------------------------------
!     -- DANS RCMACO/ALFINT, ON BOUCLE SUR TOUTES LES "ZONES" DE LA
!        CARTE.
!        DANS CMTREF, POUR NE PAS "PERDRE" LES MAILLES SANS
!        TEMP_REF, ON A AJOUTE PAR DEFAUT TEMP_REF=NAN
!        SI ON NE VEUT PAS SE PLANTER DANS ALFINT, IL FAUT RETIRER LES
!        ZONES "NAN" INUTILEMENT AJOUTEES.
!        POUR CELA, ON APPELLE TECART :
    call tecart(carcm2)
!
    dbg=.false.
    if (dbg) then
        call utimsd(6, 2, .false., .true., carcm1,&
                    1, ' ')
        call utimsd(6, 2, .false., .true., carcm2,&
                    1, ' ')
        call utimsd(6, 2, .false., .true., cartrf,&
                    1, ' ')
        call imprsd('CHAMP', carcm1, 6, 'CHAM_MATER:'//carcm1)
        call imprsd('CHAMP', carcm2, 6, 'CHAM_MATER:'//carcm2)
        call imprsd('CHAMP', carcm2, 6, 'CHAM_MATER:'//cartrf)
    endif
    call detrsd('CHAMP', carcm1)
    call copisd('CHAMP', 'G', carcm2, carcm1)
    call detrsd('CHAMP', carcm2)
!
!     6) MENAGE :
!     ---------------------------------------------------------------
    call jedetr('&&CMTREF.LISMAIL')
!
50  continue
    call jedema()
end subroutine
