subroutine cmtref(chmat, nomail)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
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
#include "asterfort/utimsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=8) :: chmat, nomail
! ----------------------------------------------------------------------
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
! ======================================================================
!  IN/VAR : CHMAT   : CHAM_MATER
!  IN     : NOMAIL  : MAILLAGE
! ----------------------------------------------------------------------
    aster_logical :: dbg
! ----------------------------------------------------------------------
!
    integer :: iret, jlcm1, jltrf
    integer :: nbcm1, nbtrf, kcm1, ktrf, codcm1, codtrf, igd
    integer :: nccm1, nctrf, nucm1, nutrf, kk
    integer :: ico, nm, nbma, ninter, codint, ncm1, ntrf
    real(kind=8) :: tref, valr(2)
    character(len=8) :: mater, nocp
    character(len=8) :: ktref, nomgd
    character(len=19) :: carcm1, carcm2, cartrf
    integer, pointer :: lismail(:) => null()
    character(len=8), pointer :: valv(:) => null()
    integer, pointer :: dcm1(:) => null()
    integer, pointer :: dtrf(:) => null()
    character(len=8), pointer :: ncmp(:) => null()
    character(len=8), pointer :: vcm1(:) => null()
    real(kind=8), pointer :: vtrf(:) => null()
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
    call dismoi('NB_MA_MAILLA', nomail, 'MAILLAGE', repi=nbma)
    if (nbma .eq. 0) goto 50
    AS_ALLOCATE(vi=lismail, size=nbma)
!
!     2) MISE EN MEMOIRE DES OBJETS DE CARCM1 ET CARTRF :
!     ---------------------------------------------------------------
    call jeveuo(carcm1//'.DESC', 'L', vi=dcm1)
    call jeveuo(carcm1//'.VALE', 'L', vk8=vcm1)
    nbcm1 = dcm1(3)
    igd = dcm1(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    ASSERT(nomgd.eq.'NOMMATER')
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nccm1)
    ASSERT(nccm1.ge.30)
!
    call jeveuo(cartrf//'.DESC', 'L', vi=dtrf)
    call jeveuo(cartrf//'.VALE', 'L', vr=vtrf)
    nbtrf = dtrf(3)
    igd = dtrf(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
    ASSERT(nomgd.eq.'NEUT_R')
    call jelira(jexnom('&CATA.GD.NOMCMP', nomgd), 'LONMAX', nctrf)
!
!     3) ALLOCATION DE CARCM2 :
!     ---------------------------------------------------------------
    call alcart('V', carcm2, nomail, 'NOMMATER')
    call jeveuo(carcm2//'.NCMP', 'E', vk8=ncmp)
    call jeveuo(carcm2//'.VALV', 'E', vk8=valv)
!
!     4) REMPLISSAGE DE CARCM2 :
!     ---------------------------------------------------------------
    do kcm1 = 1, nbcm1
        codcm1 = dcm1(3+2* (kcm1-1)+1)
        ASSERT(codcm1.eq.1 .or. codcm1.eq.3)
        nucm1 = dcm1(3+2* (kcm1-1)+2)
!        -- ON STOCKE LES NOMS DES MATERIAUX AFFECTES (28 MAX) :
        ico = 0
        nocp='MATXX'
        do kk = 1, 28
            mater = vcm1(nccm1* (kcm1-1)+kk)
            if (mater .eq. ' ') goto 20
            ico = ico + 1
            valv(ico) = mater
            call codent(ico, 'G', nocp(4:5))
            ncmp(ico) = nocp
 20         continue
        end do
        nm = ico
        ASSERT(nm.gt.0 .and. nm.le.28)
        ncmp(nm+1) = 'LREF'
        valv(nm+1) = 'TREF=>'
        ncmp(nm+2) = 'VREF'
!
!        -- LISTE DES MAILLES CONCERNEES PAR KCM1 :
        if (codcm1 .eq. 3) then
            call jeveuo(jexnum(carcm1//'.LIMA', nucm1), 'L', jlcm1)
            call jelira(jexnum(carcm1//'.LIMA', nucm1), 'LONMAX', ncm1)
        else
            jlcm1 = 1
            ncm1 = 0
        endif
!       -- POUR NE PAS PERDRE LES MAILLES QUI NE SONT
!          CONCERNEES PAR AUCUN KTRF :
        valv(nm+2) = 'NAN'
        if (codcm1 .eq. 1) then
            call nocart(carcm2, 1, nm+2)
!
        else
            call nocart(carcm2, 3, nm+2, mode='NUM', nma=ncm1,&
                        limanu=zi(jlcm1))
        endif
!
        do ktrf = 1, nbtrf
            codtrf = dtrf(3+2* (ktrf-1)+1)
            ASSERT(codtrf.eq.1 .or. codtrf.eq.3)
            nutrf = dtrf(3+2* (ktrf-1)+2)
            tref = vtrf(nctrf* (ktrf-1)+1)
            if (tref .eq. r8vide()) then
                ktref='NAN'
            else
                valr(1) = -275.d0
                valr(2) = 4000.d0
                if (tref .le. valr(1) .and. tref .ge. valr(2)) then
                    call utmess('F', 'MODELISA_22', nr=2, valr=valr)
                endif
                write (ktref,'(F8.2)') tref
            endif
            valv(nm+2) = ktref
!           -- LISTE DES MAILLES CONCERNEES PAR KTRF :
            if (codtrf .eq. 3) then
                call jeveuo(jexnum(cartrf//'.LIMA', nutrf), 'L', jltrf)
                call jelira(jexnum(cartrf//'.LIMA', nutrf), 'LONMAX', ntrf)
            else
                jltrf = 1
                ntrf = 0
            endif
!           -- CALCUL DE LA LISTE DES MAILLES CONCERNEES PAR KCM1/KTRF:
            call cmtrf2(codcm1, codtrf, ncm1, zi(jlcm1), ntrf,&
                        zi(jltrf), nbma, codint, lismail, ninter)
            ASSERT(codint.eq.1 .or. codint.eq.3)
            if (ninter .eq. 0) goto 30
!
            if (codint .eq. 1) then
                call nocart(carcm2, 1, nm+2)
!
            else
                call nocart(carcm2, 3, nm+2, mode='NUM', nma=ninter,&
                            limanu=lismail)
            endif
!
 30         continue
        end do
    end do
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
        call utimsd(6, 2, .false._1, .true._1, carcm1,&
                    1, ' ')
        call utimsd(6, 2, .false._1, .true._1, carcm2,&
                    1, ' ')
        call utimsd(6, 2, .false._1, .true._1, cartrf,&
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
    AS_DEALLOCATE(vi=lismail)
!
 50 continue
    call jedema()
end subroutine
