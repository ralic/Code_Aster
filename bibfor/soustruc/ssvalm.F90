subroutine ssvalm(statut, option, mo, ma, isma,&
                  jresl, nbvel)
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
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jeveuo.h"
#include "asterfort/matrot.h"
#include "asterfort/ssrone.h"
#include "asterfort/ssvaro.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mo, ma
    character(len=*) :: option, statut
    integer :: isma, jresl
! ----------------------------------------------------------------------
!     BUT:
!
!         DONNER L'ADRESSE DE L'OBJET JEVEUX CONTENANT LA MATRICE
!         ELEMENTAIRE (CONDENSEE) CORRESPONDANT A L'OPTION : OPTION
!         (CET OBJET N'EST PAS FORCEMENT LA MATRICE CONDENSEE : XP_EE
!          CAR IL PEUT Y AVOIR ROTATION/SYMETRIE DE LA SOUS-STRUCTURE)
!
!     IN:    STATUT : 'DEBUT','FIN', OU ' '(COURANT)
!     ---
!
!            LES STATUTS : 'DEBUT' ET 'FIN' SERVENT A PREPARER LA BOUCLE
!                       SUR LES MATRICES ELEMENTAIRES. (OBLIGATOIRES !)
!            EXEMPLE:
!
!              DO 1  ISMA= 1, NB_MAILLES
!
!           1  CONTINUE
!
!
!
!            OPTION(K16):  'RIGI_MECA', 'MASS_MECA', 'AMOR_MECA',
!            MO(K8) : NOM DU MODELE
!            MA(K8) : NOM DU MAILLAGE
!            ISMA   : NUMERO DE LA (SUPER)MAILLE
!
!     OUT:   JRESL : ADRESSE DE LA MATRICE CONDENSEE.
!     ----   NBVEL   : NOMBRE DE VALEURS DE CETTE MATRICE.
!
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: rota
    character(len=16) :: optio2
    character(len=8) ::  nomacr
    real(kind=8) :: lambda(6, 6), angl(3), pgl(3, 3)
    character(len=24) :: nomob
!-----------------------------------------------------------------------
    integer :: i, iadesm, ianmcr, iaparr, iasssa, iavmat
    integer :: iret, j, jsma, nbsma, nbssa, nbvel, nddle
    integer :: ndim, nmxval
!-----------------------------------------------------------------------
    optio2=option
!
!     -- SI APPEL INITIAL : ON ALLOUE UN OBJET SUFFISANT :
!     ----------------------------------------------------
    if (statut(1:5) .eq. 'DEBUT') then
        call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
        call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbssa)
        if (nbssa .gt. 0) then
            call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
            call jeveuo(ma//'.NOMACR', 'L', ianmcr)
            nmxval=0
            do 10 jsma = 1, nbsma
                if (zi(iasssa-1+jsma) .eq. 1) then
                    nomacr=zk8(ianmcr-1+jsma)
                    call jeveuo(nomacr//'.DESM', 'L', iadesm)
                    nddle=zi(iadesm-1+4)
!             --LA DIMENSION DES MATRICES CONDENSEES EST DONNEE PAR
!               LA RIGIDITE:
                    ndim=nddle*(nddle+1)/2
                    nmxval=max(nmxval,ndim)
                endif
 10         continue
            if (nmxval .gt. 0) then
                call wkvect('&&SSVALM.VALEURS', 'V V R', nmxval, jresl)
            endif
        endif
    endif
!
!
!     -- SI APPEL FINAL : ON DETRUIT LES OBJETS DE TRAVAIL :
!     ------------------------------------------------------
    if (statut(1:3) .eq. 'FIN') then
        call jedetr('&&SSVALM.VALEURS')
        call jeexin('&&SSVARO.IINO', iret)
        if (iret .gt. 0) call jedetr('&&SSVARO.IINO')
    endif
!
!
!     -- SI APPEL COURANT : ON RECOPIE OU ON TOURNE :
!     -----------------------------------------------
    if (statut(1:1) .eq. ' ') then
        call jeveuo(ma//'.NOMACR', 'L', ianmcr)
        nomacr=zk8(ianmcr-1+isma)
        call jeveuo(nomacr//'.DESM', 'L', iadesm)
        nddle=zi(iadesm-1+4)
!
        if (optio2(1:4) .eq. 'RIGI') then
!          NOMOB=NOMACR//'.KP_EE'
            nomob=nomacr//'.MAEL_RAID_VALE'
        else if (optio2(1:4).eq.'MASS') then
!          NOMOB=NOMACR//'.MP_EE'
            nomob=nomacr//'.MAEL_MASS_VALE'
        else if (optio2(1:4).eq.'AMOR') then
!          NOMOB=NOMACR//'.AP_EE'
            nomob=nomacr//'.MAEL_AMOR_VALE'
        else
            ASSERT(.false.)
        endif
!
        call jeveuo(nomob, 'L', iavmat)
        nbvel=nddle*(nddle+1)/2
!
!
!       -- RECOPIE (OU ROTATION):
!       -------------------------
        call jeveuo('&&SSVALM.VALEURS', 'E', jresl)
        call ssrone(ma, isma, rota)
!
        if (rota(1:3) .eq. 'NON') then
!         RECOPIE:
            do 20 i = 1, nbvel
                zr(jresl-1+i)=zr(iavmat-1+i)
 20         continue
        else if (rota(1:3).eq.'OUI') then
!         ROTATION:
            call jeveuo(ma//'.PARA_R', 'L', iaparr)
            angl(1)=zr(iaparr-1+14*(isma-1)+4)
            angl(2)=zr(iaparr-1+14*(isma-1)+5)
            angl(3)=zr(iaparr-1+14*(isma-1)+6)
            call matrot(angl, pgl)
            do 40 i = 1, 3
                do 30 j = 1, 3
                    lambda(i,j)=pgl(i,j)
                    lambda(i,j+3)=0.d0
                    lambda(i+3,j)=0.d0
                    lambda(i+3,j+3)=pgl(i,j)
 30             continue
 40         continue
            call ssvaro(lambda, 'LG', .true., 'EXTE', nomacr,&
                        iavmat, jresl)
        else
            ASSERT(.false.)
        endif
!
        call jelibe(nomob)
    endif
!
!
!
!
end subroutine
