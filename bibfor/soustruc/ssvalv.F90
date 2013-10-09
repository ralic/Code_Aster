subroutine ssvalv(statut, nomcas, mo, ma, isma,&
                  idresl, long)
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
! INSPI  SSVALM
    implicit none
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/matrot.h"
#include "asterfort/ssrone.h"
#include "asterfort/ssvaro.h"
#include "asterfort/ssvau1.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mo, ma
    character(len=*) :: statut
    character(len=8) :: nomcas
    integer :: isma, idresl
! ----------------------------------------------------------------------
!     BUT:
!
!         DONNER L'ADRESSE DE L'OBJET JEVEUX CONTENANT LE VECTEUR
!         ELEMENTAIRE (CONDENSE) CORRESPONDANT A NOMCHAR
!         (CET OBJET N'EST PAS FORCEMENT LE VECTEUR CONDENSE : XP_EE
!          CAR IL PEUT Y AVOIR ROTATION/SYMETRIE DE LA SOUS-STRUCTURE)
!
!     IN:    STATUT : 'DEBUT','FIN', OU ' '(COURANT)
!     ---
!
!            LES STATUTS : 'DEBUT' ET 'FIN' SERVENT A PREPARER LA BOUCLE
!                       SUR LES VECTEURS ELEMENTAIRES. (OBLIGATOIRES !)
!            EXEMPLE:
!
!              DO 1  ISMA= 1, NB_MAILLES
!
!           1  CONTINUE
!
!
!            NOMCAS    :   NOM DU CAS DE CHARGE
!            MO(K8) : NOM DU MODELE
!            MA(K8) : NOM DU MAILLAGE
!            ISMA   : NUMERO DE LA (SUPER)MAILLE
!
!     OUT:   IDRESL : ADRESSE DU VECTEUR CONDENSE.
!     ----   LONG   : NOMBRE DE VALEURS DE CE VECTEUR.
!
!
! ----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) ::  nomacr, rota
    real(kind=8) :: lambda(6, 6), angl(3), pgl(3, 3)
!
!
!
!     1- SI APPEL INITIAL : ON ALLOUE UN OBJET SUFFISANT :
!     ----------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadesm, ialica, ialich, ianmcr, iaparr, iasssa
    integer :: idres2, iret, j, jsma, long, nbsma
    integer :: nbssa, nddle, nddli, nddlt, nmxval
!-----------------------------------------------------------------------
    if (statut(1:5) .eq. 'DEBUT') then
        call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
        call dismoi('NB_SS_ACTI', mo, 'MODELE', repi=nbssa)
        if (nbssa .gt. 0) then
            call jeveuo(mo//'.MODELE    .SSSA', 'L', iasssa)
            call jeveuo(ma//'.NOMACR', 'L', ianmcr)
            nmxval=0
            do 1 jsma = 1, nbsma
                if (zi(iasssa-1+jsma) .eq. 1) then
                    nomacr= zk8(ianmcr-1+jsma)
                    call jeveuo(nomacr//'.DESM', 'L', iadesm)
                    nddle = zi(iadesm-1+4)
                    nddli = zi(iadesm-1+5)
                    nddlt=nddli+nddle
                    nmxval= max(nmxval,nddlt)
                endif
  1         continue
            if (nmxval .gt. 0) then
                call wkvect('&&SSVALV.VALEURS', 'V V R', nmxval, idresl)
!           --          '&&SSVALV.VALTEMP' EST UN VECTEUR DE TRAVAIL :
                call wkvect('&&SSVALV.VALTEMP', 'V V R', nmxval, idres2)
            endif
        endif
    endif
!
!
!     2- SI APPEL FINAL : ON DETRUIT L OBJET DE TRAVAIL :
!     ---------------------------------------------------
    if (statut(1:3) .eq. 'FIN') then
        call jedetr('&&SSVALV.VALEURS')
        call jedetr('&&SSVALV.VALTEMP')
        call jeexin('&&SSVARO.IINO', iret)
        if (iret .gt. 0) call jedetr('&&SSVARO.IINO')
    endif
!
!
!     3- SI APPEL COURANT :
!     ---------------------
    if (statut(1:1) .eq. ' ') then
        call jeveuo(ma//'.NOMACR', 'L', ianmcr)
        nomacr= zk8(ianmcr-1+isma)
        call jeveuo(nomacr//'.DESM', 'L', iadesm)
        nddle = zi(iadesm-1+4)
        nddli = zi(iadesm-1+5)
        nddlt=nddli+nddle
        long= nddle
        call jeveuo(jexnom(nomacr//'.LICH', nomcas), 'L', ialich)
!
!
!       3.1- ON DETERMINE SI ON DOIT FAIRE LA ROTATION:
!       -----------------------------------------------
        call ssrone(ma, isma, rota)
!
!
!       3.2- RECOPIE (OU ROTATION) DE .LICA DANS .VALEURS :
!       ---------------------------------------------------
        call jeveuo('&&SSVALV.VALEURS', 'E', idresl)
        call jeveuo(jexnom(nomacr//'.LICA', nomcas), 'L', ialica)
!
        if (rota(1:3) .eq. 'NON') then
!         RECOPIE DU VECTEUR DEJA CONDENSE :
            do 2 i = nddli+1, nddlt
                zr(idresl-1+i)= zr(ialica-1+nddlt+i)
  2         continue
!
        else if (rota(1:3).eq.'OUI') then
!         ROTATION:
            call jeveuo(ma//'.PARA_R', 'L', iaparr)
            angl(1) = zr(iaparr-1+14*(isma-1)+4)
            angl(2) = zr(iaparr-1+14*(isma-1)+5)
            angl(3) = zr(iaparr-1+14*(isma-1)+6)
            call matrot(angl, pgl)
            do 710 i = 1, 3
                do 712 j = 1, 3
                    lambda(i,j) = pgl(i,j)
                    lambda(i,j+3) = 0.d0
                    lambda(i+3,j) = 0.d0
                    lambda(i+3,j+3) = pgl(i,j)
712             continue
710         continue
!
            if (zk8(ialich-1+1)(1:3) .eq. 'NON') then
!
!           -- LE CHARGEMENT N'EST PAS "SUIVEUR" :
                call ssvaro(lambda, 'GL', .false., 'TOUS', nomacr,&
                            ialica, idresl)
                call jeveuo('&&SSVALV.VALTEMP', 'E', idres2)
                call ssvau1(nomacr, idresl, idres2)
                call ssvaro(lambda, 'LG', .false., 'EXTE', nomacr,&
                            idres2, idresl)
!
            else if (zk8(ialich-1+1)(1:3).eq.'OUI') then
!
!           -- LE CHARGEMENT EST "SUIVEUR" :
                call ssvaro(lambda, 'LG', .false., 'EXTE', nomacr,&
                            ialica+nddlt, idresl)
            else
                call utmess('F', 'SOUSTRUC_47')
            endif
!
        else
            ASSERT(.false.)
        endif
!
!
        idresl=idresl+nddli
!
    endif
!
end subroutine
